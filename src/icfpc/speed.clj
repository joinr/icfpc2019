(ns icfpc.speed
  (:require [criterium.core :as c]))

;;let's define some macros that let us leverage type hints, unpack
;;destructured vectors, and other goodies that allow us to use extant
;;expressive code, but apply significant optimizations that add up on
;;hot paths....

;;Some basic transforms we'd like to apply in argvectors:

;;[[x y :as xy] ] =>
;; [^clojure.lang.Indexed xy]
;;  (let [x (.nth xy 0)
;;        y (.nth xy 1) ]
;;     ....)

;;we'd like to type hint fields for types and structs...

;;It'd be "really nice" if the type provided could be reflected on so
;;the keys would be automatic field lookups or else delegated to
;;valAt, or in the case of a generic class, fall back to the (get ..)
;;behavior.  Also maybe allow the keys to be hinted?

;;[{:keys [x y] :as ^level m}] =>
;; [^level m]
;;  (let [x (.x m)
;;        y (.y m)]
;;             )

(defn flat-binds [xs]
  (reduce (fn [acc [l r]]
            (conj acc l r))
          [] xs))

(defn can-be? [^Class super ^Class child]
  (.isAssignableFrom super  child))

(defn looks-like? [class obj]
  (or (instance? class obj)
      (can-be? class (type obj))))

(defn field-binds [coll flds]
  (flat-binds
   (for [f flds]
     `[~f (~(symbol (str "." f)) ~coll)])))

(defn slot-getter [class]
  (let [f (cond (or (can-be? clojure.lang.IPersistentVector class)           
                    (can-be? clojure.lang.Indexed class))
                '.nth
                
                (can-be? java.util.List class)
                '.get
                
                (can-be? clojure.lang.IFn class)
                'invoke
                :else
                'nth)]
    (fn [coll idx]
      `(~f ~coll ~idx))))
      
(defn key-getter [class]
  (let [f (cond (or (can-be? clojure.lang.IPersistentMap class)
                    (can-be? clojure.lang.IPersistentSet class)
                    (can-be? clojure.lang.Associative class)
                    )
                '.valAt
                
                (can-be? java.util.Map class)
                '.get
                
                (can-be? clojure.lang.IFn class)
                'invoke
                :else
                'get)]
    (fn [coll k]
      `(~f ~coll ~k))))

(defn slot-binds
  ([coll get-slot flds]
   (flat-binds
    (for [[idx f] (map-indexed vector flds)
          :when (not= f '_)]
      `[~f ~(get-slot coll idx)])))
  ([coll flds]
   (let [tag (-> coll meta :tag)
         getter (slot-getter (eval tag))]
     (slot-binds coll getter  flds))))

(defn key-binds
  ([coll get-key flds]
   (flat-binds
    (for [f flds]
      `[~f ~(get-key coll (keyword f))])))
  ([coll flds]
   (let [getter (-> coll meta :tag eval key-getter)]
     (key-binds coll getter flds))))

(defn ensure-distinct [& ks]
  (let [shared  (map first
                     (filter (fn [[k v]]
                               (> v 1)) (frequencies (apply concat ks))))]
    (when (seq shared)
      (throw (ex-info "duplicate fields or keys detected, check your destructuring form!"
                      {:shared shared })))))
    
(defmacro with-fields
  [fields coll & body]
  (let [[_ flds coll & rest] &form
        tag    (or (some-> coll meta :tag name symbol)
                   (cond (map? coll)   'clojure.lang.IPersistentMap
                         (vector? coll) 'clojure.lang.IPersistentVector
                         (set? coll)    'clojure.lang.IPersistentSet
                         :else          'Object))
        tagged (with-meta (gensym "coll") {:tag tag})]
    (cond  (= tag 'Object)
           (do (assert (not (:fields flds)) "cannot use :fields key with untyped object!")
               `(let [~fields ~coll]
                  ~@body))
           (map? fields) ;;{:keys flds :as something}         
           (let [flds   (get fields :fields)
                 ks     (get fields :keys)
                 _      (ensure-distinct flds ks)
                 tgt    (when-let [res (get fields :as)]
                          (if (-> res meta :tag)
                            res
                            (with-meta res {:tag tag})))]
              `(let [~tagged ~coll
                     ~@(into (if tgt `[~tgt ~coll] [])
                             (concat (field-binds tagged flds)
                                     (key-binds   tagged ks)))]
                 ~@body))          
           (vector? fields)
           (do _ (ensure-distinct flds)
               `(let [~tagged ~coll
                      ~@(slot-binds tagged flds)]
                  ~@body))
          :else (throw
                 (ex-info "invalid field spec"
                          {:fields fields :expected [:or :map :vector]})))))

(defmacro with-slots [bindings & body]
  (let [_ (assert (vector? bindings) "slot bindings must be a vector!")
        [_ bindings & body] &form
        n  (count bindings)]
    (cond (zero? n) `~(first body)
          (= n 2)   (let [[l r]  bindings]
                      `(with-fields ~l ~r ~@body))
          (even? n)
          (let [[l r] (vec (take 2 bindings))]
            `(with-fields ~l ~r
               (with-slots ~(vec (drop 2 bindings)) ~@body)))
          :else (throw (ex-info "bindings must have an even number of entries!" {})))))



(comment ;testing
  
#_(binding [*print-meta* true]
  (pprint
   (macroexpand-1
    (macroexpand-1
     '(with-slots  [{:keys [a] :fields [x y]}  ^java.util.Map  m
                    [j k _ i]  [1 2 3]]
        (+ a x y))))))


(defn test0  [m v]
  (let [{:keys [x y]}    m
        [a b]            v]
    (+ x y a b)))

;; icfpc.speed> (let [p (->pair 1 2) v [3 4]] (c/quick-bench (test0 p v)))
;; Evaluation count : 8058756 in 6 samples of 1343126 calls.
;;              Execution time mean : 73.013581 ns
;;     Execution time std-deviation : 1.607189 ns
;;    Execution time lower quantile : 71.567415 ns ( 2.5%)
;;    Execution time upper quantile : 75.222269 ns (97.5%)
;;                    Overhead used : 1.794404 ns

(defn test1 [m v]
  (with-slots [{:fields [x y]} ^pair m
               [a b]           ^clojure.lang.PersistentVector v]
    (+ x y a b)))

;; icfpc.speed> (let [p (->pair 1 2) v [3 4]] (c/quick-bench (test1 p v)))
;; Evaluation count : 25859514 in 6 samples of 4309919 calls.
;;              Execution time mean : 21.713334 ns
;;     Execution time std-deviation : 0.562775 ns
;;    Execution time lower quantile : 21.204084 ns ( 2.5%)
;;    Execution time upper quantile : 22.387651 ns (97.5%)
;;                    Overhead used : 1.794404 ns        

)

(comment 
;;becomes with-fields...
(with-fields [{:keys [x y] :as  m} ^pair m]
  (+ x y))

;;becomes with-keys...
(with-fields [{:keys [x y] :as  m} m]
  (+ x y))

;;becomes with-slots
(with-fields [[x y] m]
  (+ x y))

(with-fields [[x y] m]
  (+ x y))
)

;;testing
(comment
  
(defrecord pair [x y])


(defn raw [{:keys [x y] :as m}]
  (+ x y))

;; icfpc.speed> (let [p (->pair 10 20)] (c/quick-bench (raw p)))
;; Evaluation count : 10322586 in 6 samples of 1720431 calls.
;;              Execution time mean : 56.816063 ns
;;     Execution time std-deviation : 0.971773 ns
;;    Execution time lower quantile : 55.210343 ns ( 2.5%)
;;    Execution time upper quantile : 57.832995 ns (97.5%)
;;                    Overhead used : 1.794404 ns


  
(defn flds [m]
  (with-fields ^pair m [x y]
    (+ x y)))

;;6x faster.
;; icfpc.speed> (let [p (->pair 10 20)] (c/quick-bench (flds p)))
;; Evaluation count : 54464514 in 6 samples of 9077419 calls.
;;              Execution time mean : 9.376441 ns
;;     Execution time std-deviation : 0.316239 ns
;;    Execution time lower quantile : 8.950396 ns ( 2.5%)
;;    Execution time upper quantile : 9.688262 ns (97.5%)
;;                    Overhead used : 1.794404 ns
)

(comment
  ;;we'd like something along the following
  ;;transforms...
  (defn blah [{:fields [x y] :as ^level m} k]
    (+ x y k))

  (defn blah [m k]
    (with-fields [^level m [x y]]
      (+ x y k)))

  (defn blah [m k]
    (let [x     (.x ^level m)
          y     (.y ^level m)]
      (+ x y k)))

  ;;we want to do the same thing for indices..
  (defn blah [^Indexed [x y :as xy]]
    (+ x y))

  (defn blah [^Indexed xy]
    (let [x (.nth xy 0)
          y (.nth xy 1)]
      (+ x y)))
)

;;some higher minded reflection stuff...
;;to improve field detection at compile time., maybe later.

;stolen from stack overflow
;; (defn static? [field]
;;   (java.lang.reflect.Modifier/isStatic
;;    (.getModifiers field)))

;; (defmacro record-headers
;;   "Returns a vector of the field names from a record type."
;;   [recname]
;;   (let [rname (symbol (str "->" recname))]
;;   `(vec (map str (first (:arglists (meta #'~rname)))))))

;; (defn get-record-field-names [record]
;;   (->> record
;;        .getDeclaredFields
;;        (remove static?)
;;        (map #(.getName %))
;;        (remove #{"__meta" "__extmap"})))

;;destructuring reproduced from clojure.core...
;;the ideal, language-level solution is to
;;inject field-based destructuring directly
;;and not have to jump through hoops.
;;It looks like we can do that and
;;provide our own let, fn, defn, defmacro
;;forms...that's a bit more invasive and
;;less specialized than I'd like to go at
;;this point, but could be very useful
;;for performance-oriented stuff...


;; icfpc.speed> (#'clojure.core/destructure '[{:keys [x y] :as m} b])
;; [map__8975
;;  b
;;  map__8975
;;  (if
;;   (clojure.core/seq? map__8975)
;;   (clojure.lang.PersistentHashMap/create
;;    (clojure.core/seq map__8975))
;;   map__8975)
;;  m
;;  map__8975
;;  x
;;  (clojure.core/get map__8975 :x)
;;  y
;;  (clojure.core/get map__8975 :y)]
