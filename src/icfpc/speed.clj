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

(def warn-on-generic (atom true))

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
         getter (slot-getter (eval (or tag 'Object)))]
     (slot-binds coll getter  flds))))

(defn key-binds
  ([coll get-key flds]
   (flat-binds
    (for [f flds]
      `[~f ~(get-key coll (keyword f))])))
  ([coll flds]
   (let [tag (-> coll meta :tag)
         getter (key-getter (eval (or tag 'Object)))]
     (key-binds coll getter flds))))

(defn ensure-distinct [& ks]
  (let [shared  (map first
                     (filter (fn [[k v]]
                               (> v 1)) (frequencies (apply concat ks))))]
    (when (seq shared)
      (throw (ex-info "duplicate fields or keys detected, check your destructuring form!"
                      {:shared shared })))))

(defn generic-warning [fields coll]
  (let [getter (cond (vector? fields) :nth
                     (map? fields) :get)]
    (println
     [:with-slots.warning/using-generic
      getter
      :ns *ns*
      :fields fields
      :coll   coll
      :try-hinting
      (case getter
        :nth
        '[clojure.lang Indexed IPersistentVector java.util.List]
        :get '[clojure.lang Associative IPersistentMap, java.util.Map])])))

(defn as-binds
  ([fields coll]
   (let [tag    (or (some-> coll meta :tag name symbol)
                    (cond (map? coll)   'clojure.lang.IPersistentMap
                          (vector? coll) 'clojure.lang.IPersistentVector
                          (set? coll)    'clojure.lang.IPersistentSet
                          :else          'Object))
         tagged (with-meta (gensym "coll") {:tag tag})]
     (as-binds tagged fields coll)))
  ([tagged fields coll]
   (let [tag (-> tagged meta :tag)]
     (cond  (= tag 'Object)
            (do (assert (not (:fields fields)) "cannot use :fields key with untyped object!")
                (when @warn-on-generic
                  (generic-warning fields coll))
                `[~fields ~coll])
            (map? fields) ;;{:keys flds :as something}      
            (let [flds   (get fields :fields)
                  ks     (get fields :keys)
                  _      (ensure-distinct flds ks)
                  tgt    (when-let [res (get fields :as)]
                           (if (-> res meta :tag)
                             res
                             (with-meta res {:tag tag})))]
              `[~tagged ~coll
                ~@(into (if tgt `[~tgt ~coll] [])
                        (concat (field-binds tagged flds)
                                (key-binds   tagged ks)))])
            (vector? fields)
            (do (ensure-distinct fields)
                (if (= tagged coll)
                  `[~@(slot-binds tagged fields)]
                  `[~tagged ~coll
                    ~@(slot-binds tagged fields)]))
            :else (throw
                   (ex-info "invalid field spec"
                            {:fields fields :expected [:or :map :vector]}))))))
  
#_(defmacro with-fields
  [fields coll & body]
  (let [[_ fields coll & rest] &form]
    `(let [~@(as-binds fields coll)]
       ~@body)))

(defn scrape-symbols [l]
  (cond (symbol? l) l
        (map? l)    (concat (:fields l)
                            (:keys l))
        (vector? l) l
        :else nil))
        
(defn unify-binds [bindings]
  (let [n       (count bindings)
        _  (assert (and (vector? bindings)
                        (pos? n)
                        (even? n)) "slot bindings must be a vector with an even number of entries!")
        tags (atom {})       
        get-tag (fn [x]
                  (let [k (name x)]
                    (if-let [t (-> x meta :tag)]
                      (if-not (@tags k)
                        (do (swap! tags assoc k t)
                            t)
                        (do (when (not= t (@tags k))
                              (println [:with-slots.warning/changed-tags
                                        :symbol x :tag t :previously (@tags k)]))
                            t))
                      (@tags k))))
        tag (fn [x]
              (if-let [t (get-tag x)]
                (with-meta x {:tag t})
                x))
        tag-bind (fn [[l r]]
                   (let [scrape? (or (map? l) (vector? l))
                         xs (when scrape? (mapv tag (scrape-symbols l)))]
                     [l (if (symbol? r) (tag r) r)]))
        n (atom 0)
        res  (apply concat (for [b (partition 2 bindings)]
                             (let [[l r] (tag-bind b)
                                   re-use? (and (pos? @n)
                                                (symbol? r))
                                   _ (swap! n inc)]
                               (cond (symbol? l) [l r]
                                     re-use?     (as-binds r l r)
                                     :else       (as-binds l r)))))]
    res))

(defmacro with-slots
  "Allows for efficient, type-based destructuring similar to the
  idiomatic destructuring forms of Clojure, with some limitations.
  Bindings are presented as the typical vector, with an even number of
  entries, where the preceding odd binding establishes binds for the
  even successor.  Unlike typical forms, bindings leverage
  type-hinting information - both on the left hand side and the right
  hand side - to establish efficient operations beyond the generic
  destructuring forms established with maps and vectors, e.g. get and
  nth.

  Callers may use {:fields [a b ^clojure.lang.Counted c] }, along with
  a type-hinted rhs, to denote establishing bindings for a, b, c, by
  invoking like-named direct, type-hinted field applications on the
  rhs, ala (.a ^some-type rhs).

  Any binding var hinted on the LHS will propogate its hint throughout
  later bindings.  This allows an expressive form of efficient
  destructuring for the consenting adult, which allows idiomatic
  expressivity without the accompanying significant loss of
  performance.

  map destructuring for {:keys [...]} follows that of :fields, except
  the bindings are established via either a (.valAt ..) or (.get ..)
  or (get ...) depending on the presented type, get being the fallback.
  This allows usage with types supporting the java.util.Map interface.
  Literal maps are automatically inferred with efficient getters.

  Vector or indexed destructuring is similarly supported,
  [^some-type x y] ^clojure.lang.Indexed coll will invoke efficient
  .nth indexing operations rather than the slower, more general nth.
  Depending on the presented type, either .nth, .get, or nth will be
  used, allowing operation with structures supporting the
  java.util.List interface.  Literal vectors are automatically
  inferred with efficient getters.  The & rest notation is currently
  NOT supported...

  The remaining rules act identically to let semantics.  If a symbol
  is bound to the LHS, then the binding is passed through
  untouched (including hints).

  with-slots tries to scan the input bindings to find
  discrepancies (such as duplicate binds), and to re-use existing
  hinted information for binds.  In the case that the user decides to
  re-hint a RHS var that has already been hinted a-priori, with-slots
  will allow the hint for that binding, but revert to prior hinting
  unless the user continues to specify new hints.  This seems rare in
  practice.

  It's common to import the symbols for the
  [clojure.lang Counted Indexed] interfaces when using with-slots.

  An example:
  
  (with-slots
    [{:fields [^Counted path
               ^Indexed position]} ^botmove (->botmove [] [1 2])
     {:keys [a b] :fields [hashCode]}    {:a 2 :b 3}
     [x y]          position         
     path-length   (.count path)]
    [hashCode (+ x y)])
  "
  [bindings & body]
  (let [[_ bindings & body] &form]
    `(let [~@(unify-binds bindings)]
       ~@body)))

(comment ;testing

  (defrecord botmove [path position])
 
  (binding [*print-meta* true]
    (pprint
     (macroexpand-all
      '(with-slots [{:fields [^clojure.lang.Counted path  ^clojure.lang.Indexed position]} ^botmove move                                  
                    [x y]         pos         
                    path-length   (.count path)
                    blah  2
                    blee  (map inc (range 10))]
         2))))
  
  (with-slots [{:fields [^clojure.lang.Counted path
                         ^clojure.lang.Indexed position]} ^botmove (->botmove [] [1 2])
               {:keys [a b] :fields [hashCode]}    {:a 2 :b 3}
                [x y]          position         
               path-length   (.count path)]
    [hashCode (+ x y)])
  
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
