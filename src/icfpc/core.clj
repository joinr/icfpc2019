(ns icfpc.core)

(defn queue [& xs]
  (into clojure.lang.PersistentQueue/EMPTY xs))

(defn spy [x]
  (println x)
  x)

(defmacro cond+ [& clauses]
  (when clauses
    (let [[c1 c2 & cs] clauses]
      (cond
        (< (count clauses) 2) (throw (IllegalArgumentException. "cond requires an even number of forms"))
        (= c1 :let)          `(let ~c2 (cond+ ~@cs))
        (= c1 :do)           `(do ~c2 (cond+ ~@cs))
        (= c1 :when-some)    `(if-some ~c2 ~(first cs) (cond+ ~@(next cs)))
        :else                `(if ~c1 ~c2 (cond+ ~@cs))))))

#_(defrecord Point [x y]
  Object
  (toString [_] (str "(" x "," y ")"))
  clojure.lang.Indexed
  (nth [_ i] (case i 0 x 1 y))
  (nth [_ i nf] (case i 0 x 1 y nf)))

;;fastmath
;;https://github.com/generateme/fastmath/blob/master/src/fastmath/vector.clj
(defn- dhash-code
  "double hashcode"
  (^long [^long state ^double a]
   (let [abits (Double/doubleToLongBits a)
         elt (bit-xor abits (unsigned-bit-shift-right abits 32))]
     (unchecked-add elt (unchecked-multiply 31 state))))
  (^long [^double a]
   (let [abits (Double/doubleToLongBits a)
         elt (bit-xor abits (unsigned-bit-shift-right abits 32))]
     (unchecked-add elt 31))))

(deftype Point [^long x ^long y
                ^:unsynchronized-mutable ^int _hasheq
                ^:unsynchronized-mutable ^int _hash] 
  Object
  (toString [_] (str "(" x "," y ")"))
  ;;technique shamelessly borrowed from fastmath!
  (equals [_ v]
    (and (instance? Point v)
         (let [^Point v v]
           (and (== x (.x v))
                (== y (.y v))))))
  clojure.lang.IHashEq
  (hasheq [this]
    (if (== _hasheq (int -1))
      (let [h (unchecked-int (dhash-code (dhash-code x) y))]
        (do (set! _hasheq (int h))
            h))
      _hasheq))
  (hashCode [this]
    (if (== _hash (int -1))
      (let [h (unchecked-int (dhash-code (dhash-code x) y))]
        (do (set! _hash (int h))
            h))
      _hash))
  (hashCode [_]
    (try (unchecked-int (dhash-code (dhash-code x) y))
         (catch Exception e (throw (ex-info "bad hash!" {:x x :y y})))))
  clojure.lang.Indexed
  (nth [_ i]    (case i 0 x 1 y))
  (nth [_ i nf] (case i 0 x 1 y nf))
  clojure.lang.ILookup
  (valAt [_ k]    (case k :x x :y y (throw (ex-info "invalid-key!" {:unknown-key k}))))
  (valAt [_ k nf] (case k :x x :y y nf))
  java.util.Map
  (get [this k] (.valAt this k))
  (put    [this k v]  (throw (ex-info "unsupported-op!" {})))
  (putAll [this c] (throw (ex-info "unsupported-op!" {})))
  (clear  [this] (throw (ex-info "unsupported-op!" {})))
  (containsKey   [this k]
    (case k :x true :y true false))
  (containsValue [this o]
    (throw (ex-info "unsupported-op!" {})))
  (entrySet [this] (set (seq {:x x :y y})))
  (keySet   [this] #{x y}))

(defn ->Point [x y]
  (Point. (long x) (long y)))



(defrecord lev
    [name
     ^int  width
     ^int  height
     ^bytes grid
     ^bytes zones-grid
     zones-area
     ^shorts weights
     bots
     empty
     collected-boosters
     spawns
     boosters]
  clojure.lang.IFn
  (invoke [this k] (.valAt this k)))
    

(defn l-coord->idx ^long [^lev level x y] (+ x (* y (.width level))))
(defn l-get-level
  ([^lev level x y]
   (aget ^bytes (.grid level) (l-coord->idx level x y)))
  ([^lev level x y default]
   (if (and
         (< -1 x (.width level))
         (< -1 y (.height level)))
     (aget ^bytes (.grid level) (l-coord->idx level x y))
     default)))

(defn coord->idx ^long [level x y]
  (if (instance? icfpc.core.lev level)
    (l-coord->idx level x y)
    (+ x (* y (:width level)))))

(defn get-level
  ([level x y]
   (if (instance? icfpc.core.lev level)
    (l-get-level level x y)
    (aget ^bytes (:grid level) (coord->idx level x y))))
  ([level x y default]
   (if (instance? icfpc.core.lev level)
     (l-get-level level x y default)
     (if (and
          (< -1 x (:width level))
          (< -1 y (:height level)))
       (aget ^bytes (:grid level) (coord->idx level x y))
       default))))

(def EMPTY (byte 0))
(def OBSTACLE (byte 1))
(def WRAPPED (byte 2))
(def EXTRA_HAND \B)
(def FAST_WHEELS \F)
(def DRILL \L)
(def SPAWN \X)
(def TELEPORT \R)
(def CLONE \C)
(def WAIT \Z)
(def UNKNOWN \?)

(def UP    \W)
(def DOWN  \S)
(def LEFT  \A)
(def RIGHT \D)
(def ROTATE_CW  \E)
(def ROTATE_CCW \Q)
(def SET_BEAKON \R)
(def JUMP       \T)
(def REPLICATE  \C)

(defn spend
  ([v] (cond (nil? v) nil (> v 1) (dec v) :else 0))
  ([map k k2]
   (let [v ((map k) k2)]
     (cond
       (nil? v) map
       (> v 1)  (update map k assoc k2 (dec v))
       :else    (update map k dissoc k2)))))

(defn coord->idx ^long [level x y] (+ x (* y (:width level))))
(defn get-level
  ([level x y]
   (aget ^bytes (:grid level) (coord->idx level x y)))
  ([ level x y default]
   (if (and
         (< -1 x (:width level))
         (< -1 y (:height level)))
     (aget ^bytes (:grid level) (coord->idx level x y))
     default)))

(defn set-level [level x y value]
  (aset-byte (:grid level) (coord->idx level x y) value)
  level)

(defn get-zone [level x y]
  (aget ^bytes (:zones-grid level) (coord->idx level x y)))

(defn zone-area [level zone]
  ((:zones-area level) zone))

(defn seek [pred coll]
  (some #(if (pred %) %) coll))

(defn path-score [path]
  (count (re-seq #"[A-Z]" path)))

(defn sol-score [sol]
  (->> (clojure.string/split sol #"#")
    (map path-score)
    (reduce max)))

(defn level-score [level]
  (->> (:bots level)
    (map #(path-score (:path %)))
    (reduce max)))

(defn arr-reduce [f init ^bytes arr]
  (areduce arr i ret init
    (f ret (aget arr i))))
