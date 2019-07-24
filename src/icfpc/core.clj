(ns icfpc.core
  (:require [clojure.walk :as walk]))

(defn queue [& xs]
  (into clojure.lang.PersistentQueue/EMPTY xs))

(defn spy [x]
  (println x)
  x)


;;despite the direct method invocations, this is actually
;;apparently less inlineable than the assoc variant...
;;we get an edge if we use assoc (at least for
;;persistent maps).  Fascinating.
#_(defmacro assoc* [m & kvs]
  (let [sym (with-meta (gensym "map") {:tag 'clojure.lang.Associative})]
    `(let [~sym ~m] (-> ~sym  ~@(for [[k f] (partition 2 kvs)] `(.assoc ~k ~f))))))

(defmacro assoc*
  "Expanded version of the variadic implementation of clojure.core/assoc,
   except it avoids the variadic call, saving some performance."
  [m & kvs]
  (let [sym (with-meta (gensym "map") {:tag 'clojure.lang.Associative})]
    `(let [~sym ~m] (-> ~sym  ~@(for [[k f] (partition 2 kvs)] `(assoc ~k ~f))))))


(defmacro cond+ [& clauses]
  (when clauses
    (let [[c1 c2 & cs] clauses]
      (cond
        (< (count clauses) 2) (throw (IllegalArgumentException. "cond requires an even number of forms"))
        (= c1 :let)          `(let ~c2 (cond+ ~@cs))
        (= c1 :do)           `(do ~c2 (cond+ ~@cs))
        (= c1 :when-some)    `(if-some ~c2 ~(first cs) (cond+ ~@(next cs)))
        :else                `(if ~c1 ~c2 (cond+ ~@cs))))))

;;somehow condp is faster than mine; they look identical, although the
;predicate is bound earlier...
(defmacro case-if [k & kvs]
  (let [n (count kvs)
        _ (assert (if (odd? n) (> n 1) true)
                  "either use even cases or odd with final as a default")
        s (gensym "k")]
    `(condp identical? ~k ~@kvs)))

(defmacro defrecord+
  "Like defrecord, but adds default map-like function application
   semantics to the record.  Fields are checked first in O(1) time,
   then general map lookup is performed.  Users may supply and optional
   ^:static hint for the arg vector, which will enforce the invariant
   that the record always and only has the pre-defined fields, and
   will throw an exception on any operation that tries to access
   fields outside of the predefined static fields.  This moves
   the record into more of a struct-like object.

   Note: this is not a full re-immplementation of defrecord,
   and still leverages the original's code emission.  The main
   difference is the implementation of key-lookup semantics
   ala maps-as-functions, and drop-in performance that should
   be equal-to or superior to the clojure.core/defrecord
   implementation.  Another refinement that makes arraymaps
   superior for fields <= 8, is the avoidance of a case dispatch
   which is slower in practice than a linear scan or a
   sequential evaluation of if identical? expressions.
   Small records defined in this way should be competitive
   in general purpose map operations."
  [name keys & impls]
  (let [fields (map keyword keys)
        binds  (reduce (fn [acc [l r]]
                     (conj acc l r))
                   []
                   (map vector fields (map #(with-meta % {})  keys)))
        
        [_ name keys & impls] &form
        this (gensym "this")
        k    (gensym "k")
        default (gensym "default")
        n       (count keys)
        caser   (if (<= n 8) 'case-if 'case)
        rform (->> `(~'defrecord ~name ~keys ~@impls
                     ~'clojure.lang.IFn
                     (~'invoke [~this ~k]
                      (~caser ~k
                       ~@binds
                       ~(if (-> keys meta :strict)
                          `(throw (ex-info "key not in strict record" {:key ~k}))
                          `(.valAt ~this ~k))))
                     (~'invoke [~this ~k ~default]
                      (~caser ~k
                       ~@binds
                       ~(if (-> keys meta :strict)
                          `(throw (ex-info "key not in strict record" {:key ~k}))
                          `(.valAt ~this ~k ~default)))))
                    macroexpand-1
                    (walk/postwalk-replace {'clojure.core/case caser
                                            'case caser}))
        ]
    `(~@rform)))

(defmacro <*
  "Evaluates exprs one at a time, from left to right, ensuring
   < property holds"
  {:added "1.0"}
  ([x] true)
  ([x y] `(< ~x ~y))
  ([x y & next]
   `(let [le# ~x]
      (and (< ~x ~y)
           (<* ~y ~@next)))))

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
                ^:unsynchronized-mutable ^int _hash
                ] 
  Object
  (toString [_] (str "(" x "," y ")"))
  ;;technique shamelessly borrowed from fastmath!
  (equals [this v]
    (and (instance? Point v)
         (and (== x (.x ^Point v))
              (== y (.y ^Point v)))))
  clojure.lang.IHashEq
  (hasheq [this]
    (if (== _hasheq (int -1))
      (let [h (.hashCode this)]
        (do (set! _hasheq (int h))
            h))
      _hasheq))
  (hashCode [this]
    (if (== _hash (int -1))
      (let [h (mix-collection-hash
               (unchecked-int (dhash-code (dhash-code x) y)) 2)]
        (do (set! _hash (int h))
            h))
      _hash))
  clojure.lang.Indexed
  (nth [_ i]    (case i 0 x 1 y))
  (nth [_ i nf] (case i 0 x 1 y nf))
  clojure.lang.ILookup
  (valAt [_ k]    (case-if k :x x :y y (throw (ex-info "invalid-key!" {:unknown-key k}))))
  (valAt [_ k nf] (case-if k :x x :y y nf))
  java.util.Map
  (get [this k] (.valAt this k))
  (put    [this k v]  (throw (ex-info "unsupported-op!" {})))
  (putAll [this c] (throw (ex-info "unsupported-op!" {})))
  (clear  [this] (throw (ex-info "unsupported-op!" {})))
  (containsKey   [this k]
    (case-if k :x true :y true false))
  (containsValue [this o]
    (throw (ex-info "unsupported-op!" {})))
  (entrySet [this] (set (seq {:x x :y y})))
  (keySet   [this] #{x y})
  )

#_(definline ->Point [x y]
  `(Point. (long ~x) (long ~y) -1 -1))

(defn ^Point ->Point [x y]
  (Point. (long x) (long y) -1 -1))

(defprotocol ILevel
  (lev-width   [level])
  (lev-height  [level])
  (lev-grid    [level])
  (lev-weights [level])
  (lev-zones   [level]))

(defprotocol IFringe
  (has-fringe? [this o])
  (add-fringe  [this o])
  (clear-fringe! [this]))

(definterface IByteMap
  (getByte ^byte   [ ^int i ^int j])
  (setByte ^byte   [ ^int i ^int j ^byte v])
  (getBytes ^"[[B" []))

(defn fill-bytes [^"[[B" bytes v]
  (let [b (byte v)]
    (do (areduce bytes idx res bytes
                 (java.util.Arrays/fill ^bytes (aget bytes idx) b))
        bytes)))

(defn ->byte-grid [w h]
  (let [^"[[B" bytes (make-array Byte/TYPE (long w) (long h))]
    (reify IByteMap
      (getByte [this  i j]        
        (aget ^bytes (aget bytes i) j))
      (setByte [this  i j v]
        (aset ^bytes (aget bytes i)
              j v)
        this)
      (getBytes [this] bytes)
      clojure.lang.IFn
      (invoke [this] bytes))))

(definline get-byte  [bm i j]
  (let [b (with-meta (gensym "bytemap") {:tag 'icfpc.core.IByteMap})]
    `(let [~b ~bm]
       (.getByte ~b (int ~i) (int ~j)))))

(definline set-byte  [bm i j v]
  (let [b (with-meta (gensym "bytemap") {:tag 'icfpc.core.IByteMap})]
    `(let [~b ~bm]
       (.setByte ~b (int ~i) (int ~j) (byte ~v)))))





;;this hasn't panned out yet, which is curious since it's competing against
;;an ArrayMap....needs more research.
(defrecord+ robot ^:strict [x y layout active-boosters picked-booster path current-zone plan])


#_(definline new-bot [x y]
  `(robot.             ~x 
                       ~y
                       [[0 0] [1 0] [1 1] [1 -1]]
                       {}
                       nil
                       ""
                       nil
                       nil))

#_(defn new-bot [x y]
  (robot.             x 
                      y
                      [[0 0] [1 0] [1 1] [1 -1]]
                      {}
                      nil
                      ""
                      nil
                      nil))

(defn new-bot [x y]
  {:x               x 
   :y               y
   :layout          [[0 0] [1 0] [1 1] [1 -1]]
   :active-boosters {}
   :picked-booster  nil
   :path            ""
   :current-zone    nil})

(defrecord+ lev
  ^:strict
  [name
     ^int  width
     ^int  height
     ^IByteMap grid
     ^IByteMap zones-grid
     zones-area
     ^shorts weights
     bots
     empty
     collected-boosters
     spawns
     boosters
     beakons
     bot
     zones?
     ^icfpc.core.IFringe fringe]
  ILevel
  (lev-width   [this] width)
  (lev-height  [this] height)
  (lev-grid    [this] grid)
  (lev-weights [this] weights)
  (lev-zones   [this] zones-grid)
  IFringe
  (has-fringe? [this o] (.has-fringe? fringe o))
  (add-fringe  [this o] (do (.add-fringe fringe o) this))
  (clear-fringe! [this] (do (.clear-fringe! fringe) this)))

(extend-protocol
    ILevel
  clojure.lang.PersistentArrayMap
  (lev-width   [this] (.valAt this :width))
  (lev-height  [this] (.valAt this :height))
  (lev-grid    [this] (.valAt this :grid))
  (lev-weights [this] (.valAt this :weights))
  (lev-zones   [this] (.valAt this :zones-grid))
  clojure.lang.PersistentHashMap
  (lev-width   [this] (.valAt this :width))
  (lev-height  [this] (.valAt this :height))
  (lev-grid    [this] (.valAt this :grid))
  (lev-weights [this] (.valAt this :weights))
  (lev-zones   [this] (.valAt this :zones-grid)))

(def ^:const EMPTY (byte 0))
(def ^:const OBSTACLE (byte 1))
(def ^:const WRAPPED (byte 2))

;;Instructions that determine
;;our bot's movement and actions I think.
;;We have 18 discrete states.
;;Path lengths can be arbitrarly large.
(def ^:const EXTRA_HAND \B)
(def ^:const FAST_WHEELS \F)
(def ^:const DRILL \L)
(def ^:const SPAWN \X)
(def ^:const TELEPORT \R)
(def ^:const CLONE \C)
(def ^:const WAIT \Z)
(def ^:const UNKNOWN \?)

(def ^:const UP    \W)
(def ^:const DOWN  \S)
(def ^:const LEFT  \A)
(def ^:const RIGHT \D)

(def ^:const ROTATE_CW  \E)
(def ^:const ROTATE_CCW \Q)
(def ^:const SET_BEAKON \R)
(def ^:const JUMP       \T)
(def ^:const REPLICATE  \C)

(defn spend
  ([v] (cond (nil? v) nil (> v 1) (dec v) :else 0))
  ([map k k2]
   (let [v ((map k) k2)]
     (cond
       (nil? v) map
       (> v 1)  (update map k assoc k2 (dec v))
       :else    (update map k dissoc k2)))))

;;These are all performance killers due to function
;;call overhead, numerous calls, and boxed math.

;;(defn coord->idx ^long [level x y] (+ x (* y (:width level))))
#_(defn get-level
  ([level x y]
   (aget ^bytes (:grid level) (coord->idx level x y)))
  ([ level x y default]
   (if (and
         (< -1 x (:width level))
         (< -1 y (:height level)))
     (aget ^bytes (:grid level) (coord->idx level x y))
     default)))

(definline coord->idx [level x y]
  `(let [w# (lev-width ~level)]
     (unchecked-add ~x (unchecked-multiply ~y w#))))

(defn get-level
  "blah"
  {:inline (fn
             ([level x y]
              (let [b (with-meta `(lev-grid ~level) {:tag 'icfpc.core.IByteMap})]
                `(.getByte ~b  ~x ~y)))
             ([level x y default]
              (let [b (with-meta `(lev-grid ~level) {:tag 'icfpc.core.IByteMap})]
                `(if (and
                      (<* -1 ~x (lev-width ~level))
                      (<* -1 ~y (lev-height ~level)))
                   (.getByte  ~b ~x ~y)
                 ~default))))
    :inline-arities #{3 4}}
  ([level x y]  (get-byte (lev-grid level)  x y))
  ([level x y default]
   (if (and
        (<* -1 x (lev-width  level))
        (<* -1 y (lev-height level)))
     (get-byte (lev-grid level) x y)
     default)))

(definline set-level [level x y value]
  `(do (set-byte (lev-grid ~level) ~x ~y ~value)
       ~level))

(defn get-zone [level x y]
  (get-byte (lev-zones level) x y))

(defn zone-area [level zone]
  ((level :zones-area) zone))

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

(defn arr-reduce2 [f init ^"[[B" arr]
  (reduce (fn [acc i]              
            (let [^bytes c (aget arr i)]
              (arr-reduce f acc c)))
          init
          (range (alength arr))))

       
     
