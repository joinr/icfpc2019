(ns icfpc.perf
  (:require [icfpc.main :as main]))


;;Starting with an example problem
;;and benching it for hotspots...
;;analysis is in performance.org

;;Problem 49 seems as good as any (it was
;;invoked in the rust examples)..


(in-ns 'icfpc.core)


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

(deftype Point [^long x ^long y]
  Object
  (toString [_] (str "(" x "," y ")"))
  ;;technique shamelessly borrowed from fastmath!
  (equals [_ v]
    (and (instance? Point v)
         (let [^Point v v]
           (and (== x (.x v))
                (== y (.y v))))))
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

(in-ns 'icfpc.perf)


;;patches to try.
(in-ns 'icpc.level)


;;perf: using point records as keys to vector for coords.
;;called by valid-hand? frequently
;;point ctr fn could be inlined or macrofied...
;;probably faster to reduce eagerly and avoid
;;lazy seq stuff.
;;This is building a map, hand-blocks, which is
;;being used for lookups based on ->Points as
;;keys.  Likely improvement is to
;;define a function hand-blocks that leverages
;;a more efficient int mapping of points to
;;coordinates.  Looks like points are just
;;int pairs...Incurring a LOT of overhead
;;on lookups here.
(def hand-blocks-map
  (into {(->Point 1 -1) [[1 -1]]
         (->Point 1 0)  [[1 0]]
         (->Point 1 1)  [[1 1]]}
    (for [maxy (range 2 20)]
      [(->Point 1 maxy) (vec
                          (concat
                            (for [y (range 1 (inc (quot maxy 2)))] [0 y])
                            (for [y (range (int (Math/ceil (/ maxy 2.0))) (inc maxy))] [1 y])))])))


(let [^ints ys (int-array (sort (map :y (keys icfpc.level/hand-blocks))))]
  (defn hand-blocks [^long dx ^long dy]
    (when (== dx 1)
      (let [idx (java.util.Arrays/binarySearch ys dy)]
        (when (pos? idx)
          (aget ys idx))))))

(defn valid?
  ([x y {:keys [width height] :as level}]
    (when (and
            (< -1 x width)
            (< -1 y height)
            (or
              (booster-active? level DRILL)
              (not= OBSTACLE (get-level level x y))))
      level))
  ([{:keys [bots] :as level}]
    (let [{:keys [x y]} (nth bots *bot*)]
      (valid? x y level))))

;;perf: this is a hot spot
;;perf: dstructuring of map args costs "some"
(defn valid-hand? [x y dx dy {:keys [width height] :as level}]
  (let [x' (+ x dx) y' (+ y dy)] ;;perf: hinted numeric ops could help here...
    (when (and
            (< -1 x' width)  ;;perf: calls to variadic fn <, clojure.lang.numbers boxed comp.
            (< -1 y' height) ;;perf: calls to variadic fn <, clojure.lang.numbers boxed comp.
            (every? ;;perf: every? coerces to seq, some cost from chunking.
             ;;perf: not= is comparing ifpc.core/OBSTACLE (a byte boxed in a var...) to result from
             ;;get-level.  Going though boxed comparison, possible optimization is (not (identical? ...))
             ;;destructuring in predicate incurs overhead.
             ;;possible boxed 
             (fn [[dx' dy']] (not= OBSTACLE (get-level level (+ x dx') (+ y dy'))))
             ;;perf: ->Point constructor may be incurring overhead here and in
             ;;hand-blocks, which is a map being used for lookups.
              (or (hand-blocks (->Point dx dy)) (throw (Exception. (str "Unknown hand offset" dx dy))))))
      level)))

(in-ns 'icfpc.perf)


;;(time (main/solve "prob-049"))
;;Solved prob-049 {:score 960, :time 187} was 1050 (-8.6%) / 993 (-3.3%)
;;"Elapsed time: 380.802954 msecs"
