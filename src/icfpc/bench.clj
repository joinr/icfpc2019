;;a testing namespace for various
;;strategies to improve performance.
(ns icfpc.bench
  (:require [criterium.core :as c]
            [clj-tuple :as tup]
            [clojure.data.int-map :as i]
            [icfpc [core :as ic :refer [->Point]] [level :as level] [bot :as bot]]
            )
  (:import [java.util HashSet HashMap ArrayDeque]))

(def big "prob-277")

;;Destructuring
(defn addv [[x y]]
  (+ x y))

;; icfpc.bench> (let [x [2 3]] (c/quick-bench (addv x)))
;; Evaluation count : 35200962 in 6 samples of 5866827 calls.
;;              Execution time mean : 14.648314 ns
;;     Execution time std-deviation : 0.525945 ns
;;    Execution time lower quantile : 14.004719 ns ( 2.5%)
;;    Execution time upper quantile : 15.123550 ns (97.5%)
;;                    Overhead used : 2.077443 ns
(defn addxy [xy]
  (+ (nth xy 0) (nth xy 1)))

;; icfpc.bench> (let [x [2 3]] (c/quick-bench (addv x)))
;; Evaluation count : 41150184 in 6 samples of 6858364 calls.
;;              Execution time mean : 12.712408 ns
;;     Execution time std-deviation : 0.224799 ns
;;    Execution time lower quantile : 12.407420 ns ( 2.5%)
;;    Execution time upper quantile : 12.900320 ns (97.5%)
;;                    Overhead used : 2.077443 ns

(defn addxyh [^clojure.lang.Indexed xy]
  (+ (.nth xy 0) (.nth xy 1)))

;; icfpc.bench> (let [x [2 3]] (c/quick-bench (addxyh x)))
;; Evaluation count : 46898400 in 6 samples of 7816400 calls.
;;              Execution time mean : 11.077783 ns
;;     Execution time std-deviation : 0.354576 ns
;;    Execution time lower quantile : 10.740108 ns ( 2.5%)
;;    Execution time upper quantile : 11.477872 ns (97.5%)
;;                    Overhead used : 2.077443 ns
;; nil


;;let's look at efficient ways to define strucutral stuff.
;;We're interested in very fast lookups.
;;this is the legacy defrecord-based inplementation
;;of "points" used pervasively.

(defrecord rpoint [x y]
  Object
  (toString [_] (str "(" x "," y ")"))
  clojure.lang.Indexed
  (nth [_ i] (case i 0 x 1 y))
  (nth [_ i nf] (case i 0 x 1 y nf)))
                
(def rhand-blocks-map
  (into {(->rpoint 1 -1) [[1 -1]]
         (->rpoint 1 0)  [[1 0]]
         (->rpoint 1 1)  [[1 1]]}
    (for [maxy (range 2 20)]
      [(->rpoint 1 maxy) (vec
                          (concat
                            (for [y (range 1 (inc (quot maxy 2)))] [0 y])
                            (for [y (range (int (Math/ceil (/ maxy 2.0))) (inc maxy))] [1 y])))])))


;; icfpc.bench> (let [p (->rpoint 1 1)]
;;    (c/quick-bench (rhand-blocks-map p)))
;; Evaluation count : 4247874 in 6 samples of 707979 calls.
;;              Execution time mean : 143.885505 ns
;;     Execution time std-deviation : 4.233257 ns
;;    Execution time lower quantile : 136.524483 ns ( 2.5%)
;;    Execution time upper quantile : 148.042767 ns (97.5%)
;;                    Overhead used : 2.077443 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers

;;If we leverage the Point implementation reproduced here...
(comment
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
  ;; (hashCode [_]
  ;;   (try (unchecked-int (dhash-code (dhash-code x) y))
  ;;        (catch Exception e (throw (ex-info "bad hash!" {:x x :y y})))))
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
  (Point. (long x) (long y) -1 -1))
)

;;we should get more efficient hashing based on the
;;compound key - which is now known numeric.
(def hand-blocks-map
  (into {(->Point 1 -1) [[1 -1]]
         (->Point 1 0)  [[1 0]]
         (->Point 1 1)  [[1 1]]}
    (for [maxy (range 2 20)]
      [(->Point 1 maxy) (vec
                          (concat
                            (for [y (range 1 (inc (quot maxy 2)))] [0 y])
                            (for [y (range (int (Math/ceil (/ maxy 2.0))) (inc maxy))] [1 y])))])))

;;(let [p (->Point 1 1)]
;;   (c/quick-bench (hand-blocks-map p)))
;; Evaluation count : 5524302 in 6 samples of 920717 calls.
;;              Execution time mean : 107.134105 ns
;;     Execution time std-deviation : 2.203033 ns
;;    Execution time lower quantile : 104.545895 ns ( 2.5%)
;;    Execution time upper quantile : 109.161936 ns (97.5%)
;;                    Overhead used : 2.077443 ns

(def thand-blocks-map
  (into {(tup/tuple 1 -1) [[1 -1]]
         (tup/tuple 1 0)  [[1 0]]
         (tup/tuple 1 1)  [[1 1]]}
    (for [maxy (range 2 20)]
      [(tup/tuple 1 maxy) (vec
                          (concat
                            (for [y (range 1 (inc (quot maxy 2)))] [0 y])
                            (for [y (range (int (Math/ceil (/ maxy 2.0))) (inc maxy))] [1 y])))])))

;; icfpc.bench> (let [p (tup/tuple 1 1)]
;;                (c/quick-bench (thand-blocks-map p)))
;; Evaluation count : 3071208 in 6 samples of 511868 calls.
;;              Execution time mean : 193.279457 ns
;;     Execution time std-deviation : 2.663821 ns
;;    Execution time lower quantile : 190.363072 ns ( 2.5%)
;;    Execution time upper quantile : 196.136542 ns (97.5%)
;;                    Overhead used : 2.077443 ns


(def vhand-blocks-map
  (into {(vector 1 -1) [[1 -1]]
         (vector 1 0)  [[1 0]]
         (vector 1 1)  [[1 1]]}
    (for [maxy (range 2 20)]
      [(vector 1 maxy) (vec
                          (concat
                            (for [y (range 1 (inc (quot maxy 2)))] [0 y])
                            (for [y (range (int (Math/ceil (/ maxy 2.0))) (inc maxy))] [1 y])))])))

;; icfpc.bench> (let [p [1 1]]
;;                (c/quick-bench (vhand-blocks-map p)))
;; Evaluation count : 2927430 in 6 samples of 487905 calls.
;;              Execution time mean : 202.861814 ns
;;     Execution time std-deviation : 4.799391 ns
;;    Execution time lower quantile : 198.693162 ns ( 2.5%)
;;    Execution time upper quantile : 208.401574 ns (97.5%)
;;                    Overhead used : 2.077443 ns

(defn assoc-in-empty
  "Associates a value in a nested associative structure, where ks is a
  sequence of keys and v is the new value and returns a new nested structure.
  If any levels do not exist, hash-maps will be created."
  {:added "1.0"
   :static true}
  [m [k & ks] v]
  (if ks
    (assoc m k (assoc-in-empty (get m k (empty m)) ks v))
    (assoc m k v)))

;;nested maps.
(defn push [m x y v]
  (assoc-in-empty m [x y] v))

(defn lookup [m x y]
  (-> m (get x) (get y)))

(def hand-blocks-map2
  (reduce (fn [m [{:keys [x y]} v]]
            (push m x y v))
          {}
          (into [[(->Point 1 -1) [[1 -1]]]
                    [(->Point 1 0)  [[1 0]]]
                    [(->Point 1 1)  [[1 1]]]]
                   (for [maxy (range 2 20)]
                   [(->Point 1 maxy)
                    (vec
                     (concat
                      (for [y (range 1 (inc (quot maxy 2)))] [0 y])
                      (for [y (range (int (Math/ceil (/ maxy 2.0))) (inc maxy))] [1 y])))]))))

(defn hm2
  ([^icfpc.core.Point p] (lookup hand-blocks-map2 (.x p) (.y p)))
  ([x y] (lookup hand-blocks-map2 x y)))

;; Evaluation count : 8954028 in 6 samples of 1492338 calls.
;;              Execution time mean : 65.354918 ns
;;     Execution time std-deviation : 0.979165 ns
;;    Execution time lower quantile : 64.062568 ns ( 2.5%)
;;    Execution time upper quantile : 66.184779 ns (97.5%)
;;                    Overhead used : 2.077443 ns

(def hand-blocks-imap2
  (reduce (fn [m [{:keys [x y]} v]]
            (push m x y v))
          (i/int-map)
          (into [[(->Point 1 -1) [[1 -1]]]
                    [(->Point 1 0)  [[1 0]]]
                    [(->Point 1 1)  [[1 1]]]]
                   (for [maxy (range 2 20)]
                   [(->Point 1 maxy)
                    (vec
                     (concat
                      (for [y (range 1 (inc (quot maxy 2)))] [0 y])
                      (for [y (range (int (Math/ceil (/ maxy 2.0))) (inc maxy))] [1 y])))]))))

(defn hmi2
  ([^icfpc.core.Point p] (lookup hand-blocks-imap2 (.x p) (.y p)))
  ([x y] (lookup hand-blocks-imap2 x y)))

;; Evaluation count : 27750288 in 6 samples of 4625048 calls.
;;              Execution time mean : 19.922335 ns
;;     Execution time std-deviation : 0.625456 ns
;;    Execution time lower quantile : 19.236502 ns ( 2.5%)
;;    Execution time upper quantile : 20.616468 ns (97.5%)
;;                    Overhead used : 2.077443 ns


;;It looks like there's serious performance improvements using
;;- initially even nested hashmaps for encoding the coordinates,
;;then going beyond to nested intmaps.

;;10ns is maybe the fastest we can do lookups on int-maps...

;; icfpc.bench> (c/quick-bench (get hand-blocks-imap1 5))
;; Evaluation count : 47990514 in 6 samples of 7998419 calls.
;;              Execution time mean : 10.522641 ns
;;     Execution time std-deviation : 0.246504 ns
;;    Execution time lower quantile : 10.273374 ns ( 2.5%)
;;    Execution time upper quantile : 10.790710 ns (97.5%)
;;                    Overhead used : 2.077443 ns


;;Given the nature of the data, e.g. the lookup table
;;always has a 1 x coordinate, we can probably simplify
;;even futher and avoid a lookup.

(def hand-blocks-imap1 (get hand-blocks-imap2 1))
(defn hmi1
  ([^icfpc.core.Point p] (lookup hand-blocks-imap1 (.x p) (.y p)))
  ([x y] (if (== x 1) (get hand-blocks-imap1 y) (throw (ex-info "invalid x coord" {:x x})))))


;;if we don't unpack, we incur minor overhead.
;; icfpc.bench> (let [p (->Point 1 10)] (c/quick-bench (hmi1 1 10)))
;; Evaluation count : 43680534 in 6 samples of 7280089 calls.
;;              Execution time mean : 12.189651 ns
;;     Execution time std-deviation : 0.253797 ns
;;    Execution time lower quantile : 11.861749 ns ( 2.5%)
;;    Execution time upper quantile : 12.430877 ns (97.5%)
;;                    Overhead used : 2.077443 ns


;;if you have to do field access, still in the same
;;ballpark, e.g. 20ns


;;so one current takeaway is that replacing the
;;lookup table with a 2d intmap is a performance boon.


;;we can get even more out if we squeeze into arrays,
;;and use binary search.
(let [kvs      (sort-by (comp :y key) (seq hand-blocks-map))
      ^objects entries (object-array (map val kvs))
      ^ints ys (int-array (map (comp :y key) kvs))
      ]
  (defn hand-blocks [^long dx ^long dy]
    (when (== dx 1)
      (let [idx (java.util.Arrays/binarySearch ys dy)]
        (when (> idx -1)
          (aget entries  idx))))))


;; icfpc.bench> (c/quick-bench (hand-blocks 1 10))
;; Evaluation count : 61632366 in 6 samples of 10272061 calls.
;;              Execution time mean : 7.811289 ns
;;     Execution time std-deviation : 0.279691 ns
;;    Execution time lower quantile : 7.496925 ns ( 2.5%)
;;    Execution time upper quantile : 8.198301 ns (97.5%)
;;                    Overhead used : 2.077443 ns


;;how efficient is our hashing if we use Points in a hashet,
;;as in bot/explore which hashes paths?

;;based on a large run,
;;we end up with
;;icfpc.bot/fringe #<Atom@6701b342: 45302>
;;icfpc.bot/adds #<Atom@53faa665: 4731376>

(def h (reduce (fn [^HashSet acc p]
                 (doto acc (.add p))) (java.util.HashSet.)
               (for [i (range 20)
                     j (range 20)]
                 (->Point i j))))
          
