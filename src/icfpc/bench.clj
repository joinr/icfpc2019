;;a testing namespace for various
;;strategies to improve performance.
(ns icfpc.bench
  (:require [criterium.core :as c]
            [clj-tuple :as tup]
            [clojure.data.int-map :as i]
            [icfpc [core :as ic :refer [->Point]] [level :as level] [bot :as bot] [bif :as b]]
            [primitive-math :as p]
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
          

;;core/get-level and core/coord->idx
;;are on hot paths, particularly in bot/explore...

;; (defprotocol ILevel
;;   (lev-width   [level])
;;   (lev-height  [level])
;;   (lev-grid    [level])
;;   (lev-weights [level]))

;; (defrecord lev [name
;;                 ^int  width
;;                 ^int  height
;;                 ^bytes grid
;;                 ^bytes zones-grid
;;                 zones-area
;;                 ^shorts weights
;;                 bots
;;                 empty
;;                 collected-boosters
;;                 spawns
;;                 boosters]
;;   ILevel
;;   (lev-width   [this] width)
;;   (lev-height  [this] height)
;;   (lev-grid    [this] grid)
;;   (lev-weights [this] weights)
;;   clojure.lang.IFn
;;   (invoke [this k] (case k
;;                      :name name
;;                      :width width
;;                      :grid grid
;;                      :zones-grid zones-grid
;;                      :zones-area zones-area
;;                      :weights weights
;;                      :bots bots
;;                      :empty empty
;;                      :collected-boosters collected-boosters
;;                      :spawns spawns
;;                      :boosters boosters
;;                      (.valAt this k)))
;;   (invoke [this k default]
;;     (case k
;;       :name name
;;       :width width
;;       :grid grid
;;       :zones-grid zones-grid
;;       :zones-area zones-area
;;       :weights weights
;;       :bots bots
;;       :empty empty
;;       :collected-boosters collected-boosters
;;       :spawns spawns
;;       :boosters boosters
;;       (.valAt this k default))))

;; (extend-protocol
;;     ILevel
;;   clojure.lang.PersistentArrayMap
;;   (lev-width   [this] (.valAt this :width))
;;   (lev-height  [this] (.valAt this :height))
;;   (lev-grid    [this] (.valAt this :grid))
;;   (lev-weights [this] (.valAt this :weights))
;;   clojure.lang.PersistentHashMap
;;   (lev-width   [this] (.valAt this :width))
;;   (lev-height  [this] (.valAt this :height))
;;   (lev-grid    [this] (.valAt this :grid))
;;   (lev-weights [this] (.valAt this :weights)))

;; (def l (->lev "blah" 10 10 (byte-array 10) (byte-array 10) 10 (short-array 10) [] nil [] [] [] 0 true))
;; ;;hashmap version (note, the field length exceeds arraymap...)
;; (def lm (into {} l))

;; icfpc.bench> (c/quick-bench (lev-width l))
;; Evaluation count : 93370056 in 6 samples of 15561676 calls.
;;              Execution time mean : 4.529833 ns
;;     Execution time std-deviation : 0.078534 ns
;;    Execution time lower quantile : 4.453300 ns ( 2.5%)
;;    Execution time upper quantile : 4.626149 ns (97.5%)
;;                    Overhead used : 2.101342 ns

;; icfpc.bench> (let [l {:width 10 :height 10 :grid 10 :weights 10}] (c/quick-bench (width l)))
;; Evaluation count : 65143986 in 6 samples of 10857331 calls.
;;              Execution time mean : 7.316122 ns
;;     Execution time std-deviation : 0.097029 ns
;;    Execution time lower quantile : 7.210819 ns ( 2.5%)
;;    Execution time upper quantile : 7.409845 ns (97.5%)
;;                    Overhead used : 2.101342 ns

;; icfpc.bench> (c/quick-bench (.lev-width ^lev l))
;; Evaluation count : 107930478 in 6 samples of 17988413 calls.
;;              Execution time mean : 3.521487 ns
;;     Execution time std-deviation : 0.123192 ns
;;    Execution time lower quantile : 3.359547 ns ( 2.5%)
;;    Execution time upper quantile : 3.662013 ns (97.5%)
;;                    Overhead used : 2.101342 ns

;; icfpc.bench> (c/quick-bench (.width ^lev l))
;; Evaluation count : 104486472 in 6 samples of 17414412 calls.
;;              Execution time mean : 3.545602 ns
;;     Execution time std-deviation : 0.085816 ns
;;    Execution time lower quantile : 3.407037 ns ( 2.5%)
;;    Execution time upper quantile : 3.631632 ns (97.5%)
;;                    Overhead used : 2.101342 ns

;;icfpc.bench> (c/quick-bench (l :width))
;; Evaluation count : 75303822 in 6 samples of 12550637 calls.
;;              Execution time mean : 5.958670 ns
;;     Execution time std-deviation : 0.059501 ns
;;    Execution time lower quantile : 5.889378 ns ( 2.5%)
;;    Execution time upper quantile : 6.036775 ns (97.5%)
;;                    Overhead used : 2.101342 ns
;; nil


;;array maps with non-inlined protocol dispatch are 4.6x slower
;; icfpc.bench> (c/quick-bench (lev-width lm))
;; Evaluation count : 35475168 in 6 samples of 5912528 calls.
;;              Execution time mean : 14.921462 ns
;;     Execution time std-deviation : 0.150381 ns
;;    Execution time lower quantile : 14.741645 ns ( 2.5%)
;;    Execution time upper quantile : 15.067046 ns (97.5%)
;;                    Overhead used : 2.101342 ns


;;keyword lookups are about 8x slower...
;; icfpc.bench> (c/quick-bench (:width lm))
;; Evaluation count : 22123224 in 6 samples of 3687204 calls.
;;              Execution time mean : 24.354733 ns
;;     Execution time std-deviation : 0.495892 ns
;;    Execution time lower quantile : 23.712388 ns ( 2.5%)
;;    Execution time upper quantile : 24.888828 ns (97.5%)
;;                    Overhead used : 2.101342 ns
;; nil

;;function application lookups are about 3.6x slower
;; icfpc.bench> (c/quick-bench (lm :width))
;; Evaluation count : 44544324 in 6 samples of 7424054 calls.
;;              Execution time mean : 11.617911 ns
;;     Execution time std-deviation : 0.247750 ns
;;    Execution time lower quantile : 11.260342 ns ( 2.5%)
;;    Execution time upper quantile : 11.882004 ns (97.5%)
;;                    Overhead used : 2.101342 ns


;;calls to clojure.core/get are ~10x slower, despite
;;get being inlined.

;; icfpc.bench> (c/quick-bench (get lm :width))
;; Evaluation count : 19603224 in 6 samples of 3267204 calls.
;;              Execution time mean : 29.265280 ns
;;     Execution time std-deviation : 0.783067 ns
;;    Execution time lower quantile : 27.942104 ns ( 2.5%)
;;    Execution time upper quantile : 29.926778 ns (97.5%)
;;                    Overhead used : 2.101342 ns

;;direct method invocation is on par with function lookup,
;;at 3.6x slower than field access or protocol invocation.

;; icfpc.bench> (c/quick-bench (.valAt ^clojure.lang.Associative lm :width))
;; Evaluation count : 44632860 in 6 samples of 7438810 calls.
;;              Execution time mean : 11.389798 ns
;;     Execution time std-deviation : 0.256336 ns
;;    Execution time lower quantile : 11.100377 ns ( 2.5%)
;;    Execution time upper quantile : 11.747791 ns (97.5%)
;;                    Overhead used : 2.101342 ns


;;So the story his is....for repeated reads, field access is
;;king (duh) and is interchangeable with inlined protocol
;;implementations that dispatch to the field (effectively an
;;externally visible field access).  If you implement
;;a function invocation for the record that mimics
;;the implementation of .valAt, dispatching on
;;fields first, you get pretty close to direct
;;field access with function application, although
;;a tiny bit slower 33% relative to field access#

;;If using hashmaps, function application lookup
;;faster, then some slight overhead of a protocol
;;cache lookup,
;;then is better than keyword invocation,
;;which is better than calling get apparently.

;;metrics may differ for arraymaps though.

;;so right off the bat, we should get some performance
;;improvement if we switch to protocols.


;;compare map destructuring...
(defn stuff [{:keys [width height]}]
  width)

;; Evaluation count : 8567238 in 6 samples of 1427873 calls.
;;              Execution time mean : 68.463234 ns
;;     Execution time std-deviation : 0.729966 ns
;;    Execution time lower quantile : 67.198677 ns ( 2.5%)
;;    Execution time upper quantile : 69.119342 ns (97.5%)
;;                    Overhead used : 1.825577 ns

(defn stuffraw [m]
  (let [w (m :width)
        h (m :height)]
   w))

;; Evaluation count : 32140476 in 6 samples of 5356746 calls.
;;              Execution time mean : 17.080791 ns
;;     Execution time std-deviation : 0.297715 ns
;;    Execution time lower quantile : 16.765005 ns ( 2.5%)
;;    Execution time upper quantile : 17.440539 ns (97.5%)
;;                    Overhead used : 1.825577 ns


(defn not-zero [n]
  (not (zero? n)))

;; Evaluation count : 132124596 in 6 samples of 22020766 calls.
;;              Execution time mean : 2.839144 ns
;;     Execution time std-deviation : 0.111993 ns
;;    Execution time lower quantile : 2.705042 ns ( 2.5%)
;;    Execution time upper quantile : 2.987240 ns (97.5%)
;;                    Overhead used : 1.787913 ns
(defn ne-zero [n]
  (not= n 0))
;; Evaluation count : 89775102 in 6 samples of 14962517 calls.
;;              Execution time mean : 5.040152 ns
;;     Execution time std-deviation : 0.257824 ns
;;    Execution time lower quantile : 4.797250 ns ( 2.5%)
;;    Execution time upper quantile : 5.354460 ns (97.5%)
;; Overhead used : 1.787913 ns

(defn neq-zero [n]
  (not (== n 0)))

;; Evaluation count : 82853334 in 6 samples of 13808889 calls.
;;              Execution time mean : 5.685828 ns
;;     Execution time std-deviation : 0.109642 ns
;;    Execution time lower quantile : 5.503110 ns ( 2.5%)
;;    Execution time upper quantile : 5.792646 ns (97.5%)
;;                    Overhead used : 1.787913 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers


;;intersting numeric comparisons.

;; icfpc.bench> (c/quick-bench (== icfpc.core/OBSTACLE  10))
;; Evaluation count : 98036544 in 6 samples of 16339424 calls.
;;              Execution time mean : 4.346203 ns
;;     Execution time std-deviation : 0.069625 ns
;;    Execution time lower quantile : 4.224626 ns ( 2.5%)
;;    Execution time upper quantile : 4.404160 ns (97.5%)
;;                    Overhead used : 1.787913 ns
;; nil
;; icfpc.bench> (c/quick-bench (identical? icfpc.core/OBSTACLE  10))
;; Evaluation count : 114117066 in 6 samples of 19019511 calls.
;;              Execution time mean : 3.511408 ns
;;     Execution time std-deviation : 0.044961 ns
;;    Execution time lower quantile : 3.452161 ns ( 2.5%)
;;    Execution time upper quantile : 3.560770 ns (97.5%)
;;                    Overhead used : 1.787913 ns
;; nil
;; icfpc.bench> (c/quick-bench (= icfpc.core/OBSTACLE  10))
;; Evaluation count : 107691366 in 6 samples of 17948561 calls.
;;              Execution time mean : 3.880414 ns
;;     Execution time std-deviation : 0.055376 ns
;;    Execution time lower quantile : 3.820468 ns ( 2.5%)
;;    Execution time upper quantile : 3.959699 ns (97.5%)
;;                    Overhead used : 1.787913 ns
;; nil
;; icfpc.bench> (c/quick-bench (= icfpc.core/OBSTACLE  10))

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


;; icfpc.bench> (c/quick-bench (< -1 2 3))
;; Evaluation count : 17679474 in 6 samples of 2946579 calls.
;;              Execution time mean : 33.098853 ns
;;     Execution time std-deviation : 0.655936 ns
;;    Execution time lower quantile : 32.031441 ns ( 2.5%)
;;    Execution time upper quantile : 33.706702 ns (97.5%)
;;                    Overhead used : 1.787913 ns


;;macro-based comparison is 10x faster due to not
;;using restfn for > 3 args.
;; icfpc.bench> (c/quick-bench (<* -1 2 3))
;; Evaluation count : 114145476 in 6 samples of 19024246 calls.
;;              Execution time mean : 3.481508 ns
;;     Execution time std-deviation : 0.032622 ns
;;    Execution time lower quantile : 3.440453 ns ( 2.5%)
;;    Execution time upper quantile : 3.516167 ns (97.5%)
;;                    Overhead used : 1.787913 ns




;; icfpc.bench> (c/quick-bench (icfpc.level/wxy->idx 100 10 10))
;; Evaluation count : 129504678 in 6 samples of 21584113 calls.
;;              Execution time mean : 2.424968 ns
;;     Execution time std-deviation : 0.100511 ns
;;    Execution time lower quantile : 2.287303 ns ( 2.5%)
;;    Execution time upper quantile : 2.539626 ns (97.5%)
;;                    Overhead used : 2.110365 ns

;; Found 2 outliers in 6 samples (33.3333 %)
;; 	low-severe	 1 (16.6667 %)
;; 	low-mild	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers


;;(def ilev (icfpc.core/->lev  "blah" 10 10 (byte-array 10) (byte-array 10) 10 (short-array 10) [] nil [] [] [] nil))

;;this gets called a ton.  The lookup to get the grids
;;makes this 10x slower.  Note that in many cases,
;;we're only operating on the width....

;; icfpc.bench> (c/quick-bench (icfpc.core/coord->idx ilev 10 10))
;; Evaluation count : 25814454 in 6 samples of 4302409 calls.
;;              Execution time mean : 22.130189 ns
;;     Execution time std-deviation : 1.135869 ns
;;    Execution time lower quantile : 21.200341 ns ( 2.5%)
;;    Execution time upper quantile : 23.957781 ns (97.5%)
;;                    Overhead used : 2.110365 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.9208 % Variance is moderately inflated by outliers

(defn wxy->idx [width x y]
  (unchecked-add x (unchecked-multiply y width)))

(defn pwxy->idx ^long [^long width ^long x ^long y]
  (p/+ x (p/*  y  width)))


;;hashset fun...
;;Looking at getting more efficient hashing.
;;Possibly faster eq...
;;One big bottleneck is checking existence of points.

(def ps (->> (slurp "points.edn")
             clojure.edn/read-string
             (map (fn [[{:keys [x y]} _]]
                    (icfpc.core/->Point x y)))
             (sort-by (juxt first second))
             vec))

;;how long does it take for us to hash stuff?

;; icfpc.bench> (c/quick-bench (do (doto (HashSet.) (.addAll ps)) nil))
;; Evaluation count : 30 in 6 samples of 5 calls.
;;              Execution time mean : 21.327122 ms
;;     Execution time std-deviation : 954.556715 µs
;;    Execution time lower quantile : 20.588371 ms ( 2.5%)
;;    Execution time upper quantile : 22.935103 ms (97.5%)
;;                    Overhead used : 1.805194 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; nil
  
;; icfpc.bench> (let [s (doto (HashSet.) (.addAll  ps)) p (nth ps 1000)] (c/quick-bench (.contains ^HashSet s p)))
;; Evaluation count : 33819096 in 6 samples of 5636516 calls.
;;              Execution time mean : 15.872475 ns
;;     Execution time std-deviation : 0.256494 ns
;;    Execution time lower quantile : 15.588379 ns ( 2.5%)
;;    Execution time upper quantile : 16.157568 ns (97.5%)
;;                    Overhead used : 1.805194 ns


;; icfpc.bench> (c/quick-bench (do (io.lacuna.bifurcan.LinearSet/from ^java.lang.Iterable ps) nil))
;; Evaluation count : 42 in 6 samples of 7 calls.
;;              Execution time mean : 16.784856 ms
;;     Execution time std-deviation : 986.423969 µs
;;    Execution time lower quantile : 16.037391 ms ( 2.5%)
;;    Execution time upper quantile : 18.381126 ms (97.5%)
;;                    Overhead used : 1.805194 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 14.5722 % Variance is moderately inflated by outliers


;; icfpc.bench> (let [s (apply b/->linear-set ps) p (nth ps 1000)] (c/quick-bench (.contains ^io.lacuna.bifurcan.LinearSet s p)))
;; Evaluation count : 32065182 in 6 samples of 5344197 calls.
;;              Execution time mean : 17.817323 ns
;;     Execution time std-deviation : 1.445025 ns
;;    Execution time lower quantile : 16.342552 ns ( 2.5%)
;;    Execution time upper quantile : 19.974870 ns (97.5%)
;;                    Overhead used : 1.805194 ns




;;exploring the overhead of instance checks...

(comment
  
(let [x [1]]
  (c/quick-bench (instance? clojure.lang.Indexed x)))

(defn indexed [^clojure.lang.Indexed xy]
  (let [x (.nth  xy 0)
        y (.nth  xy 1)]
    (+ 2 3)))

;;fastest, but bloated...
(defn indexhack [xy]
  (if (instance? clojure.lang.Indexed xy)
    (let [x (.nth ^clojure.lang.Indexed xy 0)
          y (.nth ^clojure.lang.Indexed xy 1)]
      (+ 2 3))
    (let [x (nth xy 0)
          y (nth xy 1)]
      (+ 2 3))))

;; icfpc.speed> (let [x [1 2]] (c/quick-bench (indexhack x)))
;; Evaluation count : 83808372 in 6 samples of 13968062 calls.
;;              Execution time mean : 5.389102 ns
;;     Execution time std-deviation : 0.179385 ns
;;    Execution time lower quantile : 5.105898 ns ( 2.5%)
;;    Execution time upper quantile : 5.572797 ns (97.5%)
;;                    Overhead used : 1.794404 ns

;;still too slow, additional fn calls,
;;wants to be inlined.
(let [f1 (fn [^clojure.lang.Indexed xy]
           (let [x (.nth  xy 0)
                 y (.nth xy 1)]
             (+ x y)))
      f2 (fn [xy]
           (let [x (nth  xy 0)
                 y (nth xy 1)]
             (+ x y)))]
  (defn indexhack3 [xy]
    (if (instance? clojure.lang.Indexed xy)
      (f1 xy)
      (f2 xy))))
  
(definline fast-nth [coll idx]
  `(.nth ~(with-meta coll {:tag 'clojure.lang.Indexed}) ~idx))

;;this is too slow.
(defn indexhack2 [xy]
  (let [idx?  (instance? clojure.lang.Indexed xy)
        nf    (if idx? fast-nth nth)
        x     (nf xy 0)
        y     (nf xy 0)]
    (+ x y)))
       
(defn normal [[x y]]
  (+ x y))
)

;; icfpc.speed> (let [x [1 2]] (c/quick-bench (normal x)))
;; Evaluation count : 27181620 in 6 samples of 4530270 calls.
;;              Execution time mean : 20.106737 ns
;;     Execution time std-deviation : 0.858973 ns
;;    Execution time lower quantile : 19.096891 ns ( 2.5%)
;;    Execution time upper quantile : 21.462080 ns (97.5%)
;;                    Overhead used : 1.794404 ns

;; Found 2 outliers in 6 samples (33.3333 %)
;; 	low-severe	 1 (16.6667 %)
;; 	low-mild	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
