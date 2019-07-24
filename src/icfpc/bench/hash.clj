(ns icfpc.bench.hash
  (:require [criterium.core :as c]))


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
      (let [h (mix-collection-hash (unchecked-int (dhash-code (dhash-code x) y)) 2)]
        (do (set! _hash (int h))
            h))
      _hash)))

;;replicates clojure's hash-ordered-coll for 2 inputs.
(defn hash-o [seed x]
  (unchecked-add-int
   (unchecked-multiply-int 31 seed)
   (hash x)))

(defn hash-orderedxy
  [x y]
  (-> (hash-o 1 x)
      (hash-o y)
      (mix-collection-hash 2)))

(deftype intPoint [^long x ^long y
                ^:unsynchronized-mutable ^int _hasheq
                ^:unsynchronized-mutable ^int _hash
                ] 
  Object
  (toString [_] (str "(" x "," y ")"))
  ;;technique shamelessly borrowed from fastmath!
  (equals [this v]
    (and (instance? intPoint v)
         (and (== x (.x ^intPoint v))
              (== y (.y ^intPoint v)))))
  clojure.lang.IHashEq
  (hasheq [this]
    (if (== _hasheq (int -1))
      (let [h (hash-orderedxy x y)]
        (do (set! _hasheq (int h))
            h))
      _hasheq))
  (hashCode [this]
    (if (== _hash (int -1))
      (let [h (hash-orderedxy x y)]
        (do (set! _hash (int h))
            h))
      _hash)))


(defn ->point [x y]
  (Point. (long x) (long y) -1 -1))

(defn ->ipoint [x y]
  (intPoint. (long x) (long y) -1 -1))

(def vecmap
  {[12 74] \L,
   [85 303] \L,
   [27 298] \F,
   [323 328] \F,
   [88 163] \B,
   [133 221] \B,
   [8 378] \B,
   [260 146] \F,
   [51 343] \F,
   [38 323] \L,
   [324 323] \B,
   [97 65] \B,
   [307 240] \F,
   [156 293] \L,
   [167 378] \C,
   [40 60] \B,
   [284 358] \F,
   [14 380] \F,
   [144 230] \R,
   [164 212] \R,
   [303 3] \B,
   [216 108] \L,
   [279 323] \F,
   [86 168] \B,
   [118 352] \L,
   [219 90] \C,
   [213 280] \C,
   [131 363] \B,
   [80 364] \B,
   [213 394] \B,
   [78 5] \L,
   [291 373] \F,
   [91 298] \L,
   [154 371] \F,
   [298 162] \F,
   [306 280] \L,
   [246 375] \F,
   [162 324] \F,
   [209 134] \F,
   [254 145] \F,
   [209 56] \F,
   [231 279] \C,
   [157 233] \B})

(def pointmap
  {(->point 12 74)   \L
   (->point 85 303)  \L
   (->point 27 298)  \F
   (->point 323 328) \F
   (->point 88 163)  \B
   (->point 133 221) \B
   (->point 8 378)   \B
   (->point 260 146) \F
   (->point 51 343)  \F
   (->point 38 323)  \L
   (->point 324 323) \B
   (->point 97 65)   \B
   (->point 307 240) \F
   (->point 156 293) \L
   (->point 167 378) \C
   (->point 40 60)   \B
   (->point 284 358) \F
   (->point 14 380)  \F
   (->point 144 230) \R
   (->point 164 212) \R
   (->point 303 3)   \B
   (->point 216 108) \L
   (->point 279 323) \F
   (->point 86 168)  \B
   (->point 118 352) \L
   (->point 219 90)  \C
   (->point 213 280) \C
   (->point 131 363) \B
   (->point 80 364)  \B
   (->point 213 394) \B
   (->point 78 5)    \L
   (->point 291 373) \F
   (->point 91 298)  \L
   (->point 154 371) \F
   (->point 298 162) \F
   (->point 306 280) \L
   (->point 246 375) \F
   (->point 162 324) \F
   (->point 209 134) \F
   (->point 254 145) \F
   (->point 209 56)  \F
   (->point 231 279) \C
   (->point 157 233) \B})

(def ipointmap
  {(->ipoint 12 74)   \L
   (->ipoint 85 303)  \L
   (->ipoint 27 298)  \F
   (->ipoint 323 328) \F
   (->ipoint 88 163)  \B
   (->ipoint 133 221) \B
   (->ipoint 8 378)   \B
   (->ipoint 260 146) \F
   (->ipoint 51 343)  \F
   (->ipoint 38 323)  \L
   (->ipoint 324 323) \B
   (->ipoint 97 65)   \B
   (->ipoint 307 240) \F
   (->ipoint 156 293) \L
   (->ipoint 167 378) \C
   (->ipoint 40 60)   \B
   (->ipoint 284 358) \F
   (->ipoint 14 380)  \F
   (->ipoint 144 230) \R
   (->ipoint 164 212) \R
   (->ipoint 303 3)   \B
   (->ipoint 216 108) \L
   (->ipoint 279 323) \F
   (->ipoint 86 168)  \B
   (->ipoint 118 352) \L
   (->ipoint 219 90)  \C
   (->ipoint 213 280) \C
   (->ipoint 131 363) \B
   (->ipoint 80 364)  \B
   (->ipoint 213 394) \B
   (->ipoint 78 5)    \L
   (->ipoint 291 373) \F
   (->ipoint 91 298)  \L
   (->ipoint 154 371) \F
   (->ipoint 298 162) \F
   (->ipoint 306 280) \L
   (->ipoint 246 375) \F
   (->ipoint 162 324) \F
   (->ipoint 209 134) \F
   (->ipoint 254 145) \F
   (->ipoint 209 56)  \F
   (->ipoint 231 279) \C
   (->ipoint 157 233) \B})



(comment
  
;;lookup, value present
(let [p [157 233]]
  (c/quick-bench (vecmap p)))

;; Evaluation count : 2533962 in 6 samples of 422327 calls.
;;              Execution time mean : 237.251948 ns
;;     Execution time std-deviation : 4.050451 ns
;;    Execution time lower quantile : 232.752947 ns ( 2.5%)
;;    Execution time upper quantile : 241.509138 ns (97.5%)
;;                    Overhead used : 1.850824 ns

(let [p (->point 157 233)]
  (c/quick-bench (pointmap p)))

;; Evaluation count : 9691290 in 6 samples of 1615215 calls.
;;              Execution time mean : 60.151956 ns
;;     Execution time std-deviation : 1.279862 ns
;;    Execution time lower quantile : 58.778799 ns ( 2.5%)
;;    Execution time upper quantile : 61.466818 ns (97.5%)
;;                    Overhead used : 1.850824 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;; Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
        
(let [p (->ipoint 157 233)]
  (c/quick-bench (ipointmap p)))

;; Evaluation count : 9482004 in 6 samples of 1580334 calls.
;;              Execution time mean : 61.526101 ns
;;     Execution time std-deviation : 1.154711 ns
;;    Execution time lower quantile : 60.486741 ns ( 2.5%)
;;    Execution time upper quantile : 63.406237 ns (97.5%)
;;                    Overhead used : 1.850824 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers


;;Value not present.
(let [p [0 233]]
  (c/quick-bench (vecmap p)))

;; Evaluation count : 40129668 in 6 samples of 6688278 calls.
;;              Execution time mean : 18.072788 ns
;;     Execution time std-deviation : 3.169969 ns
;;    Execution time lower quantile : 14.644917 ns ( 2.5%)
;;    Execution time upper quantile : 21.898640 ns (97.5%)
;;                    Overhead used : 1.850824 ns
        
(let [p (->point 0 233)]
  (c/quick-bench (pointmap p)))

;; Evaluation count : 37329936 in 6 samples of 6221656 calls.
;;              Execution time mean : 13.143984 ns
;;     Execution time std-deviation : 0.270425 ns
;;    Execution time lower quantile : 12.791190 ns ( 2.5%)
;;    Execution time upper quantile : 13.401468 ns (97.5%)
;;                    Overhead used : 2.335710 ns

(let [p (->ipoint 0 233)]
  (c/quick-bench (ipointmap p)))

;; Evaluation count : 34991874 in 6 samples of 5831979 calls.
;;              Execution time mean : 15.802488 ns
;;     Execution time std-deviation : 0.217156 ns
;;    Execution time lower quantile : 15.535274 ns ( 2.5%)
;;    Execution time upper quantile : 16.064159 ns (97.5%)
;;                    Overhead used : 1.850824 ns



;;Since vectors of ints and ipoints have the same hash
;;function, they are hasheq, so we should be able to
;;lookup points fast?



;;lookup known point as vector, from pointmap
(let [p [157 233]]
  (c/quick-bench (ipointmap p)))

;;lookup unknown point as vector, from pointmap
(let [p [0 233]]
  (c/quick-bench (ipointmap p)))


)
