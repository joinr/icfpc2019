;;Testing a variety of search fringes
;;for the icfpc.bot/explore function.
(ns icfpc.fringe)

(defprotocol IFringe
  (has-fringe? [this o])
  (add-fringe  [this o]))

(defn ->hash-fringe []
  (let [h (HashSet.)]
    (reify IFringe
      (has-fringe? [this o] (.contains h o))
      (add-fringe [this o] (do (.add h o) this)))))

(definline widx [width x y]
  `(unchecked-add ~x
    (unchecked-multiply ~y ~width)))

(defn ->int-fringe [width]
  (let [^io.lacuna.bifurcan.IntMap h  (.linear (io.lacuna.bifurcan.IntMap.))]
    (reify IFringe
      (has-fringe? [this  o]
        (.get h ^int (widx  width (.x ^icfpc.core.Point o) (.y ^icfpc.core.Point o))))
      (add-fringe [this  o]
        (do (.put h  ^int (widx  width (.x ^icfpc.core.Point o) (.y ^icfpc.core.Point o)) true)
            this)))))

(defn ->lin-fringe []
  (let [^io.lacuna.bifurcan.LinearMap h  (io.lacuna.bifurcan.LinearMap.)]
    (reify IFringe
      (has-fringe? [this  o]
        (.get h o))
      (add-fringe [this   o]
        (do (.put h o true)
            this)))))

(def pools (java.util.concurrent.ConcurrentHashMap.))

(defn ->bit-fringe [w h]
  (let [^"[[Z" bits (make-array Boolean/TYPE (long w) (long h))]
    (reify IFringe
      (has-fringe? [this   o]        
        (aget ^booleans (aget bits (.nth ^clojure.lang.Indexed o 0))
              (.nth ^clojure.lang.Indexed o 1)))
      (add-fringe [this    o]
        (aset ^booleans (aget bits (.nth  ^clojure.lang.Indexed  o 0))
              (.nth ^clojure.lang.Indexed  o 1) true)
        this)
      clojure.lang.IFn
      (invoke [this] bits))))

(defn clear! [k fr w h]
  (let [^"[[Z" bits (fr)]
    (if (and (= (alength bits) w)
             (= (alength ^booleans (aget bits 0)) h))
      (do (areduce bits idx res bits
                   (java.util.Arrays/fill ^booleans (aget bits idx) false))
          fr)
      (let [fr (->bit-fringe w h)
            _  (.put ^java.util.concurrent.ConcurrentHashMap pools k fr)]
        fr))))
  
(defn ->pooled-fringe [w h]
  (let [k (Thread/currentThread)]
    (if-let [prior (.get ^java.util.concurrent.ConcurrentHashMap pools k)]
      (clear! k prior w h)
      (let [fr  (->bit-fringe w h)
            _   (.put ^java.util.concurrent.ConcurrentHashMap pools k fr)]
        fr))))
