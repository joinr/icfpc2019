;;Testing a variety of search fringes
;;for the icfpc.bot/explore function.
(ns icfpc.fringe
  (:require [icfpc.core :as core]))

(defn clear-bits! [^"[[Z" bits]
  (do (areduce bits idx res bits
                   (java.util.Arrays/fill ^booleans (aget bits idx) false))
      bits))

(defn ->bit-fringe [w h]
  (let [^"[[Z" bits (make-array Boolean/TYPE (long w) (long h))]
    (reify core/IFringe
      (has-fringe? [this   o]        
        (aget ^booleans (aget bits (.nth ^clojure.lang.Indexed o 0))
              (.nth ^clojure.lang.Indexed o 1)))
      (add-fringe [this    o]
        (aset ^booleans (aget bits (.nth  ^clojure.lang.Indexed  o 0))
              (.nth ^clojure.lang.Indexed  o 1) true)
        this)
      (clear-fringe! [this]
        (do (areduce bits idx res bits
                   (java.util.Arrays/fill ^booleans (aget bits idx) false))
            this))
      clojure.lang.IFn
      (invoke [this] bits))))

