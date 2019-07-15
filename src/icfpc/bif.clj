(ns icfpc.bif
  (:import [io.lacuna.bifurcan
            Set  LinearSet
            Map  LinearMap
            List LinearList
            IntMap 
            ]))



(defn ^LinearSet ->linear-set [& xs]
  (LinearSet/from ^java.lang.Iterable xs))


(defn ^IntMap  ->int-map []
  )
