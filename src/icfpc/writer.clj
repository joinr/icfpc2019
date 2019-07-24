(ns icfpc.writer
  (:require
   [icfpc.core :refer :all]
   [clojure.string :as str])
  (:import [icfpc.core lev]))

(defn floor? [^lev level ^long x ^long y]
  (if (or (< x 0)
          (>= x (.width level))
          (< y 0)
          (>= y (.height level)))
    false
    (= EMPTY (get-level level x y))))

(defn segments
  ([^lev level]
   (let [[x y] (first 
                (for [y (range 0 (.height level))
                      x (range 0 (.width level))
                      :when (and (floor? level x y) (not (floor? level x (dec y))))]
                  [x y]))]
     (segments [x y] [(inc x) y] level)))
  ([p0 p1 level]
   (loop [acc [p0 p1]]
     (let [[x0 y0] (first acc)
           [x y]   (nth acc (- (count acc) 2))
           [x' y'] (nth acc (- (count acc) 1))
           LT (floor? level (dec x') y')
           RT (floor? level x' y')
           LB (floor? level (dec x') (dec y'))
           RB (floor? level x' (dec y'))]
                                        ; (println x y "->" x' y' LT RT LB RB)
       (cond
         (= [x' y'] [x0 y0]) ;; done
         (pop acc)

                                        ; (> (count acc) 10)
                                        ; [:error1 acc]

         (and (= x x') (> y' y)) ;; up
         (cond
           (= LT RB) (recur (conj acc [(dec x') y'])) ;; turn left
           (= RT LB) (recur (conj acc [(inc x') y'])) ;; turn right
           (and (= LT LB) (= RT RB)) (recur (conj (pop acc) [x' (inc y')])) ;; continue up
           :else [:error2 acc LT RT LB RB]) 

         (and (> x' x) (= y y')) ;; right
         (cond
           (= LT RB) (recur (conj acc [x' (dec y')])) ;; turn down
           (= RT LB) (recur (conj acc [x' (inc y')])) ;; turn up
           (and (= LT RT) (= LB RB)) (recur (conj (pop acc) [(inc x') y'])) ;; continue right
           :else [:error3 acc LT RT LB RB])

         (and (= x x') (< y' y)) ;; down
         (cond
           (= LT RB) (recur (conj acc [(inc x') y'])) ;; turn right
           (= RT LB) (recur (conj acc [(dec x') y'])) ;; turn left
           (and (= LT LB) (= RT RB)) (recur (conj (pop acc) [x' (dec y')])) ;; continue down
           :else [:error4 acc LT RT LB RB])

         (and (< x' x) (= y y')) ;; left
         (cond
           (= LT RB) (recur (conj acc [x' (inc y')])) ;; turn up
           (= RT LB) (recur (conj acc [x' (dec y')])) ;; turn down
           (and (= LT RT) (= LB RB)) (recur (conj (pop acc) [(dec x') y'])) ;; continue left
           :else [:error5 acc LT RT LB RB])

         :else [:error6 acc LT RT LB RB])))))

(defn desc [level]
  (let [segments (segments level)]
    (str 
     (str/join "," (map (fn [[x y]] (str "(" x "," y ")")) segments))
     "#"
     "(" (:x level) "," (:y level) ")"
     "##"
     (str/join ";"
               (map (fn [[[x y] code]] (str code "(" x "," y ")")) (:boosters level))))))
