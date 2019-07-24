(ns icfpc.generator
  (:require
   [icfpc.core :refer :all]
   [icfpc.level :refer :all]
   [icfpc.parser :as parser]
   [icfpc.writer :as writer]
   [fastmath.core :as m])
  (:import [icfpc.core lev]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn build-path [parents point]
  (loop [path []
         current point
         depth 0]
    (if-let [parent (get parents current)]
      (if (or (< 10000 depth) (= parent current))
        (do
          (prn "PATH TOO LONG!")
          (conj path current))
        (recur (conj path current) parent (inc depth)))
      path)))

(defn bfs [level [x y :as start] target block]
  (loop [q (queue start)
         parents {start nil}]
    (if-let [p (peek q)]
      (let [ns (filter (fn [[x y :as n]]
                         (and
                          (not (contains? parents n))
                          (not= (get-level level x y) block)))
                       (neighbours level p))
            parents' (reduce (fn [ps n]
                               (assoc ps n p))
                             parents ns)
            finded (filter (fn [[x y :as n]]
                             (= (get-level level x y) target)) ns)]
        (if (not-empty finded)
          (build-path parents' (first finded))
          (recur (into (pop q) ns) parents')))
      (prn "CANT FIND PATH!"))))

(defn fill-connected-component [^lev level points target block]
  (let [[i-x i-y]  (first points)
        include-last (rest points)
        level (reduce (fn [^lev level next-include]
                        (let [path (bfs level next-include target block)]
                          (reduce (fn [^lev level [px py]]
                                    (set-level level px py target))
                                  level
                                  path)))
                      (set-level level i-x i-y target)
                      include-last)]
    (reduce (fn [^lev level [x y]]
              (set-level level x y target))
            level
            points)))

(defn fill-bfs [level [x y :as start] target block]
  (loop [q (queue start)
         parents {start nil}
         level level]
    (if-let [p (peek q)]
      (let [ns (filter (fn [[x y :as n]]
                         (and
                          (not (contains? parents n))
                          (not= (get-level level x y) block)))
                       (neighbours level p))
            parents' (reduce (fn [ps n]
                               (assoc ps n p))
                             parents ns)]
        (recur (into (pop q) ns) parents' (reduce (fn [^lev level [x y]]
                                                    (set-level level x y target))
                                                  level
                                                  ns)))
      level)))

(defn min-area ^long [^long t-size]
  (long (m/ceil (* 0.2 (* t-size t-size)))))

(defn inflate [level]
  (let [empty-fields (points-by-value level EMPTY)
        borders (filter
                 (fn [[x y]] (not= (get-level level x y) OBSTACLE))
                 (clojure.set/difference (into #{} (mapcat #(neighbours level %) empty-fields))
                                         (into #{} empty-fields)))]
    (reduce
     (fn [^lev level [x y]]
       (set-level level x y EMPTY))
     level
     borders)))

(defn inflate-min-area [level ^long min-area]
  (if (< (count (points-by-value level EMPTY)) min-area)
    (recur (inflate level) min-area)
    level))

(defn place-boosters [level boosters]
  (let [availible-positions (shuffle (points-by-value level EMPTY))]
    (reduce
     (fn [level [i b]]
       (let [[x y] (get availible-positions i)]
             (update level :boosters assoc [x y] b)))
     level
     (map (fn [i v] [i v])
          (range)
          (mapcat (fn [[booster count]]
                    (repeat count
                     (case booster
                       :extra-hands EXTRA_HAND
                       :fast-wheels FAST_WHEELS
                       :drills DRILL
                       :teleports TELEPORT
                       :cloning CLONE
                       :spawns SPAWN)))
                  boosters)))))

(defn place-bot [level]
  (let [[x y] (first (shuffle (points-by-value level EMPTY)))]
    (assoc level :x x :y y )))

(defn add-edge-bottom [level segments puzzle]
  (let [exclude (set (:exclude puzzle))]
    (first
     (for [[[^long x ^long y] [^long x' ^long y']] (map vector segments (next (cycle segments)))
           :when (and (= y y') (> y 0) (< x x') (>= (- x' x) 3))
           :let  [y'' (dec y)]
           x''   (range (inc x) (dec x'))
           :when (not (exclude [x'' y'']))]
       [x'' y'']))))

(defn add-edge-top [level segments puzzle]
  (let [exclude (set (:exclude puzzle))]
    (first
     (for [[[^long x ^long y] [^long x' ^long y']] (map vector segments (next (cycle segments)))
           :when (and (= y y') (< y ^int (:height level)) (< x' x) (>= (- x x') 3))
           :let  [y'' y]
           x''   (range (inc x') (dec x))
           :when (not (exclude [x'' y'']))]
       [x'' y'']))))

(defn add-edge-left [level segments puzzle]
  (let [exclude (set (:exclude puzzle))]
    (first
     (for [[[^long x ^long y] [^long x' ^long y']] (map vector segments (next (cycle segments)))
           :when (and (= x x') (> x 0) (< y' y) (>= (- y y') 3))
           :let  [x'' (dec x)]
           y''   (range (inc y') (dec y))
           :when (not (exclude [x'' y'']))]
       [x'' y'']))))

(defn add-edge-right [level segments puzzle]
  (let [exclude (set (:exclude puzzle))]
    (first
     (for [[[^long x ^long y] [^long x' ^long y']] (map vector segments (next (cycle segments)))
           :when (and (= x x') (< x ^int (:width level)) (< y y') (>= (- y' y) 3))
           :let  [x'' x]
           y''   (range (inc y) (dec y'))
           :when (not (exclude [x'' y'']))]
       [x'' y'']))))

(defn add-edges [^lev level puzzle]
  (let [segments (writer/segments level)]
    (if (>= (count segments) ^int (:v-min puzzle))
      level
      (let [[x y] (or (add-edge-bottom level segments puzzle)
                      (add-edge-top   level segments puzzle)
                      (add-edge-left  level segments puzzle)
                      (add-edge-right level segments puzzle)
                      (throw (Exception. (str "Can’t add enough edges: has " (count segments) " need " (:v-min puzzle)))))]
        (recur (set-level level x y EMPTY) puzzle)))))

(defn add-boundaries [{:keys [^int width ^int height] :as level} puzzle]
  (let [exclude (set (:exclude puzzle))
        p1      (->> (map (fn [y] [2 y]) (range 2 (- height 2)))
                     (seek #(not (exclude %))))
        p2      (->> (map (fn [y] [(- width 2) y]) (range 2 (- height 2)))
                     (reverse)
                     (seek #(not (exclude %))))]
    (update puzzle :include into [p1 p2])))

(defn generate-level [puzzle-name]
  (let [puzzle (parser/parse-puzzle puzzle-name)
        ^long t-size (:t-size puzzle)
        init-level {:name               (str puzzle-name ".desc")
                    :width              t-size
                    :height             t-size
                    :grid               (vec (repeat (* t-size t-size) UNKNOWN))
                    :boosters           {}
                    :x                  0
                    :y                  0
                    :layout             [[0 0] [1 0] [1 1] [1 -1]]
                    :collected-boosters {}
                    :active-boosters    {}
                    :path               ""}
        level (reduce (fn [^lev level [x y]]
                        (set-level level x y OBSTACLE))
                      init-level
                      (:exclude puzzle))
        puzzle (add-boundaries level puzzle)
        level (fill-connected-component level (:include puzzle) EMPTY OBSTACLE)
        level (reduce
               (fn [^lev level [x y]]
                 (set-level level x y UNKNOWN))
               level
               (:exclude puzzle))
        level (fill-connected-component level (:exclude puzzle) OBSTACLE EMPTY)
        level (inflate-min-area level (min-area (:t-size puzzle)))
        level (fill-bfs level (last (:exclude puzzle)) OBSTACLE EMPTY)
        level (reduce (fn [^lev level [x y]]
                        (set-level level x y EMPTY))
                      level
                      (points-by-value level UNKNOWN))
        level (add-edges level puzzle)
        level (place-boosters level (select-keys puzzle [:extra-hands :fast-wheels :drills :teleports :cloning :spawns]))
        level (place-bot level)]
    level))
