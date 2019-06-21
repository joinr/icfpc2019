(ns icfpc.bot
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [icfpc.core :refer :all]
   [icfpc.level :refer :all]))

(s/def :level/width nat-int?)
(s/def :level/height nat-int?)
(s/def :level/grid (s/coll-of #{EMPTY OBSTACLE WRAPPED EXTRA_HAND FAST_WHEELS DRILL X_UNKNOWN_PERK} :kind vector?))
(s/def :boosters/amount nat-int?)
(s/def :boosters/ttl nat-int?)
(s/def :bot/collected-boosters
       (s/map-of #{EXTRA_HAND FAST_WHEELS DRILL X_UNKNOWN_PERK} :boosters/amount))
(s/def :bot/active-boosters (s/map-of #{EXTRA_HAND FAST_WHEELS DRILL X_UNKNOWN_PERK} :boosters/ttl))
(s/def :bot/layout vector?)
(s/def :bot/x nat-int?)
(s/def :bot/y nat-int?)

(s/def :frame/frame
       (s/keys :level/width
               :level/height
               :level/grid

               :bot/collected-boosters
               :bot/active-boosters
               :bot/layout
               :bot/x
               :bot/y))

(defn add-extra-hand [level]
  (let [max-x (first (apply max-key first (:bot/layout level)))]
    (update level :bot/layout conj [(inc max-x) 0])))

(defn fast-wheel-on [level]
  (update level :bot/active-boosters assoc FAST_WHEELS 50))

(defn drill-on [level]
  (update level :bot/active-boosters assoc DRILL 30))

(defn activate-booster [level booster]
  (condp = booster
    EXTRA_HAND (add-extra-hand level)
    FAST_WHEELS (fast-wheel-on level)
    DRILL (drill-on level)))

(defn has-available-booster [level booster]
  (let [v (get (:bot/collected-boosters level) booster)]
    (and (some? v)
         (< 0 v))))

(defn update-boosters [active-boosters]
  (into {}
        (map (fn [[booster ttl :as tpl]]
               (if (= booster EXTRA_HAND)
                 tpl
                 (when (< 0 ttl)
                   [booster (dec ttl)]))))
        active-boosters))

(defn move [level dx dy]
  (let [FW? (is-booster-active level FAST_WHEELS)]
    (-> level
        (update :bot/x + (cond-> dx FW? (* 2)))
        (update :bot/y + (cond-> dy FW? (* 2)))
        ;; todo smth with fast-wheels -- quiclky passed by nodes won't be colored
        (update :bot/active-boosters update-boosters))))

(defn rotate-ccw [level]
  (update level :bot/layout
    (fn [layout]
      (mapv (fn [[dx dy]] [(- dy) dx]) layout))))

(defn rotate-cw [level]
  (update level :bot/layout
    (fn [layout]
      (mapv (fn [[dx dy]] [dy (- dx)]) layout))))

(def max-path-len 10)

(defn lookahead-impl [queue paths]
  (let [[level path score seen] (peek queue)
        {:level/keys [width height] :bot/keys [x y]} level]
    (cond
      (empty? queue)
      (when-some [[level path score] (first paths)]
        (when (pos? score)
          [level path]))

      (>= (count path) max-path-len)
      (recur (pop queue) (conj paths [level path score]))

      :else
      (let [moves (for [[level' path'] [[(rotate-cw level)  (conj path ROTATE_CW)]
                                        [(rotate-ccw level) (conj path ROTATE_CCW)]
                                        [(move level  0  1) (conj path UP)]
                                        [(move level  0 -1) (conj path DOWN)]
                                        [(move level  1  0) (conj path RIGHT)]
                                        [(move level -1  0) (conj path LEFT)]]
                        :let [pos [(:bot/x level') (:bot/y level')]]
                        :when (valid? level')
                        :when (not (contains? seen pos))]
                    [(mark-wrapped level') path' (+ score (position-score level')) (conj seen pos)])]
        (recur
          (into (pop queue) moves)
          (conj paths [level path score]))))))

(defn lookahead [level]
  (lookahead-impl
    (queue [level [] 0 #{[(:bot/x level) (:bot/y level)]}])
    (sorted-set-by (fn [[_ path score]
                        [_ path' score']]
                     (compare
                       [score' (count path')]
                       [score (count path)])))))

(defn make-move-impl [queue seen]
  (let [[{:level/keys [width height]
          :bot/keys [x y] :as level} path score] (peek queue)]
    (cond
      (empty? queue)
      nil

      (< 0 score)
      [level path score]

      :else
      (let [moves (->>
                   (for [[level' path'] (into [[(move level  0  1) (conj path UP)]
                                               [(move level  0 -1) (conj path DOWN)]
                                               [(move level  1  0) (conj path RIGHT)]
                                               [(move level -1  0) (conj path LEFT)]
                                               [(rotate-cw level)  (conj path ROTATE_CW)]
                                               [(rotate-ccw level) (conj path ROTATE_CCW)]]
                                              ; (filter some?)
                                              ;; we can always add additional hand, but no need to activate
                                              ;; other boosters if we already have active one
                                              []
                                              #_[(when (has-available-booster level EXTRA_HAND)
                                                     [(activate-booster level EXTRA_HAND) (conj path EXTRA_HAND)])
                                               (when (and (has-available-booster level FAST_WHEELS)
                                                          (not (is-booster-active level FAST_WHEELS)))
                                                     [(activate-booster level FAST_WHEELS) (conj path FAST_WHEELS)])
                                               (when (and (has-available-booster level DRILL)
                                                          (is-booster-active level DRILL))
                                                     [(activate-booster level DRILL) (conj path DRILL)])])
                         :when (valid? level')
                         :when (not (contains? seen [(:bot/x level') (:bot/y level')]))]
                     [(mark-wrapped level') path' (+ score (position-score level' path'))])
                   (sort-by (fn [[_ _ score]] score))
                   (reverse))]
        (recur
          (into (pop queue) moves)
          (into seen (map (fn [[level' _ _]] [(:bot/x level') (:bot/y level')]) moves)))))))

(defn make-move [level]
  (make-move-impl (queue [level [] 0 ]) #{[(:bot/x level) (:bot/y level)]}))

(defn print-level [{:level/keys [width height name] :as level}]
  (println name)
  (doseq [y (range (dec height) -1 -1)]
    (doseq [x (range 0 width)
            :let [v (get-level level x y)]]
        (cond
          (and (= x (:bot/x level)) (= y (:bot/y level)))
          (print "\033[37;1;41m☺\033[0m")

          (= v EMPTY)
          (print "\033[103m•\033[0m")

          (= v WRAPPED)
          (print "\033[43m+\033[0m")

          :else
          (print (get-level level x y))))
    (println))
  (println))

(defn solve [level & [{:keys [debug? delay] :or {debug? true delay 50}}]]
  (loop [path  []
         level (mark-wrapped level)]
    (if-some [[level' path'] (or 
                               (lookahead level)
                               (make-move level))]
      (do
        (when debug?
          (println "\033[2J")
          (println (str "[" (:bot/x level) "," (:bot/y level) "] -> [" (:bot/x level') "," (:bot/y level') "] via " (str/join path')))
          (print-level level')
          (println (count (into path path')) "via" (str/join (into path path')))
          (when (some? delay)
            (Thread/sleep delay)))
        (recur (into path path') level'))
      (let [res (str/join path)]
        (println "SCORE" (count res))
        res))))

(comment
  (solve prob-001 true)
)