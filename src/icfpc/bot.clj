(ns icfpc.bot
  (:require
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [icfpc.core :refer :all]
   [icfpc.level :refer :all])
  (:import
   [java.util Collection HashMap HashSet ArrayDeque]))

(def ^:dynamic *disabled* #{})
(def ^:dynamic *explore-depth* 5)
(def ^:dynamic *zones?* true)

(s/def :level/width nat-int?)
(s/def :level/height nat-int?)
(s/def :level/grid (s/coll-of #{EMPTY OBSTACLE WRAPPED EXTRA_HAND FAST_WHEELS DRILL SPAWN} :kind vector?))
(s/def :boosters/amount nat-int?)
(s/def :boosters/ttl nat-int?)
(s/def :bot/collected-boosters
       (s/map-of #{EXTRA_HAND FAST_WHEELS DRILL SPAWN} :boosters/amount))
(s/def :bot/active-boosters (s/map-of #{EXTRA_HAND FAST_WHEELS DRILL SPAWN} :boosters/ttl))
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

(defn next-hand [level]
  (let [layout (-> level :bots (nth *bot*) :layout set)]
    (cond
      (contains? layout [1 0]) ;; rigth
      [1 (inc (apply max (map second layout)))]

      (contains? layout [0 1]) ;; up
      [(dec (apply min (map first layout))) 1]

      (contains? layout [-1 0]) ;; left
      [-1 (dec (apply min (map second layout)))]

      (contains? layout [0 -1]) ;; down
      [(inc (apply max (map first layout))) -1])))

(defn add-extra-hand [{:keys [bots] :as level}]
  (when (and
          (not (*disabled* EXTRA_HAND))
          (booster-collected? level EXTRA_HAND))
    (when-some [[x y] (next-hand level)]
      (-> level
        (spend :collected-boosters EXTRA_HAND)
        (update-bot :layout conj [x y])
        (update-bot :path str "B(" x "," y ")")))))

(defn has-space? [{:keys [bots] :as level}]
  (let [{:keys [x y]} (nth bots *bot*)]
    (or
      (every? #(= EMPTY (get-level level (+ x %) y OBSTACLE)) (range 1 5))
      (every? #(= EMPTY (get-level level (- x %) y OBSTACLE)) (range 1 5))
      (every? #(= EMPTY (get-level level x (+ y %) OBSTACLE)) (range 1 5))
      (every? #(= EMPTY (get-level level x (- y %) OBSTACLE)) (range 1 5)))))

(defn add-fast-wheels [level]
  (when (and
          (not (*disabled* FAST_WHEELS))
          (booster-collected? level FAST_WHEELS)
          (not (booster-active? level FAST_WHEELS))
          (has-space? level))
    (-> level
      (spend :collected-boosters FAST_WHEELS)
      (update-bot :active-boosters update FAST_WHEELS (fnil + 0) 51)
      (update-bot :path str FAST_WHEELS))))

(defn add-drill [level]
  (when (and
          (not (*disabled* DRILL))
          (booster-collected? level DRILL)
          (not (booster-active? level DRILL)))
    (-> level
      (spend :collected-boosters DRILL)
      (update-bot :active-boosters update DRILL (fnil + 0) 31)
      (update-bot :path str DRILL))))

(defn set-beakon [{:keys [bots] :as level}]
  (let [{:keys [x y]} (nth bots *bot*)]
    (when (and
            (not (*disabled* TELEPORT))
            (booster-collected? level TELEPORT)
            (not (contains? (:beakons level) [x y]))
            (every?
              (fn [[bx by]] (>= (+ (Math/abs ^long (- x bx)) (Math/abs ^long (- y by))) 50))
              (:beakons level)))
      (-> level
        (spend :collected-boosters TELEPORT)
        (update :beakons (fnil conj []) [x y])
        (update-bot :path str SET_BEAKON)))))

(defn extra-move [level dx dy]
  (if (booster-active? level FAST_WHEELS)
    (let [level' (-> level
                   (update-bot :x + dx)
                   (update-bot :y + dy))]
      (if (valid? level')
        (mark-wrapped level')
        level))
    level))

(defn move [level dx dy action]
  (some-> level
    (update-bot :x + dx)
    (update-bot :y + dy)
    (valid?)
    (mark-wrapped)
    (extra-move dx dy)
    (update-bot :path str action)))

(defn jump [{:keys [bots] :as level} idx]
  (when-some [[bx by] (nth (:beakons level) idx nil)]
    (let [{:keys [x y]} (nth bots *bot*)]
      (when (not= [x y] [bx by])
        (-> level
          (update-bot :x (constantly bx))
          (update-bot :y (constantly by))
          (mark-wrapped)
          (update-bot :path str JUMP "(" bx "," by ")"))))))

(defn reduplicate [{:keys [bots spawns] :as level}]
  (let [{:keys [x y]} (nth bots *bot*)]
    (when (and
            (booster-collected? level CLONE)
            (spawns [x y]))
      (-> level
        (update-bot :path str REPLICATE)
        (update     :bots conj (new-bot x y))
        (spend      :collected-boosters CLONE)))))

(defn act [level action]
  (condp = action
    UP     (move level 0 1 UP)
    DOWN   (move level 0 -1 DOWN)
    LEFT   (move level -1 0 LEFT)
    RIGHT  (move level 1 0 RIGHT)
    :jump0 (jump level 0)
    :jump1 (jump level 1)
    :jump2 (jump level 2)
    WAIT   (update-bot level :path str WAIT)))

#_(defn <3 [x y z]
  (and (< x y)
       (< y z)))

(defn can-step? [x y drill? drilled  level]
  (let [width (if (instance? icfpc.core.lev level)
                (.width ^icfpc.core.lev level)
                (.valAt ^clojure.lang.Associative level :width))
        height (if (instance? icfpc.core.lev level)
                 (.height ^icfpc.core.lev level)
                 (.valAt ^clojure.lang.Associative level :height))]
    (and
     (<3 -1 x width)
     (<3 -1 y height)
     (or
      drill?
      (drilled (->Point x y))
      (not= OBSTACLE (get-level level x y))))))

#_(defn can-step? [x y drill? drilled {:keys [width height] :as level}]
  (and
    (< -1 x width)
    (< -1 y height)
    (or
      drill?
      (drilled (->Point x y))
      (not= OBSTACLE (get-level level x y)))))

(defn step [x y dx dy fast? drill? drilled level]
  (let [x' (+ x dx) y' (+ y dy)]
    (when (can-step? x' y' drill? drilled level)
      (if fast?
        (let [x'' (+ x' dx) y'' (+ y' dy)]
          (if (can-step? x'' y'' drill? drilled level)
            (->Point x'' y'')
            (->Point x' y')))
        (->Point x' y')))))

(defn rate [[x y] ^icfpc.core.lev level]
  (let [;{:keys [boosters weights width height bots] :as level}
        boosters (.boosters level)
        weights  (.weights level)
        width    (.width level)
        height   (.height level)
        bots     (.bots level)
        {:keys [layout current-zone]} (nth bots *bot*)
        zones? *zones?*]
    (cond
      (boosters [x y])
      (if (or (not zones?) (== current-zone (get-zone level x y))) 100 0)
      ; (= EMPTY (get-level level x y)) (max 1 (aget weights (coord->idx level x y)))
      ; (= EMPTY (get-level level x y)) 1
      :else
      (reduce
        (fn [acc [dx dy]]
          (let [x' (unchecked-add x dx) y' (unchecked-add y dy)]
            (if (and
                  (or
                    (and (zero? dx) (zero? dy))
                    (valid-hand? x y dx dy level))
                  (zero? (get-level level x' y'))
                  (or (not zones?) (== current-zone (get-zone level x y))))
              (unchecked-add acc (max 1 (aget ^shorts weights (coord->idx level x' y'))))
              acc)))
        0
        layout)
      :else 0)))

#_(defn rate [[x y] {:keys [boosters weights width height bots] :as level}]
  (let [{:keys [layout current-zone]} (nth bots *bot*)]
    (cond
      (boosters [x y])
      (if (or (not *zones?*) (= current-zone (get-zone level x y))) 100 0)
      ; (= EMPTY (get-level level x y)) (max 1 (aget weights (coord->idx level x y)))
      ; (= EMPTY (get-level level x y)) 1
      :else
      (reduce
        (fn [acc [dx dy]]
          (let [x' (+ x dx) y' (+ y dy)]
            (if (and
                  (or
                    (= [0 0] [dx dy])
                    (valid-hand? x y dx dy level))
                  (= EMPTY (get-level level x' y'))
                  (or (not *zones?*) (= current-zone (get-zone level x y))))
              (+ acc (max 1 (aget ^shorts weights (coord->idx level x' y'))))
              acc)))
        0
        layout)
      :else 0)))

;; (def fringe (atom 0))
;; (def adds   (atom 0))

;; (defn add! [ ^HashSet s v]
;;   (do (swap! fringe #(max % (count s)))
;;       (swap! adds inc)
;;       (.add s v)))

(defprotocol IFringe
  (has-fringe? [this o])
  (add-fringe  [this o]))

(defn ->hash-fringe []
  (let [h (HashSet.)]
    (reify IFringe
      (has-fringe? [this o] (.contains h o))
      (add-fringe [this o] (do (.add h o) this)))))

(defn ->int-fringe []
  
  )
  
(defn explore* [{:keys [bots beakons] :as level} rate-fn]
  (let [{:keys [x y active-boosters]} (nth bots *bot*)
        ;;we're hashing a lot here....
        ;;paths is just a set of [x y] coordinates.
        paths (-> (->hash-fringe) (add-fringe (->Point x y)))
        queue (doto (ArrayDeque.) (.addAll [[[] (->Point x y) (active-boosters FAST_WHEELS 0) (active-boosters DRILL 0) #{}]]))]
    (loop [max-len   *explore-depth*
           best-path nil
           best-pos  nil
           best-rate (double 0)]
      (if-some [[path [x y :as pos] fast drill drilled :as move] (.poll queue)]
        (if (< (count path) max-len)
          ;; still exploring inside max-len
          (do
            ;; moves
            (doseq [[move dx dy] [[LEFT -1 0] [RIGHT 1 0] [UP 0 1] [DOWN 0 -1]]
                    :let  [pos' (step x y dx dy (pos? fast) (pos? drill) drilled level)]
                    :when (some? pos')
                    ;;haven't visited [x y] yet.
                    :when (not (has-fringe? paths pos')) 
                    :let  [path'    (conj path move)
                           drilled' (cond-> drilled (pos? drill) (conj pos'))]]
              (add-fringe paths pos')
              (.add queue [path' pos' (spend fast) (spend drill) drilled']))
            ;; jumps
            (doseq [[move pos'] [[:jump0 (nth (:beakons level) 0 nil)]
                                 [:jump1 (nth (:beakons level) 1 nil)]
                                 [:jump2 (nth (:beakons level) 2 nil)]]
                    :when (some? pos')
                    ;;haven't visited [x y] yet.
                    :when (not (has-fringe? paths pos'))
                    :let [path' (conj path move)]]
              (add-fringe paths pos')
              (.add queue [path' pos' (spend fast) (spend drill) drilled]))
            (cond+
              (empty? path)      (recur max-len best-path best-pos best-rate)
              :let [rate (double (/ (rate-fn pos level) (count path)))]
              (zero? rate)       (recur max-len best-path best-pos best-rate)
              (zero? best-rate)  (recur max-len path pos rate)
              (> rate best-rate) (recur max-len path pos rate)
              (< rate best-rate) (recur max-len best-path best-pos best-rate)
              (< (count path) (count best-path)) (recur max-len path pos rate)
              :else (recur max-len best-path best-pos best-rate)))
          ;; only paths with len > max-len left, maybe already have good solution?
          (if (nil? best-path)
            (do
              (.addFirst queue move)
              (recur (+ max-len *explore-depth*) nil nil (double 0))) ;; not found anything, try expand
            [best-path best-pos]))
        [best-path best-pos]))))

#_(defn explore* [{:keys [bots beakons] :as level} rate-fn]
  (let [{:keys [x y active-boosters]} (nth bots *bot*)
        ;;we're hashing a lot here....
        ;;paths is just a set of [x y] coordinates.
        paths (doto (HashSet.) (.addAll [(->Point x y)]))
        queue (doto (ArrayDeque.) (.addAll [[[] (->Point x y) (active-boosters FAST_WHEELS 0) (active-boosters DRILL 0) #{}]]))]
    (loop [max-len   *explore-depth*
           best-path nil
           best-pos  nil
           best-rate (double 0)]
      (if-some [[path [x y :as pos] fast drill drilled :as move] (.poll queue)]
        (if (< (count path) max-len)
          ;; still exploring inside max-len
          (do
            ;; moves
            (doseq [[move dx dy] [[LEFT -1 0] [RIGHT 1 0] [UP 0 1] [DOWN 0 -1]]
                    :let  [pos' (step x y dx dy (pos? fast) (pos? drill) drilled level)]
                    :when (some? pos')
                    ;;haven't visited [x y] yet.
                    :when (not (.contains paths pos')) 
                    :let  [path'    (conj path move)
                           drilled' (cond-> drilled (pos? drill) (conj pos'))]]
              (#_add! .add paths pos')
              (.add queue [path' pos' (spend fast) (spend drill) drilled']))
            ;; jumps
            (doseq [[move pos'] [[:jump0 (nth (:beakons level) 0 nil)]
                                 [:jump1 (nth (:beakons level) 1 nil)]
                                 [:jump2 (nth (:beakons level) 2 nil)]]
                    :when (some? pos')
                    ;;haven't visited [x y] yet.
                    :when (not (.contains paths pos'))
                    :let [path' (conj path move)]]
              (.add paths pos')
              (.add queue [path' pos' (spend fast) (spend drill) drilled]))
            (cond+
              (empty? path)      (recur max-len best-path best-pos best-rate)
              :let [rate (double (/ (rate-fn pos level) (count path)))]
              (zero? rate)       (recur max-len best-path best-pos best-rate)
              (zero? best-rate)  (recur max-len path pos rate)
              (> rate best-rate) (recur max-len path pos rate)
              (< rate best-rate) (recur max-len best-path best-pos best-rate)
              (< (count path) (count best-path)) (recur max-len path pos rate)
              :else (recur max-len best-path best-pos best-rate)))
          ;; only paths with len > max-len left, maybe already have good solution?
          (if (nil? best-path)
            (do
              (.addFirst queue move)
              (recur (+ max-len *explore-depth*) nil nil (double 0))) ;; not found anything, try expand
            [best-path best-pos]))
        [best-path best-pos]))))

(defn explore [level rate-fn]
  (first (explore* level rate-fn)))

(defn wait-off-fast [{:keys [bots] :as level}]
  (let [{:keys [active-boosters x y]} (nth bots *bot*)
        fast (active-boosters FAST_WHEELS 0)]
    (when (pos? fast)
      (repeat fast WAIT))))

(defn zone-char [n]
  (cond
    (= n 0) \0
    (nil? n) \?
    :else (char
            (+ (dec (int \a))
               (mod n (- (int \z) (int \a)))))))

(defn print-level [{:keys [bots width height name boosters beakons spawns] :as level} 
                   & {:keys [colored? max-w max-h] :or {max-w 50 max-h 20 colored? true zones? false}}]
  (println name)
  (let [beakons (set beakons)
        {:keys [x y]} (last bots)]
  (doseq [y (range
              (min (dec height) (+ y max-h))
              (dec (max 0 (- y max-h))) -1)]
    (doseq [x (range (max 0 (- x max-w)) (min width (+ x max-w)))
            :let [v (get-level level x y)
                  booster (get boosters [x y])]]
        (cond+
          :when-some [i (seek #(= [x y] [(:x (nth bots %)) (:y (nth bots %))])
                          (range 0 (count bots)))]
          (if colored?
            (print (str "\033[97;101m" i "\033[0m"))
            (print "☺"))

          (some? booster)
          (if colored?
            (print (str "\033[97;42m" booster "\033[0m"))
            (print booster))

          (contains? spawns [x y])
          (if colored?
            (print (str "\033[97;44mX\033[0m"))
            (print "X"))

          (contains? beakons [x y])
          (if colored?
            (print (str "\033[97;44m@\033[0m"))
            (print "@"))

          (= v EMPTY)
          (if colored?
            (print (str "\033[103m" (zone-char (get-zone level x y)) "\033[0m"))
            (print (zone-char (get-zone level x y))))

          (= v WRAPPED)
          (if colored?
            (print "\033[97;43m.\033[0m")
            (print "+"))

          (= v OBSTACLE)
          (print ".")

          :else
          (print (get-level level x y))))
    (println)))
  (println))

(defn print-step
  ([{:keys [bots collected-boosters path] :as level}]
    (print-level level)
    (println "Active:"    (for [bot bots]
                            (filterv #(pos? (second %)) (:active-boosters bot))))
    (println "Collected:" collected-boosters)
    (println "Score:"     (level-score level))
    (println "Zones:"     (mapv #(zone-char (:current-zone %)) bots)))
  ([level delay]
    (println "\033[2J")
    (print-step level)
    (when (some? delay)
      (Thread/sleep delay))))

(defn collect-clone [{:keys [boosters collected-boosters] :as level}]
  (when (and
          (= 0 *bot*)
          (= 0 (collected-boosters CLONE 0))
          (some (fn [[_ b]] (= b CLONE)) boosters))
    (explore level (fn [[x y] level]
                     (if (= (boosters [x y]) CLONE) 1 0)))))

(defn goto-spawn [{:keys [bots spawns collected-boosters] :as level}]
  (let [{:keys [x y]} (nth bots *bot*)]
    (when (and
            (= 0 *bot*)
            (pos? (collected-boosters CLONE 0))
            (not (spawns [x y])))
      (explore level (fn [[x y] level]
                       (if (spawns [x y]) 1 0))))))

(defn choose-next-zone [{:keys [bots] :as level}]
  (let [{:keys [current-zone]} (nth bots *bot*)]
    (when (or (nil? current-zone)
            (= 0 (zone-area level current-zone)))
      (let [taken        (set (map :current-zone bots))
            unfinished   (set
                           (for [[zone area] (:zones-area level)
                                 :when (pos? area)]
                              zone))
            untaken      (set/difference unfinished taken)
            look-in      (if (empty? untaken) unfinished untaken)
            [path [x y]] (explore* level
                           (fn [[x y] level]
                             (cond+
                               (not= EMPTY (get-level level x y)) 0
                               (look-in (get-zone level x y)) 1
                               :else 0)))]
        (-> level
          (update-bot :current-zone (constantly (get-zone level x y)))))))) ;; TODO set path too

(defn advance* [level]
  (cond+
    :let [bot (nth (:bots level) *bot*)]

    :when-some [level' (when *zones?* (choose-next-zone level))]
    (recur level')

    :when-some [picked-booster (:picked-booster bot)]
    (recur
      (-> level
        (update :collected-boosters update picked-booster (fnil inc 0))
        (update-bot :picked-booster (constantly nil))))

    :when-some [plan (not-empty (:plan bot))]
    (let [action (first plan)
          level' (-> (act level action)
                   (wear-off-boosters))]
      (if false #_(> (- (:empty level) (:empty level')) 3)
        (update-bot level' :plan (constantly nil))
        (update-bot level' :plan #(drop 1 %))))

    :when-some [level' (or
                         (reduplicate level)
                         (add-extra-hand level)
                         (add-fast-wheels level) 
                         (add-drill level)
                         (set-beakon level))]
    (wear-off-boosters level')

    :when-some [plan (or 
                       (goto-spawn level)
                       (collect-clone level)
                       (explore level rate)
                       (wait-off-fast level))]
    (recur (update-bot level :plan (constantly plan)))))

(defn advance [level]
  (try
    (advance* level)
    (catch Exception e
      (println "BOT" *bot* (nth (:bots level) *bot*))
      (print-step level)
      (throw e))))

(defn solve [level & [{:keys [debug? delay disabled zones? explore-depth] :or {debug? true}}]]
  (let [t0 (System/currentTimeMillis)
        *last-frame (atom 0)]
    (binding [*disabled*      (or disabled *disabled*)
              *explore-depth* (or explore-depth *explore-depth*)
              *zones?*        (if zones? zones? *zones?*)]
      (loop [level (binding [*bot* 0] (mark-wrapped level))]
        (when (.isInterrupted (Thread/currentThread))
          (throw (InterruptedException.)))

        (when (or (some? delay)
                (and debug? (>= (- (System/currentTimeMillis) @*last-frame) 200)))
          (print-step level delay)
          (reset! *last-frame (System/currentTimeMillis)))

        (if (= 0 (:empty level))
          (do
            (when debug? (print-step level))
            {:path  (str/join "#" (map :path (:bots level)))
             :score (level-score level)
             :time  (- (System/currentTimeMillis) t0)})
          (recur
            (reduce
              (fn [level i]
                (binding [*bot* i]
                  (if-some [level' (advance level)]
                    (if (= 0 (:empty level'))
                      (reduced level')
                      level')
                    level)))
              level
              (range 0 (count (:bots level))))))))))
