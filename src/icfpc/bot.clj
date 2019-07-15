(ns icfpc.bot
  (:require
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [icfpc.core :refer :all]
   [icfpc.level :refer :all]
   [icfpc.bif])
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
  (let [layout (-> level :bots (nth (level :bot) ) :layout set)]
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
  (let [{:keys [x y]} (nth bots (level :bot) )]
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
  (let [{:keys [x y]} (nth bots (level :bot) )]
    (when (and
            (not (*disabled* TELEPORT))
            (booster-collected? level TELEPORT)
            (not (contains? (level :beakons) [x y]))
            (every?
              (fn [[bx by]] (>= (+ (Math/abs ^long (- x bx)) (Math/abs ^long (- y by))) 50))
              (level :beakons)))
      (-> level
        (spend :collected-boosters TELEPORT)
        (update :beakons (fnil conj []) #_(->Point x y) [x y])
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
  (when-some [[bx by] (nth (level :beakons) idx nil)]
    (let [{:keys [x y]} (nth bots (level :bot) )]
      (when (not= [x y] [bx by])
        (-> level
          (update-bot :x (constantly bx))
          (update-bot :y (constantly by))
          (mark-wrapped)
          (update-bot :path str JUMP "(" bx "," by ")"))))))

(defn reduplicate [{:keys [bots spawns] :as level}]
  (let [{:keys [x y]} (nth bots (level :bot) )]
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


(defn can-step?
  [x y drill? drilled  ^icfpc.core.lev level]
  (and
   (<* -1 x (.width level))
   (<* -1 y (.width level))
   (or
    drill?
    (drilled (->Point x y))
    (not= OBSTACLE (get-level level x y)))))

#_(definline can-step?
  [x y drill? drilled  level]
  `(let [width#  (.width ~(with-meta  level {:tag 'icfpc.core.lev}))
         height# (.height ~(with-meta level {:tag 'icfpc.core.lev}))]
     (and
      (<* -1 ~x width#)
      (<* -1 ~y height#)
      (or
       ~drill?
       (~drilled (->Point ~x ~y))
       (not= OBSTACLE (get-level ~level ~x ~y))))))


#_(defn can-step? [x y drill? drilled {:keys [width height] :as level}]
  (and
    (< -1 x width)
    (< -1 y height)
    (or
      drill?
      (drilled (->Point x y))
      (not= OBSTACLE (get-level level x y)))))

#_(definline step [x y dx dy fast? drill? drilled level]
  `(let [x'# (unchecked-add ~x ~dx) y'# (unchecked-add ~y ~dy)]
     (when (can-step? x'# y'# ~drill? ~drilled ~level)
       (if ~fast?
         (let [x''# (unchecked-add x'# ~dx) y''# (unchecked-add y'# ~dy)]
           (if (can-step? x''# y''# ~drill? ~drilled ~level)
             (->Point x''# y''#)
             (->Point x'# y'#)))
         (->Point x'# y'#)))))

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
  (let [boosters (.boosters level)
        weights  (.weights level)
        width    (.width level)
        height   (.height level)
        bots     (.bots level)
        bot      (.bot level)
        zones?   (.zones? level)
        #_{:keys [layout current-zone]}
        botstate (nth bots bot)
        layout       (.valAt ^clojure.lang.IPersistentMap botstate :layout)
        current-zone (.valAt ^clojure.lang.IPersistentMap botstate :current-zone)
        ]
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
  (let [{:keys [layout current-zone]} (nth bots (level :bot)  )]
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

(def prior (atom nil))

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

(defn clear! [fr w h]
  (let [^"[[Z" bits (fr)]
    (if (and (= (alength bits) w)
             (= (alength ^booleans (aget bits 0)) h))
      (do (areduce bits idx res bits
                   (java.util.Arrays/fill ^booleans (aget bits idx) false))
          fr)
      (reset! prior (->bit-fringe w h)))))
  
(defn ->pooled-fringe [w h]
  (if @prior
    (clear! @prior w h)           
    (reset! prior (->bit-fringe w h))))
  
        
;;drilled is a hashset.

(defn explore* [{:keys [bots beakons width height] :as level} rate-fn]
  (let [{:keys [x y active-boosters] :as bot} (nth bots (level :bot)  )
        ;;we're hashing a lot here....
        ;;paths is just a set of [x y] coordinates.
        ^icfpc.bot.IFringe paths (-> (->pooled-fringe width height) (add-fringe (->Point x y)))
        queue (doto (ArrayDeque.) (.addAll [[[] (->Point x y) (active-boosters FAST_WHEELS 0) (active-boosters DRILL 0) #{}]]))
        explore-depth *explore-depth*]
    (loop [max-len   explore-depth
           best-path nil
           best-pos  nil
           best-rate 0.0]
      (if-some [[path [x y :as pos] fast drill drilled :as move] (.poll queue)]
        (let [path-length (.count ^clojure.lang.Counted path)]
          (if (< path-length max-len)
            ;; still exploring inside max-len
            (do
              ;; moves
              (doseq [[move dx dy] [[LEFT -1 0] [RIGHT 1 0] [UP 0 1] [DOWN 0 -1]]
                      :let  [pos' (step x y dx dy (pos? fast) (pos? drill) drilled level)]
                      :when (some? pos')
                      ;;haven't visited [x y] yet.
                      :when (not (.has-fringe? paths pos')) 
                      :let  [path'    (conj path move)
                             drilled' (cond-> drilled (pos? drill) (conj pos'))]]
                (.add-fringe  paths pos')
                (.add queue [path' pos' (spend fast) (spend drill) drilled']))
              ;; jumps
              (doseq [[move pos'] [[:jump0 (nth (level :beakons) 0 nil)]
                                   [:jump1 (nth (level :beakons) 1 nil)]
                                   [:jump2 (nth (level :beakons) 2 nil)]]
                      :when (some? pos')
                      ;;haven't visited [x y] yet.
                      :when (not (has-fringe? paths pos'))
                      :let [path' (conj path move)]]
                (.add-fringe paths pos')
                (.add queue [path' pos' (spend fast) (spend drill) drilled]))
              (cond+
               (zero? path-length) (recur max-len best-path best-pos best-rate)
               :let [rate (/ (rate-fn pos level) (double  path-length))]
               (zero? rate)       (recur max-len best-path best-pos best-rate)
               (zero? best-rate)  (recur max-len path pos rate)
               (> rate best-rate) (recur max-len path pos rate)
               (< rate best-rate) (recur max-len best-path best-pos best-rate)
               (< path-length     (.count ^clojure.lang.Counted  best-path)) (recur max-len path pos rate)
               :else (recur max-len best-path best-pos best-rate)))
            ;; only paths with len > max-len left, maybe already have good solution?
            (if (nil? best-path)
              (do
                (.addFirst queue move)
                (recur (unchecked-add #_+ max-len explore-depth) nil nil 0.0 #_(double 0))) ;; not found anything, try expand
              [best-path best-pos])))
        
        [best-path best-pos]))))

#_(defn explore* [{:keys [bots beakons] :as level} rate-fn]
  (let [{:keys [x y active-boosters]} (nth bots (level :bot)  )
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
            (doseq [[move pos'] [[:jump0 (nth (level :beakons) 0 nil)]
                                 [:jump1 (nth (level :beakons) 1 nil)]
                                 [:jump2 (nth (level :beakons) 2 nil)]]
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
  (let [{:keys [active-boosters x y]} (nth bots (level :bot)  )
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
          :when-some [i (seek #(= [x y] [((nth bots %) :x ) ((nth bots %) :y)])
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
                            (filterv #(pos? (second %)) (bot :active-boosters))))
    (println "Collected:" collected-boosters)
    (println "Score:"     (level-score level))
    (println "Zones:"     (mapv #(zone-char (% :current-zone)) bots)))
  ([level delay]
    (println "\033[2J")
    (print-step level)
    (when (some? delay)
      (Thread/sleep delay))))

(defn collect-clone [{:keys [boosters collected-boosters bot] :as level}]
  (when (and
          (= 0 bot )
          (= 0 (collected-boosters CLONE 0))
          (some (fn [[_ b]] (= b CLONE)) boosters))
    (explore level (fn [[x y] level]
                     (if (= (boosters [x y]) CLONE) 1 0)))))

(defn goto-spawn [{:keys [bot bots spawns collected-boosters] :as level}]
  (let [{:keys [x y]} (nth bots (level :bot))]
    (when (and
            (= 0 bot )
            (pos? (collected-boosters CLONE 0))
            (not (spawns [x y])))
      (explore level (fn [[x y] level]
                       (if (spawns [x y]) 1 0))))))

(defn choose-next-zone [{:keys [bots] :as level}]
  (let [{:keys [current-zone]} (nth bots (level :bot)  )]
    (when (or (nil? current-zone)
            (= 0 (zone-area level current-zone)))
      (let [taken        (set (map :current-zone bots))
            unfinished   (set
                           (for [[zone area] (level :zones-area )
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

;(definline nempty [coll]
  

(defn advance* [level]
  (cond+
    :let [bot (nth (level :bots) (level :bot))]

    :when-some [level' (when (level :zones?) #_*zones?* (choose-next-zone level))]
    (recur level')

    :when-some [picked-booster (bot :picked-booster)]
    (recur
      (-> level
        (update :collected-boosters update picked-booster (fnil inc 0))
        (update-bot :picked-booster (constantly nil))))

    :when-some [plan #_(some-map (bot :plan)) (not-empty (bot :plan))]
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
      (println "BOT" (level :bot)  (nth (level :bots) (level :bot) ))
      (print-step level)
      (throw e))))

;;all this delay stuff....probably doesn't show up in the rust version
(defn solve [level & [{:keys [debug? delay disabled zones? explore-depth] :or {debug? true}}]]
  (let [t0 (System/currentTimeMillis)
        *last-frame (atom 0)]
    (binding [*disabled*      (or disabled *disabled*)
              *explore-depth* (or explore-depth *explore-depth*)
              ;*zones?*        (if zones? zones? *zones?*)
              ]
      (loop [level (mark-wrapped (assoc level :bot 0 :zones? (or zones? *zones?*)))]
        (when (.isInterrupted (Thread/currentThread))
          (throw (InterruptedException.)))
        (when (or (some? delay)
                (and debug? (>= (- (System/currentTimeMillis) @*last-frame) 200)))
          (print-step level delay)
          (reset! *last-frame (System/currentTimeMillis)))

        (if (zero? (level :empty))
          (do
            (when debug? (print-step level))
            {:path  (str/join "#" (map :path (level :bots) ))
             :score (level-score level)
             :time  (- (System/currentTimeMillis) t0)})
          (recur
            (reduce
              (fn [level i]
                (if-some [level' (advance (assoc level :bot i))]
                    (if (zero? (level' :empty))
                      (reduced level')
                      level')
                    level))
              level
              (range 0 (count (level :bots))))))))))

#_(defn solve [level & [{:keys [debug? delay disabled zones? explore-depth] :or {debug? true}}]]
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

        (if (= 0 (level :empty ))
          (do
            (when debug? (print-step level))
            {:path  (str/join "#" (map :path (level :bots) ))
             :score (level-score level)
             :time  (- (System/currentTimeMillis) t0)})
          (recur
            (reduce
              (fn [level i]
                (binding [*bot* i]
                  (if-some [level' (advance level)]
                    (if (= 0 (level' :empty))
                      (reduced level')
                      level')
                    level)))
              level
              (range 0 (count (level :bots))))))))))
