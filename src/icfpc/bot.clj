(ns icfpc.bot
  (:require
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [icfpc.core :refer :all]
   [icfpc.level :refer :all]
   [icfpc.fringe :as fringe]
   [fastmath.core :as m]
   [icfpc.speed :refer [with-slots]]
   [icfpc.speed.string :refer [make-string]])
  (:import
   [java.util Collection HashMap HashSet ArrayDeque]
   [clojure.lang Indexed Counted IPersistentMap IPersistentVector IPersistentSet]
   [icfpc.core lev Point robot]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:dynamic *disabled* #{})
(def ^:dynamic ^:long *explore-depth* 5)
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

(defn next-hand [^lev level]
  (let [layout (-> (.bots level) (nth (.bot level)) :layout set)]
    (cond
      (contains? layout [1 0]) ;; rigth
      [1 (inc ^int (apply clojure.core/max (map second layout)))]

      (contains? layout [0 1]) ;; up
      [(dec ^int (apply clojure.core/min (map first layout))) 1]

      (contains? layout [-1 0]) ;; left
      [-1 (dec ^int (apply clojure.core/min (map second layout)))]

      (contains? layout [0 -1]) ;; down
      [(inc ^int (apply clojure.core/max (map first layout))) -1])))

(defn add-extra-hand [^lev level]
  (when (and
          (not (*disabled* EXTRA_HAND))
          (booster-collected? level EXTRA_HAND))
    (when-some [[x y] (next-hand level)]
      (-> level
        (spend :collected-boosters EXTRA_HAND)
        (update-bot :layout conj [x y])
        (update-bot :path str "B(" x "," y ")")))))

(defn has-space? [^lev level]
  (let [{:keys [^int x ^int y]} (nth (.bots level) (.bot level))]
    (or
     (every? (fn [^long v] (== 0 (long (get-level level (+ x v) y OBSTACLE)))) (range 1 5))
     (every? (fn [^long v] (== 0 (long (get-level level (- x v) y OBSTACLE)))) (range 1 5))
     (every? (fn [^long v] (== 0 (long (get-level level x (+ y v) OBSTACLE)))) (range 1 5))
     (every? (fn [^long v] (== 0 (long (get-level level x (- y v) OBSTACLE)))) (range 1 5)))))

(defn add-fast-wheels [level]
  (when (and
         (not (*disabled* FAST_WHEELS))
         (booster-collected? level FAST_WHEELS)
         (not (booster-active? level FAST_WHEELS))
         (has-space? level))
    (-> level
        (spend :collected-boosters FAST_WHEELS)
        (update-bot :active-boosters update FAST_WHEELS (fnil clojure.core/+ 0) 51)
        (update-bot :path str FAST_WHEELS))))

(defn add-drill [level]
  (when (and
         (not (*disabled* DRILL))
         (booster-collected? level DRILL)
         (not (booster-active? level DRILL)))
    (-> level
        (spend :collected-boosters DRILL)
        (update-bot :active-boosters update DRILL (fnil clojure.core/+ 0) 31)
        (update-bot :path str DRILL))))

(defn set-beakon [^lev level]
  (let [{:keys [^long x ^long y]} (nth (.bots level) (.bot level))]
    (when (and
           (not (*disabled* TELEPORT))
           (booster-collected? level TELEPORT)
           (not (contains? (.beakons level) [x y]))
           (every?
            (fn [[^long bx ^long by]] (>= (+ (m/abs (- x bx)) (m/abs (- y by))) 50))
            (.beakons level)))
      (-> level
          (spend :collected-boosters TELEPORT)
          (update :beakons (fnil conj []) #_(->Point x y) [x y])
          (update-bot :path str SET_BEAKON)))))

(defn extra-move [level ^long dx ^long dy]
  (if (booster-active? level FAST_WHEELS)
    (let [level' (-> level
                     (update-bot :x clojure.core/+ dx)
                     (update-bot :y clojure.core/+ dy))]
      (if (valid? level')
        (mark-wrapped level')
        level))
    level))

;;this one has at 8441 min...the only thing I
;;can guess is that inlining is a factor.
;;turns out it was the assoc, multi-arity
;;call to assoc incurs restfn debt.
;;make-string gets us away from restfn,
;;down for 8411, meh.
(defn move [^lev level ^long dx ^long dy action]
  (some-> level
    (map-bot  (fn moveupd [bot]
                (with-slots [{:keys [^long x ^long y path]} ^IPersistentMap bot]
                  (assoc* bot :x    (+ x dx)
                              :y    (+ y dy)
                              :path (make-string #_str path action)))))
    (valid?)
    (mark-wrapped)
    (extra-move dx dy)))

;;for some wierd reason, I think this version is more inlining friendly...
;;we get down to 8625 somehow...edit [multiple calls to
;;update outweighed be variadic assoc costs]
#_(defn move [level dx dy action]
  (some-> level
    (update-bot :x + dx)
    (update-bot :y + dy)
    (valid?)
    (mark-wrapped)
    (extra-move dx dy)
    (update-bot :path str action)))

(defn jump [^lev level idx]
  (when-some [[bx by] (nth (.beakons level) idx nil)]
    (let [{:keys [x y]} (nth (.bots level) (.bot level) )]
      (when (not= [x y] [bx by])
        (-> level
          (update-bot :x (constantly bx))
          (update-bot :y (constantly by))
          (mark-wrapped)
          (update-bot :path str JUMP "(" bx "," by ")"))))))

(defn reduplicate [^lev level]
  (let [{:keys [x y]} (nth (.bots level) (.bot level) )]
    (when (and
            (booster-collected? level CLONE)
            ((.spawns level) [x y]))
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
  [x y drill? drilled  ^lev level]
  (and
   (< -1 ^long x (.width level))
   (< -1 ^long y (.height level))
   (or
    drill?
    (drilled (->Point x y))
    (not= OBSTACLE (get-level level x y)))))

(defn step [x y dx dy fast? drill? drilled level]
  (let [x' (+ ^long x ^long dx) y' (+ ^long y ^long dy)]
    (when (can-step? x' y' drill? drilled level)
      (if fast?
        (let [x'' (+ x' ^long dx) y'' (+ y' ^long dy)]
          (if (can-step? x'' y'' drill? drilled level)
            (->Point x'' y'')
            (->Point x' y')))
        (->Point x' y')))))

(defn rate [xy  level]
  (with-slots [[^long x ^long y] ^Indexed xy
               {:fields [boosters weights width height ^Indexed bots bot zones?]}
               ^lev level
                                        ;{:fields   [layout current-zone]} ^robot
               {:keys   [layout ^byte current-zone]} ^IPersistentMap
               (.nth bots bot)]
    (cond
      ;;THis is a persistentmap invocation.  We're hashing the vector [x y]...
      (boosters #_(->Point x y) [x y])
      (if (or (not zones?) (== current-zone ^byte (get-zone level x y))) 100 0)
      :else
      (reduce
       (fn [^long acc  dxdy]
         (with-slots
           [[^long dx ^long dy] ^Indexed dxdy
            x' (+ x dx)
            y' (+ y dy)]
           (if (and
                (or
                 (and (zero? dx) (zero? dy))
                 (valid-hand? x y dx dy level))
                (zero? ^long (get-level level x' y'))
                (or (not zones?) (== current-zone ^byte (get-zone level x y))))
             (+ acc (max 1 (aget ^shorts weights (coord->idx level x' y'))))
             acc)))
       0
       layout)
      :else 0)))
        
;;drilled is a persistent hashset.
;;we can probably improve drilled performance.

;;interesting note: as with other research, array maps are competitive with records even with
;;field lookups.  This is definately not the case for non-array maps.  Curious.
;;In this case, field access win out marginally.
(defrecord botmove [^IPersistentVector path ^Point pos ^int fast ^int drill ^IPersistentSet drilled])

(definline init-fringe [fringe x y]
  (let [fringe (with-meta fringe {:tag 'icfpc.core.IFringe})
        cleared    (with-meta `(.clear-fringe! ~fringe) {:tag 'icfpc.core.IFringe})]
    `(.add-fringe 
       ~cleared
       (->Point ~x ~y))))

(defn explore* [level rate-fn]
  (with-slots [{:fields [width height ^Indexed bots bot ^Indexed beakons fringe]} ^lev level 
               {:keys [x y ^IPersistentMap active-boosters]} ^IPersistentMap (.nth bots bot)
               ;;we're hashing a lot here....paths is just a set of [x y] coordinates.
               ^icfpc.core.IFringe paths (init-fringe fringe x y)
               ^java.util.ArrayDeque queue (doto (ArrayDeque.)
                                             (.add (botmove. [] (->Point x y) (active-boosters FAST_WHEELS 0) (active-boosters DRILL 0) #{})))
               explore-depth *explore-depth*
               ;;maybe some more opt here...
               ;;could be optimized too...
               jumps    (when beakons
                          (delay (let [b0   (.nth beakons  0  nil)
                                       b1   (.nth beakons  1  nil)
                                       b2   (.nth beakons  2  nil)]
                                   (into []  (filter identity)
                                         [(when b0 [:jump0 b0])
                                          (when b1 [:jump1 b1])
                                          (when b2 [:jump2 b2])]))))
               beakons? (and jumps (pos? (.count ^Indexed @jumps)))]
    
    (loop [^long max-len   explore-depth
           best-path nil
           best-pos  nil
           best-rate 0.0]
      (if-some [move (.poll queue)]
        (with-slots [{:fields [^Counted path ^Indexed pos fast drill drilled]} ^botmove move                                  
                     [x y]         pos         
                     path-length   (.count path)]
          (if (< path-length max-len)
            (do 
              ;; still exploring inside max-len
              ;; moves
              (doseq [mv [[LEFT -1 0] [RIGHT 1 0] [UP 0 1] [DOWN 0 -1]]]
                (with-slots [[move dx dy] ^Indexed mv 
                             ;;this is a Point
                             pos'  (step x y dx dy (pos? fast) (pos? drill) drilled level)]
                  (when (and (some? pos')
                             ;;haven't visited [x y] yet.
                             (not (.has-fringe? paths pos')))
                    (let [path'    (conj path move) ;;slow conj to vector.
                          drilled' (cond-> drilled (pos? drill) (conj pos'))]
                      (.add-fringe  paths pos')
                      (.add queue (botmove. path' pos' (spend fast) (spend drill) drilled'))))))
              ;; jumps
              (when beakons?
                (doseq [^Indexed mv #_jumps (.deref ^clojure.lang.IDeref jumps)]
                  (let [pos' (.nth  mv 1)]
                    (when   ;;haven't visited [x y] yet.
                        (not (.has-fringe? paths pos'))
                      (let [move  (.nth mv 0)
                            path' (conj path move)]
                        (.add-fringe paths pos')
                        (.add queue (botmove. path' pos' (spend fast) (spend drill) drilled)))))))
              (cond+
               (zero? path-length) (recur max-len best-path best-pos best-rate)
               :let [rate (/ (double (rate-fn pos level)) (double  path-length))]
               (zero? rate)       (recur max-len best-path best-pos best-rate)
               (zero? best-rate)  (recur max-len path pos rate)
               (> rate best-rate) (recur max-len path pos rate)
               (< rate best-rate) (recur max-len best-path best-pos best-rate)
               (< path-length     (.count ^Counted  best-path)) (recur max-len path pos rate)
               :else (recur max-len best-path best-pos best-rate)))
            ;; only paths with len > max-len left, maybe already have good solution?
            (if (nil? best-path)
              (do
                (.addFirst queue move)
                (recur (+  max-len ^long explore-depth) nil nil 0.0)) ;; not found anything, try expand
              [best-path best-pos])))        
        [best-path best-pos]))))

#_(defn explore* [level rate-fn]
  (with-slots [{:fields [width height ^Indexed bots bot ^Indexed beakons fringe]} ^lev level 
               {:keys [x y ^IPersistentMap active-boosters]} ^IPersistentMap (.nth bots bot)
               ;;we're hashing a lot here....paths is just a set of [x y] coordinates.
               ^icfpc.core.IFringe paths (-> fringe  clear-fringe! (add-fringe (->Point x y)))
               ^java.util.ArrayDeque queue (doto (ArrayDeque.)
                                             (.add (botmove. [] (->Point x y) (active-boosters FAST_WHEELS 0) (active-boosters DRILL 0) #{})))
               explore-depth *explore-depth*]
    (loop [max-len   explore-depth
           best-path nil
           best-pos  nil
           best-rate 0.0]
      (if-some [move (.poll queue)]
        (with-slots [{:fields [^Counted path ^Indexed pos fast drill drilled]} ^botmove move                                  
                     [x y]         pos         
                     path-length   (.count path)]
          (if (< path-length max-len)
            ;; still exploring inside max-len
            (let [b0 (and beakons (.nth beakons 0 nil))
                  b1 (and beakons (.nth beakons 1 nil))
                  b2 (and beakons (.nth beakons 2 nil))]
              ;; moves
              (doseq [mv [[LEFT -1 0] [RIGHT 1 0] [UP 0 1] [DOWN 0 -1]]]
                (with-slots [[move dx dy] ^Indexed mv 
                             ;;this is a Point
                             pos'  (step x y dx dy (pos? fast) (pos? drill) drilled level)]
                    (when (and (some? pos')
                               ;;haven't visited [x y] yet.
                               (not (.has-fringe? paths pos')))
                      (let [path'    (conj path move) ;;slow conj to vector.
                            drilled' (cond-> drilled (pos? drill) (conj pos'))]
                        (.add-fringe  paths pos')
                        (.add queue (botmove. path' pos' (spend fast) (spend drill) drilled'))))))
              ;; jumps
              (doseq [^Indexed mv [[:jump0 b0] [:jump1 b1] [:jump2 b2]]]
                (let [move (.nth mv 0)
                      pos' (.nth  mv 1)]
                  (when (and  (some? pos')
                              ;;haven't visited [x y] yet.
                              (not (.has-fringe? paths pos')))
                    (let [path' (conj path move)]
                      (.add-fringe paths pos')
                      (.add queue (botmove. path' pos' (spend fast) (spend drill) drilled))))))
              (cond+
               (zero? path-length) (recur max-len best-path best-pos best-rate)
               :let [rate (/ (rate-fn pos level) (double  path-length))]
               (zero? rate)       (recur max-len best-path best-pos best-rate)
               (zero? best-rate)  (recur max-len path pos rate)
               (> rate best-rate) (recur max-len path pos rate)
               (< rate best-rate) (recur max-len best-path best-pos best-rate)
               (< path-length     (.count ^Counted  best-path)) (recur max-len path pos rate)
               :else (recur max-len best-path best-pos best-rate)))
            ;; only paths with len > max-len left, maybe already have good solution?
            (if (nil? best-path)
              (do
                (.addFirst queue move)
                (recur (unchecked-add  max-len explore-depth) nil nil 0.0)) ;; not found anything, try expand
              [best-path best-pos])))        
        [best-path best-pos]))))

(defn explore [level rate-fn]
  #_(first (explore* level rate-fn))
  (.nth  ^Indexed (explore* level rate-fn) 0))

(defn wait-off-fast [^lev level]
  (let [{:keys [active-boosters x y]} (nth (.bots level) (.bot level))
        ^long fast (active-boosters FAST_WHEELS 0)]
    (when (pos? fast)
      (repeat fast WAIT))))

(defn zone-char [n]
  (cond
    (zero? ^long n) \0
    (nil? n) \?
    :else (char
           (+ (dec (int \a))
              (mod ^long n (- (int \z) (int \a)))))))

(defn print-level [{:keys [bots ^int width ^int height name boosters beakons spawns] :as ^lev level} 
                   & {:keys [colored? ^long max-w ^long max-h] :or {max-w 50 max-h 20 colored? true zones? false}}]
  (println name)
  (let [beakons (set beakons)
        {:keys [^long x ^long y]} (last bots)]
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
                           (filterv #(pos? ^int (second %)) (bot :active-boosters))))
   (println "Collected:" collected-boosters)
   (println "Score:"     (level-score level))
   (println "Zones:"     (mapv #(zone-char (% :current-zone)) bots)))
  ([level delay]
   (println "\033[2J")
   (print-step level)
   (when (some? delay)
     (Thread/sleep delay))))

#_(defn collect-clone [level]
  (with-slots [{:fields [boosters collected-boosters bot]} ^lev level]
    (when (and
           (= 0 bot )
           (= 0 (collected-boosters CLONE 0))
           (some (fn [[_ b]] (= b CLONE)) boosters))
      (explore level (fn [xy level]
                       (with-slots [[x y] ^Indexed xy]
                         (if (= (boosters [x y]) CLONE) 1 0)))))))

(defn collect-clone [^lev level]
  (let [boosters (.boosters level)]
    (when (and
           (= 0 (.bot level))
           (= 0 ((.collected-boosters level) CLONE 0))
           (some (fn [[_ b]] (= b CLONE)) boosters))
      (explore level (fn [[x y] level]
                       (if (= (boosters [x y]) CLONE) 1 0))))))

#_(defn goto-spawn [level]
  (with-slots [{:fields [bot ^Indexed bots  spawns collected-boosters]} ^lev level
               {:keys [x y]} ^IPersistentMap (.nth bots bot)]
    (when (and
            (= 0 bot )
            (pos? (collected-boosters CLONE 0))
            (not (spawns [x y])))
      (explore level (fn [[x y] level]
                         (if (spawns [x y]) 1 0))
               #_(fn [xy level]
                 (with-slots [[x y] ^Indexed xy]
                   (if (spawns [x y]) 1 0)))))))

(defn goto-spawn [^lev level]
  (let [spawns (.spawns level)
        {:keys [x y]} (nth (.bots level) (.bot level))]
    (when (and
           (= 0 (.bot level))
           (pos? (long ((.collected-boosters level) CLONE 0)))
           (not (spawns [x y])))
      (explore level (fn [[x y] level]
                       (if (spawns [x y]) 1 0))))))

#_(defn choose-next-zone [level]
  (with-slots [{:fields [^Indexed bots bot]} ^lev level
               {:keys [current-zone]}      ^IPersistentMap (.nth bots bot)]
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
                           (fn [xy level]
                             (with-slots [[x y] ^Indexed xy]          
                               (cond+
                                (not= EMPTY (get-level level x y)) 0
                                (look-in (get-zone level x y)) 1
                                :else 0))))]
        (-> level
          (update-bot :current-zone (constantly (get-zone level x y)))))))) ;; TODO set path too

(defn choose-next-zone [^lev level]
  (let [{:keys [current-zone]} (nth (.bots level) (.bot level)  )]
    (when (or (nil? current-zone)
              (== 0 (long (zone-area level current-zone))))
      (let [taken        (set (map :current-zone (.bots level)))
            unfinished   (set
                          (for [[zone ^long area] (.zones-area level)
                                :when (pos? area)]
                            zone))
            untaken      (set/difference unfinished taken)
            look-in      (if (empty? untaken) unfinished untaken)
            [path [x y]] (explore* level
                                   (fn [[^long x ^long y] ^lev level]
                                     (cond+
                                      (not= EMPTY (get-level level x y)) 0
                                      (look-in (get-zone level x y)) 1
                                      :else 0)))]
        (-> level
            (update-bot :current-zone (constantly (get-zone level x y)))))))) ;; TODO set path too

;(definline nempty [coll]
  

(defn advance* [^lev level]
  (cond+
   :let [bot (nth (.bots level) (.bot level))]

   :when-some [level' (when (.zones? level) (choose-next-zone level))]
   (recur level')

   :when-some [picked-booster (bot :picked-booster)]
   (recur
    (-> level
        (update :collected-boosters update picked-booster (fnil clojure.core/inc 0))
        (update-bot :picked-booster (constantly nil))))

   :when-some [plan (not-empty (bot :plan))]
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
        *last-frame (atom 0)
        delay? (some? delay)]
    (binding [*disabled*      (or disabled *disabled*)
              *explore-depth* (or explore-depth *explore-depth*)
                                        ;*zones?*        (if zones? zones? *zones?*)
              ]
      (loop [level (mark-wrapped (assoc level :bot 0 :zones? (or zones? *zones?*)))]
        ;;this is costing us some runtime.  combined with the pooled checks
        ;;invoking Thread/currentThread
        ;;The rust implementation doesn't do any of this....
        #_(when (.isInterrupted (Thread/currentThread))
            (throw (InterruptedException.)))
        (when (or delay?
                  (and debug? (>= (- (System/currentTimeMillis) ^long @*last-frame) 200)))
          (print-step level delay)
          (reset! *last-frame (System/currentTimeMillis)))

        (if (zero? ^long (level :empty))
          (do
            (when debug? (print-step level))
            {:path  (str/join "#" (map :path (level :bots) ))
             :score (level-score level)
             :time  (- (System/currentTimeMillis) t0)})
          (recur
           (reduce
            (fn [level i]
              (if-some [level' (advance (assoc level :bot i))]
                (if (zero? ^int (level' :empty))
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
