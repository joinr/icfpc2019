(ns icfpc.level
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [icfpc.core :refer :all]
   [icfpc.parser :as parser]
   [icfpc.speed :refer [with-slots]]
   [icfpc.fringe :as fringe])
  (:import
   [java.util Arrays]
   [clojure.lang Indexed Counted IPersistentMap IPersistentVector IPersistentSet]
   [icfpc.core lev Point IByteMap]))
  
(def ^:dynamic *bot*)

(defn booster-active? [level booster]
  (-> level :bots (nth (level :bot) ) :active-boosters (get booster 0) pos?))

(defn booster-collected? [level booster]
  (-> level :collected-boosters (get booster 0) pos?))

#_(defn update-bot [level key f & args]
  (apply update level :bots update (level :bot) update key f args))

(defmacro update-bot [level key f & args]
  `(update ~level :bots
           (fn [arg#]
             (update arg# (~level :bot)
                     (fn [inner#]
                       (update inner# ~key (fn [final#]
                                             (~f final# ~@args))))))))


(defmacro map-bot [level f & args]
  `(update ~level :bots
           (fn [arg#]
             (update arg# (~level :bot)
                     (fn [inner#]
                       (~f inner# ~@args))))))

;;perf: using point records as keys to vector for coords.
;;called by valid-hand? frequently
;;point ctr fn could be inlined or macrofied...
;;probably faster to reduce eagerly and avoid
;;lazy seq stuff.
;;This is building a map, hand-blocks, which is
;;being used for lookups based on ->Points as
;;keys.  Likely improvement is to
;;define a function hand-blocks that leverages
;;a more efficient int mapping of points to
;;coordinates.  Looks like points are just
;;int pairs...Incurring a LOT of overhead
;;on lookups here.
#_(def hand-blocks
  (into {(->Point 1 -1) [[1 -1]]
         (->Point 1 0)  [[1 0]]
         (->Point 1 1)  [[1 1]]}
        (for [maxy (range 2 20)]
          [(->Point 1 maxy) (vec
                             (concat
                              (for [y (range 1 (inc (quot maxy 2)))] [0 y])
                              (for [y (range (int (Math/ceil (/ maxy 2.0))) (inc maxy))] [1 y])))])))

(def hand-blocks-map
  (into {(->Point 1 -1) [[1 -1]]
         (->Point 1 0)  [[1 0]]
         (->Point 1 1)  [[1 1]]}
    (for [maxy (range 2 20)]
      [(->Point 1 maxy) (vec
                          (concat
                            (for [y (range 1 (inc (quot maxy 2)))] [0 y])
                            (for [y (range (int (Math/ceil (/ maxy 2.0))) (inc maxy))] [1 y])))])))


;;We can just make this into an array lookup,
;;no need for binary search....-1 is the outlier, handle
;;it as a special case.
(let [kvs      (sort-by (comp :y key) (seq hand-blocks-map))
      ^objects entries (object-array (map val kvs))
      ^ints ys (int-array (map (comp :y key) kvs))
      ]
  (defn hand-blocks [^long dx ^long dy]
    (when (== dx 1)
      (let [idx (java.util.Arrays/binarySearch ys dy)]
        (when (> idx -1)
          (aget entries  idx))))))
  
(defn valid?
  ([x y {:keys [width height] :as level}]
    (when (and
            (<* -1 x width)
            (<* -1 y height)
            (or
              (booster-active? level DRILL)
              (not= OBSTACLE (get-level level x y))))
      level))
  ([{:keys [bots] :as level}]
    (let [{:keys [x y]} (nth bots (level :bot) )]
      (valid? x y level))))

;; (defn valid?
;;   ([x y {:keys [width height] :as level}]
;;     (when (and
;;             (< -1 x width)
;;             (< -1 y height)
;;             (or
;;               (booster-active? level DRILL)
;;               (not= OBSTACLE (get-level level x y))))
;;       level))
;;   ([{:keys [bots] :as level}]
;;     (let [{:keys [x y]} (nth bots *bot*)]
;;       (valid? x y level))))


(defn every-fast? [pred xs]
  (reduce (fn [acc x]
            (if (pred x)
              acc
              (reduced nil))) true xs))

;;===========
;;WIERD
;;=========
;;If we use any unchecked math for either obstacle?
;;or valid-hand?, which calls obstacle?, we end up
;;diverging in the answer arrived at in the baseline.
;;I have NO IDEA what's driving that, but
;;perf hit is minor...

#_(defn obstacle?  [level x y dx' dy']
  (not (identical? OBSTACLE (get-level level (unchecked-add x dx') (unchecked-add y dy')))))

(defn obstacle?  [level x y dx' dy']
  (not= OBSTACLE (get-level level (+ x dx') (+ y dy'))))

;;perf: this is a hot spot
;;perf: dstructuring of map args costs "some"
(defn valid-hand? [x y dx dy  level]
  (with-slots
    [{:fields [width height]} ^lev level
     ;;using unchecked math here will actually cause out answer to diverge!
     ;;unknown why the math is sensitive...
     x' (+ x dx) y' (+ y dy)] ;;perf: hinted numeric ops could help here...
    (when (and
           (<* -1 x' width)  ;;perf: calls to variadic fn <, clojure.lang.numbers boxed comp.
           (<* -1 y' height) ;;perf: calls to variadic fn <, clojure.lang.numbers boxed comp.
           (every-fast? ;;perf: every? coerces to seq, some cost from chunking.
            ;;perf: not= is comparing ifpc.core/OBSTACLE (a byte boxed in a var...) to result from
            ;;get-level.  Going though boxed comparison, possible optimization is (not (identical? ...))
            ;;destructuring in predicate incurs overhead.
            ;;possible boxed
            (fn [dxdy]
              (with-slots [[dx' dy'] ^Indexed dxdy]
                (obstacle? level x y dx' dy')))
            ;;perf: ->Point constructor may be incurring overhead here and in
            ;;hand-blocks, which is a map being used for lookups.
            (or (hand-blocks dx dy) (throw (Exception. (str "Unknown hand offset" dx dy))))))
      level)))

;;look and see if we can slotify the for
;;expression here, need to filter.
(defn bot-covering [level]
  (with-slots
    [{:fields [^Indexed bots bot]}   ^lev level
     {:keys [x y layout]}            ^IPersistentMap (.nth bots bot)]
    (for [[dx dy] layout
          :when (if (= [0 0] [dx dy])
                  (valid? x y level)
                  (valid-hand? x y dx dy level))]
      [(+ x dx) (+ y dy)])))

(defn pick-booster [level]
  (with-slots [{:fields [^Indexed bots bot boosters]}    ^lev level
               {:keys [x y]}        ^IPersistentMap (.nth bots bot)]
    ;;Possible slow path here...hashing [x y] pairs.
    (if-some [booster (boosters [x y])]
      (-> level
        (update :boosters dissoc [x y])
        (update-bot :picked-booster (constantly booster)))
      level)))

(defn wear-off-boosters [level]
  (cond-> level
    (booster-active? level FAST_WHEELS)
    (update :bots update (level :bot)  spend :active-boosters FAST_WHEELS)

    (booster-active? level DRILL)
    (update :bots update (level :bot)  spend :active-boosters DRILL)))

(defn drill [level]
  (with-slots [{:fields [^Indexed bots bot]}    ^lev level
               {:keys [x y]}        ^IPersistentMap (.nth bots bot)]
    (if (and (booster-active? level DRILL)
             (= OBSTACLE (get-level level x y)))
      (set-level level x y WRAPPED)
      level)))

(defn mark-wrapped [level]
  (with-slots [{:fields [boosters]} ^lev level]
    (reduce
     (fn [level [x y]]
       (if (= EMPTY (get-level level x y))
         (-> level
             (set-level x y WRAPPED)
             (update :zones-area update (get-zone level x y) dec)
             (update :empty dec))
         level))
     (-> level pick-booster drill)
     (bot-covering level))))

(defn bounds [points]
  (let [xs (map first points)
        ys (map second points)]
    [(apply max xs) (apply max ys)]))

;;possible slow path here with destructiring...check profiling.
(defn vertical-segments [corners]
  (let [segments (partition 2 1 (into corners (take 1 corners)))]
    (keep (fn [[[from-x from-y] [to-x to-y]]]
            (when (= from-x to-x)
              {:x from-x
               :from-y (min from-y to-y)
               :to-y (max from-y to-y)}))
          segments)))

(defn fill-level [level corners obstacles]
  (let [segments (sort-by :x (apply concat (into [(vertical-segments corners)] (map vertical-segments) obstacles)))]
    (reduce (fn [level y]
              (let [xs (map :x (filter (fn [m]
                                         (let [from-y (m :from-y)
                                               to-y   (m :to-y)]
                                           (and (<= from-y y) (< y to-y))))
                                         segments))
                    rs (take-nth 2 (partition 2 1 xs))]
                (reduce (fn [level [from-x to-x]]
                          (reduce (fn [level x]
                                    (set-level level x y EMPTY))
                                  level
                                  (range from-x to-x)))
                        level
                        rs)))
            level
            (range (:height level)))))

(defn build-boosters [boosters]
  (into {}
    (for [[b [x y]] boosters
          :when (not= b SPAWN)]
      [[x y] b])))

(defn build-spawns [boosters]
  (into #{}
    (for [[b [x y]] boosters
          :when (= b SPAWN)]
      [x y])))

;;minor time sink for load-level, could
;;be optimized a bit
(defn weights [{:keys [width height] :as level}]
  (short-array
    (for [y (range 0 height)
          x (range 0 width)]
      (reduce + 0
         (for [dxdy [[0 1] [0 -1] [-1 0] [1 0] [1 1] [-1 -1] [-1 1] [1 -1]]]
           (with-slots [[dx dy] ^Indexed dxdy
                        x' (+ x dx)
                        y' (+ y dy)]
             (if (or (< x' 0) (>= x' width) (< y' 0) (>= y' height)
                     (= (get-level level x' y') OBSTACLE))
               1
               0)))))))

;;inline possibility here...
(defn valid-point?
  ([{:keys [width height] :as level} xy]
   (valid-point? width height xy))
  ([width height xy]
   (with-slots  [[x y]  ^Indexed xy]
     (and (<* -1 x width) (<* -1 y height)))))

(defn neighbours [level [x y]]
  (with-slots [{:fields [width height]}  ^lev level]
    (filter #(valid-point? width height %)
            [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])))

(defn zone? [width height keep? xy]
  (transduce (comp (comp (mapcat (fn expand [xy]
                                   (with-slots [[x y] ^Indexed xy]
                                     [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])))
                         (filter (fn valid [xy] (valid-point? width height xy))))
                   (comp (keep keep?)
                         (take 1)))
             (completing (fn [acc x]
                           (reduced x)))
             nil
             [xy]))

#_(definline zone? [width height keep? xy]
  `(transduce (comp (comp (mapcat (fn ~'expand [[x# y#]]
                                    [[(inc x#) y#] [(dec x#) y#] [x# (inc y#)] [x# (dec y#)]]))
                          (filter (fn ~'valid [xy#] (valid-point? ~width ~height xy#))))
                    (comp (keep ~keep?)
                          (take 1)))
              (completing (fn [acc# x#]
                            (reduced x#)))
              nil
              [~xy]))

(defn points-by-value [level value]
  (for [i (range 0 (level :width ))
        j (range 0 (level :height ))
        :when (= value (get-level level i j))]
    [i j]))

(defn shuffle*
  "Return a random permutation of coll"
  {:added "1.2"
   :static true}
  [^java.util.Collection coll]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al (java.util.Random. 42))
    (clojure.lang.RT/vector (.toArray al))))

;;this is ugly, but the inlining appears to
;;be worth it, with the frequency of calls.
(definline get-zones! [width height grid zone-grid points]
  (let [xs   (with-meta (gensym "xs")   {:tag 'clojure.lang.ISeq})
        nxny (with-meta (gensym "nxny") {:tag 'Indexed})
        p    (with-meta (gensym "p")    {:tag 'Indexed})]
  `(loop [end?# true
          ~xs   ~points]
     (if-let [~p (.first ~xs)]
       (with-slots
         [[x# y#] ~p]
         (if (= (get-byte ~zone-grid x# y#)0)
           (let [z# (zone? ~width ~height
                          (fn ~'not-empty [~nxny]
                            (with-slots 
                              [[nx# ny#] ~nxny]
                              (when (= EMPTY (get-byte ~grid nx# ny#))
                                (let [z# (get-byte ~zone-grid nx# ny#)]
                                  (when (not (zero? z#))
                                    z#)))))
                          ~p)]
            (recur (if (some? z#)
                     (do (set-byte ~zone-grid x# y# z#)
                         end?#)
                     false)
                   (.more ~xs)))
           (recur end?# (.more ~xs))))
       end?#))))
      
(defn generate-zones [level bots]
  (let [width  (level :width)
        height (level :height)
        grid   (level :grid)
        max-iteration-count (* width height)
        ;;these are all long pairs....
        empty-points (points-by-value level EMPTY)
        zones-count  bots
        centers (map-indexed (fn [idx z] [(inc idx) z]) (take zones-count (shuffle* empty-points)))
        zone-grid (->byte-grid width height)
        zones-map {:width  width
                   :height height
                   :grid   zone-grid}
        _         (fill-bytes (zone-grid) 0)

        zones-map (reduce (fn [zm idx-xy]
                            (with-slots [[idx ^Indexed xy] ^Indexed idx-xy
                                         [x y]    xy]
                              (do (set-byte zone-grid  x y idx)
                                  zm)))
                          zones-map
                          centers)
        zones-map (loop [zones zones-map
                         iteration 0]
                    (let [end? (get-zones! width height grid zone-grid empty-points)]
                      (if (and (< iteration max-iteration-count) (not end?))
                        (recur zones (unchecked-inc iteration))
                        (do
                          (when (= iteration max-iteration-count)
                            (println "Can’t generate zones for" (level :name )))
                          zones))))
        zones-area (into {}
                         (map
                          (fn [z-points]
                            (with-slots [[z ^Counted points] ^Indexed z-points]
                              [z (.count points)]))
                          (group-by first (for [x (range width)
                                                y (range height)
                                                :when (= EMPTY (get-byte grid x y))]
                                            [(get-byte zone-grid x y) [x y]]))))
        level (assoc level :zones-grid (zones-map :grid)
                           :zones-area zones-area)
        update-bot (fn [bot]
                     (assoc bot :current-zone (get-zone level (bot :x) (bot :y))))
        level (update level :bots #(mapv update-bot %))]
    level))

(defn maybe-add-bonuses [level name]
  (let [[_ name] (re-matches #"(.*)\.desc" name)
        buy (io/file (str "problems/" name ".buy"))]
    (if (.exists buy)
      (let [bonuses (str/trim (slurp buy))]
        (reduce
          (fn [level bonus]
            (update level :collected-boosters update bonus (fnil inc 0)))
          level
          bonuses))
      level)))

(defn load-level [name]
  (let [{:keys [bot-point corners obstacles boosters]} (parser/parse-level name)
        [width height] (bounds corners)
        grid       (->byte-grid width height) 
        _          (fill-bytes (grid) OBSTACLE)
        init-level {:name     name
                    :width    width
                    :height   height
                    :grid     grid
                    :boosters (build-boosters boosters)
                    :spawns   (build-spawns boosters)
                    :collected-boosters {}
                    :bots     [(new-bot (first bot-point) (second bot-point))]}
        level (fill-level init-level corners obstacles)
        empty (arr-reduce2  #(if (= EMPTY %2) (inc %1) %1) 0 (grid) )
        level (assoc level
                     :weights (weights level)
                     :empty   empty)
        level (maybe-add-bonuses level name)
        clones-count (+ 
                       (count (filter #(= CLONE (val %)) (level :boosters )))
                       ((level :collected-boosters ) CLONE 0)
                       1)
        fringe  (fringe/->bit-fringe width height)]
    (-> (generate-zones level clones-count)
        (assoc :fringe fringe)
        map->lev)))

(comment
  (def lvl (load-level "prob-002.desc"))
  (apply max (vals (:zones-area lvl)))
  (apply min (vals (:zones-area lvl)))
  (get-level lvl 5 5)
  (map (get-level lvl (first %) (second %))
       (neighbours lvl [5 5]))
  (get-level lvl 11 0)
  (:zones-grid lvl)

  (closest-zone (assoc-in lvl [:zones-area 1] 0) 16 0)

  (count (points-by-value lvl' EMPTY))

#{:block-number
  :epoch
  :t-size
  :v-min
  :v-max
  :extra-hands
  :fast-wheels
  :drills
  :teleports
  :cloning
  :spawns
  :include
  :exclude}


  (def puzzle (parser/parse-puzzle "puzzle.cond"))
  (def lvl (generate-level "puzzle.cond"))
  (+ 1 (count (icfpc.writer/segments lvl)))
  (spit "puzzle-solved.desc" (icfpc.writer/desc lvl))

  (:boosters lvl)

  (select-keys puzzle [:extra-hands
  :fast-wheels
  :drills
  :teleports
  :cloning
  :spawns])

  *e

  (def puzzle (parser/parse-puzzle "first.cond"))
  (def lvl (generate-level "first.cond"))
  (spit "first.desc" (icfpc.writer/desc lvl))


  (count (icfpc.writer/segments lvl))



  (:width lvl)
  (:height lvl)

  (icfpc.bot/print-level lvl :colored? false :max-w 1000 :max-h 1000)
  (icfpc.parser/validate-puzzle (parser/parse-puzzle "first.cond") lvl)

  (keys puzzle)
  (:t-size puzzle)
  (:v-min puzzle)
  (:v-max puzzle)
  (count (:include puzzle))
  (count (:exclude puzzle))


  (= (ray-path [1 1] [3 0]) [[1 1] [2 0] [2 1] [3 0]])
  (= (ray-path [1 1] [3 2]) [[1 1] [2 1] [2 2] [3 2]])

  (= (ray-path [0 1] [3 0]) [[0 1] [1 1] [2 0] [3 0]])

  (= (ray-path [0 0] [3 1]) [[0 0] [1 0] [2 1] [3 1]])


  (= (ray-path [0 0] [3 3]) [[0 0] [1 1] [2 2] [3 3]])
  (= (ray-path [5 5] [1 1]) [[1 1] [2 2] [3 3] [4 4] [5 5]])
  (= (ray-path [0 5] [5 0]) [[0 5] [1 4] [2 3] [3 2] [4 1] [5 0]])

  (def lvls (mapv (fn [n]
                    (load-level (format "prob-%03d.desc" n)))
                  (range 1 51)))

  (build-boosters
   (:boosters (parser/parse-level "prob-050.desc")))

  (doseq [lvl lvls]
    (icfpc.bot/print-level lvl))


  (into [(vertical-segments corners)] (map vertical-segments obstacles))


  (icfpc.bot/print-level (load-level "prob-150.desc"))

  (def test-level
    {:width  10
     :height 8
     :x 1
     :y 1
     :grid   [OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE 
              OBSTACLE EMPTY    EMPTY    EMPTY    OBSTACLE EMPTY    EMPTY    EMPTY    EMPTY    OBSTACLE 
              OBSTACLE EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    OBSTACLE
              OBSTACLE EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    OBSTACLE
              OBSTACLE EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    OBSTACLE
              OBSTACLE EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    OBSTACLE
              OBSTACLE EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    EMPTY    OBSTACLE
              OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE OBSTACLE]})

  (def test-puzzle
    {:v-min 44
     :exclude [[5 0] [2 3] [3 3]]})

  (icfpc.bot/print-level (icfpc.level/add-edges test-level test-puzzle))
  )
