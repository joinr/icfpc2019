(ns icfpc.level
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [icfpc.core :refer :all]
   [icfpc.parser :as parser]
   [icfpc.writer :as writer]
   #_[clojure.data/intmap :as i] )
  (:import
   [java.util Arrays]))

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

(defn every-fast? [pred xs]
  (reduce (fn [acc x]
            (if (pred x)
              acc
              (reduced nil))) true xs))

(defn obstacle?  [level x y dx' dy']
  (not (identical? OBSTACLE (get-level level (unchecked-add x dx') (unchecked-add y dy')))))

;;perf: this is a hot spot
;;perf: dstructuring of map args costs "some"
(defn valid-hand? [x y dx dy ^icfpc.core.lev level #_{:keys [width height] :as level}]
  (let [width  (.width level)
        height (.height level)
        x' (unchecked-add x dx) y' (unchecked-add y dy)] ;;perf: hinted numeric ops could help here...
    (when (and
            (<* -1 x' width)  ;;perf: calls to variadic fn <, clojure.lang.numbers boxed comp.
            (<* -1 y' height) ;;perf: calls to variadic fn <, clojure.lang.numbers boxed comp.
            (every-fast? ;;perf: every? coerces to seq, some cost from chunking.
             ;;perf: not= is comparing ifpc.core/OBSTACLE (a byte boxed in a var...) to result from
             ;;get-level.  Going though boxed comparison, possible optimization is (not (identical? ...))
             ;;destructuring in predicate incurs overhead.
             ;;possible boxed
             (fn [[dx' dy']]
               (obstacle? level x y dx' dy'))
             #_(fn [[dx' dy']] (not= OBSTACLE (get-level level (+ x dx') (+ y dy'))))
             ;;perf: ->Point constructor may be incurring overhead here and in
             ;;hand-blocks, which is a map being used for lookups.
              (or (hand-blocks dx dy #_(->Point dx dy)) (throw (Exception. (str "Unknown hand offset" dx dy))))))
      level)))


#_(defn valid-hand? [x y dx dy {:keys [width height] :as level}]
  (let [x' (+ x dx) y' (+ y dy)] ;;perf: hinted numeric ops could help here...
    (when (and
            (< -1 x' width)  ;;perf: calls to variadic fn <, clojure.lang.numbers boxed comp.
            (< -1 y' height) ;;perf: calls to variadic fn <, clojure.lang.numbers boxed comp.
            (every? ;;perf: every? coerces to seq, some cost from chunking.
             ;;perf: not= is comparing ifpc.core/OBSTACLE (a byte boxed in a var...) to result from
             ;;get-level.  Going though boxed comparison, possible optimization is (not (identical? ...))
             ;;destructuring in predicate incurs overhead.
             ;;possible boxed 
             (fn [[dx' dy']] (not= OBSTACLE (get-level level (+ x dx') (+ y dy'))))
             ;;perf: ->Point constructor may be incurring overhead here and in
             ;;hand-blocks, which is a map being used for lookups.
              (or (hand-blocks (->Point dx dy)) (throw (Exception. (str "Unknown hand offset" dx dy))))))
      level)))

(defn bot-covering [{:keys [bots] :as level}]
  (let [{:keys [x y layout]} (nth bots (level :bot)  )]
    (for [[dx dy] layout
          :when (if (= [0 0] [dx dy])
                  (valid? x y level)
                  (valid-hand? x y dx dy level))]
      [(+ x dx) (+ y dy)])))

(defn pick-booster [{:keys [bots boosters] :as level}]
  (let [{:keys [x y]} (nth bots (level :bot)  )]
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

(defn drill [{:keys [bots] :as level}]
  (let [{:keys [x y]} (nth bots (level :bot)  )]
    (if (and (booster-active? level DRILL)
             (= OBSTACLE (get-level level x y)))
      (set-level level x y WRAPPED)
      level)))

(defn mark-wrapped [{:keys [boosters] :as level}]
  (reduce
    (fn [level [x y]]
      (if (= EMPTY (get-level level x y))
        (-> level
          (set-level x y WRAPPED)
          (update :zones-area update (get-zone level x y) dec)
          (update :empty dec))
        level))
    (-> level pick-booster drill)
    (bot-covering level)))

(defn bounds [points]
  (let [xs (map first points)
        ys (map second points)]
    [(apply max xs) (apply max ys)]))

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
;;be optimized by eliding the range...
(defn weights [{:keys [width height] :as level}]
  (short-array
    (for [y (range 0 height)
          x (range 0 width)]
      (reduce + 0
        (for [[dx dy] [[0 1] [0 -1] [-1 0] [1 0] [1 1] [-1 -1] [-1 1] [1 -1]]
              :let [x' (+ x dx)
                    y' (+ y dy)]]
          (if (or (< x' 0) (>= x' width) (< y' 0) (>= y' height)
                (= (get-level level x' y') OBSTACLE))
            1
            0))))))

#_(defn valid-point? [{:keys [width height] :as level} [x y]]
  (and (< -1 x width) (< -1 y height)))

#_(defn neighbours [level [x y]]
    (filter #(valid-point? level %) [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]))


;;inline possibility here...
(defn valid-point?
  ([{:keys [width height] :as level} xy]
   (valid-point? width height xy))
  ([width height [x y]]
   (and (<* -1 x width) (<* -1 y height))))

(defn neighbours [level #_{:keys [width height] :as level} [x y]]
  (let [width  (level :width)
        height (level :height)]
    (filter #(valid-point? width height %)
            [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])))

(definline zone? [width height keep? xy]
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

;;similar re-hash from icfpc.core...
(definline wxy->idx [width x y]
  `(unchecked-add ~x (unchecked-multiply ~y ~width)))

(defn get-byte
  {:inline (fn
             ([grid width x y]
              `(aget ~(with-meta grid {:tag 'bytes}) (wxy->idx ~width ~x ~y)))
             ([grid idx]
              `(aget ~(with-meta grid {:tag 'bytes}) ~idx)))
   :inline-arities #{2 4}}
  ([^bytes grid  width  x  y]
   (aget  grid (wxy->idx width x y )))
  ([^bytes grid  ^long idx]
   (aget  grid idx)))

(defn set-byte
    {:inline (fn
             ([grid width x y v]
              `(aset ~(with-meta grid {:tag 'bytes}) (wxy->idx ~width ~x ~y) (byte ~v)))
              ([grid idx v]
               `(aset ~(with-meta grid {:tag 'bytes}) ~idx (byte ~v))))
     :inline-arities #{3 5}}
  ([^bytes grid  width  x y v]
   (aset  grid ^long (wxy->idx width x y) (byte v)))
  ([^bytes grid  idx  v]
   (aset  grid ^long idx  (byte v))))

(definline get-zones! [width height grid zone-grid points]
  (let [xs (with-meta (gensym "xs") {:tag 'clojure.lang.ISeq})]
    `(loop [end# true
            ~xs  ~points]
      (if-let [p# (.first ~xs)]
        (let [[x# y#] p#
              idx#  (wxy->idx ~width x# y#)]
          (if (= (get-byte ~zone-grid idx#) 0)
            (let [z# (zone? ~width ~height
                           (fn ~'not-empty [[nx# ny#]]
                             (let [nidx# (wxy->idx ~width nx# ny#)]
                               (when (= EMPTY (get-byte ~grid nidx#))
                                 (let [z# (get-byte ~zone-grid nidx#)]
                                   (when (not (zero? z#))
                                     z#)))))
                           p#)]

              (recur (if (some? z#)
                       (do (set-byte ~zone-grid idx# z#)
                           end#)
                       false)
                     (.more ~xs)))
            (recur end# (.more ~xs))))
        end#))))

#_(defn get-zones! [width height grid zone-grid points]
  (loop [end? true
         xs   points]
    (if-let [p (.first ^clojure.lang.ISeq xs)]
      (let [[x y] p
            idx  (wxy->idx width x y)]
        (if (= (get-byte zone-grid idx) 0)
          (let [z (zone? width height
                         (fn not-empty [[nx ny]]
                           (let [nidx (wxy->idx width nx ny)]
                             (when (= EMPTY (get-byte grid nidx))
                               (let [z (get-byte zone-grid nidx)]
                                 (when (not (zero? z))
                                   z)))))
                         p)]

            (recur (if (some? z)
                     (do (set-byte zone-grid idx z)
                         end?)
                     false)
                   (.more ^clojure.lang.ISeq xs)))
          (recur end? (.more ^clojure.lang.ISeq xs))))
      end?)))
      
(defn generate-zones [level bots]
  (let [width  (level :width )
        height (level :height)
        grid   (level :grid)
        max-iteration-count (* width height)
        ;;these are all long pairs....
        empty-points (points-by-value level EMPTY)

        zones-count  bots

        centers (map-indexed (fn [idx z] [(inc idx) z]) (take zones-count (shuffle* empty-points)))
        zone-grid (byte-array (* width height))
        zones-map {:width  width
                   :height height
                   :grid   zone-grid}
        _ (java.util.Arrays/fill ^bytes zone-grid (byte 0))

        zones-map (reduce (fn [zm [idx [x y]]]
                            (do (set-byte zone-grid width x y idx)
                                zm))
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
                          (fn [[z points]]
                            [z (count points)])
                          (group-by first (for [x (range width)
                                                y (range height)
                                                :when (= EMPTY (get-byte grid width x y))]
                                            [(get-byte zone-grid width x y) [x y]]))))
        level (assoc level :zones-grid (zones-map :grid)
                           :zones-area zones-area)
        update-bot (fn [bot]
                     (assoc bot :current-zone (get-zone level (bot :x) (bot :y))))
        level (update level :bots #(mapv update-bot %))]
    level))

#_(defn generate-zones [level bots]
  (let [width (level :width )
        height (level :height)
        max-iteration-count (* width height)
        ;;these are all long pairs....
        empty-points (points-by-value level EMPTY)

        zones-count  bots

        centers (map-indexed (fn [idx z] [(inc idx) z]) (take zones-count (shuffle* empty-points)))
        zones-map {:width width
                   :height height
                   :grid (make-array Byte/TYPE (* width height))}
        _ (java.util.Arrays/fill ^bytes (zones-map :grid) (byte 0))
        get (fn [zm ^long x ^long y] (aget ^bytes (zm :grid)        (+ x (* y width))))
        set (fn [zm ^long x ^long y v] (aset-byte ^bytes (zm :grid) (+ x (* y width)) v) zm)
        zones-map (reduce (fn [zm [idx [x y]]]
                            (set zm x y idx))
                          zones-map
                          centers)
        zones-map (loop [zones zones-map
                         iteration 0]
                    (let [{:keys [zm end?]} (reduce (fn [{:keys [zm end?]} [x y]]
                                                      (if (= (get zm x y) 0)
                                                        (let [z (first (keep (fn [[nx ny]]
                                                                               (when (== EMPTY (get-level level nx ny))
                                                                                 (let [z (get zones nx ny)]
                                                                                   (when (not= z 0)
                                                                                     z))))
                                                                             (neighbours zm [x y])))]
                                                          (if (some? z)
                                                            {:zm (set zm x y z)
                                                             :end? end?}
                                                            {:zm zm
                                                             :end? false}))
                                                        {:zm zm :end? end?}))
                                                    {:zm zones :end? true}
                                                    empty-points)]
                      (if (and (< iteration max-iteration-count) (not end?))
                        (recur zm (inc iteration))
                        (do
                          (when (= iteration max-iteration-count)
                            (println "Can’t generate zones for" (level :name )))
                          zm))))
        zones-area (into {}
                         (map
                          (fn [[z points]]
                            [z (count points)])
                          (group-by first (for [x (range width)
                                                y (range height)
                                                :when (= EMPTY (get-level level x y))]
                                            [(get zones-map x y) [x y]]))))
        level (assoc level :zones-grid (zones-map :grid)
                           :zones-area zones-area)
        update-bot (fn [bot]
                     (assoc bot :current-zone (get-zone level (bot :x) (bot :y))))
        level (update level :bots #(mapv update-bot %))]
    level))

(defn new-bot [x y]
  {:x               x 
   :y               y
   :layout          [[0 0] [1 0] [1 1] [1 -1]]
   :active-boosters {}
   :picked-booster  nil
   :path            ""
   :current-zone    nil})

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
        grid       (make-array Byte/TYPE (* width height))
        _          (java.util.Arrays/fill ^bytes grid ^Byte OBSTACLE)
        init-level {:name     name
                    :width    width
                    :height   height
                    :grid     grid
                    :boosters (build-boosters boosters)
                    :spawns   (build-spawns boosters)
                    :collected-boosters {}
                    :bots     [(new-bot (first bot-point) (second bot-point))]}
        level (fill-level init-level corners obstacles)
        empty (arr-reduce #(if (= EMPTY %2) (inc %1) %1) 0 grid)
        level (assoc level
                     :weights (weights level)
                     :empty   empty)
        level (maybe-add-bonuses level name)
        clones-count (+ 
                       (count (filter #(= CLONE (val %)) (level :boosters )))
                       ((level :collected-boosters ) CLONE 0)
                       1)]
    (-> (generate-zones level clones-count)
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
