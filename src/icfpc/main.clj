(ns icfpc.main
  (:require
   [icfpc.bot :as bot]
   [icfpc.core :as core :refer [cond+]]
   [icfpc.level :as level]
   [icfpc.generator :as generator]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.data.json :as json])
  (:import
   [java.io File]
   [java.util.concurrent CompletableFuture])
  (:gen-class))

(set! *warn-on-reflection* true)

(def log-agent (agent nil))

(defn log [& msg] (send log-agent (fn [_] (apply println (remove nil? msg)) _)))

(def coins 98648)

(defn gen-buys []
  (doseq [^File file (file-seq (io/file "problems"))
          :when (str/ends-with? (.getName file) ".buy")]
    (.delete file))
  (doseq [name (->> (file-seq (io/file "problems/"))
                 (filter (fn [^File file]
                           (and (str/ends-with? (.getName file) ".desc")
                             (let [body (slurp file)]
                               (and (zero? (count (re-seq #"C\(" body)))
                                 (pos? (count (re-seq #"X\(" body))))))))
                 (sort-by #(.length ^File %))
                 (reverse)
                 (map #(second (re-matches #".*/(prob-\d\d\d)\.desc" (.getPath ^File %))))
                 (take (quot coins 2000)))]
    (spit (str "problems/" name ".buy") "C")
    (let [sol (io/file (str "problems/" name ".sol"))]
      (when (.exists sol)
        (.delete sol)))))

(defn compare-solutions [name score]
  (->> (file-seq (io/file "."))
    (filter  #(str/starts-with? (.getName ^File %) "day"))
    (sort-by #(.getName ^File %))
    (map     #(io/file % (str name ".sol")))
    (filter  #(.exists ^File %))
    (map     #(core/sol-score (slurp %)))
    (distinct)
    (take-last 2)
    (map     #(format "%d (%+.1f%%)" % (-> (- score %) (/ %) (* 100) (float))))))

(defn solve [name & [{:keys [*left t0 throw?] :or {throw? true} :as opts}]]
  (try
    (let [level (level/load-level (str name ".desc"))
          sln   (bot/solve level (merge {:debug? false} opts))
          left  (some-> *left (swap! dec))]
      (spit (str "problems/" name ".sol") (:path sln))
      (log (when-some [t0 (:t0 opts)]
             (str (- (System/currentTimeMillis) t0) "ms"))
           (when (some? left)
             (str "Left " left))
           "Solved" name (dissoc sln :path) "was" (str/join " / " (compare-solutions name (:score sln))))
      (:score sln))
    (catch Exception e
      (if (:throw? opts true)
        (throw e)
        (println "Failed" name "with" (type e) ":" (.getMessage e))))))

(defn solve-elided-nopath [name & [{:keys [*left t0 throw?] :or {throw? true} :as opts}]]
  (try
    (let [level (level/load-level (str name ".desc"))
          sln   (bot/solve level (merge {:debug? false} opts))
          left  (some-> *left (swap! dec))]
      ;(spit (str "problems/" name ".sol") (:path sln))
      (log (when-some [t0 (:t0 opts)]
             (str (- (System/currentTimeMillis) t0) "ms"))
           (when (some? left)
             (str "Left " left))
           "Solved" name (dissoc sln :path) "was [elided]" #_(str/join " / " (compare-solutions name (:score sln))))
      (:score sln))
    (catch Exception e
      (if (:throw? opts true)
        (throw e)
        (println "Failed" name "with" (type e) ":" (.getMessage e))))))

(defn solve-elided [name & [{:keys [*left t0 throw?] :or {throw? true} :as opts}]]
  (try
    (let [level (level/load-level (str name ".desc"))
          sln   (bot/solve level (merge {:debug? false} opts))
          left  (some-> *left (swap! dec))]
      (spit (str "problems/" name ".sol") (:path sln))
      (log (when-some [t0 (:t0 opts)]
             (str (- (System/currentTimeMillis) t0) "ms"))
           (when (some? left)
             (str "Left " left))
           "Solved" name (dissoc sln :path) "was [elided]" #_(str/join " / " (compare-solutions name (:score sln))))
      (:score sln))
    (catch Exception e
      (if (:throw? opts true)
        (throw e)
        (println "Failed" name "with" (type e) ":" (.getMessage e))))))

(defn skip-till [n xs]
  (if (some? n) (drop n xs) xs))

(defn take-till [to xs]
  (if (some? to) (take to xs) xs))

(defn clear []
  (doseq [^File file (file-seq (io/file "problems"))
          :when (str/ends-with? (.getName file) ".sol")]
    (.delete file)))

(defn main [& [from till threads]]
  (clear)
  (let [from  (cond-> from (string? from) (Integer/parseInt))
        till  (cond-> till (string? till) (Integer/parseInt))
        names (->> (file-seq (io/file "problems"))
                 (map #(.getPath ^File %))
                 (filter #(str/ends-with? % ".desc"))
                 (keep #(second (re-matches #".*/(prob-\d\d\d)\.desc" %)))
                 sort
                 (take-till till)
                 (skip-till from)
                 (remove #(.exists (io/file (str "problems/" % ".sol")))))
        t0       (System/currentTimeMillis)
        threads  (or (cond-> threads (string? threads) (Integer/parseInt))
                   (.. Runtime getRuntime availableProcessors))
        executor (java.util.concurrent.Executors/newFixedThreadPool threads)
        *left    (atom (count names))]
    (log "Running" threads "threads, solving" (count names) "tasks")
    (->
      (into-array CompletableFuture
        (for [name names]
          (CompletableFuture/runAsync
            ^Runnable (bound-fn [] (solve name {:t0 t0 :throw? false :*left *left}))
            executor)))
      (CompletableFuture/allOf)
      (.join))
    (log "DONE in" (- (System/currentTimeMillis) t0) " ms")
    (.shutdown executor)))

(defn main-elided [& [from till threads]]
  (clear)
  (let [re    (case java.io.File/separator
                "\\" #".*\\(prob-\d\d\d)\.desc"
                #".*/(prob-\d\d\d)\.desc")
        from  (cond-> from (string? from) (Integer/parseInt))
        till  (cond-> till (string? till) (Integer/parseInt))
        names (->> (file-seq (io/file "problems"))
                   (map #(.getPath ^File %))
                   (filter #(str/ends-with? % ".desc"))
                   (keep #(second (re-matches re %)))
                   sort
                 (take-till till)
                 (skip-till from)
                 (remove #(.exists (io/file (str "problems/" % ".sol")))))
        t0       (System/currentTimeMillis)
        threads  (or (cond-> threads (string? threads) (Integer/parseInt))
                   (.. Runtime getRuntime availableProcessors))
        executor (java.util.concurrent.Executors/newFixedThreadPool threads)
        *left    (atom (count names))]
    (log "Running" threads "threads, solving" (count names) "tasks")
    (->
      (into-array CompletableFuture
        (for [name names]
          (CompletableFuture/runAsync
            ^Runnable (bound-fn [] (solve-elided name {:t0 t0 :throw? false :*left *left}))
            executor)))
      (CompletableFuture/allOf)
      (.join))
    (log "DONE in" (- (System/currentTimeMillis) t0) " ms")
    (.shutdown executor)))

(defn -main [& [from till threads]]
  (gen-buys)
  (main from till threads)
  (shutdown-agents))

(defn clean-main [& [from till threads]]
  (clear)
  (-main from till threads))

(defn print-solve [name]
  (bot/print-level (level/load-level (str name ".desc")))
  (solve name))

(comment
  (icfpc.main/solve "prob-002" {:debug? true})
  (icfpc.main/print-solve "prob-002")
  (icfpc.bot/solve (icfpc.level/load-level "prob-002.desc") {:delay 100})
  (icfpc.main/score-solutions "problems")
  *e
  (+ 2 2)
)

(defn score-solutions [path]
  (->> (file-seq (io/file path))
       (filter #(str/ends-with? (.getName ^File %) ".sol"))
       (map slurp)
       (map core/sol-score)
       (reduce + 0)))

(defn mine-coins [& [block excluded puzzle task]]
  (spit "mine.log" (str block excluded))
  (prn block excluded puzzle task))

(defn run-lambda [& commands]
  (let [result (shell/with-sh-dir "client/lambda-client"
                                  (apply shell/sh
                                         "./lambda-cli.py"
                                         commands))]
    (when (not= 0 (:exit result))
      (prn "Err: " (:err result)))
    (:out result)))

(defn mine-block [block]
  (let [block (Integer/parseInt (str/trim block))
        puzzle (run-lambda "getblockinfo" (str block) "puzzle")
        task (run-lambda "getblockinfo" (str block) "task")
        puzzle-name (format "puzzle-%03d.cond" block)
        task-name (format "task-%03d.desc" block)
        solution-name (format "solution-%03d.sol" block)
        generated-name (format "generated-%03d.desc" block)]
    (println (format "Round: %03d" block))
    (spit (str "./puzzles/" puzzle-name) puzzle)
    (spit (str "./puzzles/" task-name) task)
    (println "Generating level...")
    (let [puzzle (icfpc.parser/parse-puzzle (str "../puzzles/" puzzle-name))
          level (icfpc.generator/generate-level (str "../puzzles/" puzzle-name))]
      (icfpc.parser/validate-puzzle puzzle level)
      (spit (str "./puzzles/" generated-name) (icfpc.writer/desc level))
      (println "Level generated" generated-name)
      (println "Trying to solve level...")
      (let [level (level/load-level (str "../puzzles/" task-name))
            sln   (bot/solve level (merge
                                    {:debug? false
                                     :lookahead? false #_(<= (:width level) 200)}))]
        (println "Solved" solution-name (dissoc sln :path))
        (spit (str "./puzzles/" solution-name) (:path sln))))
    (shutdown-agents)))

(defn mine []
  (let [block (Integer/parseInt (str/trim (run-lambda "getmininginfo" "block")))
        puzzle (run-lambda "getmininginfo" "puzzle")
        task (run-lambda "getmininginfo" "task")
        puzzle-name (format "puzzle-%03d.cond" block)
        task-name (format "task-%03d.desc" block)
        solution-name (format "solution-%03d.sol" block)
        generated-name (format "generated-%03d.desc" block)]
    (println (format "Round: %03d balance %s" block (str/trim (run-lambda "getbalance"))))
    (if (.exists (io/file "puzzles" generated-name))
      (println "Already solved, nothing to do")
      (do
        (spit (str "./puzzles/" puzzle-name) puzzle)
        (spit (str "./puzzles/" task-name) task)
        (println "Generating level...")
        (let [puzzle (icfpc.parser/parse-puzzle (str "../puzzles/" puzzle-name))
              level (icfpc.generator/generate-level (str "../puzzles/" puzzle-name))]
          (icfpc.parser/validate-puzzle puzzle level)
          (spit (str "./puzzles/" generated-name) (icfpc.writer/desc level))
          (println "Level generated" generated-name)
          (println "Trying to solve level...")
          (let [level (level/load-level (str "../puzzles/" task-name))
                sln   (bot/solve level (merge
                                        {:debug? false
                                         :lookahead? false #_(<= (:width level) 200)}))]
            (println "Solved" solution-name (dissoc sln :path))
            (spit (str "./puzzles/" solution-name) (:path sln)))
          (println (run-lambda "submit" (str block) (str "../../puzzles/" solution-name) (str "../../puzzles/" generated-name))))))
    (shutdown-agents)))


(comment

  (run-lambda "")
  (def block (Integer/parseInt (clojure.string/trim (run-lambda "getmininginfo" "block"))))
  (mine)

  (spit "./puzzles/foo.txt" "hello")
  (spit "./puzzles/foo.txt" "world")

  (mine)
  *e

  )

