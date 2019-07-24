;;simple ns to compare rust scores
;;with baseline clojure scores for
;;verification.
(ns icfpc.compare)

;;Sample problem
;;problems/prob-277.desc 	score 10473 	time 29272 ms

(defn rust->edn [lines]
  (into {} 
        (for [l lines]
          (let [[p s t] (clojure.string/split l #"\t")]
            [(first (re-seq #"prob-[0-9]+" p))
             {:score (clojure.edn/read-string
                      (second (clojure.string/split s #"\s")))
              :time  (clojure.edn/read-string
                      (second (clojure.string/split t #"\s")))}]))))


(defn compare-scores [l]
  (let [baseline (clojure.edn/read-string (slurp "scores.edn"))]
    (let [diffs (->> (keys baseline)
                     (map (fn [k] {:prob k :score (:score (l k))
                                   :baseline-score (:score (baseline k))}))
                     (filter (fn [{:keys [prob baseline-score score]}]
                               (not= baseline-score score)))
                     vec)
          ndiff (count diffs)]
      (doseq [d (sort-by :prob diffs)]
        (println d))
      (println [:there-are (inc ndiff) :differences-from-baseline]))))

(comment ;;comparing against icfpc2019-rust 6d2c59330a42adfb4b15147ccf03c34c38959ba5

  (-> (slurp "rust-scores6d2c59.txt")
      clojure.string/split-lines
      rust->edn
      compare-scores)

  (spit "differences.txt"  (with-out-str (-> (slurp "rust-scores6d2c59.txt")
                                             clojure.string/split-lines
                                             rust->edn
                                             compare-scores)))

;; {:prob prob-002, :score 446, :baseline-score 427}
;; {:prob prob-003, :score 242, :baseline-score 245}
;; {:prob prob-004, :score 492, :baseline-score 476}
;; {:prob prob-005, :score 202, :baseline-score 257}
;; {:prob prob-006, :score 144, :baseline-score 163}
;; {:prob prob-007, :score 172, :baseline-score 179}
;; {:prob prob-008, :score 215, :baseline-score 206}
;; {:prob prob-009, :score 144, :baseline-score 138}
;; {:prob prob-010, :score 459, :baseline-score 494}
;; {:prob prob-011, :score 448, :baseline-score 428}
;; {:prob prob-012, :score 455, :baseline-score 425}
;; {:prob prob-013, :score 454, :baseline-score 477}
;; {:prob prob-014, :score 446, :baseline-score 413}
;; {:prob prob-015, :score 415, :baseline-score 429}
;; {:prob prob-016, :score 393, :baseline-score 396}
;; {:prob prob-017, :score 413, :baseline-score 429}
;; {:prob prob-018, :score 424, :baseline-score 415}
;; {:prob prob-019, :score 374, :baseline-score 378}
;; {:prob prob-020, :score 387, :baseline-score 395}
;; {:prob prob-021, :score 1262, :baseline-score 1296}
;; {:prob prob-022, :score 630, :baseline-score 589}
;; {:prob prob-023, :score 1170, :baseline-score 1130}
;; {:prob prob-024, :score 1024, :baseline-score 1028}
;; {:prob prob-025, :score 621, :baseline-score 655}
;; {:prob prob-026, :score 1211, :baseline-score 1163}
;; {:prob prob-027, :score 1426, :baseline-score 1387}
;; {:prob prob-028, :score 1240, :baseline-score 1226}
;; {:prob prob-029, :score 1004, :baseline-score 1055}
;; {:prob prob-030, :score 777, :baseline-score 748}
;; {:prob prob-031, :score 1347, :baseline-score 1367}
;; {:prob prob-032, :score 1446, :baseline-score 1392}
;; {:prob prob-033, :score 1282, :baseline-score 1275}
;; {:prob prob-034, :score 1328, :baseline-score 1357}
;; {:prob prob-035, :score 1035, :baseline-score 1073}
;; {:prob prob-036, :score 556, :baseline-score 564}
;; {:prob prob-037, :score 1134, :baseline-score 1145}
;; {:prob prob-038, :score 701, :baseline-score 742}
;; {:prob prob-039, :score 912, :baseline-score 929}
;; {:prob prob-040, :score 1219, :baseline-score 1163}
;; {:prob prob-041, :score 1139, :baseline-score 1146}
;; {:prob prob-042, :score 1033, :baseline-score 991}
;; {:prob prob-043, :score 1261, :baseline-score 1202}
;; {:prob prob-044, :score 1051, :baseline-score 1036}
;; {:prob prob-045, :score 1204, :baseline-score 1201}
;; {:prob prob-046, :score 1022, :baseline-score 1089}
;; {:prob prob-047, :score 858, :baseline-score 873}
;; {:prob prob-048, :score 1159, :baseline-score 1190}
;; {:prob prob-049, :score 977, :baseline-score 960}
;; {:prob prob-050, :score 1194, :baseline-score 1250}
;; {:prob prob-051, :score 2542, :baseline-score 2595}
;; {:prob prob-052, :score 2575, :baseline-score 2645}
;; {:prob prob-053, :score 2238, :baseline-score 2365}
;; {:prob prob-054, :score 2656, :baseline-score 2418}
;; {:prob prob-055, :score 2797, :baseline-score 2799}
;; {:prob prob-056, :score 3125, :baseline-score 3126}
;; {:prob prob-057, :score 3030, :baseline-score 2946}
;; {:prob prob-058, :score 2603, :baseline-score 2685}
;; {:prob prob-059, :score 2683, :baseline-score 2723}
;; {:prob prob-060, :score 1991, :baseline-score 1896}
;; {:prob prob-061, :score 2679, :baseline-score 2763}
;; {:prob prob-062, :score 2975, :baseline-score 3045}
;; {:prob prob-063, :score 3188, :baseline-score 3094}
;; {:prob prob-064, :score 2791, :baseline-score 2747}
;; {:prob prob-065, :score 3479, :baseline-score 3284}
;; {:prob prob-066, :score 2912, :baseline-score 2685}
;; {:prob prob-067, :score 2597, :baseline-score 2566}
;; {:prob prob-068, :score 2211, :baseline-score 2299}
;; {:prob prob-069, :score 2528, :baseline-score 2701}
;; {:prob prob-070, :score 2525, :baseline-score 2604}
;; {:prob prob-071, :score 2755, :baseline-score 2747}
;; {:prob prob-072, :score 2590, :baseline-score 2488}
;; {:prob prob-073, :score 3784, :baseline-score 3839}
;; {:prob prob-074, :score 3104, :baseline-score 2997}
;; {:prob prob-075, :score 2890, :baseline-score 3004}
;; {:prob prob-076, :score 3483, :baseline-score 3539}
;; {:prob prob-077, :score 2184, :baseline-score 2167}
;; {:prob prob-078, :score 1943, :baseline-score 1955}
;; {:prob prob-079, :score 3275, :baseline-score 3299}
;; {:prob prob-080, :score 3363, :baseline-score 3275}
;; {:prob prob-081, :score 2139, :baseline-score 2184}
;; {:prob prob-082, :score 3379, :baseline-score 3335}
;; {:prob prob-083, :score 3154, :baseline-score 3284}
;; {:prob prob-084, :score 3204, :baseline-score 3200}
;; {:prob prob-085, :score 3792, :baseline-score 3721}
;; {:prob prob-086, :score 2480, :baseline-score 2410}
;; {:prob prob-087, :score 3427, :baseline-score 3433}
;; {:prob prob-088, :score 3420, :baseline-score 3538}
;; {:prob prob-089, :score 4030, :baseline-score 3984}
;; {:prob prob-090, :score 3486, :baseline-score 3491}
;; {:prob prob-091, :score 3575, :baseline-score 3588}
;; {:prob prob-092, :score 3200, :baseline-score 3329}
;; {:prob prob-093, :score 3214, :baseline-score 3146}
;; {:prob prob-094, :score 2860, :baseline-score 2755}
;; {:prob prob-095, :score 3309, :baseline-score 3319}
;; {:prob prob-096, :score 3524, :baseline-score 3589}
;; {:prob prob-097, :score 3858, :baseline-score 4002}
;; {:prob prob-098, :score 3465, :baseline-score 3324}
;; {:prob prob-099, :score 3030, :baseline-score 3017}
;; {:prob prob-100, :score 3110, :baseline-score 3143}
;; {:prob prob-101, :score 10130, :baseline-score 10179}
;; {:prob prob-102, :score 8388, :baseline-score 8543}
;; {:prob prob-103, :score 12151, :baseline-score 12222}
;; {:prob prob-104, :score 9660, :baseline-score 9653}
;; {:prob prob-105, :score 9502, :baseline-score 9424}
;; {:prob prob-106, :score 10208, :baseline-score 9490}
;; {:prob prob-107, :score 8470, :baseline-score 8275}
;; {:prob prob-108, :score 10329, :baseline-score 10462}
;; {:prob prob-109, :score 11090, :baseline-score 11016}
;; {:prob prob-110, :score 9279, :baseline-score 9280}
;; {:prob prob-111, :score 9997, :baseline-score 10265}
;; {:prob prob-112, :score 9070, :baseline-score 9134}
;; {:prob prob-113, :score 7600, :baseline-score 7604}
;; {:prob prob-114, :score 8955, :baseline-score 8915}
;; {:prob prob-115, :score 8283, :baseline-score 8245}
;; {:prob prob-116, :score 8779, :baseline-score 7890}
;; {:prob prob-117, :score 10361, :baseline-score 10617}
;; {:prob prob-118, :score 10074, :baseline-score 10011}
;; {:prob prob-119, :score 9803, :baseline-score 9835}
;; {:prob prob-120, :score 9008, :baseline-score 9532}
;; {:prob prob-121, :score 10069, :baseline-score 9806}
;; {:prob prob-122, :score 10392, :baseline-score 10415}
;; {:prob prob-123, :score 9329, :baseline-score 9251}
;; {:prob prob-124, :score 10695, :baseline-score 10740}
;; {:prob prob-125, :score 10741, :baseline-score 10731}
;; {:prob prob-126, :score 11246, :baseline-score 11063}
;; {:prob prob-127, :score 7438, :baseline-score 7765}
;; {:prob prob-128, :score 10516, :baseline-score 10537}
;; {:prob prob-129, :score 10855, :baseline-score 11113}
;; {:prob prob-130, :score 10118, :baseline-score 10218}
;; {:prob prob-131, :score 10112, :baseline-score 10129}
;; {:prob prob-132, :score 10470, :baseline-score 10535}
;; {:prob prob-133, :score 7976, :baseline-score 7608}
;; {:prob prob-134, :score 9978, :baseline-score 9659}
;; {:prob prob-135, :score 9851, :baseline-score 10048}
;; {:prob prob-136, :score 12603, :baseline-score 12341}
;; {:prob prob-137, :score 9966, :baseline-score 9921}
;; {:prob prob-138, :score 11846, :baseline-score 11904}
;; {:prob prob-139, :score 8043, :baseline-score 8105}
;; {:prob prob-140, :score 8264, :baseline-score 8474}
;; {:prob prob-141, :score 8555, :baseline-score 8934}
;; {:prob prob-142, :score 7226, :baseline-score 7245}
;; {:prob prob-143, :score 10361, :baseline-score 10667}
;; {:prob prob-144, :score 10616, :baseline-score 10855}
;; {:prob prob-145, :score 10521, :baseline-score 10469}
;; {:prob prob-146, :score 9624, :baseline-score 9619}
;; {:prob prob-147, :score 11632, :baseline-score 11411}
;; {:prob prob-148, :score 7108, :baseline-score 7035}
;; {:prob prob-149, :score 11028, :baseline-score 10780}
;; {:prob prob-150, :score 8631, :baseline-score 8891}
;; {:prob prob-151, :score 1789, :baseline-score 1766}
;; {:prob prob-152, :score 1606, :baseline-score 1572}
;; {:prob prob-153, :score 2048, :baseline-score 2008}
;; {:prob prob-154, :score 1944, :baseline-score 1939}
;; {:prob prob-155, :score 2303, :baseline-score 2294}
;; {:prob prob-156, :score 1550, :baseline-score 1692}
;; {:prob prob-157, :score 1732, :baseline-score 1727}
;; {:prob prob-158, :score 1846, :baseline-score 1807}
;; {:prob prob-159, :score 1276, :baseline-score 1295}
;; {:prob prob-160, :score 2808, :baseline-score 2765}
;; {:prob prob-161, :score 2657, :baseline-score 2757}
;; {:prob prob-162, :score 3761, :baseline-score 3758}
;; {:prob prob-163, :score 1844, :baseline-score 1613}
;; {:prob prob-164, :score 2384, :baseline-score 2434}
;; {:prob prob-165, :score 3071, :baseline-score 3112}
;; {:prob prob-166, :score 2730, :baseline-score 2881}
;; {:prob prob-167, :score 4117, :baseline-score 4135}
;; {:prob prob-168, :score 2592, :baseline-score 2578}
;; {:prob prob-169, :score 3514, :baseline-score 3393}
;; {:prob prob-170, :score 3155, :baseline-score 3117}
;; {:prob prob-171, :score 3350, :baseline-score 3302}
;; {:prob prob-172, :score 3637, :baseline-score 3722}
;; {:prob prob-173, :score 2893, :baseline-score 2923}
;; {:prob prob-174, :score 3734, :baseline-score 3852}
;; {:prob prob-175, :score 2760, :baseline-score 2747}
;; {:prob prob-176, :score 3634, :baseline-score 3600}
;; {:prob prob-177, :score 3233, :baseline-score 3454}
;; {:prob prob-178, :score 3153, :baseline-score 3320}
;; {:prob prob-179, :score 2923, :baseline-score 2862}
;; {:prob prob-180, :score 3127, :baseline-score 3230}
;; {:prob prob-181, :score 7565, :baseline-score 7298}
;; {:prob prob-182, :score 4562, :baseline-score 4463}
;; {:prob prob-183, :score 9048, :baseline-score 9104}
;; {:prob prob-184, :score 6592, :baseline-score 6597}
;; {:prob prob-185, :score 5879, :baseline-score 5649}
;; {:prob prob-186, :score 4794, :baseline-score 4679}
;; {:prob prob-187, :score 3681, :baseline-score 3682}
;; {:prob prob-188, :score 8078, :baseline-score 8233}
;; {:prob prob-189, :score 9820, :baseline-score 9562}
;; {:prob prob-190, :score 7327, :baseline-score 7396}
;; {:prob prob-191, :score 8903, :baseline-score 8956}
;; {:prob prob-192, :score 9671, :baseline-score 9289}
;; {:prob prob-193, :score 10793, :baseline-score 10794}
;; {:prob prob-194, :score 9548, :baseline-score 9460}
;; {:prob prob-195, :score 11095, :baseline-score 10838}
;; {:prob prob-196, :score 10199, :baseline-score 10304}
;; {:prob prob-197, :score 6863, :baseline-score 6996}
;; {:prob prob-198, :score 9144, :baseline-score 8758}
;; {:prob prob-199, :score 9741, :baseline-score 9933}
;; {:prob prob-200, :score 8209, :baseline-score 8189}
;; {:prob prob-201, :score 11112, :baseline-score 10798}
;; {:prob prob-202, :score 9609, :baseline-score 9560}
;; {:prob prob-203, :score 10031, :baseline-score 10036}
;; {:prob prob-204, :score 10562, :baseline-score 10687}
;; {:prob prob-205, :score 10105, :baseline-score 10431}
;; {:prob prob-206, :score 11745, :baseline-score 11672}
;; {:prob prob-207, :score 9931, :baseline-score 10069}
;; {:prob prob-208, :score 10060, :baseline-score 10193}
;; {:prob prob-209, :score 7417, :baseline-score 7520}
;; {:prob prob-210, :score 10127, :baseline-score 10101}
;; {:prob prob-211, :score 24555, :baseline-score 24369}
;; {:prob prob-212, :score 20723, :baseline-score 19554}
;; {:prob prob-213, :score 24807, :baseline-score 24986}
;; {:prob prob-214, :score 22843, :baseline-score 23137}
;; {:prob prob-215, :score 26969, :baseline-score 28986}
;; {:prob prob-216, :score 25399, :baseline-score 25394}
;; {:prob prob-217, :score 21956, :baseline-score 21938}
;; {:prob prob-218, :score 34607, :baseline-score 34441}
;; {:prob prob-219, :score 29583, :baseline-score 29052}
;; {:prob prob-220, :score 29412, :baseline-score 28999}
;; {:prob prob-221, :score 1144, :baseline-score 1215}
;; {:prob prob-222, :score 1310, :baseline-score 1227}
;; {:prob prob-223, :score 1305, :baseline-score 1455}
;; {:prob prob-224, :score 1564, :baseline-score 1717}
;; {:prob prob-225, :score 1020, :baseline-score 1157}
;; {:prob prob-226, :score 1124, :baseline-score 1294}
;; {:prob prob-227, :score 895, :baseline-score 939}
;; {:prob prob-228, :score 729, :baseline-score 807}
;; {:prob prob-229, :score 1071, :baseline-score 1079}
;; {:prob prob-230, :score 998, :baseline-score 1080}
;; {:prob prob-231, :score 1399, :baseline-score 1708}
;; {:prob prob-232, :score 2081, :baseline-score 2021}
;; {:prob prob-233, :score 1617, :baseline-score 1640}
;; {:prob prob-234, :score 1642, :baseline-score 1756}
;; {:prob prob-235, :score 1286, :baseline-score 1421}
;; {:prob prob-236, :score 1432, :baseline-score 1582}
;; {:prob prob-237, :score 1823, :baseline-score 1723}
;; {:prob prob-238, :score 1842, :baseline-score 1943}
;; {:prob prob-239, :score 1596, :baseline-score 1701}
;; {:prob prob-240, :score 1929, :baseline-score 1762}
;; {:prob prob-241, :score 2904, :baseline-score 3272}
;; {:prob prob-242, :score 2843, :baseline-score 3323}
;; {:prob prob-243, :score 1837, :baseline-score 1792}
;; {:prob prob-244, :score 3100, :baseline-score 3236}
;; {:prob prob-245, :score 2124, :baseline-score 2257}
;; {:prob prob-246, :score 2587, :baseline-score 2545}
;; {:prob prob-247, :score 2353, :baseline-score 2369}
;; {:prob prob-248, :score 2007, :baseline-score 1936}
;; {:prob prob-249, :score 2302, :baseline-score 2386}
;; {:prob prob-250, :score 2610, :baseline-score 2561}
;; {:prob prob-251, :score 3387, :baseline-score 3354}
;; {:prob prob-252, :score 3552, :baseline-score 4019}
;; {:prob prob-253, :score 2933, :baseline-score 3348}
;; {:prob prob-254, :score 2999, :baseline-score 2807}
;; {:prob prob-255, :score 4103, :baseline-score 4342}
;; {:prob prob-256, :score 3057, :baseline-score 3147}
;; {:prob prob-257, :score 3259, :baseline-score 3168}
;; {:prob prob-258, :score 2873, :baseline-score 2613}
;; {:prob prob-259, :score 3680, :baseline-score 3902}
;; {:prob prob-260, :score 3591, :baseline-score 4140}
;; {:prob prob-261, :score 3345, :baseline-score 3409}
;; {:prob prob-262, :score 4189, :baseline-score 4562}
;; {:prob prob-263, :score 3669, :baseline-score 4522}
;; {:prob prob-264, :score 3709, :baseline-score 3633}
;; {:prob prob-265, :score 2720, :baseline-score 2751}
;; {:prob prob-266, :score 3232, :baseline-score 3180}
;; {:prob prob-267, :score 3528, :baseline-score 3114}
;; {:prob prob-268, :score 3797, :baseline-score 4022}
;; {:prob prob-269, :score 3719, :baseline-score 4232}
;; {:prob prob-270, :score 3697, :baseline-score 3454}
;; {:prob prob-271, :score 5752, :baseline-score 6194}
;; {:prob prob-272, :score 6695, :baseline-score 6814}
;; {:prob prob-273, :score 10702, :baseline-score 10971}
;; {:prob prob-274, :score 7241, :baseline-score 6481}
;; {:prob prob-275, :score 4709, :baseline-score 4821}
;; {:prob prob-276, :score 7828, :baseline-score 8130}
;; {:prob prob-277, :score 10473, :baseline-score 11013}
;; {:prob prob-278, :score 8573, :baseline-score 8176}
;; {:prob prob-279, :score 9729, :baseline-score 9979}
;; {:prob prob-280, :score 5406, :baseline-score 6458}
;; {:prob prob-281, :score 6852, :baseline-score 6576}
;; {:prob prob-282, :score 6283, :baseline-score 7345}
;; {:prob prob-283, :score 6594, :baseline-score 6387}
;; {:prob prob-284, :score 5273, :baseline-score 5433}
;; {:prob prob-285, :score 6366, :baseline-score 6807}
;; {:prob prob-286, :score 7390, :baseline-score 7932}
;; {:prob prob-287, :score 8466, :baseline-score 8692}
;; {:prob prob-288, :score 5030, :baseline-score 5887}
;; {:prob prob-289, :score 10687, :baseline-score 10288}
;; {:prob prob-290, :score 6606, :baseline-score 6811}
;; {:prob prob-291, :score 8580, :baseline-score 8883}
;; {:prob prob-292, :score 9634, :baseline-score 8941}
;; {:prob prob-293, :score 8803, :baseline-score 9953}
;; {:prob prob-294, :score 8540, :baseline-score 9694}
;; {:prob prob-295, :score 7369, :baseline-score 7670}
;; {:prob prob-296, :score 3218, :baseline-score 3348}
;; {:prob prob-297, :score 4165, :baseline-score 4490}
;; {:prob prob-298, :score 7114, :baseline-score 7142}
;; {:prob prob-299, :score 9351, :baseline-score 10521}
;; {:prob prob-300, :score 8374, :baseline-score 8879}
;; [:there-are 300 :differences-from-baseline]



 )
