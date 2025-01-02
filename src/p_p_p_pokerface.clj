(ns p-p-p-pokerface)

(defn rank [card]
  (let [rpl {\T 10, \J 11, \Q 12, \K 13, \A 14 }
        [r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get rpl r))))

(defn suit [card]
  (let [[_ sc] card]
    (str sc)))

(defn multi? [hand m]
  (not (empty? (filter (fn [x] (= x m))
                       (vals (frequencies (map rank hand)))))))

(defn pair? [hand]
  (multi? hand 2))

(defn three-of-a-kind? [hand]
  (multi? hand 3))

(defn four-of-a-kind? [hand]
  (multi? hand 4))

(defn flush? [hand]
  (= 5 (first (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (let [freqs (sort (vals (frequencies (map rank hand))))]
    (and (= 2 (first freqs)) (= 3 (second freqs)))))

(defn two-pairs? [hand]
  (let [freqs (frequencies (vals (frequencies (map rank hand))))
        ]
    (or (= 2 (get freqs 2)) (= 1 (get freqs 4)))))


(defn straight? [hand]
  (let [ranks (fn [hand] (map rank hand))
        is-straight? (fn [ranks]
                       (let [lr (fn [r] (first r))]
                         (= ranks (range (lr ranks) (+ (lr ranks) 5)))))]
    (or (is-straight? (sort (ranks hand)))
        (is-straight? (sort (replace {14 1} (ranks hand)))))
))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map (fn [[ch val]] (if (ch hand) val 0))
              checkers))))
