(ns sudoku-solver.core
  (:use clojure.set))

(def all-possible (set (rest (range 10))))

(defn puzzle-from-string [s]
  "Fills in the puzzle from a string"
  (let [puzzle (filter (fn [c]
                         (and (not= \newline (char c)) (not= \space (char c))))
                       s)]
    (if (not= (count puzzle) 81)
      (println "invalid puzzle")
      (zipmap 
        (range 81)
        (map (fn [c]
             (if (or (= c \.) (= c \0))
               all-possible
               (hash-set (Integer/parseInt (str c)))))
           puzzle)))))

(defn get-set-val [s]
  (if (= 1 (count s))
    (first s)
    "."))

(defn print-board [puzzle]
  (doseq [row (range 9)]
    (loop [cols (range 9)
           output ""]
      (if-not (seq cols)
        (println output)
        (let [col (first cols)
              cur-set (get puzzle (+ (* row 9) col))
              pipe (if (= 2 (mod col 3))
                     "|"
                     "")
              loc-char (get-set-val cur-set)]
          (recur (rest cols) (str output loc-char pipe)))))
    (when (= (mod row 3) 2)
      (println "---+---+---"))))

(defn row [cur]
  (int (/ cur 9)))

(defn col [cur]
  (mod cur 9))

(defn get-row [cur]
  (let [r (row cur)]
    (map #(+ (* r 9) %) (range 9))))

(defn get-column [cur]
  (let [c (col cur)]
      (map #(+ (* 9 %) c) (range 9))))

(defn first-block-cell [cur]
  (let [row-adj (* 9 (mod (row cur) 3))
        col-adj (mod (col cur) 3)]
    (- cur row-adj col-adj)))

(defn next-3 [cur]
  [cur (inc cur) (+ cur 2)])

(defn get-block [cur]
  (let [first-cell (first-block-cell cur)
        row1 (next-3 first-cell)
        row2 (next-3 (+ first-cell 9))
        row3 (next-3 (+ first-cell 18))]
    (concat row1 row2 row3)))

(defn get-related-cells [cur]
  (let [row (get-row cur)
        col (get-column cur)
        block (get-block cur)]
    (disj (into (hash-set) (concat row col block)) cur)))

(defn get-possib [puzzle locs]
  (reduce (fn [loc1 loc2]
            (let [p1 (get puzzle loc1)
                  p2 (get puzzle loc2)]
            (union p1 p2)))
          locs))

(defn rem-possib [puzzle loc n]
  (loop [set-keys (get-related-cells loc)
         new-puz puzzle]
    (if-not (seq set-keys)
      new-puz
      (let [cur (first set-keys)
            prev-size (count (get new-puz cur))
            updated-set (disj (get new-puz cur) n)
            new-size (count updated-set)
            updated-puz (assoc new-puz cur updated-set)
            final-puz (cond   
                          (= 1 prev-size) new-puz
                          (> new-size 1) updated-puz
                          (= 1 new-size) (rem-possib updated-puz cur (first updated-set)))]
        (recur (rest set-keys) final-puz)))))

(defn get-cells [puzzle locs]
  (zipmap locs (map (partial get puzzle) locs)))

(defn solve [string]
  (let [puzzle (puzzle-from-string string)
        solved (filter #(= 1 (count (val %))) puzzle)]
    (loop [given solved
           new-puz puzzle]
      (if-not (seq given)
        (do
          (print-board new-puz)
          new-puz)
        (let [cur-entry (first given)
              loc (key cur-entry)
              num-to-remove (first (val cur-entry))]
          (recur (rest given) (rem-possib new-puz loc num-to-remove)))))))
