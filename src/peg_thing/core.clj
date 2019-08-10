(ns peg-thing.core
  (require [clojure.set :as set])
  (require [clojure.string :as s])
  (:gen-class))


(declare successful-move prompt-move game-over query-rows characters-as-strings prompt-rows)


(defn positive-numbers
  ([] (positive-numbers 1))
  ([n] (lazy-seq (cons n (positive-numbers (inc n))))))


;;another way much simpler
(defn tri*
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))


(def tri (tri*))

(defn triangular?
  [n]
  (= n (last (take-while #(>= n %) tri))))

;;ne marche pas sur des lazy-seq tu es obligé de passer par du take/drop
(defn row-tri2
  [n]
  (nth n tri))

(defn row-tri
  [n]
  (last (take n tri)))

(defn row-num
  [pos]
  (inc (count (take-while #(> pos %) tri))))

(defn connect
  [board max-pos pos neigh dest]
  (if (<= dest max-pos)
    (reduce
      (fn [new-board [p1 p2]]
        (assoc-in new-board [p1 :connections p2] neigh))
      board
      [[pos dest] [dest pos]])
    board))

(defn connect-right
  [board max pos]
  (let [neigh       (inc pos)
        destination (inc neigh)]
    (if-not (or (triangular? pos) (triangular? neigh))
      (connect board max pos neigh destination))))

(defn connect-down-left
  [board max pos]
  (let [row         (row-num pos)
        neigh       (+ pos row)
        destination (+ neigh (row-num neigh))]
    (connect board max pos neigh destination)))


(defn connect-down-right
  [board max pos]
  (let [row         (row-num pos)
        neigh       (+ 1 pos row)
        destination (+ 1 neigh (row-num neigh))]
    (connect board max pos neigh destination)))

(defn add-pos
  [board max pos]
  (let [peggedBoard (assoc-in board [pos :pegged] true)]
    (reduce (fn [newboard func] (func newboard max pos)) peggedBoard [connect-right connect-down-left connect-down-right])))

;;using functions as coll for reduce
(defn clean
  [text]
  (reduce
    (fn [string str-fun]
      (str-fun string))
    text
    [s/trim #(s/replace % #"lol" "LOL")]))

(defn new-board
  [rows]
  (let [initial-board {:rows rows}
        max-pos       (row-tri rows)]
    (reduce (fn [board pos] (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))

(defn pegged?
  [board pos]
  (get-in board [pos :pegged]))

(defn remove-peg
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))

(defn valid-moves
  [board pos]
  (into {}
        (filter
          (fn [dest neig]
            (and (pegged? board neig) (not (pegged? board dest))))
          (get-in board [pos :connections]))))

(def my-board (assoc-in (new-board 5) [4 :pegged] false))

(defn valid-move?
  [board p1 p2]
  (get (valid-moves board p1) p2))

(defn make-move
  [board p1 p2]
  (if-let [jumped (valid-move? board p1 p2)]
    (move-peg (remove-peg board jumped) p1 p2)))

(defn can-move?
  [board]
  (some (comp not-empty (partial valid-moves board)) (map first (filter #(get (second %) :pegged)) board)))


;;render

(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)

(defn render-pos
  [board pos]
  (str (nth letters (dec pos))
       (if (get-in board [pos :pegged])
         "O"
         "-")))

(defn row-positions
  [row-num]
  (range (inc (or (row-tri (dec row-num)) 0))
         (inc (row-tri row-num))))

(defn row-padding
  "String od spaces to add to the beginning of a row to center it"
  [row-num rows]
  (let [pad-length (/ (* (- rows row-num) pos-chars) 2)]
    (apply str (take pad-length (repeat " ")))))

(defn render-row
  [board row-num]
  (str (row-padding row-num (:rows board))
       (s/join " " (map (partial render-pos board) (row-positions row-num)))))

(defn print-board
  [board]
  (doseq [row-num (range 1 (inc (:rows board)))]
    (println (render-row board row-num))))

(defn letter->pos
  [letter]
  (inc (- (int (first letter)) alpha-start)))

(defn get-input
  ([] get-input nil)
  ([default]
   (let [input (clojure.string/trim (read-line))]
     (if (empty? input)
       default
       (clojure.string/lower-case input)))))

(defn user-entered-invalid-move
  [board]
  (println "\n!!! That was an invalid move:")
  (prompt-move board))

(defn user-entered-valid-move
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))

(defn prompt-move
  [board]
  (println "\nHere's your board:")
  (print-board board)
  (println "Move from where to where? Enter two letters:")
  (let [input (map letter->pos (characters-as-strings (get-input)))]
    (if-let [new-board (make-move board (first input) (second input))]
      (user-entered-valid-move new-board)
      (user-entered-invalid-move board))))

(defn game-over
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println "Game over! you had " remaining-pegs "pegs left")
    (print-board board)
    (println "Play Again? y/n [y]")
    (let [input (get-input "y")]
      (if (= "y" input)
        (prompt-rows)
        (do
          (println "Bye!")
          (System/exit 0))))))

(defn prompt-empty-peg
  [board]
  (println "Here's your board:")
  (print-board board)
  (println "Remove which peg? [e]")
  (prompt-move (remove-peg board (letter->pos (get-input "e")))))

(defn prompt-rows
  []
  (println "How many Rows? [5]")
  (let [rows  (Integer. (get-input 5))
        board (new-board rows)]
    (prompt-empty-peg board)))

(defn characters-as-strings
  [text]

  (s/trim text))


(defn -main
  "Starting Peg Thing Game:"
  [& args]
  (prompt-rows))

