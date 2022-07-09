(ns ttt.core
  (:require [clojure.string :as str]))

(def presentation-symbols
  {:plus "+"
   :circle "o"
   :empty "-"})

(def initial-board
  [[:empty :empty :empty]
   [:empty :empty :empty]
   [:empty :empty :empty]])

(def next-turn
  {:plus :circle
   :circle :plus})

(defn board-presentation [board]
  (let [present-rows (comp (map #(map presentation-symbols %))
                           (map #(str/join "|" %))
                           (map #(str "|" % "|")))
        rows (into [] present-rows board)]
    (str/join "\n" rows)))

(defn valid-move? [move board]
  (= :empty (get-in board move)))

(defn place-on-board [value placement board]
  (assoc-in board placement value))

(defn get-winner [board]
  (let [winner (comp (map set) (filter #(= (count %) 1)) (map #(disj % :empty)))
        columns (partition 3 (apply interleave board))
        cross [(mapv #(get-in board [% %]) (range 3))
               (mapv #(get-in board [% (- 2 %)]) (range 3))]]
    (ffirst (into [] winner (concat board columns cross)))))
