(ns ttt.core
  (:require [clojure.string :as str]))

(def presentation-symbols
  {:plus "+"
   :circle "o"
   :empty "-"})

(def board
  [[:empty :empty :empty]
   [:empty :empty :empty]
   [:empty :empty :empty]])

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
