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
  (let [find-winner (comp (mapcat frequencies)
                          (filter #(= 3 (val %)))
                          (filter #(not= (key %) :empty)))]
    (when-let [winner (or (first (into [] find-winner board))
                          (first (into [] find-winner (partition 3 (apply interleave board)))))]
      (key winner))))
