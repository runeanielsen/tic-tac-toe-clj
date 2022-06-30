(ns ttt.core
  (:require [clojure.string :as str]))

(def presentation-symbols
  {:plus "+"
   :circle "o"
   :empty "-"})

(defn board
  "Creates board based on rows and columns."
  [rows columns]
  (if (or (zero? rows) (zero? columns))
    nil
    (into [] (repeat rows (into [] (repeat columns :empty))))))

(defn board-presentation [board]
  (let [present (comp (map #(map presentation-symbols %))
                      (map #(str/join "|" %))
                      (map #(str "|" % "|")))]
    (str/join "\n" (into [] present board))))
