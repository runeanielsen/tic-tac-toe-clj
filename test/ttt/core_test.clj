(ns ttt.core-test
  (:require [ttt.core :as sut]
            [clojure.test :as t :refer [deftest is testing are]]))

(deftest create-board-test
  (are [expected result] (= expected result)
    nil (sut/board 0 0)
    nil (sut/board 1 0)
    nil (sut/board 0 1)
    [[:empty :empty]] (sut/board 1 2)
    [[:empty]
     [:empty]] (sut/board 2 1)
    [[:empty :empty :empty]
     [:empty :empty :empty]
     [:empty :empty :empty]] (sut/board 3 3)))

(deftest presentation-symbols
  (are [x y] (= x y)
    "+" (sut/presentation-symbols :plus)
    "o" (sut/presentation-symbols :circle)
    "-" (sut/presentation-symbols :empty)))

(deftest board-presentation
  (testing "3x3 board with only empty fields."
    (let [expected "|-|-|-|\n|-|-|-|\n|-|-|-|"
          board [[:empty :empty :empty]
                 [:empty :empty :empty]
                 [:empty :empty :empty]]
          result (sut/board-presentation board)]
      (is (= expected result))))
  (testing "3x3 board with only empty, plus and circle."
    (let [expected "|o|+|-|\n|-|+|-|\n|-|-|o|"
          board [[:circle :plus :empty]
                 [:empty :plus :empty]
                 [:empty :empty :circle]]
          result (sut/board-presentation board)]
      (is (= expected result))))
  (testing "2x3 board with only empty fields."
    (let [expected "|o|+|-|\n|-|+|-|"
          board [[:circle :plus :empty]
                 [:empty :plus :empty]]
          result (sut/board-presentation board)]
      (is (= expected result))))
  (testing "2x1 board with only empty fields."
    (let [expected "|o|\n|-|"
          board [[:circle]
                 [:empty]]
          result (sut/board-presentation board)]
      (is (= expected result))))
  (testing "Empty board."
    (let [expected ""
          board []
          result (sut/board-presentation board)]
      (is (= expected result)))))
