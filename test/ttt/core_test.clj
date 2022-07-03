(ns ttt.core-test
  (:require [ttt.core :as sut]
            [clojure.test :as t :refer [deftest is testing are]]))

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

  (testing "3x3 board with empty, plus and circle."
    (let [expected "|o|+|-|\n|-|+|-|\n|-|-|o|"
          board [[:circle :plus :empty]
                 [:empty :plus :empty]
                 [:empty :empty :circle]]
          result (sut/board-presentation board)]
      (is (= expected result))))

  (testing "3x3 with filled."
    (let [expected "|o|+|o|\n|+|+|o|\n|+|o|o|"
          board [[:circle :plus :circle]
                 [:plus :plus :circle]
                 [:plus :circle :circle]]
          result (sut/board-presentation board)]
      (is (= expected result))))

  (testing "Empty board."
    (let [expected ""
          board []
          result (sut/board-presentation board)]
      (is (= expected result)))))

(deftest valid-move?-test
  (testing "Valid move 0,0 empty position."
    (let [move [2 2]
          board [[:empty :empty :empty]
                 [:empty :empty :empty]
                 [:empty :empty :empty]]
          result (sut/valid-move? move board)]
      (is (true? result))))

  (testing "Valid move 2,2 empty position."
    (let [move [2 2]
          board [[:empty :plus :empty]
                 [:circle :empty :plus]
                 [:circle :empty :empty]]
          result (sut/valid-move? move board)]
      (is (true? result))))

  (testing "Invalid move 2,2 already used."
    (let [move [2 2]
          board [[:empty :empty :empty]
                 [:empty :empty :empty]
                 [:empty :empty :plus]]
          result (sut/valid-move? move board)]
      (is (false? result))))

  (testing "Invalid 4,2 outside of board."
    (let [move [4 2]
          board [[:empty :plus :empty]
                 [:circle :empty :plus]
                 [:circle :empty :empty]]
          result (sut/valid-move? move board)]
      (is (false? result)))))

(deftest place-on-board-test
  (testing "Place :plus on 2,2."
    (let [expected [[:empty :empty :empty]
                    [:empty :empty :empty]
                    [:empty :empty :plus]]
          board [[:empty :empty :empty]
                 [:empty :empty :empty]
                 [:empty :empty :empty]]
          position [2 2]
          result (sut/place-on-board :plus position board)]
      (is (= expected result))))

  (testing "Place :circle on 0,0."
    (let [expected [[:circle :empty :empty]
                    [:empty :empty :empty]
                    [:empty :empty :empty]]
          board [[:empty :empty :empty]
                 [:empty :empty :empty]
                 [:empty :empty :empty]]
          position [0 0]
          result (sut/place-on-board :circle position board)]
      (is (= expected result)))))
