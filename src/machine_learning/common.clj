(ns machine-learning.common
  (:require [uncomplicate.neanderthal.core :refer :all]
            [uncomplicate.neanderthal.native :refer :all]))

;; check types!
(defn horzcat [a b]
  (let [a-rows (mrows a)
        b-rows (mrows b)]
    (if-not (= a-rows b-rows)
      (throw (Exception. "must have same number of rows"))
      (transfer! (flatten (concat a b))
                 (dge a-rows (+ (ncols a) (ncols b)))))))

;; check types!
(defn vertcat [a b]
  (let [a-cols (ncols a)
        b-cols (ncols b)]
    (if-not (= a-cols b-cols)
      (throw (Exception. "must have the same number of cols"))
      (transfer! (mapcat concat a b)
                 (dge (+ (mrows a) (mrows b)) a-cols)))))
