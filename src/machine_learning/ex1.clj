(ns machine-learning.ex1
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.data.csv :as csv]
            [machine-learning.common :refer [horzcat vertcat]]
            [uncomplicate.neanderthal.core :refer :all]
            [uncomplicate.neanderthal.native :refer :all]))

(def theta (dge 1 2))

(defn read-data [fname]
  (with-open [reader (-> fname io/resource io/reader)]
    (doall
     (csv/read-csv reader))))

(defn parse-data [input]
  (let [m (count input)
        n (-> input first count)
        input' (->> input (apply mapcat vector) (map edn/read-string))]
    (dge m n (flatten input'))))

(defn process-data
  ;; generalise
  [input]
  (let [y (copy (submatrix input 0 1 (mrows input) 1))
        X (copy (as-> input $
                  (submatrix $ 0 0 (mrows input) 1)
                  (horzcat (dge (mrows input) 1 (repeat (mrows input) 1)) $)))]
    [X y]))

(defn gradient-descent [X y theta alpha n]
  (let [m (mrows y)
        scaling (/ alpha m)]
    (loop [theta theta n n]
      (if (zero? n)
        theta
        (let [h (mm X theta)
              errors (axpy -1 y h)
              theta-change (scal scaling (mm (trans X) errors))]
          (recur (axpy -1 theta-change theta) (dec n)))))))

(defn do-it []
  (let [[X y] (-> "ex1data1.txt" read-data parse-data process-data)
        theta (dge 2 1 [0 0])
        alpha 0.01
        iterations 1500]
    (gradient-descent X y theta alpha iterations)))
