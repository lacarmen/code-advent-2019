(ns code-advent.two
  (:require
    [cljs.core :refer [*command-line-args*]]
    [clojure.string :as string]
    ["fs" :as fs]))

(defn get-input []
  (-> *command-line-args*
      first
      (fs/readFileSync #js {:encoding "UTF-8"})))

(def op-lookup 
  {1 + 
   2 *})

(defn run-program [ip program]
  (let [[op i1 i2 o] (subvec program ip (+ ip 4))]
    (if (= 99 op)
      program
      (let [result ((get op-lookup op) (get program i1) (get program i2))]
        (recur (+ ip 4) (assoc program o result))))))

(def program
  (mapv js/parseInt (string/split (get-input) #",")))

(defn part-one []
  (->> (assoc program 1 12 2 2)
       (run-program 0)
       first
       println))

(defn part-two []
  (let [target   19690720
        attempts (for [n (range 100)
                       v (range 100)] [n v])]
    (->> attempts
         (map (fn [[n v]]
                [(->> (assoc program 1 n 2 v)
                      (run-program 0)
                      (first))
                 [n v]]))
         (some (fn [[total [n v]]]
                 (if (= target total) (+ v (* 100 n)))))
         println)))
             
(part-one)
(part-two)
