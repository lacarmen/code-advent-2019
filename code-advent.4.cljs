(ns code-advent.four
  (:require
    [cljs.core :refer [*command-line-args*]]
    [clojure.string :as string]
    ["fs" :as fs]))

(defn get-input []
  (-> *command-line-args*
      first
      (fs/readFileSync #js {:encoding "UTF-8"})
      (js/console.log)))

(defn part-one []
  (->> (range 152085 670283)
       (filter #(->> (str %)
                     (map js/parseInt)
                     (apply <=)))
       (filter #(some (fn [[a b]] (= a b)) 
                      (partition 2 1 (str %))))
       count
       println))

(defn part-two []
  (->> (range 152085 670283)
       (filter #(->> (str %)
                     (map js/parseInt)
                     (apply <=)))
       (filter #(some (fn [x] (= 2 (count x)))
                      (map first (re-seq #"(\d)\1+" (str %)))))  
       count
       println))

(part-one)
(part-two)
