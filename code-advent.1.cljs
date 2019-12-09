(ns code-advent.one
  (:require
    [cljs.core :refer [*command-line-args*]]
    [clojure.string :as string]
    ["fs" :as fs]))

(defn get-input []
  (-> *command-line-args*
      (first)
      (fs/readFileSync #js {:encoding "UTF-8"})))

(defn calculate-fuel [mass]
  (- (js/Math.floor (/ mass 3)) 2))

(defn part-one []
  (->> (string/split (get-input) #"\n")
       (map (comp calculate-fuel js/parseInt))
       (apply +)
       println))

(defn calculate-fuel-fuel [mass]
   (take-while pos? (iterate calculate-fuel mass)))

(defn part-two []
  (->> (string/split (get-input) #"\n")
       (mapcat (comp calculate-fuel-fuel calculate-fuel js/parseInt))
       (apply +)
       println))

(part-one)
(part-two)

