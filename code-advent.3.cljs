(ns code-advent.three
  (:require
   [cljs.core :refer [*command-line-args*]]
   [clojure.set :as set]
   [clojure.string :as string]
   ["fs" :as fs]))

(defn get-input []
  (-> *command-line-args*
      first
      (fs/readFileSync #js {:encoding "UTF-8"})))

(defn generate-wire-segment [[x-start y-start] [direction units]]
  (case direction 
    "U" (for [y (range (inc y-start) (+ (inc y-start) units))] [x-start y])
    "D" (for [y (range (dec y-start) (- (dec y-start) units) -1)] [x-start y])
    "R" (for [x (range (inc x-start) (+ (inc x-start) units))] [x y-start])
    "L" (for [x (range (dec x-start) (- (dec x-start) units) -1)] [x y-start])))

(defn generate-wire [wire directions]
  (if (empty? directions)
    wire
    (let [next-line (generate-wire-segment (or (last wire) [0 0]) (first directions))]
      (recur (into wire next-line) (rest directions)))))

(defn parse-directions [dir-string]
  (map (fn [dir] 
         [(first dir) (js/parseInt (apply str (rest dir)))])
       (string/split dir-string #",")))

(defn generate-wires []
  (->> (string/split (get-input) #"\n")
       (mapv parse-directions)
       (mapv (partial generate-wire []))))

(defn get-intersections [wires]
  (->> wires
       (map set)
       (apply set/intersection)))

(defn part-one []
  (let [wires (generate-wires)]
    (->> wires 
         (get-intersections)
         (map (fn [[x y]] (+ (js/Math.abs x) (js/Math.abs y))))
         (apply min)
         println)))

(defn get-steps-to-intersection [wire intersection]
  (inc (count (take-while #(not= intersection %) wire))))

(defn part-two []
  (let [wires (generate-wires)]
    (->> (get-intersections wires) 
         (map (fn [intersection]
                [(get-steps-to-intersection (first wires) intersection)
                 (get-steps-to-intersection (second wires) intersection)]))
         (map (partial apply +))
         (apply min)
         println)))

(part-one)
(part-two)

