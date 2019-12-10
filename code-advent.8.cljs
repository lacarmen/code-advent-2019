(ns code-advent.one
  (:require
   [cljs.core :refer [*command-line-args*]]
   [clojure.string :as string]
   ["fs" :as fs]))

(defn get-input []
  (-> *command-line-args*
      (first)
      (fs/readFileSync #js {:encoding "UTF-8"})))

(defn parse-layers [img-width img-height]
  (-> (get-input)
      (clojure.string/split #"")
      (->> rest ;; ignore the newline
           (map js/parseInt)
           (partition (* img-width img-height)))))

(defn flatten-layers [layers]
  (->> (apply map vector layers)
       (map (comp #(if (= 1 %) \█ \space) 
                  first
                  #(remove (partial = 2) %)))))

(defn print-img [pixels width]
  (->> (partition width pixels)
       (mapv (comp println #(string/join "" %)))))

(defn part-one []
  (let [layers          (parse-layers 25 6)
        min-zeros-layer (->> layers 
                             (sort-by #(count (filter zero? %)))
                             first)]
    (-> (* (count (filter #(= 1 %) min-zeros-layer))
           (count (filter #(= 2 %) min-zeros-layer)))
        println)))

(defn part-two []
  (-> (parse-layers 25 6)
      flatten-layers
      (print-img 25)))

(part-one)
(part-two)
