(ns code-advent.six
  (:require
    [cljs.core :refer [*command-line-args*]]
    [clojure.set :as set]
    [clojure.string :as string]    
    ["fs" :as fs]))

(defn get-input []
  (-> *command-line-args*
      first
      (fs/readFileSync #js {:encoding "UTF-8"})))

(defn ->orbiter-map [orbits]
  (reduce (fn [res orbit]
            (let [[orbitee orbiter] (string/split orbit #"\)")]
              (update res orbitee (fnil conj #{}) orbiter)))
          {}
          orbits))

(defn ->orbitee-map [orbits]
  (reduce (fn [res orbit]
            (let [[orbitee orbiter] (string/split orbit #"\)")]
              (assoc res orbiter orbitee)))
          {}
          orbits))

(defn ->deep-orbiter-map [root orbiter-map]
  (if-let [orbiters (get orbiter-map root)]
    {root (apply merge (map #(->deep-orbiter-map % orbiter-map) orbiters))}
    {root nil}))

(defn get-all-orbit-paths [deep-orbiter-map]
  (let [children (fn [path]
                   (if-let [v (get-in deep-orbiter-map path)]
                     (map (fn [x] (conj path x)) (keys v))
                       []))
        branch? (fn [node] (-> (children node) seq boolean))]
    (->> (keys deep-orbiter-map)
         (map vector)
         (mapcat #(tree-seq branch? children %)))))

(defn path-to-COM [root orbitee-map]
  (if-let [orbitee (get orbitee-map root)]
    (cons root (path-to-COM orbitee orbitee-map))
    [root]))

(defn part-one []
  (->> (string/split (get-input) #"\n")
       (->orbiter-map)
       (->deep-orbiter-map "COM")
       (#(get % "COM"))
       get-all-orbit-paths
       (map count)
       (apply +)
       println))

(defn part-two []
  (let [orbitee-map (-> (get-input)
                        (string/split #"\n")
                        (->orbitee-map))
        SAN-path    (path-to-COM "SAN" orbitee-map)
        YOU-path    (path-to-COM "YOU" orbitee-map)
        common      (set/intersection (set SAN-path) (set YOU-path))]
    ;; Find the path from SAN to COM then YOU to COM and get the unique
    ;; elements from the two paths. This works because the orbitee map is
    ;; a tree (ie. children only have one parent)
    (-> (set/union 
         (set/difference (set SAN-path) common)
         (set/difference (set YOU-path) common))
        (set/difference #{"SAN" "YOU"})  ;; we want the orbit jumps, not the whole path
        count
        println)))

(part-one)
(part-two)


(->> (->orbitee-map (string/split (get-input) #"\n"))
     println)
