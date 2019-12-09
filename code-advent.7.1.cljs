(ns code-advent.seven.one
  (:require
   [cljs.core :refer [*command-line-args*]]
   [clojure.string :as string]
   [goog.string :as gstring]
   ["js-combinatorics" :as combinatorics]
   ["fs" :as fs]))

(defn get-input []
  (-> *command-line-args*
      first
      (fs/readFileSync #js {:encoding "UTF-8"})))

(def acs-program
  (mapv js/parseInt (string/split (get-input) #",")))

(def op-lookup
  {1 +
   2 *
   7 (comp #(if (true? %) 1 0) <)
   8 (comp #(if (true? %) 1 0) =)})

(defn ->computer [input]
  {:ip     0
   :mem    acs-program
   :input  input
   :output []})

(defn read-word [computer address]
  (get-in computer [:mem address]))

(defn read-offset-word [computer offset]
  (get-in computer [:mem (+ offset (:ip computer))]))

(defn write-word [computer address value]
  (assoc-in computer [:mem address] value))

(defn parse-opcode [computer]
  (let [opcode (gstring/format "%05d" (read-offset-word computer 0))
        op     (->> (subvec (into [] opcode) 3)
                    (apply str)
                    (js/parseInt))
        modes  (->> (subvec (into [] opcode) 0 3)
                    (reverse)
                    (mapv js/parseInt))]
    {:op    op
     :modes modes}))

(defn normalize-param [computer modes offset]
  (if (zero? (modes (dec offset)))
    (read-word computer (read-offset-word computer offset))
    (read-offset-word computer offset)))

(defn move-ip [computer offset]
  (update computer :ip + offset))

(defn binary-op [computer {:keys [op modes]}]
  (let [f           (op-lookup op)
        p1          (normalize-param computer modes 1)
        p2          (normalize-param computer modes 2)
        out-address (read-offset-word computer 3)]
    (-> computer
        (write-word out-address (f p1 p2))
        (move-ip 4))))

(defn input-op [computer]
  (-> computer
      (write-word (read-offset-word computer 1) (first (:input computer)))
      (update :input rest)
      (move-ip 2)))

(defn output-op [computer {:keys [modes]}]
  (-> computer
      (update :output conj (normalize-param computer modes 1))
      (move-ip 2)))

(defn jump-op [computer {:keys [op modes]}]
  (let [pred   (if (= 5 op) (comp not zero?) zero?)
        param  (normalize-param computer modes 1)
        target (normalize-param computer modes 2)]
    (if (pred param)
      (assoc computer :ip target)
      (move-ip computer 3))))

(defn run-program [computer]
  (let [opcode (parse-opcode computer)]
    (case (:op opcode)
      99 computer
      (1 2 7 8) (recur (binary-op computer opcode))
      3 (recur (input-op computer))
      4 (output-op computer opcode)
      (5 6) (recur (jump-op computer opcode)))))

(defn run-amplifiers [computers]
  (let [computer (run-program (first computers))
        result   (last (:output computer))]
    (if (empty? (rest computers))
        result
        (recur (update-in (vec (rest computers)) [0 :input] conj result)))))

(defn create-computers [phase-settings]
  (mapv #(->computer [%]) phase-settings))

(defn permutations [coll]
  (->> coll
       (clj->js)
       (.permutation combinatorics)
       (.toArray)
       (js->clj)))

(defn part-one []
  (->>  (permutations (range 5))
        (map #(-> (create-computers %)
                  (update-in [0 :input] conj 0) ;; pass 0 as the second input to computer 1
                  (run-amplifiers)))
        (apply max)
        println))

(part-one)
