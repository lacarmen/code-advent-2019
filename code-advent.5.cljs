(ns code-advent.seven.one
  (:require
   [cljs.core :refer [*command-line-args*]]
   [clojure.string :as string]
   [goog.string :as gstring]
   ["fs" :as fs]))

(defn get-input []
  (-> *command-line-args*
      first
      (fs/readFileSync #js {:encoding "UTF-8"})))

(def program
  (mapv js/parseInt (string/split (get-input) #",")))

(def op-lookup
  {1 +
   2 *
   7 (comp #(if (true? %) 1 0) <)
   8 (comp #(if (true? %) 1 0) =)})

(defn ->computer [input]
  {:ip    0
   :mem   program
   :input input})

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
  (if (zero? (modes (dec offset))) ;; dec because no mode for instruction
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
  (println (normalize-param computer modes 1))
  (-> computer
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
      4 (recur (output-op computer opcode))
      (5 6) (recur (jump-op computer opcode)))))

(defn part-one []
  (run-program (->computer [1])))

(defn part-two []
  (run-program (->computer [5])))

(part-one)
(part-two)
