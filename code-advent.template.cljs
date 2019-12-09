(ns code-advent.one
  (:require
   [cljs.core :refer [*command-line-args*]]
   [clojure.string :as string]
   ["fs" :as fs]))

(defn get-input []
  (-> *command-line-args*
      (first)
      (fs/readFileSync #js {:encoding "UTF-8"})))

(defn part-one [])
