(ns dijkstra.parser
  (:use [clojure.pprint])
  (:import java.io.File)
  (:require [clojure.string :as str]))

(defn parse-edges [edges node]
  (if (empty? edges)
    node
    (let [edge (str/split (first edges) #",")
          new-neighbours (assoc-in node [:neighbours (Integer/parseInt (first edge))]
                                   (Integer/parseInt (second edge)))]
      (recur (rest edges) new-neighbours))))

(defn parse-lines [lines nodes]
  (if (empty? lines)
    nodes
    (let [line (str/split (str/replace (first lines) "\r" "") #"\t")
          id (Integer/parseInt (first line))
          node (parse-edges (rest line) {})]
      (parse-lines (rest lines) (assoc nodes id node)))))

(defn get-nodes []
  (let [data-str (slurp (File. "data.txt"))
        lines (str/split data-str #"\n")]
    (parse-lines lines {})))

;(pprint (get-nodes))
