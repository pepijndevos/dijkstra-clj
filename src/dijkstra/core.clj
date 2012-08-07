(ns dijkstra.core
  (:import java.util.concurrent.PriorityBlockingQueue))

(defn into-graph
  ([graph [src cost dest]]
    (-> graph
      (assoc-in [src  :neighbours dest] cost)
      (assoc-in [dest :neighbours src ] cost)))
  ([graph node & nodes]
    (if nodes
      (recur (into-graph graph node) (first nodes) (next nodes))
      (into-graph graph node))))

(defn closer? [nodes node neighbour-id]
  (let [new-distance (+ (:distance node Integer/MAX_VALUE)
                        (get-in node [:neighbours neighbour-id]))]
    (when (< new-distance
             (get-in nodes [neighbour-id :distance] Integer/MAX_VALUE))
      new-distance)))

(defn explore-child [nodes node neighbour-id]
    (if-let [dist (and (not (get-in nodes [neighbour-id :visited]))
                       (closer? nodes node neighbour-id))]
      (assoc-in nodes [neighbour-id :distance] dist)
      nodes))

(defn explore-children [nodes id]
  (let [f (fn [nodes neighbour-id]
            (explore-child nodes (get nodes id) neighbour-id))
        neighbour-ids (keys (get-in nodes [id :neighbours]))]
    (reduce f nodes neighbour-ids)))

(defn push-neighbours [nodes nearest id]
  (doseq [id (keys (get-in nodes [id :neighbours]))]
    (.put nearest [id (get-in nodes [id :distance])]))
  nodes)

(defn dijkstra* [nodes nearest]
  (loop [[id distance] (.poll nearest)]
    (when id
      (if (:visited (get nodes id))
        (recur (.poll nearest))
        (cons
          [id (get nodes id)]
          (lazy-seq
            (-> nodes
              (assoc-in [id :visited] true)
              (explore-children id)
              (push-neighbours nearest id)
              (dijkstra* nearest))))))))

(defn dijkstra [nodes start]
  (let [cmp (fn [[_ x] [_ y]] (compare x y))
        q (PriorityBlockingQueue. 10 cmp)
        nodes (assoc-in nodes [start :distance] 0)]
    (.put q [start 0])
    (dijkstra* nodes q)))

;(use 'clojure.pprint 'dijkstra.parser)
;(def g (get-nodes))
;(pprint g)
;(pprint (take 1 (dijkstra g 1)))
;(time (dorun (dijkstra g 1)))
;(dorun (map (juxt first (comp :distance peek)) (dijkstra g 1)))
;(first (drop-while #(not= (first %) 7) (dijkstra g 1)))
;(def ks [7 37 59 82 99 115 133 165 188 197])
;(def ks [59])
;(def dists [2599 2610 2947 2052 2367 2399 2029 2442 2505 3068])
;(time (doall (map (comp :distance (into {} (dijkstra g 1))) ks)))
