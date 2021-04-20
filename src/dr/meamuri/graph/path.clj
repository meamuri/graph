(ns dr.meamuri.graph.path
  "Connected graphs traversing for paths searching")

(defn ^:private find-nearest-vertex
  [vertices-for-discovery path]
  (loop [frontier vertices-for-discovery
         nearest-vertex -1
         nearest-weight ##Inf]
    (if (empty? frontier)
      nearest-vertex
      (let [vertex (peek frontier)
            weight (get-in path [vertex :w] ##Inf)
            [v w] (if (or (= -1 nearest-vertex) (< weight nearest-weight))
                    ;; updated vertex and weight
                    [vertex weight]
                    ;; the same vertex and wight
                    [nearest-vertex nearest-weight])]
        (recur
         (pop frontier)
         v
         w)))))

(defn path-node-update
  [w* node]
  (fn [e]
    (-> e
        (assoc :w w*)
        (assoc :p node))))

(defn ^:private compute-next-paths
  [graph frontier u path]
  (loop [frontier* frontier
         path* path]
    (if (empty? frontier*)
      path*
      (let [v (peek frontier*)
            left (get-in path* [u :w] ##Inf)
            right (or (->> graph
                           u
                           (filter #(= (:v %) v))
                           first
                           :w) ##Inf)
            curr (get-in path* [v :w] ##Inf)
            p (min curr (+ left right))]
        (recur
         (pop frontier*)
         (update path* v (path-node-update p u)))))))

(defn ^:private dijkstra
  [graph source]
  (let [initial-frontier (-> graph
                             (dissoc source)
                             keys
                             vec)
        edges-from-source-vertex (->> graph
                                      source
                                      (map (fn [e] {(:v e) {:w (:w e)
                                                            :p source}}))
                                      vec)
        source-path {source {:w 0
                             :p nil}}
        initial-path (apply merge source-path edges-from-source-vertex)]
    (loop [path initial-path
           frontier initial-frontier]
      (if (empty? frontier)
        path
        (let [nearest-vertex (find-nearest-vertex frontier path)
              frontier* (->> frontier
                             (remove #(= % nearest-vertex))
                             vec)
              path* (compute-next-paths graph frontier* nearest-vertex path)]
          (recur
           path*
           frontier*))))))

(defn ^:private fold-path
  [paths destination]
  (loop [shortest-path '()
         previous-vertex destination]
    (if (= previous-vertex nil)
      shortest-path
      (recur
       (conj shortest-path previous-vertex)
       (:p (previous-vertex paths))))))

(defn shortest-path
  "An implementation of Dijkstra's algorithm 
   that traverses graph and outputs the shortest path
   between any 2 randomly selected vertices."
  [graph source destination]
  (-> graph
      (dijkstra source)
      (fold-path destination)))
