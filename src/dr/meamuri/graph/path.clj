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

(defn ^:private compute-next-paths
  [graph verticles-for-discovery shortest-path-vertex current-path]
  (loop [frontier verticles-for-discovery
         path current-path]
    (if (empty? frontier)
      path
      (let [vertex (peek frontier)
            current-weight (get-in path [shortest-path-vertex :w] ##Inf)
            peeked-vertex-weight (or (->> graph
                                          shortest-path-vertex
                                          (filter #(= (:v %) vertex))
                                          first
                                          :w)
                                     ##Inf)
            expected-update (+ current-weight peeked-vertex-weight)
            curr (get-in path [vertex :w] ##Inf)
            p (min curr expected-update)
            with-uodated-previous (if (not= curr p)
                                    (assoc-in path [vertex :p] shortest-path-vertex)
                                    path)]
        (recur
         (pop frontier)
         (-> with-uodated-previous
             (assoc-in [vertex :w] p)))))))

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
