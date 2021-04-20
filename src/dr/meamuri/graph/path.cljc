(ns dr.meamuri.graph.path
  "Connected graphs traversing for paths searching")

(defn ^:private shortest-v
  [frontier paths]
  (loop [frontier* frontier
         u -1
         m ##Inf]
    (if (empty? frontier*)
      u
      (let [v (peek frontier*)
            weight (get-in paths [v :w] ##Inf)
            [u* m*] (if (or (= -1 u) (< weight m))
                      [v weight]
                      [u m])]
        (recur
         (pop frontier*)
         u*
         m*)))))

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
  (let [frontier* (-> graph
                      (dissoc source)
                      keys
                      vec)
        from-source-paths (->> graph
                               source
                               (map (fn [e] {(:v e) {:w (:w e)
                                                     :p source}}))
                               vec)
        source-description {source {:w 0
                                    :p nil}}
        paths (apply merge source-description from-source-paths)]
    (loop [path paths
           frontier frontier*]
      (if (empty? frontier)
        path
        (let [u (shortest-v frontier path)
              fr* (->> frontier
                       (remove #(= % u))
                       vec)
              p* (compute-next-paths graph fr* u path)]
          (recur
           p*
           fr*))))))

(defn ^:private fold-destination
  [paths _source destination]
  (loop [shortest-path []
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
      (fold-destination source destination)
      reverse))
