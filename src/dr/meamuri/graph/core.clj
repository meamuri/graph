(ns dr.meamuri.graph.core
  "Small garph operations lib"
  {:author "Roman Dronov"})

;; v means vertex, w means weight
(def G {:1 [{:v :2 :w 2} {:v :3 :w 3}]
        :2 [{:v :4 :w 4}]
        :3 [{:v :4 :w 1}]
        :4 []})

(def test-g
  {:1 [{:v :2, :w 1} {:v :5, :w 102} {:v :4, :w 12}]
   :2 [{:v :3, :w 100} {:v :6, :w 50}]
   :3 [{:v :4, :w 1}]
   :4 []
   :5 [{:v :4, :w 5}]
   :6 [{:v :3, :w 1}]})

(defn v-neighbors
  [g v]
  (->> (g v)
       (map #(:v %))
       vec))

(defn seq-graph
  [d g s]
  ((fn rec-seq [explored frontier]
     (lazy-seq
      (if (empty? frontier)
        nil
        (let [v (peek frontier)
              neighbors (v-neighbors g v)]
          (cons v (rec-seq
                   (into explored neighbors)
                   (into (pop frontier) (remove explored neighbors))))))))
   #{s} (conj d s)))

(def seq-graph-dfs (partial seq-graph []))
(def seq-graph-bfs (partial seq-graph (clojure.lang.PersistentQueue/EMPTY)))

(seq-graph-dfs G :1) ; => (:1 :3 :4 :2)
(seq-graph-bfs G :1) ; => (:1 :2 :3 :4)

;; 2 -- Make Random Graph

(defn make-graph
  "Input:
    N - size of generated graph
    S - sparseness (number of directed edges actually; from N-1 (inclusive) to N(N-1) (inclusive))
  Output:
    simple connected graph G(n,s) with N vertices and S edges"
  [n s]
  (if-not (and (int? n) (> n 0))
    (throw (RuntimeException. "Vertices count should be natural number")))
  (let [min (- n 1)
        max (* n min)]
    (when (or (not (int? s)) (< s min) (> s max))
      (throw (RuntimeException. "Sparseness should be between (n - 1) and n * (n - 1)"))))
  (let [g (->> (range)
               (take n)
               (map #(+ 1 %))
               (map #(-> % str keyword))
               (map (fn [e] [e []]))
               (into {}))]
    g))

;; 3 -- dijkstra's shortest path

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
  [paths source destination]
  (loop [r []
         k destination]
    (if (= source k)
      (conj r source)
      (recur
       (conj r k)
       (:p (k paths))))))

(defn shortest-path
  [graph source destination]
  (-> graph
      (dijkstra source)
      (fold-destination source destination)
      reverse))
