(ns dr.meamuri.graph.core
  "Small garph operations lib"
  {:author "Roman Dronov"})

;; v means vertex, w means weight
(def G {:1 [{:v :2 :w 2} {:v :3 :w 3}]
        :2 [{:v 4 :w 4}]
        :3 [{:v 4 :w 1}]
        :4 []})

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
            weight (get paths v ##Inf)
            [u* m*] (if (< weight m)
                      [v weight]
                      [u m])]
        (recur
         (pop frontier*)
         u*
         m*)))))

(defn ^:private compute-next-paths
  [graph frontier u path]
  (loop [frontier* frontier
         path* path]
    (if (empty? frontier*)
      path*
      (let [v (peek frontier*)
            left (get path* u ##Inf)
            right (or (->> graph
                           v
                           (filter #(= (:v %) u))
                           first
                           :w) ##Inf)
            curr (get path* v ##Inf)
            p (min curr (+ left right))]
        (recur
         (pop frontier*)
         (assoc path* v p))))))

(defn shortest-path
  [graph source destination]
  (let [frontier* (-> graph
                      (dissoc source)
                      keys
                      vec)
        from-source-paths (->> graph
                               source
                               (map (fn [e] {(:v e) (:w e)}))
                               vec)
        paths (apply merge {source 0} from-source-paths)]
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
           (assoc path u p*)
           fr*))))))
