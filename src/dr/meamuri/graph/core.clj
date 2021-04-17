(ns dr.meamuri.graph.core
  "Small garph operations lib"
  {:author "Roman Dronov"})

(def G {:1 ['(:2 2) '(:3 4)]
        :2 ['(:4 5)]
        :3 ['(:4 2)]
        :4 []})

(defn v-neighbors
  [g v]
  (->> (g v)
       (map #(first %))
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

(defn generate
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
