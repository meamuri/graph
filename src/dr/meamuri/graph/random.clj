(ns dr.meamuri.graph.random
  "Make Random Graph"
  {:author "Roman Dronov"})

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
