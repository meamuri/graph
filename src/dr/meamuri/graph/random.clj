(ns dr.meamuri.graph.random
  "Make Random Graph"
  {:author "Roman Dronov"})

(defn ^:private randomize-additional-links
  [g s verticles]
  g)

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
  (let [verticles (->> (range)
                       (take n)
                       (map #(+ 1 %))
                       (map #(-> % str keyword))
                       vec)
        randomized (shuffle verticles)
        graph (->> verticles
                   (map (fn [e] [e []]))
                   (into {}))]
    (loop [sparseness s
           g graph
           connected [(first randomized)]
           unlinked (-> randomized rest vec)]
      (if (empty? unlinked)
        (randomize-additional-links g sparseness verticles)
        (let [from (rand-nth connected)
              to (rand-nth unlinked)
              weight (rand-int 100)
              node {:v to :w weight}]
          (recur
           (dec 1)
           (update g from #(conj % node)) ;; Update randomly peeked v
           (conj connected to)
           (->> unlinked (remove #(= % to)) vec)))))))
