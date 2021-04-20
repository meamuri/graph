(ns dr.meamuri.graph.random
  "Make Random Graph"
  {:author "Roman Dronov"}
  (:require [clojure.set :refer [difference]]))

(defn ^:private nodes-to-rels
  [nodes]
  (->> nodes
       (map (fn [node]
              (:v node)))
       set))

(defn ^:private to-sets
  [verticles]
  (fn [[source-node nodes]]
    (let [rels (nodes-to-rels nodes)]
      [source-node (difference verticles (conj rels source-node))])))

(defn ^:private randomize-additional-links
  [graph sparseness verticles]
  (let [sets-computation (to-sets verticles)]
    (loop [g graph
           s sparseness]
      (if (= s 0)
        g
        (let [uncompleted (->> graph
                               (map sets-computation)
                               (filter #(-> % second seq))
                               (into {}))
              from (rand-nth (keys uncompleted))
              to (-> uncompleted from vec rand-nth)
              weight (rand-int 100)
              node {:v to
                    :w weight}
              g* (update g from #(conj % node))]
          (recur
           g*
           (dec s)))))))

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
        (randomize-additional-links g sparseness (set verticles))
        (let [from (rand-nth connected)
              to (rand-nth unlinked)
              weight (rand-int 100)
              node {:v to :w weight}]
          (recur
           (dec 1)
           (update g from #(conj % node)) ;; Update randomly peeked v
           (conj connected to)
           (->> unlinked (remove #(= % to)) vec)))))))
