(ns dr.meamuri.graph.core
  "Small garph operations lib"
  {:author "Roman Dronov"})

;; v means vertex, w means weight
(def G {:1 [{:v :2 :w 2} {:v :3 :w 3}]
        :2 [{:v :4 :w 4}]
        :3 [{:v :4 :w 1}]
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
