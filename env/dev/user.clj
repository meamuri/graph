(ns user
  (:require [dr.meamuri.graph.path :refer [shortest-path]]
            [dr.meamuri.graph.random :refer [make-graph]]
            [dr.meamuri.graph.ops :refer [eccentricity diameter radius]]
            [dr.meamuri.graph.core :refer [seq-graph-dfs seq-graph-bfs]]))

;; v means vertex, w means weight
(def G*
  {:1 [{:v :2 :w 2} {:v :3 :w 3}]
   :2 [{:v :4 :w 4}]
   :3 [{:v :4 :w 1}]
   :4 []})

(def G
  {:1 [{:v :2 :w 2} {:v :3 :w 7} {:v :5 :w 3} {:v :4 :w 7}]
   :2 []
   :3 [{:v :4 :w 1}]
   :4 []
   :5 [{:v :4 :w 2}]})

(def lg
  {:1 [{:v :2 :w 1} {:v :8 :w 1} {:v :7 :w 2}]
   :2 [{:v :3 :w 5} {:v :4 :w 2} {:v :1 :w 4}]
   :3 [{:v :2 :w 1}]
   :4 [{:v :6 :w 17} {:v :5 :w 1}]
   :5 [{:v :6 :w 2}]
   :6 [{:v :3 :w 5}]
   :7 []
   :8 []})

(comment
  (seq-graph-dfs G* :1)
  (seq-graph-bfs G* :1)

  ((fn []
     (-> G
         (shortest-path :1 :4))))
  ((fn []
     (let [g (make-graph 5 7)
           p (shortest-path g :1 :4)]
       (println (str "Graph is " g))
       (println (str "Shortest path between :1 and :4 is " p)))))

  (#(let [g (make-graph 4 7)
          e (eccentricity g :1)
          d (diameter g)
          r (radius g)]
      (println (str "Graph is " g))
      (println (str "Eccentricity for graph is " e))
      (println (str "Diameter for graph is " d))
      (println (str "Radius for graph is " r))))

  (#(eccentricity lg :2)))
