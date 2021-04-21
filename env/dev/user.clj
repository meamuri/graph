(ns user
  (:require [dr.meamuri.graph.path :refer [shortest-path]]
            [dr.meamuri.graph.random :refer [make-graph]]
            [dr.meamuri.graph.ops :refer [eccentricity diameter radius]]))

;;
;;
;;       \
;;      / \
;;     /   \
;;    /     \
;;   |       \
;;   | [5]   |
;;    /   \  |
;;   |     [4]
;;   |    /
;;   | [3]
;;    /
;; [1]
;;    \
;;     [2]
;;
(def G
  {:1 [{:v :2 :w 2} {:v :3 :w 7} {:v :5 :w 3} {:v :4 :w 7}]
   :2 []
   :3 [{:v :4 :w 1}]
   :4 []
   :5 [{:v :4 :w 2}]})

(comment
  ((fn []
     (-> G
         (shortest-path :1 :4))))
  ((fn []
     (-> (make-graph 5 7)
         (shortest-path :1 :4))))

  (#(let [g (make-graph 4 7)
          e (eccentricity g :1)]
      (println (str "Graph is " g))
      (println (str "Eccentricity for graph is " e))))

  (#(let [g (make-graph 5 12)
          d (diameter g)]
      (println (str "Graph is " g))
      (println (str "Diameter for graph is " d))))

  (#(let [g (make-graph 5 10)
          r (radius g)]
      (println (str "Graph is " g))
      (println (str "Radius for graph is " r)))))
