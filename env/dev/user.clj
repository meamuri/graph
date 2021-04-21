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

  (#(-> (make-graph 3 4)
        (eccentricity :1)))

  (#(-> (make-graph 3 4)
        diameter))

  (#(-> (make-graph 3 4)
        radius)))
