(ns user
  (:require [dr.meamuri.graph.path :refer [shortest-path]]
            [dr.meamuri.graph.random :refer [make-graph]]))

(def Graph {:1 [{:v :2, :w 1} {:v :5, :w 102} {:v :4, :w 12}]
            :2 [{:v :3, :w 100} {:v :6, :w 50}]
            :3 [{:v :4, :w 1}]
            :4 []
            :5 [{:v :4, :w 5}]
            :6 [{:v :3, :w 1}]})

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
         (shortest-path :1 :4)
         str
         println)))
  ((fn []
     (-> (make-graph 5 7)
         (shortest-path :1 :6)
         str
         println))))
