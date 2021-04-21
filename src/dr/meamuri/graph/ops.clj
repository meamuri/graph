(ns dr.meamuri.graph.ops
  "A suite of functions to calculate distance properties for the graph"
  {:author "Roman Dronov"}
  (:require [dr.meamuri.graph.path :refer [shortest-path]]))

(defn eccentricity
  "The eccentricity of a vertex v is defined as the greatest distance between v and any other vertex."
  [graph source]
  (let [traverser
        (fn rec-search
          [explored frontier acc]
          (if (empty? frontier)
            acc
            (let [vertex (peek frontier)
                  neighbors (->> graph
                                 vertex
                                 (map #(:v %))
                                 vec)
                  new-acc (-> graph
                              (shortest-path source vertex)
                              count
                              dec)]
              (rec-search
               (into explored neighbors)
               (into (pop frontier) (remove explored neighbors))
               (max new-acc acc)))))
        d (clojure.lang.PersistentQueue/EMPTY)]
    (traverser #{source} (conj d source) 0)))

(defn radius
  "The radius of a graph is the minimum eccentricity of any vertex in a graph."
  [graph]
  (let [traverser
        (fn rec-search
          [explored frontier acc]
          (if (empty? frontier)
            acc
            (let [vertex (peek frontier)
                  neighbors (->> graph
                                 vertex
                                 (map #(:v %))
                                 vec)
                  new-min (eccentricity graph vertex)]
              (rec-search
               (into explored neighbors)
               (into (pop frontier) (remove explored neighbors))
               (min new-min acc)))))
        s (-> graph keys first)
        d (clojure.lang.PersistentQueue/EMPTY)]
    (traverser #{s} (conj d s) ##Inf)))

(defn diameter
  "The diameter of a graph is the maximum eccentricity of any vertex in a graph."
  [graph]
  (let [traverser
        (fn rec-search
          [explored frontier acc]
          (if (empty? frontier)
            acc
            (let [vertex (peek frontier)
                  neighbors (->> graph
                                 vertex
                                 (map #(:v %))
                                 vec)
                  new-max (eccentricity graph vertex)]
              (rec-search
               (into explored neighbors)
               (into (pop frontier) (remove explored neighbors))
               (max new-max acc)))))
        s (-> graph keys first)
        d (clojure.lang.PersistentQueue/EMPTY)]
    (traverser #{s} (conj d s) 0)))
