(ns dr.meamuri.graph.ops
  "A suite of functions to calculate distance properties for the graph"
  {:author "Roman Dronov"}
  (:require [dr.meamuri.graph.path :refer [shortest-path]]))

(defn make-traverser
  [graph source updater picker]
  (fn rec-search
    [explored frontier acc]
    (if (empty? frontier)
      acc
      (let [vertex (peek frontier)
            neighbors (->> graph
                           vertex
                           (map #(:v %))
                           vec)
            new-acc (picker graph source vertex)]
        (rec-search
         (into explored neighbors)
         (into (pop frontier) (remove explored neighbors))
         (updater new-acc acc))))))

(defn eccentricity
  "The eccentricity of a vertex v is defined as the greatest distance between v and any other vertex."
  [graph source]
  (let [picker (fn [g source vertex]
                 (-> g
                     (shortest-path source vertex)
                     count
                     dec))
        traverser (make-traverser graph source max picker)
        d (clojure.lang.PersistentQueue/EMPTY)]
    (traverser #{source} (conj d source) 0)))

(defn radius
  "The radius of a graph is the minimum eccentricity of any vertex in a graph."
  [graph]
  (let [source (-> graph keys first)
        picker (fn [g _ vertex]
                 (eccentricity g vertex))
        updated (fn [acc new-acc]
                  (-> acc
                      (min new-acc)
                      (max 1)))
        traverser (make-traverser graph source updated picker)
        d (clojure.lang.PersistentQueue/EMPTY)]
    (traverser #{source} (conj d source) ##Inf)))

(defn diameter
  "The diameter of a graph is the maximum eccentricity of any vertex in a graph."
  [graph]
  (let [source (-> graph keys first)
        picker (fn [g _ vertex]
                 (eccentricity g vertex))
        traverser (make-traverser graph source max picker)
        d (clojure.lang.PersistentQueue/EMPTY)]
    (traverser #{source} (conj d source) 0)))
