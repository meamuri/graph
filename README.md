# Graph Traversal

This link has a simple explanation of a simple Graph definition "language" and implementations of depth/breadth graph search algorithms.

http://hueypetersen.com/posts/2013/06/25/graph-traversal-with-clojure/

The code is reproduced here for readability:

```clojure

(def G {:1 [:2 :3],
        :2 [:4],
        :3 [:4],
        :4 [] })

(defn traverse-graph-dfs [g s]
  (loop [vertices [] explored #{s} frontier [s]]
    (if (empty? frontier)
      vertices
      (let [v (peek frontier)
            neighbors (g v)]
        (recur
          (conj vertices v)
          (into explored neighbors)
          (into (pop frontier) (remove explored neighbors)))))))

(defn seq-graph-dfs [g s]
  ((fn rec-dfs [explored frontier]
     (lazy-seq
       (if (empty? frontier)
         nil
         (let [v (peek frontier)
               neighbors (g v)]
           (cons v (rec-dfs
                     (into explored neighbors)
                     (into (pop frontier) (remove explored neighbors))))))))
   #{s} [s]))

(defn seq-graph-bfs [g s]
  ((fn rec-bfs [explored frontier]
     (lazy-seq
       (if (empty? frontier)
         nil
         (let [v (peek frontier)
               neighbors (g v)]
           (cons v (rec-bfs
                     (into explored neighbors)
                     (into (pop frontier) (remove explored neighbors))))))))
   #{s} (conj (clojure.lang.PersistentQueue/EMPTY) s)))

(traverse-graph-dfs G :1) ; => [:1 :3 :4 :2]
(seq-graph-dfs G :1) ; => (:1 :3 :4 :2)
(seq-graph-bfs G :1) ; => (:1 :2 :3 :4)
```

The author then simplifies it by recognising that only the initial data structure for holding the nodes traversed is different between the depth and breadth first implementations.

He then abstacts that out and the result is:

```clojure
  ((fn rec-seq [explored frontier]
     (lazy-seq
       (if (empty? frontier)
         nil
         (let [v (peek frontier)
               neighbors (g v)]
           (cons v (rec-seq
                     (into explored neighbors)
                     (into (pop frontier) (remove explored neighbors))))))))
   #{s} (conj d s)))

(def seq-graph-dfs (partial seq-graph []))
(def seq-graph-bfs (partial seq-graph (clojure.lang.PersistentQueue/EMPTY)))

(seq-graph-dfs G :1) ; => (:1 :3 :4 :2)
(seq-graph-bfs G :1) ; => (:1 :2 :3 :4)
```

# Questions

## 1. Extend the graph definition to include a weight between graph edges

For example:

```clojure
(def G {:1 [(:2 1) (:3 2)],
        :2 [(:4 4)],
        :3 [(:4 2)],
        :4 [] })
```

I've converted the items of the array into tuples with the vertex name and the weight -- as an integer -- for the edge weight from the start to end vertex.

You can choose something similar or extend it to something you prefer

## 2. Write an algorithm to randomly generate a simple directed graph using your answer from #1

Such that

```
Input:
    N - size of generated graph
    S - sparseness (number of directed edges actually; from N-1 (inclusive) to N(N-1) (inclusive))
Output:
    simple connected graph G(n,s) with N vertices and S edges
```

Please ensure that your graph is connected, otherwise you won't be able to complete the following questions.

## 3. Write an implementation of Dijkstra's algorithm that traverses your graph and outputs the shortest path between any 2 randomly selected vertices.

I should be able to write something like this for example.

```clojure
(def random-graph (make-graph 10 10))
(shortest-path random-graph (first (keys random-graph)) (last (keys random-graph)) ; => list of nodes which is the shortest path by edge weight between the 2 nodes, or no path if one does not exist.
```

## 4. Write a suite of functions to calculate distance properties for your graph.

Now that you have implemented Dijkstra's algorithm you should be able to calculate the eccentricity of any vertex in your graph, and in turn the radius and diameter of your graph.

Please re-acquaint yourself with graph distance properties https://en.wikipedia.org/wiki/Distance_(graph_theory),

* The eccentricity of a vertex v is defined as the greatest distance between v and any other vertex.
* The radius of a graph is the minimum eccentricity of any vertex in a graph.
* The diameter of a graph is the maximum eccentricity of any vertex in a graph.

I should be able to write something like this:

(def random-graph (make-graph 10 10))

(eccentricity random-graph (first (keys random-graph))) ; => number expressing eccentricity for `first` vertex in random-graph

```clojure
(radius random-graph) ; => minimal eccentricity
(diameter random-graph) ; => maximal eccentricity
```

# Lein

Please make sure I can run your code easily by using lein repl and running the functions above in the REPL.

# Java

If you've written this test in Java, please provide, a simple cmd-line interface that accepts as input N size and S sparseness, for example:

```
graph -N 5 -S 10
```

it would then print out the randomly generated graph

```clojure
{ :1 [(:2 1) (:3 2)],
  :2 [(:4 4)],
  :3 [(:4 2)],
  :4 [] }
```

and also print:

* radius
* diameter

of the randomly generated graph.

In addition, it should also:

* randomly select 2 nodes and print the shortest path distance between them,
* compute eccentricity of a random node

We'll also use JShell to interactively run some of your functions, please ensure that you can connect to your answer via JShell.
