(require '[clojure.edn :as edn] '[clojure.string :as str])
(let [d (edn/read-string (slurp (first *command-line-args*)))
      q #(str "\"" (str/replace (str %) "\"" "\\\"") "\"")
      ts-colour {:social "#2f6f9f" :fast "#2f6f9f" :medium "#5b8a3a" :slow "#9a6b1f" :glacial "#7a4a8f"}
      node (fn [{:keys [id name timescale constraint]} shape fill]
             (str "  " (q (clojure.core/name id)) " [label=" (q (str/replace name ": " "\\n"))
                  " shape=" shape " style=\"filled" (when constraint ",bold") "\" fillcolor=" (q fill)
                  " color=" (q (ts-colour timescale "#555")) " penwidth=" (if constraint 3 1.5) "];\n"))]
  (println "digraph G {\n  rankdir=TB; bgcolor=\"transparent\"; fontname=\"Helvetica\";\n  node [fontname=\"Helvetica\" fontsize=11]; edge [fontname=\"Helvetica\" fontsize=9 color=\"#666666\" fontcolor=\"#444444\"];")
  (println "  subgraph cluster_in { label=\"inputs\"; color=\"#bbbbbb\";")
  (doseq [p (get-in d [:ports :input])] (print (node p "box" "#eef3f8")))
  (println "  }\n  subgraph cluster_c { label=\"arxana-incubator\"; color=\"#bbbbbb\";")
  (doseq [c (:components d)] (print (node c "box" "#fdf7e7")))
  (println "  }\n  subgraph cluster_out { label=\"outputs\"; color=\"#bbbbbb\";")
  (doseq [p (get-in d [:ports :output])] (print (node p "box" "#eef8ee")))
  (println "  }")
  (doseq [{:keys [from to type]} (:edges d)]
    (println (str "  " (q (name from)) " -> " (q (name to)) " [label=" (q (name type)) "];")))
  (println "}"))
