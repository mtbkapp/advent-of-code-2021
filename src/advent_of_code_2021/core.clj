(ns advent-of-code-2021.core
  (:require [nextjournal.clerk :as clerk]))


; start clerk when the repl starts
#_(clerk/serve! {:browse? true :watch-paths ["src"]})

(comment
  (require '[nextjournal.clerk.view :as view])

  (spit "day01.html"
        (view/doc->static-html
          (nextjournal.clerk/eval-file "src/advent_of_code_2021/day01.clj") 
          ))

  (spit "day07.html"
        (view/doc->static-html
          (nextjournal.clerk/eval-file "src/advent_of_code_2021/day07.clj") 
          ))

  (spit "list.html"
        (view/doc->static-html
          (nextjournal.clerk/eval-file "src/advent_of_code_2021/list.clj") 
          ))

  (clojure.pprint/pprint
    (->> (file-seq (clojure.java.io/file "./src/advent_of_code_2021"))
         (filter #(re-seq #"day\d+{2}.clj" (.getName %)))
         (sort-by #(.getName %))
         )
    
    )
  )
