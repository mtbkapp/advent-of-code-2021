(ns advent-of-code-2021.core
  (:require [nextjournal.clerk :as clerk]))


; start clerk when the repl starts
(clerk/serve! {:browse? true})
(clerk/serve! {:watch-paths ["src"]})
