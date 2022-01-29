(defproject advent-of-code-2021 "0.1.0-SNAPSHOT"
  :description "My Advent of Code 2021 Solutions"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [io.github.nextjournal/clerk "0.2.214"]
                 [ubergraph "0.8.2"]]
  :repl-options {:init-ns advent-of-code-2021.core}
  :aliases {"publish" ["run" "-m" "advent-of-code-2021.publish"]})
