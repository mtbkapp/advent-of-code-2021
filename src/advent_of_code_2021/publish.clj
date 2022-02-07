(ns advent-of-code-2021.publish
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk.view :as view]
            [clojure.java.io :as io]
            [clojure.string :as string]))


(def src-dir (io/file "./src/advent_of_code_2021"))
(def day-file-pattern #"day(\d+{2}).clj")
(def publish-dir (io/file "./docs"))


(defn get-days
  []
  (->> (file-seq src-dir)
       (filter #(re-seq day-file-pattern (.getName %)))
       (remove #(.isHidden %))
       (sort-by #(.getName %))
       (map (fn [file]
              {:title (->> (.getName file)
                           (re-seq day-file-pattern)
                           first
                           second
                           (str "Day "))
               :clj-file file
               :html-file (io/file publish-dir (str (.getName file) ".html"))}))))


(defn publish-file
  [{:keys [clj-file html-file]}]
  (->> (.getCanonicalPath clj-file)
       clerk/eval-file
       view/doc->static-html
       (spit (.getCanonicalPath html-file))))


(defn build-index
  [days]
  (string/join \newline
               (into ["; # [Advent of Code 2021](https://adventofcode.com/2021)" 
                      "; Solutions by [Jason Kapp](http://github.com/mtbkapp)"
                      ""
                      "; Everything was rendered to HTML with [Clerk](https://github.com/nextjournal/clerk)."
                      ""]
                     (map (fn [{:keys [title html-file]}]
                            (str "; * [" title "](" (.getName html-file) ")")))
                     days)))


(defn publish-index
  [days]
  (let [clj-file (io/file src-dir "index.clj")
        html-file (io/file publish-dir "index.html")]
    (spit clj-file (build-index days))
    (publish-file {:clj-file clj-file :html-file html-file})
    (.delete clj-file)))


(defn publish
  []
  (.mkdir publish-dir)
  (let [days (get-days)]
    (publish-index days)
    (doseq [d days]
      (publish-file d))))


(defn -main
  [& args]
  (publish))
