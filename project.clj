(defproject snake "0.1.0-SNAPSHOT"
  :description "The classic Nokia game wirtten in Clojure."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [quil "2.7.1"]]
  :main snake.core
  :aot :all)
