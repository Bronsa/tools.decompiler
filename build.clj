(ns build
  (:refer-clojure :exclude [compile])
  (:require [clojure.tools.build.api :as b]))

(defn compile
  [_]
  (b/javac {:src-dirs ["java"]
            :class-dir "target/classes"}))
