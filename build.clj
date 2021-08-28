(ns build
  (:refer-clojure :exclude [compile])
  (:require [clojure.tools.build.api :as b]))

(defn compile
  [_]
  (b/javac {:src-dirs ["java"]
            :class-dir "target/classes"}))

(defn jar
  [_]
  (b/copy-dir {:target-dir "target/classes"
               :src-dirs ["src"]})
  (compile {})
  (b/jar {:class-dir "target/classes"
          :jar-file "tools.decompiler.jar"
          :manifest {"Premain-Class" "clojure.tools.decompiler.RetrieveClasses"}}))
