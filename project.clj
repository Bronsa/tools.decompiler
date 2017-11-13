(defproject bronsa/tools.decompiler "0.1.0-alpha1"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :java-source-paths ["java"]
  :manifest {"Premain-Class" "clojure.tools.decompiler.RetrieveClasses"}
  :dependencies [[org.clojure/clojure "1.9.0-RC1"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [org.apache.bcel/bcel "6.1"]
                 [fipp "0.6.12"]])
