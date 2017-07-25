(defproject tools.decompiler "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :jvm-opts  ["-XX:-OmitStackTraceInFastThrow"]
  :dependencies [[org.clojure/clojure "1.9.0-master-SNAPSHOT"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [org.apache.bcel/bcel "6.0"]
                 [fipp "0.6.9"]])
