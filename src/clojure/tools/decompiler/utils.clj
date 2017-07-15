(ns clojure.tools.decompiler.utils
  (:import clojure.lang.Compiler))

(defn demunge [s]
  (symbol (Compiler/demunge s)))
