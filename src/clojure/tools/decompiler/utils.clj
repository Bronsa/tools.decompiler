(ns clojure.tools.decompiler.utils
  (:import clojure.lang.Compiler))

(defn demunge [s]
  (symbol (Compiler/demunge s)))

(defn find-methods [methods matches]
  (for [method methods
        :when (= matches (select-keys method (keys matches)))]
    method))

(defn find-method [methods matches]
  ;; assert just 1
  (first (find-methods methods matches)))
