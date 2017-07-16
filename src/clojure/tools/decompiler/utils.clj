(ns clojure.tools.decompiler.utils
  (:require [clojure.string :as s])
  (:import clojure.lang.Compiler
           java.io.Writer))

(defn ungensym [s]
  (s/replace s #"(__[0-9]+)" ""))

(defn demunge [s]
  (symbol (Compiler/demunge s)))

(defn find-methods [methods matches]
  (for [method methods
        :when (= matches (select-keys method (keys matches)))]
    method))

(defn find-method [methods matches]
  ;; assert just 1
  (first (find-methods methods matches)))

(defmethod print-method (Class/forName "[Ljava.lang.Object;") [o w]
  (.write w "#array")
  (.write w " ")
  (print-method (vec o) w))
