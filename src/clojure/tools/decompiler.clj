(ns clojure.tools.decompiler
  (:require [clojure.tools.decompiler.bc :as bc]
            [clojure.tools.decompiler.ast :as ast]
            [clojure.tools.decompiler.sugar :as sa]
            [clojure.tools.decompiler.source :as src]
            [clojure.tools.decompiler.compact :as cmp]))

(defn classfile->source [filename]
  (-> filename
      (bc/analyze-classfile)
      (ast/bc->ast)
      (sa/ast->sugared-ast)
      (src/ast->clj)
      (cmp/macrocompact)
      :source))

(comment

  (require '[clojure.java.io :as io])

  (-> "test$baz.class"
      (io/resource)
      (.getFile)
      (classfile->source)))
