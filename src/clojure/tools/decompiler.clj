;;   Copyright (c) Nicola Mometto & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.decompiler
  (:require [clojure.tools.decompiler.bc :as bc]
            [clojure.tools.decompiler.ast :as ast]
            [clojure.tools.decompiler.sugar :as sa]
            [clojure.tools.decompiler.link :as l]
            [clojure.tools.decompiler.source :as src]
            [clojure.tools.decompiler.compact :as cmp]))

(defn classfile->source [filename]
  (-> filename
      (bc/analyze-classfile)
      (ast/bc->ast)
      (l/link)
      (sa/ast->sugared-ast)
      (src/ast->clj)
      (cmp/macrocompact)
      :source))

(comment

  (require '[clojure.java.io :as io])

  (doseq [f (rest (file-seq (io/file "resources/")))
          :when (.endsWith (str f) ".class")]
    (println (str f))
    (prn (classfile->source (str f)))
    (println "\n"))

  (-> "test$baz.class"
      (io/resource)
      (.getFile)
      (classfile->source)))
