;;   Copyright (c) Nicola Mometto & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.decompiler
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.tools.decompiler.bc :as bc]
            [clojure.tools.decompiler.ast :as ast]
            [clojure.tools.decompiler.sugar :as sa]
            [clojure.tools.decompiler.source :as src]
            [clojure.tools.decompiler.compact :as cmp]))

(defn classfile->source [filename]
  (-> filename
      (bc/analyze-classfile)
      (ast/bc->ast {:bc-for (fn [cname]
                              ;; WIP
                              (try
                                (some-> cname
                                        (s/replace "." "/")
                                        (str ".class")
                                        (io/resource)
                                        (.getFile)
                                        (bc/analyze-classfile))
                                (catch Exception e)))})
      (sa/ast->sugared-ast)
      (src/ast->clj)
      (cmp/macrocompact)))

(comment

  (require '[clojure.java.io :as io])

  (doseq [f (rest (file-seq (io/file "resources/")))
          :when (.endsWith (str f) ".class")]
    (try (classfile->source (str f))
         (catch Exception e
           (println "FAILED" (str f)))))

  (-> "test$baz.class"
      (io/resource)
      (.getFile)
      (classfile->source)))
