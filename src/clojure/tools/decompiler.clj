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
            [clojure.tools.decompiler.compact :as cmp]
            [fipp.clojure :as fp]))

(defn absolute-filename [filename]
  (-> filename
      (io/file)
      (.getAbsolutePath)))

(defn classfile->source [filename bc-for]
  (-> filename
      (absolute-filename)
      (bc/analyze-classfile)
      (ast/bc->ast {:bc-for bc-for})
      (sa/ast->sugared-ast)
      (src/ast->clj)
      (cmp/macrocompact)))

(defn cname [c input-path]
  (-> c
      (subs 0 (- (count c) (count ".class")))
      (subs (inc (count input-path)))))

(defn classfile? [^String f]
  (.endsWith f ".class"))

(defn bc-for [classname->path]
  (fn [classname]
    (some-> classname
            (s/replace "." "/")
            classname->path
            absolute-filename
            bc/analyze-classfile)))

(defn decompile [{:keys [input-path output-path]}]
  (let [files (filter classfile? (map str (file-seq (io/file input-path))))
        inits (filter (fn [^String i] (.endsWith i "__init.class")) files)
        classname->path (into {} (map (fn [^String classfile]
                                        [(cname classfile input-path) classfile])
                                      files))]

    (doseq [init inits
            :let [cname (cname init input-path)
                  ns-name (subs cname 0 (- (count cname) (count "__init")))
                  ns-file (str output-path "/" (s/replace ns-name "." "/") ".clj")]]
      (println (str "Decompiling " init (when output-path (str " to " ns-file))))
      (let [source (classfile->source init (bc-for classname->path))
            pprinted-source (with-out-str (fp/pprint source {:width 100}))]
        (if output-path
          (do (io/make-parents ns-file)
              (spit ns-file pprinted-source))
          (println pprinted-source))))))
