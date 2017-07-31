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
            [clojure.tools.decompiler.pprint :as pp]))

(defn absolute-filename [filename]
  (-> filename
      (io/file)
      (.getAbsolutePath)))

(defn class->source [classfile-or-classname bc-for]
  (-> classfile-or-classname
      (bc/analyze-class)
      (ast/bc->ast {:bc-for bc-for})
      (sa/ast->sugared-ast)
      (src/ast->clj)
      (cmp/macrocompact)
      (->> (keep identity)
           (remove #(and (seq? %) (= 'var (first %)))))
      (pp/pprint)))

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
            bc/analyze-class)))

(defn decompile-classfiles [{:keys [input-path output-path classes]}]
  (let [files (filter classfile? (map str (file-seq (io/file input-path))))
        classname->path (into {} (map (fn [^String classfile]
                                        [(cname classfile input-path) classfile])
                                      files))
        inits (if classes
                (mapv classname->path classes)
                (filter (fn [^String i] (.endsWith i "__init.class")) files))
        *log* (agent nil)]

    (dorun
     (pmap
      (fn [init]
        (let [cname (cname init input-path)
              ns-name (subs cname 0 (- (count cname) (count "__init")))
              ns-file (str output-path "/" (s/replace ns-name "." "/") ".clj")]
          (send-off *log* (fn [& _] (println (str "Decompiling " init (when output-path (str " to " ns-file))))))
          (let [source (class->source (absolute-filename init) (bc-for classname->path))]
            (if output-path
              (do (io/make-parents ns-file)
                  (spit ns-file source))
              (println source)))))
      inits))
    nil))

(defn decompile-classes [{:keys [classes]}]
  (let [*log* (agent nil)]
    (dorun
     (pmap
      (fn [class]
        (send-off *log* (fn [& _] (println "Decompiling" class)))
        (let [source (class->source (s/replace class "." "/")
                                    (fn [^String classname]
                                      (when-not (.startsWith classname "clojure.lang.")
                                        (bc/analyze-class (s/replace classname "." "/")))))]
          (println source)))
      classes))
    nil))
