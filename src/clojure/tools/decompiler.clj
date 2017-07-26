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
            [clojure.walk :as w]
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

(defn ->pprint-str [source]
  (with-out-str (fp/pprint source {:width 100})))

(defn elide-ns [source]
  (let [!aliases (atom {})
        w (fn [f s] (w/walk f identity s))
        f (fn f [x]
            (doto (if (seq? x)
                    (if (= 'quote (first x))
                      x
                      (do
                        (when (and (= 'clojure.core/in-ns (first x))
                                   (seq? (second x))
                                   (= 'quote (-> x second first)))
                          (let [ns (-> x second second)
                                ns (if (list? ns) (second ns) ns)]
                            (swap! !aliases assoc (name ns) "")))
                        (when (= 'clojure.core/refer-clojure (first x))
                          (let [ex (->> x (drop-while (complement #{:exclude})) second)]
                            (when (vector? ex)
                              (swap! !aliases assoc "clojure.core" (->> ex (map (comp str second)) (into #{}))))
                            (when (list? ex)
                              ;; assumes withMeta
                              (swap! !aliases assoc "clojure.core" (->> ex second rest (map (comp str second)) (into #{}))))))
                        (when (= 'clojure.core/require (first x))
                          (doseq [req (rest x)
                                  :when (vector? req)]
                            (when-let [alias (some->> req (drop-while (complement #{:as})) second second name)]
                              (let [ns (some-> req first second name)]
                                (when-not (= ns "clojure.core")
                                  (swap! !aliases assoc ns alias))))))
                        (w f x)))
                    (if (symbol? x)
                      (let [aliases @!aliases]
                        (if (= "clojure.core" (namespace x))
                          (if (contains? (get aliases "clojure.core") (name x))
                            x
                            (symbol (name x)))
                          (if-let [alias (get aliases (namespace x))]
                            (if (= "" alias)
                              (symbol (name x))
                              (symbol (str alias "/" (name x))))
                            x)))
                      (w f x)))))]
    (w f source)))

(defn classfile->source [filename bc-for]
  (-> filename
      (absolute-filename)
      (bc/analyze-classfile)
      (ast/bc->ast {:bc-for bc-for})
      (sa/ast->sugared-ast)
      (src/ast->clj)
      (cmp/macrocompact)
      (->> (keep identity)
           (remove #(and (seq? %) (= 'var (first %)))))
      (elide-ns)
      (->pprint-str)))

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

(defn decompile [{:keys [input-path output-path classes]}]
  (let [files (filter classfile? (map str (file-seq (io/file input-path))))
        classname->path (into {} (map (fn [^String classfile]
                                        [(cname classfile input-path) classfile])
                                      files))
        inits (if classes
                (mapv classname->path classes)
                (filter (fn [^String i] (.endsWith i "__init.class")) files))]

    (doseq [init inits
            :let [cname (cname init input-path)
                  ns-name (subs cname 0 (- (count cname) (count "__init")))
                  ns-file (str output-path "/" (s/replace ns-name "." "/") ".clj")]]
      (println (str "Decompiling " init (when output-path (str " to " ns-file))))
      (let [source (classfile->source init (bc-for classname->path))]
        (if output-path
          (do (io/make-parents ns-file)
              (spit ns-file source))
          (println source))))))
