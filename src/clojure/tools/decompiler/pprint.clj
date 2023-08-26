;;   Copyright (c) Nicola Mometto & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.decompiler.pprint
  (:require [clojure.walk :as w]
            [fipp.clojure :as fp]))

(defn ->pprint-str [source]
  (with-out-str (fp/pprint source {:width 100})))

(defn elide-ns [source]
  (let [!aliases (atom {})
        w (fn [f s] (w/walk f identity s))
        f (fn f [x]

            (cond
              (seq? x)
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
                    (let [ex (->> x (drop-while (complement #{:exclude})) second)
                          ex (if (vector? ex) ex (rest ex))]
                      (swap! !aliases assoc "clojure.core" (->> ex (map (comp str second)) (into #{})))))

                  (when (= 'clojure.core/import (first x))
                    (doseq [[_ k] (rest x)]
                      (swap! !aliases assoc (str k)
                             (clojure.string/replace (str k) #".*\.([^.]+)" "$1"))))

                  (when (= 'clojure.core/require (first x))
                    (doseq [req (rest x)
                            :when (vector? req)]
                      (when-let [alias (some->> req (drop-while (complement #{:as})) second second name)]
                        (let [ns (some-> req first second name)]
                          (when-not (= ns "clojure.core")
                            (swap! !aliases assoc ns alias))))))
                  (w f x)))

              (symbol? x)
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

              :else
              (w f x)))]
    (w f source)))

(defn pprint [source]
  (-> source
      (elide-ns)
      (->> (keep identity)
           (remove #(and (seq? %) (= 'var (first %)))))
      (->pprint-str)))
