;;   Copyright (c) Nicola Mometto & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.decompiler.compact
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.unifier :as u]
            [clojure.walk :as w]))

;; Adapted from kibit
(defn macrocompact-step [expr rules]
  (let [alts (l/run* [q]
               (l/fresh [pat subst]
                 (l/membero [pat subst] rules)
                 (l/project [pat subst]
                            (l/all (pat expr)
                                   (subst q)))))]
    (if (empty? alts) expr (first alts))))

(defn compile-rule [rule]
  `(let [[pat# alt#] (u/prep '~rule)]
     [(fn [expr#] (l/== expr# pat#))
      (fn [sbst#] (l/== sbst# alt#))]))

(defn raw-rule? [rule]
  (not (vector? rule)))

(defmacro defrules [name & rules]
  `(def ~name
     ~(->> (for [rule rules]
             (if (raw-rule? rule)
               rule
               (compile-rule rule)))
           (into []))))

(defrules rules
  [(let* ?binds . ?body) (clojure.core/let ?binds . ?body)]
  [(if ?test (do . ?then)) (clojure.core/when ?test . ?then)]
  [(if ?test ?then nil) (clojure.core/when ?test ?then)]

  [(clojure.core/let [?x ?y]
     (clojure.core/when ?x
       (clojure.core/let [?z ?x] . ?body)))
   (clojure.core/when-let [?z ?y] . ?body)]

  (let [?argv1 (l/lvar)
        ?argv2 (l/lvar)
        ?body (l/lvar)]
    [#(l/== % (list `let ?argv1 (l/llist `let ?argv2 ?body)))
     #(l/project [?argv1 ?argv2 ?body]
                 (l/== % `(let [~@?argv1 ~@?argv2] ~@?body)))]))

(defn macrocompact [source]
  (->> source
       (iterate (partial w/prewalk #(macrocompact-step % rules)))
       (partition 2 1)
       (drop-while #(apply not= %))
       (ffirst)))
