;;   Copyright (c) Nicola Mometto & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.decompiler.compact
  (:require [clojure.core.match :as m]
            [clojure.walk :as w]))

(defn register! [sym !occurs]
  (if (contains? @!occurs sym)
    (let [s (gensym (str (name sym) "_"))]
      (swap! !occurs update sym conj s)
      s)
    (do
      (swap! !occurs assoc sym #{})
      sym)))

(defn maybe-guard [sym guards]
  (if-let [sym-guards (get guards sym)]
    (list sym :guard sym-guards)
    sym))

(defn compile-pattern [form guards !occurs]
  (cond

    (seq? form)
    (if (and (= 'quote (first form))
             (symbol? (second form)))
      [form]
      [(list (vec (mapcat #(compile-pattern % guards !occurs) form)) :seq)])

    (vector? form)
    [(vec (mapcat #(compile-pattern % guards !occurs) form))]

    (symbol? form)

    (if (= \? (first (name form)))
      (let [fname (name form)
            ]
        (if (= \& (second fname))
          ['& (if (and (= \_ (nth fname 2))
                       (= 3 (count fname)))
                '_
                (maybe-guard (register! form !occurs) guards))]
          [(maybe-guard (register! form !occurs) guards)]))
      [(list 'quote form)])

    :else
    [form]))

(defn assert-unify [patterns]
  (list* `and
         (for [[bind unifiers] patterns]
           `(= ~bind ~@unifiers))))

(def backtrack-all (Exception.))

(defn compile-patterns [patterns]
  (->> (for [pattern patterns
             :let [[pattern guards -> replacement] (if (map? (second pattern))
                                                     pattern
                                                     [(first pattern) {} nil (last pattern)])
                   !occurs (atom {})]]
         [(compile-pattern pattern guards !occurs) `(if ~(assert-unify @!occurs)
                                                      ~replacement
                                                      (throw backtrack-all))])
       (mapcat identity)))

(defmacro compact [expr & patterns]
  `(let [expr# ~expr]
     (try
       (m/match [expr#]
         ~@(compile-patterns patterns)
         :else expr#)
       (catch Exception e#
         (if (identical? e# backtrack-all)
           expr#
           (throw e#))))))

(defn macrocompact-step [expr]
  (compact expr
           [(`let [?a ?b] (`let ?binds ?&body)) -> `(let [~?a ~?b ~@?binds] ~@?&body)]
           [(fn* ?&body) -> `(fn ~@?&body)]
           [(let* ?binds ?&body) -> `(let ~?binds ~@?&body)]
           [(if ?test (do ?&then)) -> `(when ~?test ~@?&then)]
           [(if ?test ?then nil) ->`(when ~?test ~?then)]

           [('clojure.lang.Var/pushThreadBindings ?binds) -> `(push-thread-bindings ~?binds)]
           [('clojure.lang.Var/popThreadBindings) -> `(pop-thread-bindings)]

           [(do (`push-thread-bindings ?binds)
                (try
                  ?body
                  (finally (`pop-thread-bindings))))
            ->
            `(with-bindings ~?binds ~?body)]

           [(`with-bindings ?bindings ?&body)
            {?bindings [#(contains? % 'clojure.lang.Compiler/LOADER)
                        #(= 1 (count %))]}
            ->
            `(do ~@?&body)]

           [(if (.equals ?ns ''clojure.core) nil (do ('clojure.lang.LockingTransaction/runInTransaction ?&_) nil)) -> nil]

           [(`let [?x ?y]
             (`when ?x
              (`let [?z ?x] ?&body)))
            ->
            `(when-let [~?z ~?y] ~@?&body)]

           [(loop* ?&l) -> `(loop ~@?&l)]

           [(`loop [?seq (`seq ?b) ?chunk nil ?count 0 ?i 0]
             (if (`< ?i ?count)
               (`let [?a ('.nth ?chunk ?i)]
                ?&body)
               (`when-let [?seq (`seq ?seq)]
                (if (`chunked-seq? ?seq)
                  (`let [?c (`chunk-first ?seq)]
                    (recur ?&_))
                 (`let [?a (`first ?seq)] ?&_)))))
            -> `(doseq [~?a ~?b] ~@(butlast ?&body))]

           [(`let [?x ?y]
             (if ?x
               (`let [?z ?x] ?&body)
               ?else))
            `(if-let [~?z ~?y] (do ~@?&body) ~?else)]

           [((`fn ?n ([] ?&body))) -> `(do ~@?&body)]

           [(.setMeta ?ref ?meta) -> `(reset-meta! ~?ref ~?meta)]
           [(`reset-meta! ?var ?meta) {?meta [map?
                                              #(every? % #{:line :column :file})]} -> nil]

           [(.withMeta (`list ?&body) ?meta) {?meta [#(= [:line :column] (keys %))]} -> (list ~@?&body)]

           [(.bindRoot (var ?var) (`fn ?name ?&body)) ->  `(defn ~?name ~@?&body)]))

(defn macrocompact [source]
  (w/postwalk
   (fn [node]
     (if (seq? node)
       (let [new-node (macrocompact-step node)]
         (if (= node new-node)
           node
           (recur new-node)))
       node))
   source))
