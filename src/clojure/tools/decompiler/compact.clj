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

(defn compile-pattern [form !occurs]
  (cond

    (seq? form)
    (if (and (= 'quote (first form))
             (symbol? (second form)))
      [form]
      [(list (vec (mapcat #(compile-pattern % !occurs) form)) :seq)])

    (vector? form)
    [(vec (mapcat #(compile-pattern % !occurs) form))]

    (map? form)
    [(reduce-kv (fn [m k v] (assoc m (compile-pattern k !occurs) (compile-pattern v !occurs))) {} form)]

    (symbol? form)

    (if (= \? (first (name form)))
      (if (= \& (second (name form)))
        ['& (if (and (= \_ (nth (name form) 2))
                     (= 3 (count (name form))))
              '_
              (register! form !occurs))]
        [(register! form !occurs)])
      [(list 'quote form)])

    :else
    [form]))

(defn assert-unify [patterns]
  (list* `and
         (for [[bind unifiers] patterns]
           `(= ~bind ~@unifiers))))

(def backtrack-all (Exception.))

(defn compile-patterns [patterns]
  (->> (for [[pattern -> replacement] patterns
             :let [!occurs (atom {})]]
         [(compile-pattern pattern !occurs) `(if ~(assert-unify @!occurs)
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

           ;; [(with-bindings {'clojure.lang.Compiler/LOADER _} ?&body) -> `(do ~@?&body)]

           [(if (.equals ?ns ''clojure.core) nil (do ('clojure.lang.LockingTransaction/runInTransaction ?&_) nil)) -> nil]

           [(`let [?x ?y]
             (`when ?x
              (`let [?z ?x] ?&body)))
            ->
            `(when-let [~?z ~?y] ~@?&body)]

           [(loop* ?&l) -> `(loop ~@?&l)]

           ;; [(`loop [?seq (seq ?b) ?chunk nil ?count 0 ?i 0]
           ;;   (if (< ?i ?count)
           ;;     (`let [?a ('.nth ?chunk ?i)]
           ;;      (do ?&body))
           ;;     (`when-let [?seq (seq ?seq)]
           ;;      (if (chunked-seq? ?seq)
           ;;        (`let [?c (chunk-first ?seq)]
           ;;         (recur (chunk-rest ?seq) ?c (count ?c) 0))
           ;;        (`let [?a (`first ?seq)] ?&_)))))
           ;;  ->
           ;;  `(doseq [~?a ~?b] ~@(butlast ?&body))]

           [(`let [?x ?y]
             (if ?x
               (`let [?z ?x] ?&body)
               ?else))
            `(if-let [~?z ~?y] (do ~@?&body) ~?else)]

           [((`fn ?n ([] ?&body))) -> `(do ~@?&body)]

           [(.setMeta ?ref ?meta) -> `(reset-meta! ~?ref ~?meta)]
           ;; [(`reset-meta! ?var {:arglists _ :line _ :column _ :file _}) -> nil]

           ;; [(.withMeta (`list ?&body) {:line _ :column _}) -> (list ~@?&body)]

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
