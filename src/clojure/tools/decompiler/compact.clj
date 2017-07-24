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
            [clojure.core.match :refer [match]]
            [clojure.walk :as w]))

;; ;; Adapted from kibit
;; (defn macrocompact-step [expr rules]
;;   (let [alts (l/run 1 [q]
;;                (l/fresh [pat subst]
;;                  (l/membero [pat subst] rules)
;;                  (l/project [pat subst]
;;                             (l/all (pat expr)
;;                                    (subst q)))))]
;;     (if (empty? alts) expr (first alts))))

;; (defn compile-rule [rule]
;;   `(let [[pat# alt#] (u/prep '~rule)]
;;      [(fn [expr#] (l/== expr# pat#))
;;       (fn [sbst#] (l/== sbst# alt#))]))

;; (defn raw-rule? [rule]
;;   (not (vector? rule)))

;; (defmacro defrules [name & rules]
;;   `(def ~name
;;      ~(->> (for [rule rules]
;;              (if (raw-rule? rule)
;;                rule
;;                (compile-rule rule)))
;;            (into []))))

;; (defrules rules
;;   [(let* ?binds . ?body) (clojure.core/let ?binds . ?body)]
;;   [(if ?test (do . ?then)) (clojure.core/when ?test . ?then)]
;;   [(if ?test ?then nil) (clojure.core/when ?test ?then)]

;;   [(clojure.lang.Var/pushThreadBindings ?binds) (clojure.core/push-thread-bindings ?binds)]
;;   [(clojure.lang.Var/popThreadBindings) (clojure.core/pop-thread-bindings)]

;;   [(do (clojure.core/push-thread-bindings ?binds)
;;        (try
;;          ?body
;;          (finally (clojure.core/pop-thread-bindings))))
;;    (clojure.core/with-bindings ?binds ?body)]

;;   [(clojure.core/with-bindings
;;      {clojure.lang.Compiler/LOADER ?_}
;;      . ?body)
;;    (do . ?body)]

;;   [(if (.equals ?ns 'clojure.core) nil (do (clojure.lang.LockingTransaction/runInTransaction . ?fn) nil))   nil]

;;   [(clojure.core/let [?x ?y]
;;      (clojure.core/when ?x
;;        (clojure.core/let [?z ?x] . ?body)))
;;    (clojure.core/when-let [?z ?y] . ?body)]

;;   [(loop* . ?l) (clojure.core/loop . ?l)]

;;   (let [[?seq ?b ?chunk ?count ?i ?body ?c ?a ?c ?_] (repeatedly l/lvar)]
;;     [#(l/== % `(loop [~?seq (seq ~?b) ~?chunk nil ~?count 0 ~?i 0]
;;                  (if (< ~?i ~?count)
;;                    (let [~?a (.nth ~?chunk ~?i)]
;;                      ~(l/llist 'do ?body))
;;                    (when-let [~?seq (seq ~?seq)]
;;                      (if (chunked-seq? ~?seq)
;;                        (let [~?c (chunk-first ~?seq)]
;;                          (recur (chunk-rest ~?seq) ~?c (count ~?c) 0))
;;                        ~(l/llist `let [?a (list `first ?seq)] ?_))))))
;;      #(l/project [?a ?b ?body]
;;                  (l/== % `(clojure.core/doseq [~?a ~?b] ~@(butlast ?body))))])

;;   [(clojure.core/let [?x ?y]
;;      (if ?x
;;        (clojure.core/let [?z ?x] . ?body)
;;        ?else))
;;    (clojure.core/if-let [?z ?y] (do . ?body) ?else)]

;;   [(fn* . ?body) (clojure.core/fn . ?body)]

;;   [((clojure.core/fn ?n ([] . ?body))) (do . ?body)]

;;   (let [?body1 (l/lvar)
;;         ?body2 (l/lvar)]
;;     [#(l/== % (l/llist 'do (l/llist 'do ?body1) ?body2))
;;      #(l/project [?body1 ?body2]
;;                  (l/== % `(do ~@?body1 ~@?body2)))])

;;   [(.setMeta ?ref ?meta) (clojure.core/reset-meta! ?ref ?meta)]
;;   [(clojure.core/reset-meta! ?var {:arglists ?_ :line ?__ :column ?___ :file ?____}) nil]

;;   [(.withMeta (clojure.core/list . ?body) {:line ?_ :column ?__}) (clojure.core/list . ?body)]

;;   [(.bindRoot (var ?var) (clojure.core/fn ?name . ?body))
;;    (clojure.core/defn ?name . ?body)]

;;   (let [?argv1 (l/lvar)
;;         ?argv2 (l/lvar)
;;         ?body (l/lvar)]
;;     [#(l/== % (list `let ?argv1 (l/llist `let ?argv2 ?body)))
;;      #(l/project [?argv1 ?argv2 ?body]
;;                  (l/== % `(let [~@?argv1 ~@?argv2] ~@?body)))]))

(defn macrocompact-step [expr]
  (match [expr]
         [(['fn* & ?body] :seq)] `(fn ~@?body)
         [([`let [?a ?b] ([`let ?binds & ?body] :seq)] :seq)] `(let [~?a ~?b ~@?binds] ~@?body)
         :else expr))

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
