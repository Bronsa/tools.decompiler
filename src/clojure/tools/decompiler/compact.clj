;;   Copyright (c) Nicola Mometto & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.decompiler.compact
  (:require [clojure.core.match :as m]
            [clojure.string :as s]
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
  (if-let [guard (get guards sym)]
    (list sym :guard [guard])
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
      (let [fname (name form)]
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
  (list* `and true
         (for [[bind unifiers] patterns
               :when (seq unifiers)]
           `(= ~bind ~@unifiers))))

(def backtrack-all (Exception.))

(defn compile-patterns [patterns]
  (->> (for [pattern patterns
             :let [[pattern guards _ replacement] (if (map? (second pattern))
                                                    pattern
                                                    [(first pattern) {} nil (last pattern)])
                   !occurs (atom {})]]
         [(compile-pattern pattern guards !occurs) `(if ~(assert-unify @!occurs)
                                                      ~replacement
                                                      (throw backtrack-all))])
       (mapcat identity)))

(defmacro compact
  {:style/indent 1}
  [expr & patterns]
  (let [_expr (gensym)
        [patterns else] (if (= :else (last (butlast patterns)))
                          [(-> patterns butlast butlast) (last patterns)]
                          [patterns _expr])]
    `(let [~_expr ~expr]
       (try
         (m/match [~_expr]
                  ~@(compile-patterns patterns)
                  :else ~else)
         (catch Exception e#
           (if (identical? e# backtrack-all)
             ~else
             (throw e#)))))))

(defn macrocompact-step [expr]
  (compact expr
    [(`let [?a ?b] (`let ?binds ?&body)) :-> `(let [~?a ~?b ~@?binds] ~@?&body)]
    [(fn* ?&body) :-> `(fn ~@?&body)]
    [(let* ?binds ?&body) :-> `(let ~?binds ~@?&body)]
    [(if ?test (do ?&then)) :-> `(when ~?test ~@?&then)]
    [(if ?test ?then nil) :->`(when ~?test ~?then)]
    [(`when ?test (do ?&then)) :-> `(when ~?test ~@?&then)]
    [(`let ?bindings (do ?&body)) :-> `(let ~?bindings ~@?&body)]
    [(`when-let ?bindings (do ?&body)) :-> `(when-let ~?bindings ~@?&body)]
    [(`when-some ?bindings (do ?&body)) :-> `(when-some ~?bindings ~@?&body)]
    [(`fn (?bindings (do ?&body))) :-> `(fn (~?bindings ~@?&body))]
    [(if ?test nil (do ?&body)) :-> `(when-not ~?test ~@?&body)]

    [(clojure.lang.RT/count ?arg) :-> `(count ~?arg)]
    [(clojure.lang.RT/nth ?&args) :-> `(nth ~@?&args)]
    [(clojure.lang.RT/get ?&args) :-> `(get ~@?&args)]
    [(clojure.lang.RT/isReduced ?arg) :-> `(reduced? ~?arg)]
    [(clojure.lang.RT/alength ?arg) :-> `(alength ~?arg)]
    [(clojure.lang.RT/aclone ?arg) :-> `(aclone ~?arg)]
    [(clojure.lang.RT/aget ?arr ?idx) :-> `(aget ~?arr ~?idx)]
    [(clojure.lang.RT/aset ?arr ?idx ?val) :-> `(aset ~?arr ~?idx ~?val)]
    [(clojure.lang.RT/object_array ?arg) :-> `(object-array ~?arg)]
    [(clojure.lang.Util/identical ?a ?b) :-> `(identical? ~?a ~?b)]
    [(clojure.lang.Util/equiv ?a ?b) :-> `(= ~?a ~?b)]
    [(clojure.lang.Numbers/num ?a) :-> ?a]
    [(java.lang.Long/valueOf ?a) {?a number?} :-> (long ?a)]
    [(java.lang.Integer/valueOf ?a) {?a number?} :-> (int ?a)]
    [(java.lang.Double/valueOf ?a) {?a number?} :-> (double ?a)]
    [(java.lang.Float/valueOf ?a) {?a number?} :-> (float ?a)]

    [(do ?&body)
     {?&body #(some (fn [expr]
                      (and (seq? expr)
                           (= 'do (first expr))))
                    %)}
     :->
     (list* 'do (->> (for [expr ?&body
                           :when expr]
                       (if (and (seq? expr)
                                (= 'do (first expr)))
                         (rest expr)
                         [expr]))
                     (mapcat identity)))]

    [(clojure.lang.Var/pushThreadBindings ?binds) :-> `(push-thread-bindings ~?binds)]
    [(clojure.lang.Var/popThreadBindings) :-> `(pop-thread-bindings)]

    [(do (`push-thread-bindings ?binds)
         (try
           ?&body))
     {?&body #(= `(finally (pop-thread-bindings)) (last %))}
     :->
     (let [?&body (butlast ?&body)]
       (cond

         (map? ?binds)
         (if (every? #(and (seq? %) (= 'var (first %))) (keys ?binds))
           `(binding ~(vec (mapcat (fn [[[_ var] init]] [var init]) ?binds)) ~@?&body)
           `(with-bindings ~?binds ~@?&body))

         (and (seq? ?binds) (= `hash-map (first ?binds)))
         (if (every? #(and (seq? %) (= 'var (first %))) (take-nth 2 (rest ?binds)))
           `(binding ~(vec (mapcat (fn [[[_ var] init]] [var init]) (partition 2 (rest ?binds)))) ~@?&body)
           `(with-bindings ~?binds ~@?&body))

         :else
         `(with-bindings ~?binds ~@?&body)))]

    [(`with-bindings ?bindings ?&body)
     {?bindings #(and (map? %)
                      (contains? % 'clojure.lang.Compiler/LOADER)
                      (= 1 (count %)))}
     :->
     `(with-loading-context ~@?&body)]

    [(`identical? ?x nil) :-> `(nil? ~?x)]
    [(`identical? nil ?x) :-> `(nil? ~?x)]

    [(clojure.lang.LazySeq. (`fn ?_ ([] ?&body))) :-> `(lazy-seq ~@?&body)]
    [(clojure.lang.Delay. (`fn ?_ ([] ?&body))) :-> `(delay ~@?&body)]
    [(`bound-fn* (`fn ?_ ([] ?&body))) :-> `(bound-fn ~@?&body)]

    [(.reset ?v (?f (.deref ?v) ?&args)) :-> `(vswap! ~?v ~?f ~@?&args)]

    [(`when-not (.equals ?ns ''clojure.core)
      (`dosync ?&_)
      nil)
     :->
     nil]

    [(if ?test1 ?then1 (`when ?test2 ?then2)) :-> `(cond ~?test1 ~?then1 ~?test2 ~?then2)]
    [(if ?test1 ?then1 (`cond ?&body)) :-> `(cond ~?test1 ~?then1 ~@?&body)]

    [(loop* ?&l) :-> `(loop ~@?&l)]

    [(`let [?a ?b] (try ?&body))
     {?&body #(and (seq? %)
                   (compact (last %)
                     [(finally (.close ?x)) :-> true]
                     :else false))}
     :->
     `(with-open [~?a ~?b]
        ~@(butlast ?&body))]

    [(`loop [?seq (`seq ?b) ?chunk nil ?count 0 ?i 0]
      (if (`< ?i ?count)
        (`let [?a (.nth ?chunk ?i)]
         ?&body)
        (`when-let [?seq (`seq ?seq)]
         (if (`chunked-seq? ?seq)
           (`let [?c (`chunk-first ?seq)]
            (recur ?&_))
           (`let [?a (`first ?seq) ?&_] ?&_)))))
     :-> `(doseq [~?a ~?b] ~@(butlast ?&body))]

    [(`let [?c ?t]
      (`loop [?n 0]
       (`when (`< ?n ?c)
        ?&body)))
     {?body #(compact % [(recur (`+ ?a 1)) :-> true] :else false)}
     :->
     `(dotimes [~?n ~?t]
        ~@(butlast ?&body))]

    [(`let [?l ?lock]
      (try
        (do (monitor-enter ?l)
            ?&body)
        (finally ?&_)))
     :->
     `(locking ~?lock ~@?&body)]

    [(`let [?x ?y]
      (`when ?x
       (`let [?z ?x ?&binds] ?&body)))
     {?x #(-> % name (.startsWith "temp--"))}
     :->
     (let [body (if (empty? ?&binds) `(do ~@?&body) `(let [~@?&binds] ~@?&body))]
       `(when-let [~?z ~?y] ~body))]

    [(`let [?x ?y]
      (if ?x
        (`let [?z ?x ?&binds] ?&body)
        ?else))
     {?x #(-> % name (.startsWith "temp--"))}
     :->
     (let [body (if (empty? ?&binds) `(do ~@?&body) `(let [~@?&binds] ~@?&body))]
       `(if-let [~?z ~?y] ~body ~?else))]

    [(`let [?x ?y]
      (if (`nil? ?x)
        nil
        (`let [?z ?x ?&binds] ?&body)))
     {?x #(-> % name (.startsWith "temp--"))}
     :->
     (let [body (if (empty? ?&binds) `(do ~@?&body) `(let [~@?&binds] ~@?&body))]
       `(when-some [~?z ~?y] ~body))]

    [(`let [?x ?y]
      (if (`nil? ?x)
        ?else
        (`let [?z ?x]
         ?&body)))
     {?x #(-> % name (.startsWith "temp--"))}
     :->
     `(if-some [~?z ~?y] (do ~@?&body) ~?else)]

    [(`let [?t ?x] (if ?t ?y ?t))
     {?t #(-> % name (.startsWith "and--"))}
     :->
     `(and ~?x ~?y)]
    [(`and ?x (`and ?y ?&z)) :->  `(and ~?x ~?y ~@?&z)]

    [(`let [?t ?x] (if ?t ?t ?y))
     {?t #(-> % name (.startsWith "or--"))}
     :->
     `(or ~?x ~?y)]
    [(`or ?x (`or ?y ?&z)) :-> `(or ~?x ~?y ~@?&z)]

    ;; WIP body should not use ?n
    [((`fn ?n ([] ?&body))) :-> `(do ~@?&body)]

    [(clojure.core/import* ?klass) :-> `(import '~(symbol ?klass))]

    [(.setMeta ?ref ?meta) :-> `(reset-meta! ~?ref ~?meta)]
    [(`reset-meta! ?var ?meta) {?meta (every-pred map?
                                                  (some-fn #(every? % #{:line :column :file})
                                                           #(every? % #{:column :arglists})))} :-> nil]

    [(.withMeta (`list ?&body) ?meta) {?meta #(= [:line :column] (keys %))} :-> (list ~@?&body)]

    [(clojure.lang.LockingTransaction/runInTransaction (`fn ?_ ([] ?&body))) :-> `(dosync ~@?&body)]

    ;; WIP custom message
    [(if (clojure.lang.LockingInTransaction/isRunning)
       (throw ?_)
       (do ?&body)) :-> `(io! ~@?&body)]

    [(`when-let [?bind (`seq ?xs)]
      (`let [?x ?bind]
       ?&body)) {?bind #(-> % name (.startsWith "xs--"))}
     :->
     `(when-first [~?x ~?xs]
        ~@?&body)]

    [(if (`nil? ?g)
       nil
       (?f ?g ?&args))
     {?g #(-> % name (.startsWith "G--"))}
     :-> `(some-> ~?g (~?f ~@?&args))]

    [(`let [?g (`some-> ?g ?&exprs)]
      (`some-> ?g ?&exprs2))
     :-> `(some-> ~?g ~@?&exprs ~@?&exprs2)]

    [(`let [?g ?expr]
      (`some-> ?g ?&exprs2))
     :-> `(some-> ~?expr ~@?&exprs2)]

    [(`if ?test
      (?f ?g ?&args)
      ?g)
     {?g #(-> % name (.startsWith "G--"))}
     :-> `(cond-> ~?g ~?test (~?f ~@?&args))]

    [(`let [?g (`cond-> ?g ?&exprs)]
      (`cond-> ?g ?&exprs2))
     :-> `(cond-> ~?g ~@?&exprs ~@?&exprs2)]

    [(`let [?g ?expr]
      (`cond-> ?g ?&exprs2))
     :-> `(cond-> ~?expr ~@?&exprs2)]

    [(do nil
         (`let [?v (var ?var)]
          (if (`and (.hasRoot ?v)
               (`instance? clojure.lang.MultiFn (`deref ?v)))
            nil
            (do nil
                (def ?name (clojure.lang.MultiFn. ?sname ?dispatch-fn ?d ?h))
                (var ?var)))))
     :->
     `(defmulti ~?name ~?dispatch-fn
        ~@(when-not (= ?d :default) [?d])
        ~@(when-not (= ?h '(var clojure.core/global-hierarchy)) [?h]))]

    [(`let [?s (java.io.StringWriter.)]
      (`binding [`*out* ?s]
       ?&body))
     {?&body #(compact (last %) [(`str ?_) :-> true] :else false)}
     :-> `(with-out-str ~@(butlast ?&body))]

    [(`let [?s (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. ?i))]
      (`binding [`*in* ?s]
       ?&body))
     :-> `(with-in-str ~@?&body)]

    [(`refer ''clojure.core ?&filters) :-> `(refer-clojure ~@?&filters)]

    [(`let [?v (var ?var)]
      (`when-not (.hasRoot ?v)
       nil
       (def ?name ?expr)
       (var ?var))) :-> `(defonce ~?name ~?expr)]

    [(`loop []
      (when ?test
        ?&body))
     {?&body #(= '(recur) (last %))}
     :->
     `(while ?test ~@(butlast ?&body))]

    [(.addMethod ?multi ?dispatch-val (`fn ?&body)) :-> `(defmethod ~?multi ~?dispatch-val ~@?&body)]

    [(letfn* ?binds ?&body) :-> `(letfn ~(vec (for [[_ bind] (partition 2 ?binds)]
                                                (rest bind)))
                                   ~@?&body)]

    [(`future-call (fn ([] ?&body))) :-> `(future ~@?&body)]

    [(reify* ?interfaces ?&methods) :-> `(reify ~@?interfaces ~@?&methods)]

    [(`let [?p (?ctor)]
      (`init-proxy ?p ?methods-map)
      ?p) :->
     `(proxy ~(-> ?ctor str (s/split #"\$") (rest) (butlast) (->> (mapv symbol)))
          ~@(for [[method method-fn] ?methods-map]
              (list* (symbol method) (drop 2 method-fn))))]

    [(`proxy-call-with-super (`fn ?_ ([] ?meth)) ?&_)
     {?meth #(= 'this (second %) )}
     :-> `(proxy-super ~(-> ?meth first str (subs 1) symbol) ~@(->> ?meth (drop 2)))]

    [(`+ ?a 1) :-> `(inc ~?a)]
    [(`+ 1 ?a) :-> `(inc ~?a)]
    [(`- ?a 1) :-> `(dec ~?a)]

    [(`let [?a ?arr, ?ret (`aclone ?a)]
      (`loop [?idx 0]
       (if (`< ?idx (`alength ?a))
         (do
           (`aset ?ret (java.lang.Integer/valueOf ?idx) ?expr)
           (recur (`inc ?idx)))
         ?ret)))
     :-> `(amap ~?arr ~?idx ~?ret ~?expr)]

    [(`case ?&exprs)
     {?&exprs #(and (even? (count %))
                    (compact (last %)
                      [(do (throw (java.lang.IllegalArgumentException. (`str "No matching clause: " ?_))) ?&_) :-> true]
                      :else false))}
     :-> `(case ~@(butlast ?&exprs))]

    [(`let [?g ?expr]
      (`case ?g
       ?&body))
     {?g #(-> % name (.startsWith "G--"))}
     :-> `(case ~?expr ~@?&body)]

    [(`let [?a ?arr ?len (`alength ?a)]
      (`loop [?idx 0, ?ret ?init]
       (if (`< ?idx ?len) (recur (`inc ?idx) ?expr) ?ret)))
     :-> `(areduce ~?arr ~?idx ~?ret ~?init ~?expr)]

    [(`let [?x ?obj] (?f ?x ?&args) (?g ?x ?&args2) ?&exprs)
     {?x #(-> % name (.startsWith "G--"))
      ?&exprs (fn [exprs] (every? #(and (seq? %) (= (last exprs) (second %))) (butlast exprs)))}
     :-> `(doto ~?obj (~?f ~@?&args) (~?g ~@?&args2) ~@(map #(list* (first %) (drop 2 %)) (butlast ?&exprs)))]

    [(.bindRoot (var ?var) (`fn ?name ?&body)) :->  `(defn ~(-> ?var name symbol) ~@?&body)]
    [(.bindRoot (var ?var) ?val) :->  `(def  ~(-> ?var name symbol) ~?val)]))

;; WIP for, destructuring, assert, ns, condp, with-redefs, definterface, defprotocol, defrecord, deftype

(defn macrocompact [source]
  (w/postwalk
   (fn [node]
     (if (seq? node)
       (let [new-node (macrocompact-step node)]
         (if (= node new-node)
           node
           (macrocompact new-node)))
       node))
   source))
