;;   Copyright (c) Nicola Mometto & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.decompiler.compact
  (:require [clojure.core.match :as m]
            [clojure.core.match.protocols :as mp]
            [clojure.string :as s]
            [clojure.walk :as w])
  (:import (clojure.core.match LeafNode FailNode BindNode SwitchNode)
           clojure.core.match.protocols.IPatternCompile
           clojure.lang.ExceptionInfo))

(defn compact-sequential-destructuring [binds]
  (loop [[[b v :as bind] & binds] (partition 2 binds)
         ret []]
    (cond
      (not bind)
      ret

      (and (symbol? b)
           (.startsWith (name b) "seq__")
           (seq? v)
           (= `seq (first v)))
      (let [init (second v)
            placeholder b
            [bind binds] (loop [[[b v :as bind] & binds :as curr] binds ret []]

                           (cond
                             (and (symbol? b)
                                  (.startsWith (name b) "first__"))
                             (recur binds (conj ret b))

                             (and (symbol? v)
                                  (.startsWith (name v) "first__"))
                             (recur binds (replace {v b} ret))

                             (= v placeholder)
                             [[(conj ret '& b) init] binds]

                             (= b placeholder)
                             (recur binds ret)

                             :else
                             [[(conj ret '& placeholder) init] curr]))]
        (into (into ret bind) (mapcat identity binds)))

      :else
      (recur binds (conj ret b v)))))

(defn compact-vec-destructuring [binds]
  (loop [[[b v :as bind] & binds] (partition 2 binds)
         ret []]
    (cond
      (not bind)
      ret

      (and (symbol? b)
           (.startsWith (name b) "vec__"))
      (let [init v
            placeholder b
            [bind binds] (loop [[[b v :as bind] & binds :as curr] binds ret []]

                           (cond

                             (= v placeholder)
                             (if (symbol? b)
                               [[(conj ret :as b) init] binds]
                               (recur binds b))

                             (and (seq? v)
                                  (= `nth (first v))
                                  (= placeholder (second v)))
                             (recur binds (conj ret b))

                             (and (seq? v)
                                  (= `nthnext (first v))
                                  (= placeholder (second v)))
                             [[(conj ret '& b) init] binds]

                             :else
                             [[ret init] curr]))]
        (into (into ret bind) (mapcat identity binds)))

      :else
      (recur binds (conj ret b v)))))

(defn simplify-map-destructuring [m]
  (let [{ks true opts false} (group-by (comp keyword? val) m)
        {ks true oths false} (group-by #(and (not (namespace (val %)))
                                             (and (keyword? (val %))
                                                  (symbol? (key %))
                                                  (= (name (val %))
                                                     (name (key %))))) ks)]
    (-> {}
        (cond-> (seq ks)
          (conj [:keys (mapv key ks)]))
        (into opts)
        (into oths))))

(defn compact-associative-destructuring [binds]
  (loop [[[b v :as bind] & binds] (partition 2 binds)
         ret []]
    (cond
      (not bind)
      ret

      (and (symbol? b)
           (.startsWith (name b) "map__"))
      (let [init v
            placeholder b
            [bind binds] (loop [[[b v :as bind] & binds :as curr] (rest binds) ret {}]

                           (cond

                             (= v placeholder)
                             (recur binds (assoc ret :as b))

                             (and (seq? v)
                                  (= `get (first v))
                                  (= placeholder (second v)))
                             (let [k (nth v 2)
                                   ?or (and (= 4 (count v)) (nth v 3))]
                               (recur binds (cond-> (assoc ret b k)
                                              ?or (assoc-in [:or b] ?or))))

                             :else
                             [[(simplify-map-destructuring ret) init] curr]))]
        (into (into ret bind) (mapcat identity binds)))

      :else
      (recur binds (conj ret b v)))))

(defn remove-defrecord-methods [methods]
  (for [[name :as method] methods
        :when (not ('#{hasheq hashCode equals meta withMeta valAt getLookupThunk
                       count empty cons equiv containsKey entryAt seq iterator
                       assoc without size isEmpty containsValue get put
                       remove putAll clear keySet values entrySet}
                    name))]
    method))

(defn remove-defrecord-interfaces [interfaces]
  (remove '#{clojure.lang.IHashEq
             clojure.lang.IRecord
             clojure.lang.IObj
             clojure.lang.ILookup
             clojure.lang.IKeywordLookup
             clojure.lang.IPersistentMap
             java.util.Map
             java.io.Serializable}
    interfaces))

(defn remove-defrecord-fields [fields]
  (vec (remove '#{__meta __extmap __hash __hasheq}  fields)))

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
          (if (= \_ (second fname))
            [form]
            [(maybe-guard (register! form !occurs) guards)])))
      [(list 'quote form)])

    :else
    [form]))

(defn assert-unify [patterns]
  (list* `and true
         (for [[bind unifiers] patterns
               :when (seq unifiers)]
           `(= ~bind ~@unifiers))))

(defn cont! [f]
  {::cont f})

(defprotocol NodeToClj (to-clj [_]))

(def ^:dynamic *conts*)

(defn dag-clause-to-clj [occurrence cont pattern action]
  (let [test (if (instance? IPatternCompile pattern)
               (mp/to-source* pattern occurrence)
               (m/to-source pattern occurrence))]
    [test (to-clj (assoc action :cont cont))]))

(extend-protocol NodeToClj
  LeafNode
  (to-clj [{:keys [value bindings]}]
    (if (not (empty? bindings))
      (let [bindings (remove (fn [[sym _]] (= sym '_))
                             bindings)]
        `(let [~@(apply concat bindings)]
           ~value))
      value))

  FailNode
  (to-clj [{:keys [cont]}]
    `(cont! ~cont))

  BindNode
  (to-clj [{:keys [bindings node cont]}]
    `(let [~@bindings]
       ~(to-clj (assoc node :cont cont))))

  SwitchNode
  (to-clj [{:keys [occurrence cases default cont]}]
    (let [default-cont (when-not (instance? FailNode default)
                         `([] ~(to-clj (assoc default :cont cont))))

          _default-cont (if default-cont (gensym "default-cont_") cont)

          clauses (doall
                   (mapcat (partial apply dag-clause-to-clj occurrence _default-cont) cases))
          bind-expr (-> occurrence meta :bind-expr)
          cond-expr `(cond ~@clauses
                           :else
                           (cont! ~_default-cont))]

      (when default-cont
        (swap! *conts* conj (list* _default-cont default-cont)))

      `(let [~@(when bind-expr [occurrence bind-expr])]
         ~cond-expr))))

(defn run-match [f]
  (let [ret (f)]
    (if-let [cont (::cont ret)]
      (recur cont)
      ret)))

(defn compile-patterns [patterns cont]
  (->> (for [pattern patterns
             :let [[pattern guards _ replacement] (if (map? (second pattern))
                                                    pattern
                                                    [(first pattern) {} nil (last pattern)])
                   !occurs (atom {})]]
         [(compile-pattern pattern guards !occurs) (if (seq @!occurs)
                                                     `(if ~(assert-unify @!occurs)
                                                        ~replacement
                                                        (cont! ~cont))
                                                     replacement)])
       (mapcat identity)))

(defmacro compact
  {:style/indent 1}
  [expr & patterns]
  (let [_expr (gensym "expr_")
        _cont (gensym "cont_")
        [patterns else] (if (= :else (last (butlast patterns)))
                          [(-> patterns butlast butlast) (last patterns)]
                          [patterns _expr])]

    (binding [m/*line* (-> &form meta :line)
              m/*locals* (dissoc &env '_)
              m/*warned* (atom false)
              *conts* (atom #{})]
      (let [init (-> (m/emit-matrix [expr] (concat (compile-patterns patterns _cont) [:else else]))
                     m/compile
                     (assoc :cont _cont)
                     to-clj)]
        `(run-match (fn []
                      (let [~_expr ~expr
                            ~_cont (fn [] ~else)]
                        (letfn [~@@*conts*]
                          ~init))))))))

(defn macrocompact-step [expr]
  (compact expr
    [(do ?ret) :-> ?ret]
    [(`let [?a ?b] (`let ?binds ?&body)) :-> `(let [~?a ~?b ~@?binds] ~@?&body)]
    [(fn* ?&body) :-> `(fn ~@?&body)]
    [(let* ?binds ?&body) :-> `(let ~?binds ~@?&body)]
    [(if ?test (do ?&then)) :-> `(when ~?test ~@?&then)]
    [(if ?test ?then nil) :->`(when ~?test ~?then)]
    [(`when ?test (do ?&then)) :-> `(when ~?test ~@?&then)]
    [(`let ?bindings (do ?&body)) :-> `(let ~?bindings ~@?&body)]
    [(`when-let ?bindings (do ?&body)) :-> `(when-let ~?bindings ~@?&body)]
    [(`when-some ?bindings (do ?&body)) :-> `(when-some ~?bindings ~@?&body)]
    [(`fn ?name (?bindings (do ?&body))) :-> `(fn ~?name (~?bindings ~@?&body))]
    [(if ?test nil ?&body) :-> `(when-not ~?test ~@?&body)]
    [(`when-not ?bindings (do ?&body)) :-> `(when-not ~?bindings ~@?&body)]

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

    [(.get (var ?v)) :-> ?v]

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

    [(if ?test1 ?then1 (`when ?test2 ?&then2)) :-> `(cond ~?test1 ~?then1 ~?test2 (do ~@?&then2))]
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
        (`let [?a (.nth ?chunk ?i)
               ?&binds]
         ?&body)
        (`when-let [?seq (`seq ?seq)]
         (if (`chunked-seq? ?seq)
           (`let [?c (`chunk-first ?seq)]
            (recur ?&_))
           (`let [?x (`first ?seq) ?&_] ?&_)))))
     :-> `(doseq [~?a ~?b ~@(when (seq ?&binds) [:let (vec ?&binds)])] ~@(butlast ?&body))]

    [(`let [?c ?t]
      (`loop [?n 0]
       (`when (`< ?n ?c)
        ?&body)))
     {?body #(compact % [(recur (`inc ?a)) :-> true] :else false)}
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
     {?x #(and (symbol? %) (-> % name (.startsWith "temp__")))}
     :->
     (let [body (if (empty? ?&binds) `(do ~@?&body) `(let [~@?&binds] ~@?&body))]
       `(when-let [~?z ~?y] ~body))]

    [(`let [?x ?y]
      (if ?x
        (`let [?z ?x ?&binds] ?&body)
        ?else))
     {?x #(and (symbol? %) (-> % name (.startsWith "temp__")))}
     :->
     (let [body (if (empty? ?&binds) `(do ~@?&body) `(let [~@?&binds] ~@?&body))]
       `(if-let [~?z ~?y] ~body ~?else))]

    [(`let [?x ?y]
      (if (`nil? ?x)
        nil
        (`let [?z ?x ?&binds] ?&body)))
     {?x #(and (symbol? %) (-> % name (.startsWith "temp__")))}
     :->
     (let [body (if (empty? ?&binds) `(do ~@?&body) `(let [~@?&binds] ~@?&body))]
       `(when-some [~?z ~?y] ~body))]

    [(`let [?x ?y]
      (if (`nil? ?x)
        ?else
        (`let [?z ?x]
         ?&body)))
     {?x #(and (symbol? %) (-> % name (.startsWith "temp__")))}
     :->
     `(if-some [~?z ~?y] (do ~@?&body) ~?else)]

    [(`let [?t ?x] (if ?t ?y ?t))
     {?t #(and (symbol? %) (-> % name (.startsWith "and__")))}
     :->
     `(and ~?x ~?y)]
    [(`and ?x (`and ?y ?&z)) :->  `(and ~?x ~?y ~@?&z)]

    [(`let [?t ?x] (if ?t ?t ?y))
     {?t #(and (symbol? %) (-> % name (.startsWith "or__")))}
     :->
     `(or ~?x ~?y)]
    [(`or ?x (`or ?y ?&z)) :-> `(or ~?x ~?y ~@?&z)]

    ;; WIP body should not use ?n
    [((`fn ?n ([] ?&body))) :-> `(do ~@?&body)]

    [(clojure.core/import* ?klass) :-> `(import '~(symbol ?klass))]

    [(.setMeta ?ref ?meta) :-> `(reset-meta! ~?ref ~?meta)]
    [(`reset-meta! ?var ?meta) {?meta #(and (map? %)
                                            (#{#{:column} #{:column :arglists} #{:line :column :file}
                                               #{:line :column :file :doc}
                                               #{:line :column :file :doc :arglists}} (set (keys %))))} :-> nil]

    [(.withMeta (`list ?&body) ?meta) {?meta #(and (map? %) (#{#{:line} #{:column} #{:line :column}} (set (keys %))))} :-> `(list ~@?&body)]
    [(.withMeta ?x ?meta) {?meta #(empty? %)} :-> ?x]

    [(clojure.lang.LockingTransaction/runInTransaction (`fn ?_ ([] ?&body))) :-> `(dosync ~@?&body)]

    ;; WIP custom message
    [(if (clojure.lang.LockingInTransaction/isRunning)
       (throw ?_)
       ?body) :-> `(io! ~?body)]

    [(`when-let [?bind (`seq ?xs)]
      (`let [?x ?bind]
       ?&body)) {?bind #(and (symbol? %) (-> % name (.startsWith "xs__")))}
     :->
     `(when-first [~?x ~?xs]
        ~@?&body)]

    [(if (`nil? ?g)
       nil
       (?f ?g ?&args))
     {?g #(and (symbol? %) (-> % name (.startsWith "G__")))}
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
     {?g #(and (symbol? %) (-> % name (.startsWith "G__")))}
     :-> `(cond-> ~?g ~?test (~?f ~@?&args))]

    [(`let [?g (`cond-> ?g ?&exprs)]
      (`cond-> ?g ?&exprs2))
     :-> `(cond-> ~?g ~@?&exprs ~@?&exprs2)]

    [(`let [?g ?expr]
      (`cond-> ?g ?&exprs2))
     :-> `(cond-> ~?expr ~@?&exprs2)]

    [(do
       nil
       (`let [?v (var ?var)]
        (`when-not (`and (.hasRoot ?v)
                    (`instance? clojure.lang.MultiFn (`deref ?v)))
         ?_
         (def ?name (clojure.lang.MultiFn. ?sname ?dispatch-fn ?d ?h))
         (var ?var))))
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

    [(clojure.lang.RT/classForName ?class) {?class string?} :-> (symbol ?class)]

    [(`refer ''clojure.core ?&filters) :-> `(refer-clojure ~@?&filters)]

    [(`let [?v (var ?var)]
      (`when-not (.hasRoot ?v)
       ?_
       (def ?name ?expr)
       (var ?var))) :-> `(defonce ~?name ~?expr)]

    [(`loop []
      (`when ?test
        ?&body))
     {?&body #(= '(recur) (last %))}
     :->
     `(while ~?test ~@(butlast ?&body))]

    [(.addMethod ?multi ?dispatch-val (`fn ?&body)) :-> `(defmethod ~?multi ~?dispatch-val ~@?&body)]

    [(letfn* ?binds ?&body) :-> `(letfn ~(vec (for [[_ bind] (partition 2 ?binds)]
                                                (rest bind)))
                                   ~@?&body)]

    [(`future-call (fn ?_ [] ?&body)) :-> `(future ~@?&body)]

    [(reify* ?interfaces ?&methods) :-> `(reify ~@?interfaces ~@?&methods)]
    [(.withMeta (`reify ?&body) ?_) :-> `(reify ~@?&body)]

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
     {?g #(and (symbol? %) (-> % name (.startsWith "G__")))}
     :-> `(case ~?expr ~@?&body)]

    [(`let [?a ?arr ?len (`alength ?a)]
      (`loop [?idx 0, ?ret ?init]
       (if (`< ?idx ?len) (recur (`inc ?idx) ?expr) ?ret)))
     :-> `(areduce ~?arr ~?idx ~?ret ~?init ~?expr)]

    [(`let [?x ?obj] (?f ?x ?&args) (?g ?x ?&args2) ?&exprs)
     {?x #(and (symbol? %) (-> % name (.startsWith "G__")))
      ?&exprs (fn [exprs] (every? #(and (seq? %) (= (last exprs) (second %))) (butlast exprs)))}
     :-> `(doto ~?obj (~?f ~@?&args) (~?g ~@?&args2) ~@(map #(list* (first %) (drop 2 %)) (butlast ?&exprs)))]

    [(deftype* ?record ?rtype ?argv :implements ?interfaces ?&impls)
     {?interfaces #(some #{'clojure.lang.IRecord} %)}

     :-> `(defrecord ~(symbol (name ?record)) ~(remove-defrecord-fields ?argv)
            ~@(remove-defrecord-interfaces ?interfaces) ~@(remove-defrecord-methods ?&impls))]
    [(do
       nil
       (var ?_)
       nil
       (var ?__)
       (`defrecord ?&body)
       ?&_)
     :-> `(defrecord ~@?&body)]

    [(`alter-meta! (var ?v) `assoc :doc nil) :-> nil]
    [((var clojure.core/assert-same-protocol) ?&_) :-> nil]
    [(`-reset-methods ?_) :-> nil]
    [(`alter-var-root (var ?p) `merge (`assoc ?m :sigs ?sigs :var (var ?p) :method-map ?mm :method-builders ?mb))
     :-> `(defprotocol ~(symbol (name ?p))
            ~@(->> (for [[f {:keys [arglists]}] ?sigs]
                     [(symbol (name f)) (map #(mapv second %) (rest arglists))])
                (mapcat identity)))]

    [(deftype* ?type ?ttype ?argv :implements ?interfaces ?&impls)
     {?interfaces #(some #{'clojure.lang.IType} %)}

     :-> `(deftype ~(symbol (name ?type)) ~?argv
            ~@(remove #{'clojure.lang.IType} ?interfaces) ~@?&impls)]

    [(do (deftype ?&body) ?&_) :-> `(deftype ~@?&body)]

    [(`let [?&binds] ?&body)
     {?&binds (fn [binds] (some #(and (symbol? (first %))
                                      (.startsWith (name (first %)) "map__")
                                      (compact (second %)
                                        [(if (`seq? ?m)
                                           (clojure.lang.PersistentHashMap/create (`seq ?m))
                                           ?m) :-> false]
                                        :else true))
                                (partition 2 binds)))}
     :->
     `(let [~@(compact-associative-destructuring ?&binds)] ~@?&body)]

    [(`let [?&binds] ?&body)
     {?&binds (fn [binds] (some #(and (symbol? (first %))
                                      (.startsWith (name (first %)) "seq__")
                                      (seq? (second %))
                                      (= `seq (first (second %))))
                                (partition 2 binds)))}
     :->
     `(let [~@(compact-sequential-destructuring ?&binds)] ~@?&body)]

    [(`let [?&binds] ?&body)
     {?&binds (fn [binds] (some #(and (symbol? %) (.startsWith (name %) "vec__")) (take-nth 2 binds)))}
     :->
     `(let [~@(compact-vec-destructuring ?&binds)] ~@?&body)]

    [(.set (var ?v) ?val) ?-> `(set! ~?v ~?val)]

    [(.bindRoot (var ?var) (`fn ?name ?&body)) :->  `(defn ~(-> ?var name symbol) ~@?&body)]
    [(.bindRoot (var ?var) ?val) :->  `(def  ~(-> ?var name symbol) ~?val)]))

;; WIP for, assert, ns, condp, with-redefs, definterface

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
