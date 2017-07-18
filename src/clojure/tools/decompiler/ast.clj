(ns clojure.tools.decompiler.ast
  (:require [clojure.set :as set]
            [clojure.tools.decompiler.stack :refer [peek-n pop-n]]
            [clojure.tools.decompiler.bc :as bc]
            [clojure.tools.decompiler.utils :as u]))

;; WIP casting, type hints

(def initial-ctx {:fields {}
                  :statements []
                  :ast {}})

(def initial-local-ctx {:stack []
                        :pc 0
                        :local-variable-table #{}})

;; process-* : bc, ctx -> ctx
;; decompile-* : bc, ctx -> AST

(defmulti process-insn
  (fn [ctx {:insn/keys [name]}] (keyword name))
  :hierarchy #'bc/insn-h)

(defmethod process-insn :default [ctx {:insn/keys [name]}]
  (println "INSN NOT HANDLED:" name)
  ctx)

(defn process-insns [{:keys [stack pc jump-table terminate-at] :as ctx} bc]
  (if (or (not (get jump-table pc))
          (= pc terminate-at))
    ctx
    (let [insn-n (get jump-table pc)
          {:insn/keys [length] :as insn} (nth bc insn-n)]
      (-> (process-insn ctx insn)
          (update :pc (fn [new-pc]
                        (if (= new-pc pc)
                          ;; last insn wasn't an explicit jump, goto next insn
                          (+ new-pc length)
                          new-pc)))
          (recur bc)))))

(defmethod process-insn :return [ctx _]
  ctx)

(defmethod process-insn ::bc/const-insn [ctx {:insn/keys [pool-element]}]
  (-> ctx
      (update :stack conj {:op :const
                           :val (:insn/target-value pool-element)})))

(defmethod process-insn :dup [{:keys [stack] :as ctx} _]
  (let [val (peek stack)]
    (-> ctx
        (update :stack conj val))))


(defmethod process-insn :anewarray [{:keys [stack] :as ctx} {:insn/keys [pool-element]}]
  (let [{:insn/keys [target-type]} pool-element
        {dimension :val} (peek stack)
        expr {:op :array
              :!items (atom (vec (repeat dimension {:op :const :val nil})))}]

    (-> ctx
        (update :stack pop)
        (update :stack conj expr))))

(defmethod process-insn ::bc/array-store [{:keys [stack] :as ctx} {:insn/keys [pool-element]}]
  (let [[{:keys [!items] :as array} {index :val} value] (peek-n stack 3)]
    (swap! !items assoc index value)
    (-> ctx
        (update :stack pop-n 3))))

(defmethod process-insn :monitorenter [{:keys [stack] :as ctx} _]
  (let [sentinel (peek stack)]
    (-> ctx
        (update :stack pop)
        (update :stack conj {:op :monitor-enter
                             :sentinel sentinel}))))

(defmethod process-insn :monitorexit [{:keys [stack] :as ctx} _]
  (let [sentinel (peek stack)]
    (-> ctx
        (update :stack pop)
        (update :stack conj {:op :monitor-exit
                             :sentinel sentinel}))))

(defmethod process-insn :areturn [{:keys [stack statements] :as ctx} _]
  (let [ret (peek stack)]
    (-> ctx
        (assoc :stack [] :statements []
               :ast {:op :do
                     :ret ret
                     :statements statements}))))

(defn ->do [exprs]
  {:op :do
   :statements (vec (butlast exprs))
   :ret (or (last exprs) {:op :const :val nil})})

(defn process-if [{:keys [insns jump-table stack] :as ctx} [start-then end-then] [start-else end-else]]
  (let [{then-stack :stack then-stmnts :statements} (process-insns (assoc ctx :pc start-then :terminate-at end-then :statements []) insns)
        {else-stack :stack else-stmnts :statements} (process-insns (assoc ctx :pc start-else :terminate-at end-else :statements []) insns)

        statement? (= stack then-stack else-stack)

        [then else] (if statement?
                      [then-stmnts else-stmnts]
                      [(conj then-stmnts (peek then-stack))
                       (conj else-stmnts (peek else-stack))])]

    {:then (->do then)
     :else (->do else)
     :statement? statement?}))

(defn goto-label [{:insn/keys [jump-offset label]}]
  (+ jump-offset label))

(defmethod process-insn :ifnull [{:keys [stack jump-table insns] :as ctx} {:insn/keys [label] :as insn}]
  (let [null-label (goto-label insn)

        goto-end-insn (nth insns (-> (get jump-table null-label) (- 1)))
        end-label (goto-label goto-end-insn)

        goto-else-insn (nth insns (-> (get jump-table label) (+ 2)))
        else-label (goto-label goto-else-insn)

        {then-label :insn/label} (nth insns (-> (get jump-table label) (+ 3)))

        [test _] (peek-n stack 2)

        {:keys [then else statement?]} (process-if ctx [then-label (:insn/label goto-end-insn)] [else-label end-label])]

    (-> ctx
        (update :stack pop-n 2)
        (assoc :pc end-label)
        (update (if statement? :statements :stack)
                conj {:op :if
                      :test test
                      :then then
                      :else else}))))

(defmethod process-insn :ifeq [{:keys [stack jump-table insns] :as ctx} {:insn/keys [label] :as insn}]
  (let [else-label (goto-label insn)

        goto-end-insn (nth insns (-> (get jump-table else-label) (- 2)))
        end-label (goto-label goto-end-insn)

        {then-label :insn/label} (nth insns (-> (get jump-table label) (+ 1)))

        test (peek stack)

        {:keys [then else statement?]} (process-if ctx [then-label (:insn/label goto-end-insn)] [else-label end-label])]

    (-> ctx
        (update :stack pop)
        (assoc :pc end-label)
        (update (if statement? :statements :stack)
                conj {:op :if
                      :test test
                      :then then
                      :else else}))))

(defmethod process-insn ::bc/number-compare [{:keys [stack jump-table insns] :as ctx} {:insn/keys [label] :as insn}]
  (let [offset (if (= "if_icmpne" (:insn/name insn)) 0 1)
        insn (nth insns (-> (get jump-table label) (+ offset)))

        op (case (:insn/name insn)
             "ifle" ">"
             "ifge" "<"
             "ifne" "="
             "iflt" ">="
             "ifgt" "<="
             "if_icmpne" "=")

        else-label (goto-label insn)

        goto-end-insn (nth insns (-> (get jump-table else-label) (- 2)))
        end-label (goto-label goto-end-insn)

        {then-label :insn/label} (nth insns (-> (get jump-table label) (+ offset 1)))

        [a b] (peek-n stack 2)

        {:keys [then else statement?]} (process-if ctx [then-label (:insn/label goto-end-insn)] [else-label end-label])]

    (-> ctx
        (update :stack pop-n 2)
        (assoc :pc end-label)
        (update (if statement? :statements :stack)
                conj {:op :if
                      :test {:op :invoke :fn {:op :var :ns "clojure.core" :name op} :args [a b]}
                      :then then
                      :else else}))))

;; (defmethod process-insn :goto [{:keys [stack local-variable-table] :as ctx} insn]
;;   ;; WIP ONLY works for fn loops for now, mus be rewritten to support loops, branches
;;   (let [jump-label (goto-label insn)
;;         locals (sort (map key (filter #(-> % val :start-index (= jump-label)) local-variable-table)))
;;         args (mapv local-variable-table locals)]
;;     ;; WIP conditionals
;;     (-> ctx
;;         (update :stack conj {:op :recur
;;                              :args args}))))

(defn find-local-variable [{:keys [local-variable-table]} index label]
  (->> local-variable-table
       (filter (comp #{index} :index))
       (filter (comp (partial >= label) :start-label))
       (filter (comp (partial < label) :end-label))
       (sort-by :start-label)
       (first)))

(defmethod process-insn ::bc/load-insn [ctx {:insn/keys [local-variable-element label]}]
  (let [{:insn/keys [target-index]} local-variable-element]
    (if-let [local (find-local-variable ctx target-index label)]
      (-> ctx
          (update :stack conj local))
      (throw (Exception. ":(")))))

(defn init-local-variable? [{:insn/keys [label length]} {:keys [start-label]}]
  (= (+ label length) start-label))

;; WIP loop/letfn
(defn process-lexical-block [{:keys [insns stack jump-table pc] :as ctx} {:keys [end-label] :as local-variable} init]
  (let [{:insn/keys [length]} (nth insns (get jump-table pc))
        {body-stack :stack body-stmnts :statements} (process-insns (-> ctx
                                                                       (update :pc + length)
                                                                       (assoc :terminate-at end-label)
                                                                       (assoc :statements []))
                                                                   insns)
        statement? (= stack body-stack)
        body (->do (if statement? body-stmnts (conj body-stmnts (peek body-stack))))]
    (-> ctx
        (assoc :pc end-label)
        (update (if statement? :statements :stack)
                conj {:op :let
                      :local-variable {:op :local-variable
                                       :local-variable local-variable
                                       :init init}
                      :body body}))))

(defn find-init-local [{:keys [local-variable-table]} label]
  (->> local-variable-table
       (filter (comp (partial = label) :start-label))
       (filter (comp (partial < label) :end-label))
       (sort-by :start-label)
       (first)))

(defmethod process-insn :instanceof [{:keys [stack] :as ctx} {:insn/keys [pool-element]}]
  (let [{:insn/keys [target-type]} pool-element
        instance (peek stack)]
    (-> ctx
        (update :stack pop)
        (update :stack conj {:op :invoke
                             :fn {:op :var
                                  :ns "clojure.core"
                                  :name "instance?"}
                             :args [{:op :const
                                     :val (symbol target-type)}
                                    instance]}))))

(defmethod process-insn :pop [{:keys [stack] :as ctx} {:insn/keys [label length]}]
  (let [statement (peek stack)
        ctx (-> ctx (update :stack pop))]
    (if-let [local-variable (find-init-local ctx (+ label length))]
      (process-lexical-block ctx local-variable statement)
      (-> ctx
          (update :statements conj statement)))))

(defmethod process-insn ::bc/store-insn [{:keys [stack insns jump-table] :as ctx}
                                         {:insn/keys [local-variable-element label length] :as insn}]
  (let [{:insn/keys [target-index]} local-variable-element
        {:keys [start-label end-label] :as local-variable} (find-local-variable ctx target-index (+ label length))
        init (peek stack)
        initialized-local-variable (assoc local-variable :init init)
        ctx (-> ctx
                (update :stack pop)
                (update :local-variable-table disj local-variable)
                (update :local-variable-table conj initialized-local-variable))]
    (if (init-local-variable? insn local-variable)
      ;; initialize let context
      (process-lexical-block ctx local-variable init)
      ctx)))

(defmethod process-insn :invokespecial [{:keys [stack] :as ctx} {:insn/keys [pool-element]}]
  (let [{:insn/keys [target-class target-arg-types]} pool-element
        argc (count (conj target-arg-types target-class))]
    (-> ctx
        (update :stack pop-n argc))))

(defmethod process-insn ::bc/invoke-instance-method [{:keys [stack] :as ctx} {:insn/keys [pool-element]}]
  (let [{:insn/keys [target-class target-name target-arg-types]} pool-element
        argc (count (conj target-arg-types target-class))
        [target & args] (peek-n stack argc)]
    (-> ctx
        (update :stack pop-n argc)
        (update :stack conj {:op :invoke-instance
                             :method target-name
                             :target target
                             :arg-types target-arg-types
                             :target-class target-class
                             :args args}))))

(defmethod process-insn :putstatic [{:keys [stack class-name] :as ctx} {:insn/keys [pool-element]}]
  (let [{:insn/keys [target-class target-name]} pool-element
        val (peek stack)]
    (-> ctx
        (update :stack pop)
        ;; WIP if not produce set!, logic will have to change for deftype as we can set! to this
        (cond-> (= class-name target-class)
          (update :fields assoc target-name val)))))

(defmethod process-insn :getstatic [{:keys [fields class-name] :as ctx} {:insn/keys [pool-element]}]
  (let [{:insn/keys [target-class target-name]} pool-element]
    ;; WIP logic will have to change for deftype/defrecord as we can get from this
    (if (= target-class class-name)
      (update ctx :stack conj (get fields target-name))
      (update ctx :stack conj {:op :static-field
                               :target target-class
                               :field target-name}))))

(defmethod process-insn :invokestatic [{:keys [stack] :as ctx} {:insn/keys [pool-element]}]
  (let [{:insn/keys [target-class target-name target-arg-types]} pool-element
        argc (count target-arg-types)
        args (peek-n stack argc)]
    (-> ctx
        (update :stack pop-n argc)
        (update :stack conj {:op :invoke-static
                             :target target-class
                             :method target-name
                             :arg-types target-arg-types
                             :args args}))))

(defmethod process-insn :checkcast [{:keys [stack] :as ctx} {:insn/keys [pool-element]}]
  (let [{:insn/keys [target-type]} pool-element
        target (peek stack)]
    (-> ctx
        (update :stack pop)
        (update :stack conj (assoc target :cast target-type)))))

(defn merge-local-variable-table [ctx local-variable-table]
  (update ctx :local-variable-table set/union
         (->> (for [{:local-variable/keys [name index start-label end-label]} local-variable-table]
                {:op :local
                 :start-label start-label
                 :end-label end-label
                 :index index
                 :name name})
              (into #{}))))

(defn process-method-insns [{:keys [fn-name] :as ctx} {:method/keys [bytecode jump-table local-variable-table flags]}]
  (let [ctx (-> ctx
                (merge initial-local-ctx {:jump-table jump-table})
                (cond-> (not (:static flags))
                  (update :local-variable-table conj {:op :local
                                                      :index 0
                                                      :name fn-name
                                                      :start-label 0
                                                      :end-label (-> bytecode last :insn/label)}))
                (merge-local-variable-table local-variable-table)
                (assoc :insns bytecode)
                (process-insns bytecode))]
    (apply dissoc ctx :jump-table (keys initial-local-ctx))))

(defn process-static-init [ctx {:class/keys [methods] :as bc}]
  (let [method (u/find-method methods {:method/name "<clinit>"})]
    (process-method-insns ctx method)))

(defn process-init [ctx {:class/keys [methods] :as bc}]
  (let [method (u/find-method methods {:method/name "<init>"})]
    (process-method-insns ctx method)))

;; WIP push args, not just this

(defn decompile-fn-method [ctx {:method/keys [return-type local-variable-table] :as method}]
  (let [{:keys [ast]} (process-method-insns ctx method)]
    {:op :fn-method
     :args (for [{:local-variable/keys [name type start-label index]} (->> local-variable-table
                                                                           (sort-by :local-variable/index))
                 :when (= start-label 0)]
             {:name name
              :type type})
     :body ast}))

(defn decompile-fn-methods [ctx {:class/keys [methods] :as bc}]
  (let [invokes (u/find-methods methods {:method/name "invoke"})
        invokes-static (u/find-methods methods {:method/name "invokeStatic"})
        ;; WIP is DL enabled per fn or per fn method? I can't remember. Defensively assume the latter for now
        invoke-methods (into invokes-static (for [{:method/keys [arg-types] :as invoke} invokes
                                                  :let [argc (count arg-types)]
                                                  :when (empty? (filter (fn [{:method/keys [arg-types]}]
                                                                          (= (count arg-types) argc))
                                                                        invokes-static))]
                                              invoke))
        methods-asts (mapv (partial decompile-fn-method ctx) invoke-methods)]
    {:op :fn
     :fn-methods methods-asts}))

(defn decompile-fn [{class-name :class/name
                     :class/keys [methods] :as bc}
                    ctx]
  (let [[ns fn-name] ((juxt namespace name) (u/demunge class-name))
        ast (-> ctx
                (assoc :fn-name fn-name)
                (assoc :class-name class-name)
                (process-static-init bc)
                (process-init bc)
                (decompile-fn-methods bc))]
    {:ns ns
     :fn-name fn-name
     :ast ast}))

(defn bc->ast [{:class/keys [super] :as bc}]
  ;; TODO: record, type, ns, genclass, geninterface, proxy, varargs
  (if (#{"clojure.lang.AFunction"} super)
    (decompile-fn bc initial-ctx)
    (throw (Exception. ":("))))

(comment
  (require '[clojure.tools.decompiler.bc :as bc]
           '[clojure.java.io :as io])

  (def filename (-> "test$foo.class" io/resource .getFile))
  (def bc (bc/analyze-classfile filename))

  (bc->ast bc)

  (fn* ([] "yoo"))

  )

;;; let/loop/letfn/case/deftype/reify/set!, varargs
