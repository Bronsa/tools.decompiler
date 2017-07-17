(ns clojure.tools.decompiler.ast
  (:require [clojure.tools.decompiler.stack :refer [peek-n pop-n]]
            [clojure.tools.decompiler.bc :as bc]
            [clojure.tools.decompiler.utils :as u]))

;; WIP casting, type hints

(def initial-ctx {:fields {}
                  :statements []
                  :ast {}})

(def initial-local-ctx {:stack []
                        :pc 0
                        :local-variable-table {}})

;; process-* : bc, ctx -> ctx
;; decompile-* : bc, ctx -> AST

(defmulti process-insn
  (fn [ctx {:insn/keys [name]}] (keyword name))
  :hierarchy #'bc/insn-h)

(defmethod process-insn :default [ctx {:insn/keys [name]}]
  (println "INSN NOT HANDLED:" name)
  ctx)

(defn process-insns [{:keys [stack pc jump-table terminate-at] :as ctx} bc]
  (let [insn-n (get jump-table pc)
        {:insn/keys [length] :as insn} (nth bc insn-n)
        {:keys [pc] :as new-ctx} (-> (process-insn ctx insn)
                                     (update :pc (fn [new-pc]
                                                   (if (= new-pc pc)
                                                     ;; last insn wasn't an explicit jump, goto next insn
                                                     (+ new-pc length)
                                                     new-pc))))]
    (if (or (not (get jump-table pc))
            (= pc terminate-at))
      ;; pc is out of bounds, or explicit return from the block, we're done
      ;; TODO: do we need to pop the stack?
      new-ctx
      (recur new-ctx bc))))

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

(defmethod process-insn :pop [{:keys [stack] :as ctx} _]
  (let [statement (peek stack)]
    (-> ctx
        (update :statements conj statement)
        (update :stack pop))))

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

(defmethod process-insn :areturn [{:keys [stack statements] :as ctx} _]
  (let [ret (peek stack)]
    (-> ctx
        (assoc :stack [] :statements []
               :ast {:op :do
                     :ret ret
                     :statements statements}))))

(defn process-if [{:keys [insns jump-table] :as ctx} [start-then end-then] [start-else end-else]]
  (let [{then-stack :stack then-st :statements} (process-insns (assoc ctx :pc start-then :terminate-at end-then :statements []) insns)
        {else-stack :stack else-st :statements} (process-insns (assoc ctx :pc start-else :terminate-at end-else :statemtns []) insns)

        statement? (->> end-else (get jump-table) (dec) (nth insns) :insn/name #{"pop" "pop2"})

        [then then-st else else-st] (if statement?
                                      [(last then-st) (butlast then-st)
                                       (last else-st) (butlast else-st)]
                                      [(peek then-stack) then-st
                                       (peek else-stack) else-st])]

    {:then then
     :then-st then-st
     :else else
     :else-st else-st
     :statement? statement?}))

(defn goto-label [{:insn/keys [jump-offset label] :as insn}]
  (+ jump-offset label))

;;   >>>test
;;   dup
;;   ifnull -> null-label
;;   false
;;   ifeq -> else-label
;; then-label:
;;   >>>then
;;   goto -> end-label
;; null-label:
;;   pop
;; else-label:
;;   >>>else
;; end-label:

(defmethod process-insn :ifnull [{:keys [stack local-variable-table jump-table insns] :as ctx} {:insn/keys [label] :as insn}]
  (let [null-label (goto-label insn)

        goto-end-insn  (nth insns (-> (get jump-table null-label) (- 1)))
        end-label (goto-label goto-end-insn)

        goto-else-insn (nth insns (-> (get jump-table label) (+ 2)))
        else-label (goto-label goto-else-insn)

        then-insn (nth insns (-> (get jump-table label) (+ 3)))
        then-label (:insn/label then-insn)

        [test _] (peek-n stack 2)

        {:keys [then then-st else else-st statement?]} (process-if ctx [then-label (:insn/label goto-end-insn)] [else-label end-label])]

    (-> ctx
        (update :stack pop-n 2)
        (assoc :pc end-label)
        (update (if statement? :statements :stack)
                conj {:op :if
                      :test test
                      :then {:op :do
                             :statements then-st
                             :ret then}
                      :else {:op :do
                             :statements else-st
                             :ret else}}))))

(defmethod process-insn :goto [{:keys [stack local-variable-table] :as ctx} insn]
  ;; WIP ONLY works for fn loops for now, mus be rewritten to support loops, branches
  (let [jump-label (goto-label insn)
        locals (sort (map key (filter #(-> % val :start-index (= jump-label)) local-variable-table)))
        args (mapv local-variable-table locals)]
    ;; WIP conditionals
    (-> ctx
        (update :stack conj {:op :recur
                             :args args}))))

(defmethod process-insn ::bc/load-insn [{:keys [local-variable-table] :as ctx} {:insn/keys [local-variable-element]}]
  (let [{:insn/keys [target-index]} local-variable-element]
    (if-let [[_ local] (find local-variable-table target-index)]
      (-> ctx
          (update :stack conj local))
      (throw (Exception. ":(")))))

(defmethod process-insn ::bc/store-insn [{:keys [stack] :as ctx} {:insn/keys [local-variable-element]}]
  (let [{:insn/keys [target-index]} local-variable-element
        val (peek stack)]
    (-> ctx
        (update :stack pop)
        (update :local-variable-table assoc target-index val))))

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
  (update ctx :local-variable-table merge
         (->> (for [[idx {:local-variable/keys [name start-index]}] local-variable-table]
                [idx {:op :local
                      :start-index start-index
                      :name name}])
              (into {}))))

(defn process-method-insns [{:keys [fn-name] :as ctx} {:method/keys [bytecode jump-table local-variable-table]}]
  (let [ctx (-> ctx
                (merge initial-local-ctx {:jump-table jump-table})
                (assoc-in [:local-variable-table 0] {:op :local :name fn-name})
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

(defn decompile-fn-method [ctx {:method/keys [return-type arg-types local-variable-table flags]
                                :as method}]
  (let [{:keys [ast]} (process-method-insns ctx method)]
    {:op :fn-method
     :args (for [i (range (count arg-types))
                 ;; WIP doesn't work if there's no local-variable-table
                 :let [{:local-variable/keys [name start-index type]} (get local-variable-table i)]
                 :when (zero? start-index)]
             {:type type
              :name name})
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
  ;; TODO: record, type, ns, genclass, geninterface, proxy
  (if (#{"clojure.lang.AFunction" "clojure.lang.RestFn"} super)
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
