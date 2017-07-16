(ns clojure.tools.decompiler.ast
  (:require [clojure.tools.decompiler.stack :refer [peek-n pop-n]]
            [clojure.tools.decompiler.utils :as u]))

(def initial-ctx {:fields {}
                  :ast {}})

(def initial-local-ctx {:stack []
                        :pc 0
                        :local-variable-table {}})

(def insn-h
  (-> (make-hierarchy)
      (derive :ldc ::const-insn)
      (derive :ldc_w ::const-insn)
      (derive :aconst_null ::const-insn)

      (derive :aload ::load-insn)
      (derive :aload_0 ::load-insn)
      (derive :aload_1 ::load-insn)
      (derive :aload_2 ::load-insn)
      (derive :aload_3 ::load-insn)
      (derive :dload ::load-insn)
      (derive :dload_0 ::load-insn)
      (derive :dload_1 ::load-insn)
      (derive :dload_2 ::load-insn)
      (derive :dload_3 ::load-insn)
      (derive :fload ::load-insn)
      (derive :fload_0 ::load-insn)
      (derive :fload_1 ::load-insn)
      (derive :fload_2 ::load-insn)
      (derive :fload_3 ::load-insn)
      (derive :iload ::load-insn)
      (derive :iload_0 ::load-insn)
      (derive :iload_1 ::load-insn)
      (derive :iload_2 ::load-insn)
      (derive :iload_3 ::load-insn)
      (derive :lload ::load-insn)
      (derive :lload_0 ::load-insn)
      (derive :lload_1 ::load-insn)
      (derive :lload_2 ::load-insn)
      (derive :lload_3 ::load-insn)))

;; process-* : bc, ctx -> ctx
;; decompile-* : bc, ctx -> AST

(defmulti process-insn
  (fn [ctx {:insn/keys [name]}] (keyword name))
  :hierarchy #'insn-h)

(defmethod process-insn :default [ctx {:insn/keys [name]}]
  (println "INSN NOT HANDLED:" name)
  ctx)

(defmethod process-insn :return [ctx _]
  ctx)

(defmethod process-insn ::const-insn [ctx {:insn/keys [pool-element]}]
  (-> ctx
      (update :stack conj {:op :const
                           :val (:insn/target-value pool-element)})))

(defmethod process-insn :areturn [{:keys [stack] :as ctx} _]
  (let [ast (peek stack)]
    (-> ctx
        (update :stack pop)
        (assoc :ast ast))))

(defmethod process-insn ::load-insn [{:keys [stack local-variable-table] :as ctx} _]
  (-> ctx
      (update :stack conj (get local-variable-table 0))))

(defmethod process-insn :invokespecial [{:keys [stack] :as ctx} {:insn/keys [pool-element]}]
  (let [{:insn/keys [target-class target-arg-types]} pool-element
        argc (count (conj target-arg-types target-class))]
    (-> ctx
        (update :stack pop-n argc))))

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
    (-> ctx
        ;; WIP if not produce host-field, logic will have to change for deftype/defrecord as we can get from this
        (cond-> (= target-class class-name)
          (update :stack conj (get fields target-name))))))

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

(defn process-insns [{:keys [stack pc jump-table] :as ctx} bc]
  (let [insn-n (get jump-table pc)
        {:insn/keys [length] :as insn} (nth bc insn-n)
        new-ctx (-> (process-insn ctx insn)
                    (update :pc (fn [new-pc]
                                  (if (= new-pc pc)
                                    ;; last insn wasn't an explicit jump, goto next insn
                                    (+ new-pc length)
                                    new-pc))))]
    (if-not (get jump-table (:pc new-ctx))
      ;; pc is out of bounds, we're done
      ;; TODO: do we need to pop the stack?
      new-ctx
      (recur new-ctx bc))))

(defn process-method-insns [ctx {:method/keys [bytecode jump-table]}]
  (let [ctx (merge ctx initial-local-ctx {:jump-table jump-table})
        ctx (process-insns ctx bytecode)]
    (apply dissoc ctx :jump-table (keys initial-local-ctx))))

(defn process-static-init [ctx {:class/keys [methods] :as bc}]
  (let [method (u/find-method methods {:method/name "<clinit>"})]
    (process-method-insns ctx method)))

;; WIP push args, not just this

(defn process-init [{:keys [fn-name] :as ctx} {:class/keys [methods] :as bc}]
  (let [method (u/find-method methods {:method/name "<init>"})]
    (process-method-insns (assoc-in ctx [:local-variable-table 0] {:op :local :name fn-name})
                          method)))

;; WIP push args, not just this

(defn decompile-fn-method [{:keys [fn-name] :as ctx} {:method/keys [return-type arg-types local-variable-table flags]
                                                      :as method}]
  (let [ctx (cond-> ctx (not (:static flags)) (assoc-in [:local-variable-table 0] {:op :local :name fn-name}))
        {:keys [ast]} (process-method-insns ctx method)]
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
