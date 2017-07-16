(ns clojure.tools.decompiler.ast
  (:require [clojure.tools.decompiler.stack :as stack]
            [clojure.tools.decompiler.utils :as u]))

;; WIP worth conforming the AST with t.a?

(def initial-ctx {:fields {}
                  :ast {}})

(def initial-local-ctx {:stack []
                        :pc 0
                        :local-variable-table {}})

;; process-* : bc, ctx -> ctx
;; decompile-* : bc, ctx -> AST

(defn process-insn [ctx insn]
  ctx)

(defmulti process-insn (fn [ctx {:insn/keys [name]}] name))

(defmethod process-insn :default [ctx {:insn/keys [name]}]
  (println "INSN NOT HANDLED:" name)
  ctx)

(defmethod process-insn "return" [ctx _]
  ctx)

(defmethod process-insn "ldc" [ctx {:insn/keys [pool-element]}]
  (-> ctx
      (update :stack conj {:op :const
                           :val (:insn/target-value pool-element)})))

(defmethod process-insn "areturn" [{:keys [stack] :as ctx} _]
  (let [ast (peek stack)]
    (-> ctx
        (update :stack pop)
        (assoc :ast ast))))

;; (defmethod process-insn "invokespecial" [{:keys [stack] :as ctx} {:insn/keys [pool-element]}]
;;   (let [{:insn/keys [target-class target-arg-types]} pool-element
;;         args (stack/pop-n stack (count target-arg-types))]

;;     (assoc ctx
;;            :stack (conj stack {:op :invoke-ctor
;;                                :target target-class
;;                                :args args}))))

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

(defn process-init [ctx {:class/keys [methods] :as bc}]
  (let [method (u/find-method methods {:method/name "<init>"})]
    (process-method-insns ctx method)))

(defn decompile-fn-method [ctx {:method/keys [return-type arg-types local-variable-table] :as method}]
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
  (let [class-name (u/demunge class-name)
        ns (namespace class-name)
        fn-name (name class-name)
        ast (-> ctx
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
