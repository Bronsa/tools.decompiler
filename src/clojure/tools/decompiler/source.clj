(ns clojure.tools.decompiler.source
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

(defmethod process-insn "invokespecial" [{:keys [stack] :as ctx} {:insn/keys [pool-element]}]
  (let [{:insn/keys [target-class target-arg-types]}
        args (stack/pop-n stack (count target-arg-types))]

    {:op :new
     :target target-class
     :args args}))

;; WIP no jump
(defn process-insns [ctx bytecode]
  (let [ctx (merge ctx initial-local-ctx)
        ctx (reduce process-insn ctx bytecode)]
    (dissoc ctx (keys initial-local-ctx))))

(defn process-static-init [ctx {:class/keys [methods] :as bc}]
  (let [{:method/keys [bytecode]} (u/find-method methods {:method/name "<clinit>"})]
    (process-insns ctx bytecode)))

(defn process-init [ctx {:class/keys [methods] :as bc}]
  (let [{:method/keys [bytecode]} (u/find-method methods {:method/name "<init>"})]
    (process-insns ctx bytecode)))

(defn decompile-fn-method [ctx {:method/keys [return-type arg-types bytecode local-variable-table]}]
  (let [{:keys [ast]} (process-insns ctx bytecode)]
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
  (if (#{"clojure.lang.AFunction" "clojure.lang.RestFn"} super)
    (decompile-fn bc initial-ctx)
    (throw (Exception. ":("))))

(comment
  (require '[clojure.tools.decompiler.bc :as bc]
           '[clojure.java.io :as io])

  (def filename (-> "test$bar.class" io/resource .getFile))
  (def bc (bc/analyze-classfile filename))

  (bc->ast bc)

  (fn* ([] "yoo"))

  )
