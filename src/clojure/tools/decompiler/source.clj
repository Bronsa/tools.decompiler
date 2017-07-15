(ns clojure.tools.decompiler.source
  (:require [clojure.tools.decompiler.stack :as stack]
            [clojure.tools.decompiler.utils :as u]))

(def initial-ctx {:stack []
                  :local-variable-table {}
                  :ast {}})

;; process-* : bc, ctx -> ctx
;; decompile-* : bc, ctx -> AST

(defn process-insn [ctx insn]
  ctx)

(defn process-insns [ctx bytecode]
  (reduce process-insn ctx bytecode))

(defn process-static-init [ctx {:class/keys [methods] :as bc}]
  (let [{:method/keys [bytecode]} (u/find-method methods {:method/name "<clinit>"})]
    (process-insns ctx bytecode)))

(defn process-init [ctx {:class/keys [methods] :as bc}]
  (let [{:method/keys [bytecode]} (u/find-method methods {:method/name "<init>"})]
    (process-insns ctx bytecode)))

(defn decompile-fn-method [ctx method]
  )

(defn process-fn-methods [ctx {:class/keys [methods] :as bc}]
  (let [invokes (u/find-methods methods {:method/name "invoke"})
        invokes-static (u/find-methods methods {:method/name "invokeStatic"})
        invoke-methods (into invokes-static (for [{:method/keys [arg-types] :as invoke} invokes
                                                  :let [argc (count arg-types)]
                                                  :when (empty? (filter (fn [{:method/keys [arg-types]}]
                                                                          (= (count arg-types) argc))
                                                                        invokes-static))]
                                              invoke))
        methods-asts (map (partial decompile-fn-method ctx) invoke-methods)]
    (assoc ctx
           :ast {:op :fn
                 :fn-methods [methods-asts]})))

(defn decompile-fn [{class-name :class/name
                     :class/keys [methods] :as bc}
                    ctx]
  (let [class-name (u/demunge class-name)
        ns (namespace class-name)
        fn-name (name class-name)]

    (-> ctx
        (process-static-init bc)
        (process-init bc)
        (process-fn-methods bc)
        :ast)))

(defn bc->ast [{:class/keys [super] :as bc}]
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
