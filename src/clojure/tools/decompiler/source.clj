(ns clojure.tools.decompiler.source
  (:require [clojure.tools.decompiler.stack :as stack]
            [clojure.tools.decompiler.utils :as u]))

(def initial-ctx {:stack []
                  :local-variable-table {}
                  :ast {}})

(defn process-insn [ctx insn]
  ctx)

(defn process-insns [ctx bytecode]
  (reduce process-insn ctx bytecode))

;; bc, ctx -> ctx
(defn static-init [{:class/keys [methods] :as bc} ctx]
  (let [{:method/keys [bytecode]} (u/find-method methods {:method/name "<clinit>"})]
    (process-insns ctx bytecode)))

(defn init [{:class/keys [methods] :as bc} ctx]
  (let [{:method/keys [bytecode]} (u/find-method methods {:method/name "<init>"})]
    (process-insns ctx bytecode)))

(defn decompile-fn-methods [bc ctx]
  (let [invokes (u/find-methods methods {:method/name "invoke"})
        invokes-static (u/find-methods methods {:method/name "invokeStatic"})
        invoke-methods (into invoke-static (for [{:method/keys [arg-types] :as invoke} invokes
                                                 :let [argc (count arg-types)]
                                                 :when (empty? (filter (fn [:method/keys [arg-types]]
                                                                         (= (count arg-types) argc))
                                                                       invoke-static))]))
        methods-asts (map (partial decompile-fn-method ctx) invoke-methods)]
    (assoc ctx
           :ast {:op :fn
                 :fn-methods [method-asts]})))

(defn decompile-fn [{class-name :class/name
                     :class/keys [methods] :as bc}
                    ctx]
  (let [class-name (u/demunge class-name)
        ns (namespace class-name)
        fn-name (name class-name)]

    (->> ctx
         (static-init bc)
         (init bc)
         (decompile-fn-methods bc))))

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
