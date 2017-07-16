(ns clojure.tools.decompiler.source)

(defmulti -ast->clj :op)

(defmethod -ast->clj :const [{:keys [val]}]
  val)

(defmethod -ast->clj :fn [{:keys [fn-methods]}]
  `(fn* ~@(map -ast->clj fn-methods)))

(defmethod -ast->clj :fn-method [{:keys [args body]}]
  ;; wip meta, name
  `(~(mapv (comp symbol :name) args) ~(-ast->clj body)))

(defmethod -ast->clj :keyword [{:keys [ns name]}]
  (keyword ns name))

(defmethod -ast->clj :invoke-static [{:keys [target method args]}]
  `(~(symbol target method) ~@(map -ast->clj args)))

(defmethod -ast->clj :var [{:keys [ns name]}]
  (symbol ns name))

(defmethod -ast->clj :invoke [{:keys [fn args]}]
  `(~(-ast->clj fn) ~@(map -ast->clj args)))

(defn ast->clj [{:keys [ast] :as fast}]
  (-> fast
      (assoc :source (-ast->clj ast))))


(comment
  (require '[clojure.tools.decompiler.bc :as bc]
           '[clojure.tools.decompiler.ast :as ast]
           '[clojure.tools.decompiler.sugar :as sa]
           '[clojure.java.io :as io])

  (-> "test$baz.class"
      io/resource
      .getFile
      (bc/analyze-classfile)
      (ast/bc->ast)
      (sa/ast->sugared-ast)
      (ast->clj))

  )
