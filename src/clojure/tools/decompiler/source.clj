(ns clojure.tools.decompiler.source)

(defmulti -ast->clj :op)

(defmethod -ast->clj :const [{:keys [val]}]
  val)

(defmethod -ast->clj :fn [{:keys [fn-methods]}]
  `(fn* ~@(map -ast->clj fn-methods)))

(defmethod -ast->clj :fn-method [{:keys [args body]}]
  ;; wip meta
  `(~(mapv :name args) ~(-ast->clj body)))

(defn ast->clj [{:keys [ast]}]
  (-ast->clj ast))

(comment
  (require '[clojure.tools.decompiler.bc :as bc]
           '[clojure.tools.decompiler.ast :as ast]
           '[clojure.java.io :as io])

  (-> "test$foo.class"
      io/resource
      .getFile
      (bc/analyze-classfile)
      (ast/bc->ast)
      (ast->clj))

  )
