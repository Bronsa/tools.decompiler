;;   Copyright (c) Nicola Mometto & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.decompiler.source)

(defmulti -ast->clj :op)

(defmethod -ast->clj :const [{:keys [val]}]
  val)

(defmethod -ast->clj :monitor-enter [{:keys [sentinel]}]
  `(monitor-enter ~(-ast->clj sentinel)))

(defmethod -ast->clj :monitor-exit [{:keys [sentinel]}]
  `(monitor-exit ~(-ast->clj sentinel)))

(defmethod -ast->clj :do [{:keys [statements ret]}]
  `(do ~@(map -ast->clj statements) ~(-ast->clj ret)))

(defmethod -ast->clj :local-variable [{:keys [local-variable init]}]
  `[~(-ast->clj local-variable) ~(-ast->clj init)])

(defmethod -ast->clj :let [{:keys [local-variable body]}]
  `(let* ~(-ast->clj local-variable) ~(-ast->clj body)))

(defmethod -ast->clj :loop [{:keys [local-variables body]}]
  `(loop* [~@(mapcat -ast->clj local-variables)] ~(-ast->clj body)))

(defmethod -ast->clj :if [{:keys [test then else]}]
  `(if ~@(map -ast->clj [test then else])))

(defmethod -ast->clj :set! [{:keys [target field]}]
  `(set! ~(-ast->clj target) ~(-ast->clj field)))

(defmethod -ast->clj :throw [{:keys [ex]}]
  `(throw ~(-ast->clj ex)))

(defmethod -ast->clj :new [{:keys [class args]}]
  `(~(symbol (str class ".")) ~@(map -ast->clj args)))

(defmethod -ast->clj :instance-field [{:keys [instance field]}]
  `((symbol (str ".-" field)) ~(-ast->clj instance)))

(defmethod -ast->clj :local [{:keys [name]}]
  (symbol name))

(defmethod -ast->clj :recur [{:keys [args]}]
  `(recur ~@(map -ast->clj args)))

(defmethod -ast->clj :vector [{:keys [items]}]
  (mapv -ast->clj items))

(defmethod -ast->clj :set [{:keys [items]}]
  (into #{} (map -ast->clj) items))

(defmethod -ast->clj :map [{:keys [items]}]
  (into {} (map vec (partition 2 items))))

(defmethod -ast->clj :array [{:keys [!items]}]
  (object-array (mapv -ast->clj @!items)))

(defmethod -ast->clj :fn [{:keys [fn-methods]}]
  ;; wip meta, fn name
  `(fn* ~@(map -ast->clj fn-methods)))

(defmethod -ast->clj :fn-method [{:keys [args body var-args?]}]
  ;; wip tags
  (let [argv (mapv (comp symbol :name) args)
        argv (if var-args?
               (into (pop argv) ['& (peek argv)])
               argv)]
    `(~argv ~(-ast->clj body))))

(defmethod -ast->clj :static-field [{:keys [target field]}]
  (symbol target field))

(defmethod -ast->clj :invoke-static [{:keys [target method args]}]
  `(~(symbol target method) ~@(map -ast->clj args)))

(defmethod -ast->clj :invoke-instance [{:keys [target method args]}]
  `(~(symbol (str "." method)) ~(-ast->clj target) ~@(map -ast->clj args)))

(defmethod -ast->clj :var [{:keys [ns name]}]
  (symbol ns name))

(defmethod -ast->clj :the-var [{:keys [ns name]}]
  `(var ~(symbol ns name)))

(defmethod -ast->clj :invoke [{:keys [fn args]}]
  `(~(-ast->clj fn) ~@(map -ast->clj args)))

(defmethod -ast->clj :catch [{:keys [local body] :as x}]
  `(catch ~(symbol (:type local)) ~(symbol (:name local)) ~(-ast->clj body)))

(defmethod -ast->clj :try [{:keys [body catches finally]}]
  `(try ~(-ast->clj body)
        ~@(when catches
            (mapv -ast->clj catches))
        ~@(when finally
            [`(finally ~(-ast->clj finally))])))

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
