(ns clojure.tools.decompiler.sugar)

;; WIP this could use a postwalk

(defmulti -ast->sugared-ast :op)

(defmethod -ast->sugared-ast :const [ast]
  ast)

(defmethod -ast->sugared-ast :keyword [ast]
  ast)

(defmethod -ast->sugared-ast :fn [ast]
  (-> ast
      (update :fn-methods #(mapv -ast->sugared-ast %))))

(defmethod -ast->sugared-ast :fn-method [ast]
  (-> ast
      (update :body -ast->sugared-ast)))

(defmethod -ast->sugared-ast :invoke-static [{:keys [target method] :as ast}]
  (let [{:keys [args] :as ast} (update ast :args #(mapv -ast->sugared-ast %))]

    (cond

      (and (= target "clojure.lang.RT")
           (= method "keyword")
           (= 2 (count args))
           (every? (comp #{:const} :op) args))

      {:op :keyword
       :ns (:val (first args))
       :name (:val (second args))}

      :else
      ast)))

(defn ast->sugared-ast [data]
  (-> data
      (update :ast -ast->sugared-ast)))
