(ns clojure.tools.decompiler.sugar
  (:require [clojure.tools.decompiler.utils :as u]))

;; WIP this could use a postwalk

(defmulti -ast->sugared-ast :op)

(defmethod -ast->sugared-ast :const [ast]
  ast)

(defmethod -ast->sugared-ast :keyword [ast]
  ast)

(defmethod -ast->sugared-ast :var [ast]
  ast)

(defmethod -ast->sugared-ast :invoke [ast]
  ast)

(defmethod -ast->sugared-ast :fn [ast]
  (-> ast
      (update :fn-methods #(mapv -ast->sugared-ast %))))

(defmethod -ast->sugared-ast :fn-method [ast]
  (-> ast
      (update :body -ast->sugared-ast)))

(defmethod -ast->sugared-ast :invoke-instance [ast]
  ast)

(defmethod -ast->sugared-ast :invoke-static [{:keys [^String target method] :as ast}]
  (let [{:keys [args] :as ast} (update ast :args #(mapv -ast->sugared-ast %))]

    (cond

      (and (= target "clojure.lang.RT")
           (= method "keyword")
           (= 2 (count args))
           (every? (comp #{:const} :op) args))

      {:op :keyword
       :ns (:val (first args))
       :name (:val (second args))}

      ;; best effort for now, should do better to ensure it's a var
      (and (= method "invokeStatic")
           (.contains target "$"))
      (let [[ns fn-name] ((juxt namespace name) (-> target u/ungensym u/demunge))]
       {:op :invoke
        :fn {:op :var
             :ns ns
             :name fn-name}
        :args args})

      :else
      ast)))

(defn ast->sugared-ast [data]
  (-> data
      (update :ast -ast->sugared-ast)))
