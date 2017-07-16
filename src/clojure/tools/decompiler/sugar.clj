(ns clojure.tools.decompiler.sugar
  (:require [clojure.tools.decompiler.utils :as u]))

;; WIP this could use a postwalk

(defmulti -ast->sugared-ast :op)

(defmethod -ast->sugared-ast :const [ast]
  ast)

(defmethod -ast->sugared-ast :do [ast]
  (-> ast
      (update :statements #(mapv -ast->sugared-ast %))
      (update :ret -ast->sugared-ast)))

(defmethod -ast->sugared-ast :set [ast]
  ast)

(defmethod -ast->sugared-ast :vector [ast]
  ast)

(defmethod -ast->sugared-ast :keyword [ast]
  ast)

(defmethod -ast->sugared-ast :array [ast]
  ast)

(defmethod -ast->sugared-ast :local [ast]
  ast)

(defmethod -ast->sugared-ast :var [ast]
  ast)

(defmethod -ast->sugared-ast :the-var [ast]
  ast)

(defmethod -ast->sugared-ast :invoke [ast]
  (-> ast
      (update :args #(mapv -ast->sugared-ast %))
      (update :fn -ast->sugared-ast)))

(defmethod -ast->sugared-ast :fn [ast]
  (-> ast
      (update :fn-methods #(mapv -ast->sugared-ast %))))

(defmethod -ast->sugared-ast :fn-method [ast]
  (-> ast
      (update :body -ast->sugared-ast)))

(defmethod -ast->sugared-ast :invoke-instance [{:keys [method target-class] :as ast}]
  (let [{:keys [target args] :as ast} (-> ast
                                         (update :target -ast->sugared-ast)
                                         (update ast :args #(mapv -ast->sugared-ast %)))]
   (cond

     (and (= target-class "clojure.lang.IFn")
          (= method "invoke"))

     {:op :invoke
      :fn target
      :args args}

     (and (= target-class "clojure.lang.Var")
          (= method "getRawRoot")
          (= (:op target) :the-var))

     {:op :var
      :name (:name target)
      :ns (:ns target)}

     :else
     ast)))

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

      (and (= target "clojure.lang.RT")
           (= method "var")
           (= 2 (count args))
           (every? (comp #{:const} :op) args))

      {:op :the-var
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

      (and (= target "clojure.lang.Tuple")
           (= method "create"))

      {:op :vector
       :items args}

      (and (= target "clojure.lang.RT")
           (#{"vector" "set" "mapUniqueKeys"} method)
           (= (-> args first :op) :array))
      {:op ({"vector" :vector "set" :set "mapUniqueKeys" :map} method)
       :items (-> args first :!items deref)}

      :else
      ast)))

(defn ast->sugared-ast [data]
  (-> data
      (update :ast -ast->sugared-ast)))
