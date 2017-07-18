(ns clojure.tools.decompiler.sugar
  (:require [clojure.tools.decompiler.utils :as u]
            [clojure.edn :as e]))

;; WIP this could use a postwalk

(defmulti -ast->sugared-ast :op)

(defmethod -ast->sugared-ast :const [ast]
  ast)

(defmethod -ast->sugared-ast :local-variable [ast]
  (-> ast
      (update :init -ast->sugared-ast)))

(defmethod -ast->sugared-ast :let [ast]
  (-> ast
      (update :local-variable -ast->sugared-ast)
      (update :body -ast->sugared-ast)))

(defmethod -ast->sugared-ast :loop [ast]
  (-> ast
      (update :local-variables #(mapv -ast->sugared-ast %))
      (update :body -ast->sugared-ast)))

(defmethod -ast->sugared-ast :monitor-enter [ast]
  ast)

(defmethod -ast->sugared-ast :monitor-exit [ast]
  ast)

(defmethod -ast->sugared-ast :do [ast]
  (let [{:keys [statements ret] :as ast} (-> ast
                                             (update :statements #(mapv -ast->sugared-ast %))
                                             (update :ret -ast->sugared-ast))]
    (if (empty? statements)
      ret
      ast)))

(defmethod -ast->sugared-ast :if [ast]
  (-> ast
      (update :test -ast->sugared-ast)
      (update :then -ast->sugared-ast)
      (update :else -ast->sugared-ast)))

(defmethod -ast->sugared-ast :set [ast]
  ast)

(defmethod -ast->sugared-ast :recur [ast]
  (-> ast
      (update :args #(mapv -ast->sugared-ast %))))

(defmethod -ast->sugared-ast :vector [ast]
  ast)

(defmethod -ast->sugared-ast :array [ast]
  ast)

(defmethod -ast->sugared-ast :local [ast]
  ast)

(defmethod -ast->sugared-ast :var [ast]
  ast)

(defmethod -ast->sugared-ast :the-var [ast]
  ast)

(defmethod -ast->sugared-ast :static-field [{:keys [target field] :as ast}]
  (cond
    (and (#{"clojure.lang.PersistentList" "clojure.lang.PersistentVector"
            "clojure.lang.PersistentArrayMap" "clojure.lang.PersistentHashSet"} target)
         (= "EMPTY" field))
    {:op :const
     :val (case target
            "clojure.lang.PersistentList" ()
            "clojure.lang.PersistentVector" []
            "clojure.lang.PersistentArrayMap" {}
            "clojure.lang.PersistentHashSet" #{})}

    (and (= target "java.lang.Boolean")
         (#{"TRUE" "FALSE"} field))
    {:op :const
     :val (case field "TRUE" true "FALSE" false)}

    :else
    ast))

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
          (#{"getRoot" "getRawRoot"} method)
          (= (:op target) :the-var))

     {:op :var
      :name (:name target)
      :ns (:ns target)}

     :else
     ast)))

(defmethod -ast->sugared-ast :invoke-static [{:keys [^String target method arg-types] :as ast}]
  (let [{:keys [args] :as ast} (update ast :args #(mapv -ast->sugared-ast %))]

    (cond

      (and (= target "clojure.lang.RT")
           (= method "keyword")
           (= 2 (count args))
           (every? (comp #{:const} :op) args))

      {:op :const
       :val (keyword (:val (first args)) (:val (second args)))}

      (and (= target "clojure.lang.RT")
           (= method "readString")
           (= 1 (count args))
           (string? (-> args first :val))
           ((some-fn integer? bigdec?) (try (e/read-string (-> args first :val)) (catch Exception _))))

      {:op :const
       :val (e/read-string (-> args first :val))}

      ;; WIP: this is too aggressive, might throw away useful casts
      (and (= target "clojure.lang.RT")
           (#{"doubleCast" "intCast" "box" "charCast" "booleanCast" "byteCast"
              "shortCast" "longCast" "floatCast" "uncheckedDoubleCast"
              "uncheckedIntCast" "uncheckedCharCast" "uncheckedByteCast"
              "uncheckedShortCast" "uncheckedLongCast" "uncheckedFloatCast"} method)
           (= 1 (count args)))

      (first args)

      (and (= target "clojure.lang.Numbers")
           (= "num" method)
           (= 1 (count args)))

      (first args)

      (and (= target "clojure.lang.Numbers")
           (#{"add" "divide" "multiply" "minus" "multiplyP" "minusP" "addP"
              "min" "max" "equiv" "gte" "lte" "gt" "lt"} method)
           (= 2 (count args)))

      {:op :invoke
       :fn {:op :var
            :ns "clojure.core"
            :name ({"add" "+"
                    "divide" "/"
                    "multiply" "*"
                    "minus" "-"
                    "multiplyP" "*'"
                    "minusP" "-'"
                    "addP" "+'"
                    "min" "min"
                    "max" "max"
                    "equiv" "="
                    "gte" ">="
                    "lte" "<="
                    "gt" ">"
                    "lt" "<"} method)}
       :args args}

      (and (= target "clojure.lang.Symbol")
           (= method "intern")
           (= 2 (count args))
           (every? (comp #{:const} :op) args))

      {:op :const
       :val (list 'quote (symbol (:val (first args)) (:val (second args))))}

      (and (= method "valueOf")
           (#{"java.lang.Long" "java.lang.Double"} target)
           (= 1 (count args))
           (-> args (first) :op (= :const)))

      {:op :const
       :val (-> args (first) :val)}

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
        (let [args (if (= "clojure.lang.ISeq" (last arg-types))
                     ;; variadic invoke, unroll last arg
                     (let [[args varargs] ((juxt butlast last) args)]
                       (into (vec args) (->> varargs :args first :!items deref (mapv -ast->sugared-ast))))
                     args)]
          {:op :invoke
           :fn {:op :var
                :ns ns
                :name fn-name}
           :args args}))

      (and (= target "clojure.lang.Tuple")
           (= method "create"))

      {:op :vector
       :items args}

      (and (= target "clojure.lang.RT")
           (#{"vector" "set" "mapUniqueKeys" "map"} method)
           (= (-> args first :op) :array))
      {:op ({"vector" :vector "set" :set "mapUniqueKeys" :map "map" :map} method)
       :items (-> args first :!items deref)}

      :else
      ast)))

(defn ast->sugared-ast [data]
  (-> data
      (update :ast -ast->sugared-ast)))
