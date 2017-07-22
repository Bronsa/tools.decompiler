(ns clojure.tools.decompiler.sugar
  (:require [clojure.tools.decompiler.utils :as u]
            [clojure.edn :as e]))

;; WIP this could use a postwalk

(defmulti ast->sugared-ast :op)

(defmethod ast->sugared-ast :const [ast]
  ast)

(defmethod ast->sugared-ast :case [ast]
  (-> ast
      (update :test ast->sugared-ast)
      (update :default ast->sugared-ast)
      (update :exprs #(mapv (fn [[type match test expr]]
                              [type match (if (= :collision type) test (ast->sugared-ast test)) (ast->sugared-ast expr)])
                            %))))

(defmethod ast->sugared-ast :local-variable [ast]
  (-> ast
      (update :init ast->sugared-ast)))

(defmethod ast->sugared-ast :try [{:keys [finally catches] :as ast}]
  (-> ast
      (update :body ast->sugared-ast)
      (cond-> finally
        (update :finally ast->sugared-ast))
      (cond-> catches
        (update :catches #(mapv ast->sugared-ast %)))))

(defmethod ast->sugared-ast :catch [ast]
  (-> ast
      (update :body ast->sugared-ast)))

(defmethod ast->sugared-ast :let [ast]
  (let [{:keys [body local-variables] :as ast} (-> ast
                                                   (update :local-variables #(mapv ast->sugared-ast %))
                                                   (update :body ast->sugared-ast))]
    (if (-> body :op (= :let))
      (update body :local-variables (fn [lvs] (into local-variables lvs)))
      ast)))

(defmethod ast->sugared-ast :letfn [ast]
  (-> ast
      (update :local-variables #(mapv ast->sugared-ast %))
      (update :body ast->sugared-ast)))

(defmethod ast->sugared-ast :set! [ast]
  (-> ast
      (update :target ast->sugared-ast)
      (update :val ast->sugared-ast)))

(defmethod ast->sugared-ast :loop [ast]
  (-> ast
      (update :local-variables #(mapv ast->sugared-ast %))
      (update :body ast->sugared-ast)))

(defmethod ast->sugared-ast :new [ast]
  (-> ast
      (update :args #(mapv ast->sugared-ast %))))

(defmethod ast->sugared-ast :throw [ast]
  (-> ast
      (update :ex ast->sugared-ast)))

(defmethod ast->sugared-ast :monitor-enter [ast]
  ast)

(defmethod ast->sugared-ast :monitor-exit [ast]
  ast)

(defmethod ast->sugared-ast :do [ast]
  (let [{:keys [statements ret] :as ast} (-> ast
                                             (update :statements #(mapv ast->sugared-ast %))
                                             (update :ret ast->sugared-ast))]
    (if (empty? statements)
      ret
      ast)))

(defmethod ast->sugared-ast :if [ast]
  (-> ast
      (update :test ast->sugared-ast)
      (update :then ast->sugared-ast)
      (update :else ast->sugared-ast)))

(defmethod ast->sugared-ast :set [ast]
  ast)

(defmethod ast->sugared-ast :recur [ast]
  (-> ast
      (update :args #(mapv ast->sugared-ast %))))

(defmethod ast->sugared-ast :vector [ast]
  ast)

(defmethod ast->sugared-ast :array [{:keys [!items] :as ast}]
  (swap! !items #(mapv ast->sugared-ast %))
  ast)

(defmethod ast->sugared-ast :local [ast]
  ast)

(defmethod ast->sugared-ast :var [ast]
  ast)

(defmethod ast->sugared-ast :the-var [ast]
  ast)

(defmethod ast->sugared-ast :instance-field [ast]
  (-> ast
      (update :instance ast->sugared-ast)))

(defmethod ast->sugared-ast :static-field [{:keys [target field] :as ast}]
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

(defmethod ast->sugared-ast :invoke [ast]
  (-> ast
      (update :args #(mapv ast->sugared-ast %))
      (update :fn ast->sugared-ast)))

(defmethod ast->sugared-ast :fn [ast]
  (-> ast
      (update :fn-methods #(mapv ast->sugared-ast %))))

(defmethod ast->sugared-ast :fn-method [ast]
  (-> ast
      (update :body ast->sugared-ast)))

(defmethod ast->sugared-ast :invoke-instance [{:keys [method target-class] :as ast}]
  (let [{:keys [target args] :as ast} (-> ast
                                         (update :target ast->sugared-ast)
                                         (update :args #(mapv ast->sugared-ast %)))]
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

(def math-ops
  {"add" "+"
   "addP" "+'"
   "and" "bit-and"
   "andNot" "bit-and-not"
   "clearBit" "bit-clear"
   "dec" "dec"
   "decP" "dec"
   "divide" "/"
   "equiv" "=="
   "flipBit" "bit-flip"
   "gt" ">"
   "gte" ">="
   "inc" "inc"
   "incP" "inc"
   "isNeg" "neg?"
   "isPos?" "pos?"
   "isZero" "zero?"
   "lt" "<"
   "lte" "<="
   "max" "max"
   "min" "min"
   "minus" "-"
   "minusP" "-'"
   "multiply" "*"
   "multiplyP" "*'"
   "not" "bit-not"
   "or" "bit-or"
   "quotient" "quot"
   "remainder" "rem"
   "setBit" "bit-set"
   "shiftLeft" "bit-shift-left"
   "shiftRight" "bit-shift-right"
   "testBit" "bit-test"
   "float_array" "float-array"
   "short_array" "short-array"
   "int_array" "int-array"
   "double_array" "double-array"
   "long_array" "long-array"
   "char_array" "char-array"
   "byte_array" "byte-array"
   "boolean_array" "boolean-array"
   "booleans" "booleans"
   "bytes" "bytes"
   "shorts" "shorts"
   "ints" "ints"
   "chars" "chars"
   "longs" "longs"
   "doubles" "doubles"
   "floats" "floats"
   "unchecked_add" "+"
   "unchecked_dec" "dec"
   "unchecked_inc" "inc"
   "unchecked_minus" "-"
   "unchecked_multiply" "*"
   "unsignedShiftRight" "unsigned-bit-shift-right"
   "xor" "bit-xor"})

;; TODO: desugar lists

(defmethod ast->sugared-ast :invoke-static [{:keys [^String target method arg-types] :as ast}]
  (let [{:keys [args] :as ast} (update ast :args #(mapv ast->sugared-ast %))]

    (cond

      (and (= target "clojure.lang.RT")
           (#{"count" "nth" "get" "isReduced" "alength" "aclone" "aget" "aset" "object_array"} method))

      {:op :invoke
       :fn {:op :var
            :ns "clojure.core"
            :name ({"isReduced" "reduced?" "object_array" "object-array"} method method)}
       :args args}

      (and (= target "clojure.lang.Util")
           (#{"identical" "="} method))

      ;; WIP uninline nil?
      {:op :invoke
       :fn {:op :var
            :ns "clojure.core"
            :name ({"identical" "identical?" "=" "="} method)}
       :args args}

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

      (and (= target "java.util.regex.Pattern")
           (= method "compile")
           (= 1 (count args))
           (string? (-> args first :val)))

      {:op :const
       :val (-> args first :val re-pattern)}

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
           (math-ops method))

      {:op :invoke
       :fn {:op :var
            :ns "clojure.core"
            :name (math-ops method)}
       :args args}

      (and (= target "clojure.lang.Symbol")
           (= method "intern")
           (= 2 (count args))
           (every? (comp #{:const} :op) args))

      {:op :const
       :val (list 'quote (symbol (:val (first args)) (:val (second args))))}

      (and (= method "valueOf")
           (#{"java.lang.Long" "java.lang.Double" "java.lang.Integer" "java.lang.Byte" "java.lang.Short" "java.lang.Float"} target)
           (= 1 (count args))
           (-> args (first) :op (= :const))
           (-> args (first) :val number?))

      {:op :const
       :val (-> args (first) :val)}

      (and (= target "clojure.lang.RT")
           (= method "var")
           (= 2 (count args))
           (every? (comp #{:const} :op) args))

      ;; too aggressive, on init this should intern
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
                       (into (vec args) (->> varargs :args first :!items deref (mapv ast->sugared-ast))))
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

      (and (= target "clojure.lang.PersistentList")
           (= method "create")
           (= "java.util.Arrays" (-> args first :target))
           (= "asList" (-> args first :method))
           (= :array (-> args first :args first :op)))

      {:op :list
       :items (-> args first :args first :!items deref)}

      (and (= target "clojure.lang.Reflector")
           (= method "invokeInstanceMethod"))

      {:op :invoke-instance
       :target (first args)
       :args (deref (:!items (nth args 2)))
       :method (:val (second args))}

      (and (= target "clojure.lang.Reflector")
           (= method "invokeNoArgInstanceMember"))

      {:op :invoke-instance
       :target (first args)
       :args []
       :method (:val (second args))}

      (and (= target "clojure.lang.RT")
           (#{"vector" "set" "mapUniqueKeys" "map"} method)
           (= (-> args first :op) :array))

      {:op ({"vector" :vector "set" :set "mapUniqueKeys" :map "map" :map} method)
       :items (-> args first :!items deref)}

      :else
      ast)))
