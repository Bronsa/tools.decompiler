;;   Copyright (c) Nicola Mometto & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.decompiler.bc
  (:require [clojure.string :as s])
  (:import (org.apache.bcel.classfile ClassParser JavaClass Field AccessFlags Method
                                      ConstantPool ConstantObject ConstantCP ConstantNameAndType
                                      Utility LocalVariable)
           (org.apache.bcel.generic Instruction InstructionList BranchInstruction CPInstruction ConstantPushInstruction MethodGen
                                    ConstantPoolGen LocalVariableInstruction TypedInstruction IndexedInstruction CodeExceptionGen NEWARRAY Select)
           java.io.ByteArrayInputStream
           clojure.tools.decompiler.RetrieveClasses))

;; Implementaiton is limited to the set of bytecode produced by Clojure compiler as of version 1.9.0

(set! *warn-on-reflection* true)

(defn parse-class ^JavaClass [^String classfile-or-classname]
  (let [cp (if (.endsWith classfile-or-classname ".class")
             (ClassParser. classfile-or-classname)
             (ClassParser. (ByteArrayInputStream. (get (RetrieveClasses/getClasses) classfile-or-classname)) ""))]
    (.parse cp)))

(defn parse-flags [^AccessFlags flags]
  (cond-> #{}
    (.isAbstract flags) (conj :abstract)
    (.isFinal flags) (conj :final)
    (.isInterface flags) (conj :interface)
    (.isPrivate flags) (conj :private)
    (.isProtected flags) (conj :protected)
    (.isPublic flags) (conj :public)
    (.isStatic flags) (conj :static)
    (.isSynchronized flags) (conj :synchronized)
    (.isSynthetic flags) (conj :synthetic)
    (.isVolatile flags) (conj :volatile)))

(defn parse-field [^Field field]
  (let [type (-> field
                 (.getType)
                 (str))
        name (.getName field)]
    {:field/name name
     :field/type type
     :field/flags (parse-flags field)}))

(defn class-fields [^JavaClass klass]
  (->> klass
       (.getFields)
       (mapv parse-field)))

(defmulti -parse-insn (fn [^JavaClass klass ^Instruction insn] (class insn)))

(defmethod -parse-insn :default [_ _])

(defn type-from-pool-gen [^JavaClass klass ^TypedInstruction insn]
  (->> klass
       (.getConstantPool)
       (ConstantPoolGen.)
       (.getType insn)
       (str)))

(defmethod -parse-insn LocalVariableInstruction
  [^JavaClass klass ^LocalVariableInstruction insn]
  {:insn/local-variable-element {:insn/target-type (type-from-pool-gen klass insn)
                                 :insn/target-index (.getIndex insn)}})

(defmethod -parse-insn NEWARRAY
  [^JavaClass klass ^NEWARRAY insn]
  {:insn/target-type (str (.getType insn))})

(defmethod -parse-insn BranchInstruction
  [_ ^BranchInstruction insn]
  (if (instance? Select insn)
    (let [^Select insn insn]
      {:insn/jump-targets {:insn/jump-offsets (vec (.getIndices insn))
                           :insn/default-offset (.getIndex insn)
                           :insn/jump-matches (vec (.getMatchs insn))}})
    {:insn/jump-offset (.getIndex insn)}))

(defmethod -parse-insn ConstantPushInstruction
  [^JavaClass klass ^ConstantPushInstruction insn]
  {:insn/pool-element {:insn/target-value (.getValue insn)
                       :insn/target-type (type-from-pool-gen klass insn)}})

(defn parse-pool-element [^JavaClass klass ^IndexedInstruction insn]
  (let [idx (.getIndex insn)
        pool (.getConstantPool klass)
        constant (.getConstant pool idx)]
    (if (instance? ConstantObject constant)
      {:insn/target-value (.getConstantValue ^ConstantObject constant pool)
       :insn/target-type (type-from-pool-gen klass insn)}
      ;; methods + field refs
      (let [^ConstantCP constant constant
            ^ConstantNameAndType name-and-type (.getConstant pool (.getNameAndTypeIndex constant))
            signature (.getSignature name-and-type pool)]
        (merge
         {:insn/target-class (.getClass constant pool)
          :insn/target-name (.getName name-and-type pool)}
         (if (.startsWith signature "(")
           {:insn/target-arg-types (vec (Utility/methodSignatureArgumentTypes signature false))
            :insn/target-ret-type (Utility/methodSignatureReturnType signature false)}
           {:insn/target-type (Utility/signatureToString signature false)}))))))

(defmethod -parse-insn CPInstruction
  [^JavaClass klass ^CPInstruction insn]
  {:insn/pool-element (parse-pool-element klass insn)})

(defn parse-insn [^JavaClass klass ^Instruction insn]
  (merge
   {:insn/name (.getName insn)
    :insn/length (.getLength insn)}
   (-parse-insn klass insn)))

(defn add-labels [insns insn]
  (let [label (if-let [{:insn/keys [label length]} (peek insns)]
                (+ label length)
                0)]
    (conj insns (assoc insn :insn/label label))))

(defn parse-bytecode [^JavaClass klass ^Method method]
  (->> method
       (.getCode)
       (.getCode)
       (InstructionList.)
       (.getInstructions)
       (mapv (partial parse-insn klass))
       (reduce add-labels [])))

(defn fixup-name [name]
  (let [name (or (second (re-matches #"(.*)__auto__[0-9]+$" name)) name)]
    (reduce (fn [n [p m]] (s/replace n p (str m)))
            name
            (dissoc clojure.lang.Compiler/DEMUNGE_MAP "_"))))

(defn fixup-lvt [lvt]
  (let [els (group-by (comp zero? :local-variable/start-label) lvt)
        args (get els true)
        locals (get els false)]
    (concat locals
            (second
             (reduce
              (fn [[i lvt] {:local-variable/keys [index type] :as lv}]
                [(+ i ({"long" 2 "double" 2} type 1)) (conj lvt (assoc lv :local-variable/index i))])
              [0 #{}]
              (sort-by :index args))))))

(defn parse-local-variable-table [local-variable-table]
  (fixup-lvt
   (for [^LocalVariable local-variable local-variable-table]
     #:local-variable{:name (fixup-name (.getName local-variable))
                      :start-label (.getStartPC local-variable)
                      :end-label (+ (.getStartPC local-variable)
                                    (.getLength local-variable))
                      :index (.getIndex local-variable)
                      :type (Utility/signatureToString (.getSignature local-variable) false)})))

(defn parse-exception-table [^JavaClass klass ^Method method]
  (let [cp-gen (ConstantPoolGen. (.getConstantPool klass))
        ex-handlers (.getExceptionHandlers (MethodGen. method (.getClassName klass) cp-gen))]
    (for [^CodeExceptionGen ex ex-handlers]
      #:exception-handler{:type (some-> ex (.getCatchType) (.getClassName))
                          :start-label (.getPosition (.getStartPC ex))
                          :end-label (.getPosition (.getEndPC ex))
                          :handler-label (.getPosition (.getHandlerPC ex))})))

(defn parse-method [^JavaClass klass ^Method method]
  (let [bytecode (parse-bytecode klass method)]
    #:method{:name (.getName method)
             :flags (parse-flags method)
             :return-type (-> method (.getReturnType) (str))
             :arg-types (->> method (.getArgumentTypes) (mapv str))
             :bytecode bytecode
             :jump-table (into {} (for [i (range (count bytecode))
                                        :let [{:keys [insn/label]} (nth bytecode i)]]
                                    [label i]))
             :exception-table (into #{} (parse-exception-table klass method))
             :local-variable-table (->>
                                    (some-> method
                                            (.getLocalVariableTable)
                                            (.getLocalVariableTable)
                                            (parse-local-variable-table))
                                    (into #{}))}))

(defn class-methods [^JavaClass klass]
  (->> klass
       (.getMethods)
       (remove #(.isAbstract ^Method %))
       (mapv (partial parse-method klass))))

(defn analyze-class [classfile-or-classname]
  (let [klass (parse-class classfile-or-classname)]
    {:class/name (.getClassName klass)
     :class/filename (.getSourceFileName klass)

     :class/type (if (.isClass klass)
                   :class
                   :interface)

     :class/flags (parse-flags klass)

     :class/super (-> klass (.getSuperclassName))
     :class/interfaces (vec (.getInterfaceNames klass))

     :class/fields (class-fields klass)
     :class/methods (class-methods klass)}))

(def insn-h
  (-> (make-hierarchy)
      (derive :ldc ::const-insn)
      (derive :ldc_w ::const-insn)
      (derive :ldc2_w ::const-insn)
      (derive :aconst_null ::const-insn)
      (derive :bipush ::const-insn)
      (derive :sipush ::const-insn)
      (derive :dconst_0 ::const-insn)
      (derive :dconst_1 ::const-insn)
      (derive :iconst_0 ::const-insn)
      (derive :fconst_1 ::const-insn)
      (derive :fconst_2 ::const-insn)
      (derive :fconst_3 ::const-insn)
      (derive :iconst_m1 ::const-insn)
      (derive :iconst_0 ::const-insn)
      (derive :iconst_1 ::const-insn)
      (derive :iconst_2 ::const-insn)
      (derive :iconst_3 ::const-insn)
      (derive :iconst_4 ::const-insn)
      (derive :iconst_5 ::const-insn)
      (derive :lconst_0 ::const-insn)
      (derive :lconst_1 ::const-insn)
      (derive :lconst_2 ::const-insn)

      (derive :invokeinterface ::invoke-instance-method)
      (derive :invokevirtual ::invoke-instance-method)

      (derive :astore ::store-insn)
      (derive :astore_0 ::store-insn)
      (derive :astore_1 ::store-insn)
      (derive :astore_2 ::store-insn)
      (derive :astore_3 ::store-insn)
      (derive :dstore ::store-insn)
      (derive :dstore_0 ::store-insn)
      (derive :dstore_1 ::store-insn)
      (derive :dstore_2 ::store-insn)
      (derive :dstore_3 ::store-insn)
      (derive :fstore ::store-insn)
      (derive :fstore_0 ::store-insn)
      (derive :fstore_1 ::store-insn)
      (derive :fstore_2 ::store-insn)
      (derive :fstore_3 ::store-insn)
      (derive :istore ::store-insn)
      (derive :istore_0 ::store-insn)
      (derive :istore_1 ::store-insn)
      (derive :istore_2 ::store-insn)
      (derive :istore_3 ::store-insn)
      (derive :lstore ::store-insn)
      (derive :lstore_0 ::store-insn)
      (derive :lstore_1 ::store-insn)
      (derive :lstore_2 ::store-insn)
      (derive :lstore_3 ::store-insn)

      (derive :aload ::load-insn)
      (derive :aload_0 ::load-insn)
      (derive :aload_1 ::load-insn)
      (derive :aload_2 ::load-insn)
      (derive :aload_3 ::load-insn)
      (derive :dload ::load-insn)
      (derive :dload_0 ::load-insn)
      (derive :dload_1 ::load-insn)
      (derive :dload_2 ::load-insn)
      (derive :dload_3 ::load-insn)
      (derive :fload ::load-insn)
      (derive :fload_0 ::load-insn)
      (derive :fload_1 ::load-insn)
      (derive :fload_2 ::load-insn)
      (derive :fload_3 ::load-insn)
      (derive :iload ::load-insn)
      (derive :iload_0 ::load-insn)
      (derive :iload_1 ::load-insn)
      (derive :iload_2 ::load-insn)
      (derive :iload_3 ::load-insn)
      (derive :lload ::load-insn)
      (derive :lload_0 ::load-insn)
      (derive :lload_1 ::load-insn)
      (derive :lload_2 ::load-insn)
      (derive :lload_3 ::load-insn)

      (derive :aastore ::array-store)
      (derive :bastore ::array-store)
      (derive :castore ::array-store)
      (derive :dastore ::array-store)
      (derive :fastore ::array-store)
      (derive :iastore ::array-store)
      (derive :lastore ::array-store)
      (derive :sastore ::array-store)

      (derive :areturn ::return-value)
      (derive :dreturn ::return-value)
      (derive :freturn ::return-value)
      (derive :ireturn ::return-value)
      (derive :lreturn ::return-value)
      (derive :sreturn ::return-value)

      (derive :dneg ::math-insn)
      (derive :lneg ::math-insn)

      (derive :dadd ::math-insn)
      (derive :ddiv ::math-insn)
      (derive :dmul ::math-insn)
      (derive :dsub ::math-insn)
      (derive :iadd ::math-insn)
      (derive :iand ::math-insn)
      (derive :idiv ::math-insn)
      (derive :imul ::math-insn)
      (derive :irem ::math-insn)
      (derive :ishl ::math-insn)
      (derive :ishr ::math-insn)
      (derive :isub ::math-insn)
      (derive :iushr ::math-insn)
      (derive :ladd ::math-insn)
      (derive :land ::math-insn)
      (derive :ldiv ::math-insn)
      (derive :lmul ::math-insn)
      (derive :lor ::math-insn)
      (derive :lrem ::math-insn)
      (derive :lshl ::math-insn)
      (derive :lshr ::math-insn)
      (derive :lsub ::math-insn)
      (derive :lushr ::math-insn)
      (derive :lxor ::math-insn)

      (derive :dcmpg ::number-compare)
      (derive :lcmp ::number-compare)
      (derive :dcmpl ::number-compare)
      (derive :if_icmpne ::number-compare)

      (derive :iaload ::aget)
      (derive :saload ::aget)
      (derive :faload ::aget)
      (derive :daload ::aget)
      (derive :laload ::aget)
      (derive :caload ::aget)
      (derive :baload ::aget)
      (derive :aaload ::aget)

      (derive :nop ::no-op)
      (derive :d2f ::no-op)
      (derive :d2i ::no-op)
      (derive :f2i ::no-op)
      (derive :f2d ::no-op)
      (derive :f2l ::no-op)
      (derive :i2b ::no-op)
      (derive :i2c ::no-op)
      (derive :i2d ::no-op)
      (derive :i2f ::no-op)
      (derive :i2l ::no-op)
      (derive :i2s ::no-op)
      (derive :i2f ::no-op)
      (derive :l2f ::no-op)
      (derive :l2i ::no-op)
      (derive :l2d ::no-op)

      (derive :pop ::pop)
      (derive :pop2 ::pop)

      (derive :tableswitch ::select)
      (derive :lookupswitch ::select)))
