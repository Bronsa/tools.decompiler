(ns clojure.tools.decompiler.bc
  (:import (org.apache.bcel.classfile ClassParser JavaClass Field AccessFlags Method
                                      ConstantPool ConstantObject ConstantCP ConstantNameAndType
                                      Utility LocalVariable)
           (org.apache.bcel.generic Instruction InstructionList BranchInstruction CPInstruction ConstantPushInstruction
                                    ConstantPoolGen LocalVariableInstruction TypedInstruction NEWARRAY Select)))

;; Implementaiton is limited to the set of bytecode produced by Clojure compiler as of version 1.9.0

(set! *warn-on-reflection* true)

(defn parse-classfile ^JavaClass [filename]
  (-> filename
      (ClassParser.)
      (.parse)))

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
      {:insn/jump-targets {:insn/jump-targets (vec (.getIndices insn))
                           :insn/jump-matches (vec (.getMatchs insn))}})
    {:insn/jump-target (.getIndex insn)}))

(defmethod -parse-insn ConstantPushInstruction
  [^JavaClass klass ^ConstantPushInstruction insn]
  {:insn/constant-element {:insn/target-value (.getValue insn)
                           :insn/target-type (type-from-pool-gen klass insn)}})

(defn parse-pool-element [^ConstantPool pool idx]
  (let [constant (.getConstant pool idx)]
    (if (instance? ConstantObject constant)
      {:insn/target-value (.getConstantValue ^ConstantObject constant pool)}
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
  (let [pool (.getConstantPool klass)]
    {:insn/pool-element (parse-pool-element pool (.getIndex insn))}))

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

(defn parse-local-variable-table [local-variable-table]
  (->>
   (for [^LocalVariable local-variable local-variable-table]
     [(.getIndex local-variable)
      {:local-variable/name (.getName local-variable)
       :local-variable/start-index (.getStartPC local-variable)
       :local-variable/type (Utility/signatureToString (.getSignature local-variable) false)}])
   (into {})))

(defn parse-method [^JavaClass klass ^Method method]
  (let [bytecode (parse-bytecode klass method)]
    {:method/name (.getName method)
     :method/flags (parse-flags method)
     :method/return-type (-> method (.getReturnType) (str))
     :method/arg-types (->> method (.getArgumentTypes) (mapv str))
     :method/bytecode bytecode
     :method/jump-table (into {} (for [i (range (count bytecode))
                                       :let [{:keys [insn/label]} (nth bytecode i)]]
                                   [label i]))
     :method/local-variable-table (or (some-> method
                                              (.getLocalVariableTable)
                                              (.getLocalVariableTable)
                                              (parse-local-variable-table))
                                      {})}))

(defn class-methods [^JavaClass klass]
  (->> klass
       (.getMethods)
       (mapv (partial parse-method klass))))

(defn analyze-classfile [filename]
  (let [klass (parse-classfile filename)]
    {:class/name (.getClassName klass)
     :class/filename (.getSourceFileName klass)

     :class/type (if (.isClass klass)
                   :class
                   :interface)

     :class/flags (parse-flags klass)

     :class/super (-> klass (.getSuperClass) (.getClassName))
     :class/interfaces (vec (.getInterfaceNames klass))

     :class/fields (class-fields klass)
     :class/methods (class-methods klass)}))

(comment
  (require '[clojure.java.io :as io])

  (def filename (-> "test$bar.class" io/resource .getFile))
  (def klass (parse-classfile filename))

  (def m (first (.getMethods klass)))

  (keys (bean m))

  (analyze-classfile filename)
  )
