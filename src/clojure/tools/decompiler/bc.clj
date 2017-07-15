(ns clojure.tools.decompiler.bc
  (:require [clojure.java.io :as io])
  (:import (org.apache.bcel.classfile ClassParser JavaClass Field AccessFlags Method ConstantPool ConstantObject ConstantCP)
           (org.apache.bcel.generic Instruction InstructionList
                                    BranchInstruction CPInstruction

                                    ;LocalVariableInstruction, NEWARRAY
                                    )))

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
     :field/class type
     :field/flags (parse-flags field)}))

(defn class-fields [^JavaClass klass]
  (->> klass
       (.getFields)
       (mapv parse-field)))

(defmulti -parse-insn (fn [^JavaClass klass ^Instruction insn] (class insn)))

(defmethod -parse-insn :default [_ _])

(defmethod -parse-insn BranchInstruction
  [_ ^BranchInstruction insn]
  {:insn/jump-target (.getIndex insn)})

(defn parse-pool-element [^ConstantPool pool idx]
  (let [constant (.getConstant pool idx)]
    (if (instance? constant ConstantObject)
      (.getConstantValue ^ConstantObject constant pool)
      ;; methods + field refs
      )))

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

(defn parse-method [^JavaClass klass ^Method method]
  {:method/name (.getName method)
   :method/flags (parse-flags method)
   :method/return-class (-> method (.getReturnType) (str))
   :method/arg-classes (->> method (.getArgumentTypes) (mapv str))
   :method/bytecode (parse-bytecode klass method)})

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

  (def filename (-> "test$foo.class" io/resource .getFile))
  (def klass (parse-classfile filename))

  (def m (first (.getMethods klass)))

  (keys (bean m))

  (analyze-classfile filenameo)
  )
