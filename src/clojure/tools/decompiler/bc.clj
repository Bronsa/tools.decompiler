(ns clojure.tools.decompiler.bc
  (:require [clojure.java.io :as io])
  (:import (org.apache.bcel.classfile ClassParser JavaClass Field AccessFlags

                                      ;; ConstantClass ConstantCP ConstantDouble ConstantFloat ConstantInteger
                                      ;; ConstantLong ConstantMethodHandle ConstantMethodType
                                      ;; ConstantNameAndType ConstantString ConstantUtf8

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
    {:class.field/name name
     :class.field/class type
     :class.field/flags (parse-flags field)}))

(defn class-fields [^JavaClass klass]
  (->> klass
       (.getFields)
       (mapv parse-field)))

;; (defmulti parse-constant class)

;; (defmethod parse-constant )

;; (defn class-constant-pool [^JavaClass klass]
;;   (->> klass
;;        (.getConstantPool)
;;        (.getConstantPool)
;;        (keep parse-constant)))

(defn class-methods [klass])

(defn analyze-classfile [filename]
  (let [klass (parse-classfile filename)]
    {:class/name (.getClassName klass)
     :class/filename (.getSourceFileName klass)
     :class/version {:minor (.getMinor klass)
                     :major (.getMajor klass)}

     :class/type (if (.isClass klass)
                   :class
                   :interface)

     :class/flags (parse-flags klass)

     ;; :class/constant-pool (class-constant-pool klass)

     :class/super (-> klass (.getSuperClass) (.getClassName))
     :class/interfaces (vec (.getInterfaceNames klass))

     :class/fields (class-fields klass)
     :class/methods (class-methods klass)}))

(comment
  (def filename (-> "test$foo.class" io/resource .getFile))
  (def klass (parse-classfile filename))


  (class-interfaces klass)

  )
