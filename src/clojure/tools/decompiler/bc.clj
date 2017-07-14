(ns clojure.tools.decompiler.bc
  (:require [clojure.java.io :as io])
  (:import (org.apache.bcel.classfile ClassParser JavaClass Field AccessFlags)))

(set! *warn-on-reflection* true)

(defn parse-classfile ^JavaClass [filename]
  (-> filename
      (ClassParser.)
      (.parse)))

(defn class-interfaces [^JavaClass klass]
  (->> klass
       (.getAllInterfaces)
       (mapv #(.getClassName ^JavaClass %))))

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

(defn analyze-classfile [filename]
  (let [klass (parse-classfile filename)]
    {:class/name (.getClassName klass)
     :class/interfaces (class-interfaces klass)
     :class/super (-> klass (.getSuperClass) (.getClassName))
     :class/filename (.getSourceFileName klass)
     :class/type (if (.isClass klass)
                   :class
                   :interface)
     :class/fields (class-fields klass)}))
