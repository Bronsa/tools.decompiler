(ns clojure.tools.decompiler.bc
  (:require [clojure.java.io :as io])
  (:import (org.apache.bcel.classfile ClassParser JavaClass)))

(defn classfile->bytecode ^JavaClass [filename]
  (-> filename (ClassParser.) .parse))
