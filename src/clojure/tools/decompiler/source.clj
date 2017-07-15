(ns clojure.tools.decompiler.source
  (:require [clojure.tools.decompiler.stack :as stack]
            [clojure.tools.decompiler.utils :as u]))

(def initial-ctx {:stack []
                  :local-variable-table {}
                  :ast {}})

; bc, ctx -> ctx
(defn static-init [{:class/keys [methods] :as bc} ctx]
  (let [init (u/find-method methods {:method/name "<clinit>"})]
    ctx))

(defn init [{:class/keys [methods] :as bc} ctx]
  ctx)

(defn decompile-fn [{class-name :class/name
                     :class/keys [methods] :as bc}
                    ctx]
  (let [class-name (u/demunge class-name)
        ns (namespace class-name)
        fn-name (name class-name)]

    (->> ctx
         (static-init bc)
         (init bc))))

(defn bc->ast [{:class/keys [super] :as bc}]
  (if (#{"clojure.lang.AFunction" "clojure.lang.RestFn"} super)
    (decompile-fn bc initial-ctx)
    (throw (Exception. ":("))))

(comment
  (require '[clojure.tools.decompiler.bc :as bc]
           '[clojure.java.io :as io])

  (def filename (-> "test$foo.class" io/resource .getFile))
  (def bc (bc/analyze-classfile filename))

  (bc->ast bc)
  )
