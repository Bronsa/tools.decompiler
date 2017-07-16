(ns clojure.tools.decompiler.compact)

(defn macrocompact [{:keys [source] :as fast}]
  (if (= 'fn* (first source))
    ;; WIP don't qualify if fn in scope
    (update fast :source (fn [source] (list* 'clojure.core/fn (rest source))))
    fast))
