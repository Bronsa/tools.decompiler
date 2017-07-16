(ns clojure.tools.decompiler.compaction)

(defn macrocompact [{:keys [source] :as fast}]
  (if (= 'fn* (first source))
    (update fast :source (fn [source] (list* 'fn (rest source))))
    fast))
