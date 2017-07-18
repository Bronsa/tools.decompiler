;;   Copyright (c) Nicola Mometto & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.decompiler.compact)

(defn macrocompact [{:keys [source] :as fast}]
  (if (= 'fn* (first source))
    ;; WIP don't qualify if fn in scope
    (update fast :source (fn [source] (list* 'clojure.core/fn (rest source))))
    fast))
