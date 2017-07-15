(ns clojure.tools.decompiler.stack)

(defn pop-n [stack n]
  (let [c (count stack)]
    (subvec stack 0 (- c n))))

(defn peek-n [stack n]
  (let [c (count stack)]
    (subvec stack (- c n) c)))
