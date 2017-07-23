;;   Copyright (c) Nicola Mometto & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.decompiler.ast
  (:require [clojure.set :as set]
            [clojure.tools.decompiler.bc :as bc]
            [clojure.tools.decompiler.utils :refer [peek-n pop-n] :as u]))

;; WIP casting, type hints

(declare bc->ast)

(def initial-ctx {:fields {}
                  :statements []
                  :ast {}})

(def initial-local-ctx {:stack []
                        :pc 0
                        :impure-loops #{}
                        :local-variable-table #{}
                        :exception-table #{}})

(defn pc= [terminate-at]
  (fn [{:keys [pc]}]
    (= pc terminate-at)))

(defn goto-label [{:insn/keys [jump-offset label]}]
  (+ jump-offset label))

(def nil-expr {:op :const :val nil})

(defn ->do [exprs]
  {:op :do
   :statements (vec (remove #{nil-expr} (butlast exprs)))
   :ret (or (last exprs) nil-expr)})

(defn expr+statements [ctx]
  (->do (conj (-> ctx :statements)
              (-> ctx :stack peek))))

(defn curr-insn [{:keys [insns jump-table pc]}]
  (->> pc
       (get jump-table)
       (nth insns)))

(defn insn-at [{:keys [insns jump-table pc]} {:keys [label offset] :or {label pc offset 0}}]
  (->> label
       (get jump-table)
       (+ offset)
       (nth insns)))

(defn maybe-insn-at [{:keys [insns jump-table pc]} {:keys [label offset] :or {label pc offset 0}}]
  (some->> label
           (get jump-table)
           (+ offset)
           (get insns)))

(defn find-local-variable [{:keys [local-variable-table]} index label]
  (->> local-variable-table
       (filter (comp #{index} :index))
       (filter (comp (partial >= label) :start-label))
       (filter (comp (partial < label) :end-label))
       (sort-by :start-label)
       (first)))

(defn find-init-local [{:keys [local-variable-table]} label]
  (->> local-variable-table
       (filter (comp (partial = label) :start-label))
       ;; why is this here?
       (filter (comp (partial < label) :end-label))
       (sort-by :start-label)
       (first)))

(defn init-local-variable? [{:insn/keys [label length]} {:keys [start-label]}]
  (= (+ label length) start-label))

;; process-* : bc, ctx -> ctx
;; decompile-* : bc, ctx -> AST

(def process-insn nil)
(defmulti process-insn
  (fn [_ {:insn/keys [name]}] (keyword name))
  :hierarchy #'bc/insn-h)

(defmethod process-insn :default [ctx {:insn/keys [name]}]
  (println "INSN NOT HANDLED:" name)
  ctx)

(defn start-try-block-info [pc exception-table]
  (seq (filter (comp #{pc} :start-label) exception-table)))

(defn >process-insn [{:keys [pc] :as ctx} {:insn/keys [length] :as insn}]
  (-> (process-insn ctx insn)
      (update :pc (fn [new-pc]
                    (if (= new-pc pc)
                      ;; last insn wasn't an explicit jump, goto next insn
                      (+ new-pc length)
                      new-pc)))))

(declare process-insns)

(defn process-try-block [{:keys [pc exception-table] :as ctx}]
  (let [handlers (->> (start-try-block-info pc exception-table)
                      (sort-by (comp - :end-label))
                      (partition-by :end-label)
                      (first))

        first-handler (->> handlers (sort-by :handler-label) first)

        body-end-label (:end-label first-handler)

        ret-label (-> (insn-at ctx {:label (:handler-label first-handler)
                                    :offset -1})

                      (goto-label)
                      (as-> %
                          (:insn/label (insn-at ctx {:label % :offset 1}))))

        expr-ctx (-> ctx
                     (update :exception-table #(apply disj % handlers)))

        ;; WIP: need to backup lvt?
        body-ctx (-> expr-ctx
                     (assoc :statements [])
                     (assoc :terminate? (pc= body-end-label))
                     (process-insns))

        body (->do (conj (-> body-ctx :statements) (-> body-ctx :stack peek)))

        next-insn (insn-at body-ctx {:offset 1})

        ?finally (when (seq (remove :type handlers))
                   (let [start-label (:insn/label next-insn)
                         end-label (:insn/label (insn-at ctx {:label (:handler-label first-handler) :offset -1}))
                         finally-ctx (process-insns (-> expr-ctx
                                                        (assoc :pc start-label)
                                                        (assoc :statements [])
                                                        (assoc :terminate? (pc= end-label))))]
                     (->do (-> finally-ctx :statements))))

        ?catches (when-let [catches (seq (filter :type handlers))]
                   (->>
                    (for [{:keys [handler-label type]} catches
                          :let [{:keys [start-label end-label name] :as local} (find-init-local ctx handler-label)]]
                      (let [end-label (:insn/label (insn-at ctx {:label end-label :offset -1}))
                            catch-ctx (process-insns (-> expr-ctx
                                                         (assoc :pc start-label)
                                                         (assoc :exception-table #{})
                                                         (update :stack conj local)
                                                         (assoc :statements [])
                                                         (assoc :terminate? (pc= end-label))))]
                        {:op :catch
                         :local {:name name
                                 :type type}
                         :body (->do (conj (-> catch-ctx :statements)
                                           (-> catch-ctx :stack peek)))}))
                    (into [])))

        expr (if (or ?finally
                     (seq ?catches))
               {:op :try
                :catches ?catches
                :finally ?finally
                :body body}
               body)]

    (-> ctx
        (update :stack conj expr)
        (assoc :recur? (:recur? body-ctx))
        (assoc :pc ret-label))))

;; doesn't handle wrapping try/catch/finally
(defn will-ret? [ctx label]
  (loop [off 0]
    (let [{:insn/keys [name] :as insn} (maybe-insn-at ctx {:label label :offset off})]
      (cond
        (not insn)
        true

        (or (#{"invokestatic" "checkcast"} name)
            (isa? bc/insn-h (keyword name) ::bc/no-op)
            (isa? bc/insn-h (keyword name) ::bc/return-value)
            (isa? bc/insn-h (keyword name) ::bc/invoke-instance-method))
        (recur (inc off))

        :else

        false))))

(defn process-impure-loop [{:keys [impure-loops pc] :as ctx}]
  (let [end-label (impure-loops pc)
        {body-stack :stack body-stmnts :statements :keys [pc]} (process-insns (-> ctx
                                                                                  (assoc :loop-args [])
                                                                                  (update :impure-loops disj pc)
                                                                                  (assoc :terminate? :recur?)
                                                                                  (assoc :statements [])))
        statement? (not (will-ret? ctx end-label))
        body (->do (conj body-stmnts (peek body-stack)))]
    (-> ctx
        (assoc :pc pc)
        (update (if statement? :statements :stack)
                conj {:op :loop
                      :local-variables []
                      :body body}))))

(defn process-insns [{:keys [pc jump-table exception-table terminate? impure-loops]
                      :as ctx}]
  (cond
    (or (not (get jump-table pc))
        (and terminate? (terminate? ctx)))
    ctx

    (start-try-block-info pc exception-table)
    (recur (process-try-block ctx))

    (contains? impure-loops pc)
    (recur (process-impure-loop ctx))

    :else
    (let [insn (curr-insn ctx)]
      (recur (>process-insn ctx insn)))))

(defmethod process-insn ::bc/no-op [ctx _]
  ctx)

(defmethod process-insn ::bc/const-insn [ctx {:insn/keys [pool-element]}]
  (-> ctx
      (update :stack conj {:op :const
                           :val (:insn/target-value pool-element)})))

(defmethod process-insn :swap [{:keys [stack] :as ctx} _]
  (let [[v2 v1] (peek-n stack 2)]
    (-> ctx
        (update :stack pop-n 2)
        (update :stack conj v1 v2))))

(defmethod process-insn :dup_x1 [{:keys [stack] :as ctx} _]
  (let [[v2 v1] (peek-n stack 2)]
    (-> ctx
        (update :stack pop-n 2)
        (update :stack conj v1 v2 v1))))

(defmethod process-insn :dup [{:keys [stack] :as ctx} _]
  (let [val (peek stack)]
    (-> ctx
        (update :stack conj val))))

(defmethod process-insn :anewarray [{:keys [stack] :as ctx} _]
  (let [{dimension :val} (peek stack)
        expr {:op :array
              :!items (atom (vec (repeat dimension nil-expr)))}]

    (-> ctx
        (update :stack pop)
        (update :stack conj expr))))

(defmethod process-insn ::bc/array-store [{:keys [stack] :as ctx} _]
  (let [[{:keys [!items]} {index :val} value] (peek-n stack 3)]
    (swap! !items assoc index value)
    (-> ctx
        (update :stack pop-n 3))))

(defmethod process-insn :monitorenter [{:keys [stack] :as ctx} _]
  (let [sentinel (peek stack)]
    (-> ctx
        (update :stack pop)
        (update :stack conj {:op :monitor-enter
                             :sentinel sentinel}))))

(defmethod process-insn :monitorexit [{:keys [stack] :as ctx} _]
  (let [sentinel (peek stack)]
    (-> ctx
        (update :stack pop)
        (update :stack conj {:op :monitor-exit
                             :sentinel sentinel}))))

(defmethod process-insn :return [{:keys [stack statements] :as ctx} _]
  (let [ret (peek stack)]
    (-> ctx
        (assoc :ast (->do (conj statements ret))))))

(defmethod process-insn ::bc/return-value [{:keys [stack statements] :as ctx} _]
  (let [ret (peek stack)]
    (-> ctx
        (assoc :stack [] :statements []
               :ast (->do (conj statements ret))))))

(defn process-if [{:keys [stack] :as ctx} test [start-then end-then] [start-else end-else]]
  (let [then-ctx (process-insns (assoc ctx
                                       :pc start-then
                                       :terminate? (pc= end-then)
                                       :statements []))
        else-ctx (process-insns (assoc ctx
                                       :pc start-else
                                       :terminate? (pc= end-else)
                                       :statements []))

        {then-stack :stack then-stmnts :statements then-recur? :recur?} then-ctx
        {else-stack :stack else-stmnts :statements else-recur? :recur?} else-ctx

        statement? (= stack then-stack else-stack)

        [then else] (if statement?
                      [then-stmnts else-stmnts]
                      [(conj then-stmnts (peek then-stack))
                       (conj else-stmnts (peek else-stack))])]
    (-> ctx
        (assoc :pc end-else)
        (update (if statement? :statements :stack)
                conj {:op :if
                      :test test
                      :then (->do then)
                      :else (->do else)})
        (cond-> (not statement?)
          (assoc :recur? (or then-recur? else-recur?))))))

(defmethod process-insn :ifnull [{:keys [stack] :as ctx} insn]
  (let [null-label (goto-label insn)

        goto-end-insn (insn-at ctx {:label null-label :offset -1})
        end-label (goto-label goto-end-insn)

        goto-else-insn (insn-at ctx {:offset 2})
        else-label (goto-label goto-else-insn)

        {then-label :insn/label} (insn-at ctx {:offset 3})

        [test _] (peek-n stack 2)]

    (-> ctx
        (update :stack pop-n 2)
        (process-if test [then-label (:insn/label goto-end-insn)] [else-label end-label]))))

(defmethod process-insn :ifeq [{:keys [stack] :as ctx} insn]
  (let [else-label (goto-label insn)]
    (if (not= "pop" (:insn/name (insn-at ctx {:label else-label :offset -1})))
      (-> ctx
          (assoc :pc (:insn/label (insn-at ctx {:offset 4}))))
      (let [goto-end-insn (insn-at ctx {:label else-label :offset -2})

            end-label (goto-label goto-end-insn)

            {then-label :insn/label} (insn-at ctx {:offset 1})

            test (peek stack)]

        (-> ctx
            (update :stack pop)
            (process-if test [then-label (:insn/label goto-end-insn)] [else-label end-label]))))))

(defmethod process-insn ::bc/aget [{:keys [stack] :as ctx} _]
  (let [[arr i] (peek-n stack 2)]
    (-> ctx
        (update :stack pop-n 2)
        (update :stack conj {:op :invoke
                             :fn {:op :var :ns "clojure.core" :name "aget"}
                             :args [arr i]}))))

(defmethod process-insn :arraylength [{:keys [stack] :as ctx} _]
  (let [arr (peek stack)]
    (-> ctx
        (update :stack pop)
        (update :stack conj {:op :invoke
                             :fn {:op :var :ns "clojure.core" :name "alength"}
                             :args [arr]}))))

(defmethod process-insn ::bc/number-compare [{:keys [stack] :as ctx} insn]
  (let [offset (if (= "if_icmpne" (:insn/name insn)) 0 1)
        insn (insn-at ctx {:offset offset})

        op (case (:insn/name insn)
             "ifle" ">"
             "ifge" "<"
             "ifne" "="
             "iflt" ">="
             "ifgt" "<="
             "if_icmpne" "=")

        else-label (goto-label insn)

        goto-end-insn (insn-at ctx {:label else-label :offset -2})
        end-label (goto-label goto-end-insn)

        {then-label :insn/label} (insn-at ctx {:offset (inc offset)})

        [a b] (peek-n stack 2)

        test {:op :invoke :fn {:op :var :ns "clojure.core" :name op} :args [a b]}]

    (-> ctx
        (update :stack pop-n 2)
        (process-if test [then-label (:insn/label goto-end-insn)] [else-label end-label]))))

(defmethod process-insn :goto [{:keys [loop-args] :as ctx} {:insn/keys [jump-offset]}]
  (if-not (pos? jump-offset)
    (let [args (for [{:keys [start-label index]} loop-args
                     :let [{:keys [init]} (find-local-variable ctx index start-label)]]
                 init)]
      (-> ctx
          (assoc :recur? true)
          (update :stack conj {:op :recur
                               :args (vec args)})))
    ;; case || proto inline cache
    (-> ctx
        (update :pc + jump-offset))))

(defn skip-locals-clearing-lv [ctx]
  (if (and (= "aconst_null" (:insn/name (insn-at ctx {:offset 1})))
           (isa? bc/insn-h (-> (insn-at ctx {:offset 2}) :insn/name keyword) ::bc/store-insn)
           (= (-> (curr-insn ctx) :insn/local-variable-element :insn/target-index)
              (-> (insn-at ctx {:offset 2}) :insn/local-variable-element :insn/target-index)))
    (-> ctx
        (assoc :pc (:insn/label (insn-at ctx {:offset 3}))))
    ctx))

(defmethod process-insn ::bc/load-insn [{:keys [closed-overs] :as ctx} {:insn/keys [local-variable-element label]}]
  (let [{:insn/keys [target-index]} local-variable-element]
    (if-let [local (find-local-variable ctx target-index label)]
      (-> ctx
          (update :stack conj local)
          (skip-locals-clearing-lv))
      (if (contains? closed-overs target-index)
        (-> ctx
            (update :stack conj {:op :closed-over
                                 :target target-index})
            (skip-locals-clearing-lv))
        (throw (Exception. ":("))))))

(defn find-recur-jump-label [{:keys [jump-table pc insns] :as ctx} {:keys [start-label end-label index]}]
  (loop [[{:insn/keys [name length label local-variable-element] :as insn} & insns] (drop (inc (get jump-table pc)) insns)]

    (cond

      (or (nil? insn)
          (> label end-label))
      false

      (and (isa? bc/insn-h (keyword name) ::bc/store-insn)
           (= (:insn/target-index local-variable-element) index)
           (= (:start-label (find-local-variable ctx index label)) start-label)
           (= "goto" (:insn/name (first insns)))
           (neg? (:insn/jump-offset (first insns)))
           (< (goto-label (first insns)) end-label))
      (+ label length)

      :else
      (recur insns))))

(defn find-loop-info [{:keys [local-variable-table] :as ctx} {:keys [start-label end-label] :as insn}]
  (when-let [jump-label (find-recur-jump-label ctx insn)]
    (let [insn (insn-at ctx {:label jump-label})
          loop-label (goto-label insn)]
      {:loop-label loop-label
       :loop-args (->> (for [local-variable local-variable-table
                             :when (and (= (:end-label local-variable) end-label)
                                        (>= loop-label (:start-label local-variable) start-label))]
                         local-variable)
                       (sort-by :start-label)
                       (into []))})))

(defn process-loop [ctx {:keys [loop-label loop-args]} {:keys [end-label] :as local-variable} init]
  (let [{:insn/keys [length]} (curr-insn ctx)]
    (loop [[arg & loop-args] (rest loop-args)
           args-ctx (-> ctx (update :pc + length))
           args [{:op :local-variable :local-variable local-variable :init init}]]
      (if arg
        (let [pre-insn (insn-at ctx {:label (:start-label arg) :offset -1}) ;; astore
              {:keys [statements stack] :as new-ctx} (process-insns (-> args-ctx
                                                                        (assoc :terminate? (pc= (:insn/label pre-insn)))
                                                                        (assoc :statements [])))

              local-variable (find-init-local new-ctx (:start-label arg))
              init (if (seq statements) (->do (conj statements (peek stack))) (peek stack))]
          (recur loop-args
                 (-> new-ctx
                     (update :pc + (:insn/length pre-insn))
                     (update :local-variable-table disj local-variable)
                     (update :local-variable-table conj (assoc local-variable :init init)))
                 (conj args {:op :local-variable :local-variable local-variable :init init})))

        (let [{body-stack :stack body-stmnts :statements} (process-insns (-> ctx
                                                                             (assoc :local-variable (:local-variable args-ctx))
                                                                             (assoc :pc loop-label)
                                                                             (assoc :loop-args (mapv :local-variable args))
                                                                             (assoc :terminate? (pc= end-label))
                                                                             (assoc :statements [])))

              statement? (not (will-ret? ctx end-label))
              body (->do (conj body-stmnts (peek body-stack)))]
          (-> ctx
              (assoc :pc end-label)
              (update (if statement? :statements :stack)
                      conj {:op :loop
                            :local-variables args
                            :body body})))))))

(defn process-let [{:keys [stack] :as ctx} {:keys [end-label] :as local-variable} init]
  (let [{:insn/keys [length]} (curr-insn ctx)
        body-ctx (process-insns (-> ctx
                                    (update :pc + length)
                                    (assoc :terminate? (pc= end-label))
                                    (assoc :statements [])))
        {body-stack :stack body-stmnts :statements :keys [recur?]} body-ctx
        statement? (= stack body-stack)
        body (->do (if statement? body-stmnts (conj body-stmnts (peek body-stack))))]
    (-> ctx
        (assoc :pc end-label)
        (update (if statement? :statements :stack)
                conj {:op :let
                      :local-variables [{:op :local-variable
                                          :local-variable local-variable
                                          :init init}]
                      :body body})
        (cond-> (not statement?)
          (assoc :recur? recur?)))))

(defn find-letfn-info [{:keys [local-variable-table jump-table pc insns] :as ctx} {:keys [start-label end-label]}]
  (let [local-variables (->> local-variable-table
                             (filter (comp (partial = start-label) :start-label))
                             (filter (comp (partial = end-label) :end-label))
                             (sort-by :index))]
    (when (or (> (count local-variables) 1)
              (= ["new" "dup" "invokespecial" (:insn/name (curr-insn ctx))]
                 (mapv :insn/name (take 4 (subvec insns (get jump-table pc) (count insns)))))
              (= (:insn/local-variable-element (curr-insn ctx))
                 (:insn/local-variable-element (insn-at ctx {:offset 4}))))
      local-variables)))

(defn process-letfn [{:keys [local-variable-table pc bc-for] :as ctx} target-index]
  (let [{:keys [index start-label end-label]} (->> local-variable-table
                                                   (filter (comp (partial < pc) :start-label))
                                                   first)]
    (if (= target-index index)
      (let [local-variables (->> local-variable-table
                                 (filter (comp #{start-label} :start-label))
                                 (sort-by :index))
            letfn-fns (loop [pc pc fns []]
                        (let [insn (curr-insn (assoc ctx :pc pc))]
                          (cond
                            (= (count fns) (count local-variables))
                            fns

                            (= "new" (:insn/name insn))
                            (recur (+ pc (:insn/length insn))
                                   (conj fns (-> insn :insn/pool-element :insn/target-value)))

                            :else
                            (recur (+ pc (:insn/length insn)) fns))))

            init-local-variables (map (fn [lv fn]
                                        (let [init (bc->ast (bc-for fn)
                                                            {:bc-for bc-for
                                                             :fn-name (:name lv)})]
                                          (assoc lv :init init)))
                                      local-variables letfn-fns)
            {:keys [stack] :as ctx} (update ctx :stack pop)
            body-ctx (-> ctx
                         (assoc :statements [])
                         (update :local-variable-table #(apply disj % local-variables))
                         (update :local-variable-table #(apply conj % init-local-variables))
                         (assoc :pc start-label)
                         (assoc :terminate? (pc= end-label))
                         (process-insns))
            {body-stack :stack body-stmnts :statements :keys [recur?]} body-ctx
            statement? (= stack body-stack)
            body (->do (if statement? body-stmnts (conj body-stmnts (peek body-stack))))]
        (-> ctx
            (assoc :pc end-label)
            (update (if statement? :statements :stack)
                    conj {:op :letfn
                          :local-variables (mapv (fn [{:keys [init] :as lv}]
                                                   {:op :local-variable
                                                    :local-variable (dissoc lv :init)
                                                    :init init})
                                                 init-local-variables)
                          :body body})
            (cond-> (not statement?)
              (assoc :recur? recur?))))
      (throw (Exception. ":(")))))

(defn process-lexical-block [ctx local-variable init]
  (if-let [loop-info (find-loop-info ctx local-variable)]
    (process-loop ctx loop-info local-variable init)
    (process-let ctx local-variable init)))

(defmethod process-insn ::bc/pop [{:keys [stack] :as ctx} {:insn/keys [label length]}]
  (if-let [statement (peek stack)]
    (if-let [local-variable (find-init-local ctx (+ label length))]
      (-> ctx
          (update :stack pop)
          (update :local-variable-table disj local-variable)
          (update :local-variable-table conj (assoc local-variable :init statement))
          (process-lexical-block local-variable statement))
      (-> ctx
          (update :stack pop)
          (update :statements conj statement)))
    ctx))

(defmethod process-insn ::bc/store-insn [{:keys [stack] :as ctx} {:insn/keys [local-variable-element label length] :as insn}]
  (let [{:insn/keys [target-index]} local-variable-element]
    (if-let [local-variable (find-local-variable ctx target-index (+ label length))]
      (let [init (peek stack)
            initialized-local-variable (assoc local-variable :init init)
            ctx (-> ctx
                    (update :stack pop)
                    (update :local-variable-table disj local-variable)
                    (update :local-variable-table conj initialized-local-variable))]
        (if (init-local-variable? insn local-variable)
          (process-lexical-block ctx local-variable init)
          ctx))
      (process-letfn ctx target-index))))

(defn parse-collision-expr [exprs {:keys [test then else]}]
  (let [node [(-> test :args first) then]
        exprs (conj exprs node)]
    (if (= :if (-> else :ret :op))
      (recur exprs (:ret else))
      exprs)))

(defmethod process-insn ::bc/select [{:keys [stack] :as ctx} {:insn/keys [jump-targets label] :as insn}]
  (let [{:insn/keys [jump-offsets default-offset jump-matches]} jump-targets

        test (peek stack)

        shift+mask? (= :invoke (:op test))
        ?shift (when shift+mask?
                 (-> test :args first :args second :val))
        ?mask (when shift+mask?
                (-> test :args second :val))
        test (cond-> test shift+mask? (-> :args first :args first))

        hash-test? (= :invoke-static (:op test))
        test (cond-> test hash-test? (-> :args first))

        jump-labels (mapv (partial + label) jump-offsets)

        default-label (+ default-offset label)


        label-match (->> (for [i (range (count jump-labels))
                               :let [label (nth jump-labels i)
                                     match (nth jump-matches i)]
                               :when (not= label default-label)]
                           [label match])
                         (into []))

        ;; WIP: extract & refactor
        exprs (->> (for [i (range (count label-match))
                         :let [[label match] (nth label-match i)
                               [next-label] (nth (conj label-match [default-label nil]) (inc i))
                               end-label (:insn/label (insn-at ctx {:label next-label :offset -1}))]]

                     (cond

                       (= "getstatic" (:insn/name (insn-at ctx {:label label})))
                       (let [test-ctx (-> ctx
                                          (assoc :pc label
                                                 :statements []
                                                 :terminate? (pc= end-label))
                                          (process-insns))
                             test (expr+statements test-ctx)
                             exprs (-> test :body :ret :body :ret)]
                         [:collision match (parse-collision-expr [] exprs) test (:recur? test-ctx)])

                       hash-test?
                       (let [{:keys [stack] :as test-ctx} (-> ctx
                                                              (assoc :pc label
                                                                     :statements []
                                                                     :terminate? (fn [ctx]
                                                                                   (#{"if_acmpne" "invokestatic"}
                                                                                    (:insn/name (curr-insn ctx)))))
                                                              (process-insns))
                             test (peek stack)
                             hash-identity? (= "if_acmpne" (:insn/name (curr-insn test-ctx)))
                             start-expr-label (if hash-identity?
                                                (:insn/label (insn-at test-ctx {:offset 1}))
                                                (:insn/label (insn-at test-ctx {:offset 2})))
                             expr-ctx (-> ctx
                                          (assoc :pc start-expr-label
                                                 :statements []
                                                 :terminate? (pc= end-label))
                                          (process-insns))]
                         [(if hash-identity? :hash-identity :hash-equiv) match test (expr+statements expr-ctx) (:recur? expr-ctx)])

                       :else

                       (if (or (= "invokevirtual" (:insn/name (insn-at ctx {:offset -1})))
                               (and (= "invokevirtual" (:insn/name (maybe-insn-at ctx {:offset -5})))
                                    (= "iand" (:insn/name (insn-at ctx {:offset -1})))))

                         (let [{:keys [stack] :as test-ctx} (-> ctx
                                                                (assoc :pc label
                                                                       :statements []
                                                                       :terminate? (fn [ctx]
                                                                                     (= "invokestatic" (:insn/name (curr-insn ctx)))))
                                                                (process-insns))
                               test (peek stack)

                               expr-ctx (-> ctx
                                            (assoc :pc (:insn/label (insn-at test-ctx {:offset 2}))
                                                   :statements []
                                                   :terminate? (pc= end-label))
                                            (process-insns))]
                           [:int match test (expr+statements expr-ctx) (:recur? expr-ctx)])

                         (let [{:keys [stack] :as test-ctx} (-> ctx
                                                                (assoc :pc label
                                                                       :statements []
                                                                       :terminate? (some-fn (pc= end-label)
                                                                                            (fn [ctx]
                                                                                              (= "lcmp" (:insn/name (curr-insn ctx))))))
                                                                (process-insns))]

                           (if (= "lcmp" (:insn/name (curr-insn test-ctx)))
                             (let [[test _] (peek-n stack 2)
                                   expr-ctx (-> ctx
                                                (assoc :pc (:insn/label (insn-at test-ctx {:offset 2}))
                                                       :statements []
                                                       :terminate? (pc= end-label))
                                                (process-insns))]
                               [:int match test  (expr+statements expr-ctx) (:recur? expr-ctx)])

                             ;; huh?
                             [:int match match (peek stack) (:recur? test-ctx)])))))

                   (into []))

        end-label (-> (insn-at ctx {:label default-label :offset -1}) (goto-label))

        default-expr (-> ctx
                         (assoc :pc default-label
                                :statements []
                                :terminate? (pc= end-label))
                         (process-insns)
                         (expr+statements))
        expr {:op :case
              :test test
              :shift (or ?shift 0)
              :mask (or ?mask 0)
              :default default-expr
              :type (if (= "lookuptable" (:insn/name insn)) :sparse :compact)
              :switch-type (if hash-test? (if (every? (comp #{:hash-identity} first) exprs) :hash-identity :hash-equiv) :int)
              :skip-check (when hash-test?
                            (->> (for [i (range (count exprs))
                                       :let [[type] (nth exprs i)]
                                       :when (= :collision type)]
                                   i)
                                 (into #{})))
              :exprs exprs}
        recur? (boolean (seq (for [[_ _ _ _ recur?] exprs
                                   :when recur?]
                               true)))]

    (-> ctx
        (assoc :recur? recur?)
        (update :stack pop)
        (update :stack conj expr)
        (assoc :pc end-label))))

(defmethod process-insn :instanceof [{:keys [stack] :as ctx} {:insn/keys [pool-element]}]

  (if (or (isa? bc/insn-h (-> (maybe-insn-at ctx {:offset 5}) :insn/name keyword) ::bc/select)
          (and (isa? bc/insn-h (-> (maybe-insn-at ctx {:offset 9}) :insn/name keyword) ::bc/select)
               (= "ishr" (-> (insn-at ctx {:offset 6}) :insn/name))))

    (-> ctx
        (assoc :pc (:insn/label (insn-at ctx {:offset 5}))))


    (let [{:insn/keys [target-type]} pool-element
          instance (peek stack)]
      (-> ctx
          (update :stack pop)
          (update :stack conj {:op :invoke
                               :fn {:op :var
                                    :ns "clojure.core"
                                    :name "instance?"}
                               :args [{:op :const
                                       :val (symbol target-type)}
                                      instance]})))))

;; WIP new on :new rather than invokespecial
(defmethod process-insn :new [ctx _]
  ctx)

(defmethod process-insn :invokespecial [{:keys [stack bc-for] :as ctx} {:insn/keys [pool-element]}]
  (let [{:insn/keys [target-class target-arg-types]} pool-element
        argc (count target-arg-types)
        args (peek-n stack argc)]
    (-> ctx
        (update :stack pop-n (inc argc))
        (update :stack conj (let [bc (bc-for target-class)]
                              (if (#{"clojure.lang.AFunction" "clojure.lang.RestFn"} (:class/super bc))
                                (bc->ast bc {:bc-for bc-for})
                                {:op :new
                                 :class target-class
                                 :args args}))))))

(defmethod process-insn :athrow [{:keys [stack] :as ctx} _]
  (let [ex (peek stack)]
    (-> ctx
        (update :stack pop)
        (update :statements conj {:op :throw
                                  :ex ex}))))


(defmethod process-insn ::bc/invoke-instance-method [{:keys [stack] :as ctx} {:insn/keys [pool-element]}]
  (let [{:insn/keys [target-class target-name target-ret-type target-arg-types]} pool-element
        argc (count (conj target-arg-types target-class))
        [target & args] (peek-n stack argc)]
    (-> ctx
        (update :stack pop-n argc)
        (update (if (= "void" target-ret-type) :statements :stack)
                conj {:op :invoke-instance
                      :method target-name
                      :target target
                      :arg-types target-arg-types
                      :target-class target-class
                      :args args}))))

(defmethod process-insn :putstatic [{:keys [stack class-name] :as ctx} {:insn/keys [pool-element]}]
  (let [{:insn/keys [target-class target-name]} pool-element
        val (peek stack)
        ctx (update ctx :stack pop)]
    (if (= class-name target-class)
      (-> ctx
          (update :fields assoc target-name val))
      (-> ctx
          (update :statements conj {:op :set!
                                    :target {:op :static-field
                                             :target target-class
                                             :field target-name}
                                    :val val})))))

(defn process-keyword-invoke [{:keys [fields] :as ctx} {:insn/keys [pool-element]}]
  (let [{:insn/keys [target-name]} pool-element
        {:keys [pc statements stack]} (process-insns (assoc ctx
                                                            :pc (:insn/label (insn-at ctx {:offset 2}))
                                                            :terminate? (fn [ctx]
                                                                          (->> (curr-insn ctx)
                                                                               :insn/name
                                                                               (= "dup_x2")))
                                                            :statements []))
        target (->do (conj statements (peek stack)))]
    (-> ctx
        (assoc :pc (+ pc 36)) ;; why bother writing robust code when we can just hardcode bytecode offsets
        (update :stack conj {:op :invoke
                             :fn (-> (get-in fields [target-name :args 0 :args 1])
                                     (update :val keyword))
                             :args [target]}))))

(defmethod process-insn :getstatic [{:keys [fields class-name] :as ctx} {:insn/keys [pool-element] :as insn}]
  (let [{:insn/keys [target-class target-name target-type]} pool-element]
    (cond

      (and (= target-type "clojure.lang.ILookupThunk")
           (= target-class class-name))
      (process-keyword-invoke ctx insn)

      (= target-class class-name)
      (update ctx :stack conj (get fields target-name))

      :else
      (update ctx :stack conj {:op :static-field
                               :target target-class
                               :field target-name}))))

(defmethod process-insn :putfield [{:keys [class-name stack] :as ctx} {:insn/keys [pool-element]}]
  (let [{:insn/keys [target-class target-name]} pool-element
        [instance val] (peek-n stack 2)]
    (-> ctx
        (update :stack pop-n 2)
        (cond-> (not= :closed-over (:op val))
          (update :statements conj {:op :set!
                                    :target (if (= target-class class-name)
                                              {:op :local
                                               :name target-name}
                                              {:op :instance-field
                                               :instance instance
                                               :field target-name})
                                    :val val})))))

(defn skip-locals-clearing-field [ctx]
  ;; WIP must make sure it's not a mutable deftype field
  (if (and (= "aload_0" (:insn/name (insn-at ctx {:offset 1})))
           (= "aconst_null" (:insn/name (insn-at ctx {:offset 2})))
           (= "putfield" (:insn/name (insn-at ctx {:offset 3})))
           (= (-> (curr-insn ctx) :insn/pool-element :insn/target-name)
              (-> (insn-at ctx {:offset 3}) :insn/pool-element :insn/target-name)))
    (-> ctx
        (assoc :pc (:insn/label (insn-at ctx {:offset 4}))))
    ctx))

(defmethod process-insn :getfield [{:keys [fields class-name stack] :as ctx} {:insn/keys [pool-element]}]
  (let [{:insn/keys [target-class target-name]} pool-element
        instance (peek stack)
        ctx (update ctx :stack pop)]
    (if (= target-class class-name)
      (-> ctx
          (update :stack conj (get fields target-name {:op :local :name target-name}))
          (skip-locals-clearing-field))
      (update ctx :stack conj {:op :instance-field
                               :instance instance
                               :field target-name}))))

(defmethod process-insn :invokestatic [{:keys [stack] :as ctx} {:insn/keys [pool-element]}]
  (let [{:insn/keys [target-class target-name target-ret-type target-arg-types]} pool-element
        argc (count target-arg-types)
        args (peek-n stack argc)]
    (-> ctx
        (update :stack pop-n argc)
        (update (if (= "void" target-ret-type) :statements :stack)
                conj {:op :invoke-static
                      :target target-class
                      :method target-name
                      :arg-types target-arg-types
                      :args args}))))

(defmethod process-insn ::bc/math-insn [{:keys [stack] :as ctx} {:insn/keys [name]}]
  (let [argc (if (#{"dneg" "lneg"} name) 1 2)
        args (peek-n stack argc)
        op  ({"dadd" "+"
              "ddiv" "/"
              "dmul" "*"
              "dneg" "-"
              "dsub" "-"
              "iadd" "+"
              "iand" "bit-and"
              "idiv" "/"
              "imul" "*"
              "irem" "rem"
              "ineg" "-"
              "ishl" "bit-shift-left"
              "ishr" "bit-shift-right"
              "isub" "-"
              "iushr" "unsigned-bit-shift-right"
              "ladd" "+"
              "land" "bit-and"
              "ldiv" "quot"
              "lneg" "-"
              "lmul" "*"
              "lor" "bit-or"
              "lrem" "rem"
              "lshl" "bit-shift-left"
              "lshr" "bit-shift-right"
              "lsub" "-"
              "lushr" "unsigned-bit-shift-right"
              "lxor" "bit-xor"} name)]
    (-> ctx
        (update :stack pop-n argc)
        (update :stack conj {:op :invoke
                             :fn {:op :var
                                  :ns "clojure.core"
                                  :name op}
                             :args args}))))

(defmethod process-insn :checkcast [{:keys [stack] :as ctx} {:insn/keys [pool-element]}]
  (let [{:insn/keys [target-type]} pool-element
        target (peek stack)]

    (cond-> ctx

      target
      (-> (update :stack pop)
          (update :stack conj (assoc target :cast target-type))))))

;; protocol inline caches
(defmethod process-insn :if_acmpeq [{:keys [stack] :as ctx} _]
  (-> ctx
      (update :stack pop-n 2)
      (update :pc + 17)))

(defn merge-tables [ctx local-variable-table exception-table]
  (let [lvt (->> (for [{:local-variable/keys [name index start-label end-label]} local-variable-table]
                   {:op :local
                    :start-label start-label
                    :end-label end-label
                    :index index
                    :name name})
                 (into #{}))
        et (->> (for [{:exception-handler/keys [type start-label end-label handler-label]} exception-table]
                  {:start-label start-label
                   :end-label end-label
                   :handler-label handler-label
                   :type type})
                (into #{}))]
    (-> ctx
        (assoc :local-variable-table lvt)
        (assoc :exception-table et)
        (assoc :loop-args (->> lvt
                               (filter (comp zero? :start-label))
                               (sort-by :index)
                               (vec))))))

(defn collect-impure-loops-data [{:keys [insns] :as ctx}]
  (loop [[insn & insns] insns data #{}]
    (if insn
      (if (and (= "goto" (:insn/name insn))
               (not (pos? (:insn/jump-offset insn)))
               (not (isa? bc/insn-h (-> ctx
                                        (assoc :pc (:insn/label insn))
                                        (maybe-insn-at {:offset -1})
                                        :insn/name
                                        keyword)
                          ::bc/store-insn)))
        (recur insns (conj data (goto-label insn)))
        (recur insns data))
      (assoc ctx :impure-loops data))))

(defn process-method-insns [{:keys [fn-name] :as ctx} {:method/keys [bytecode jump-table local-variable-table flags exception-table]}]
  (-> ctx
      (merge initial-local-ctx {:jump-table jump-table})
      (merge-tables local-variable-table exception-table)
      (cond-> (not (:static flags))
        (-> (update :local-variable-table disj {:op :local
                                                :start-label 0
                                                :end-label (-> bytecode peek :insn/label)
                                                :index 0
                                                :name "this"})
            (update :local-variable-table conj {:op :local
                                                :this? true
                                                :index 0
                                                :name (or fn-name "this")
                                                :start-label 0
                                                :end-label (-> bytecode peek :insn/label)})
            (update :loop-args #(vec (rest %)))))
      (assoc :insns bytecode)
      (collect-impure-loops-data)
      (process-insns)))

(defn process-static-init [{:keys [bc-for] :as ctx} {:class/keys [methods]}]
  (let [method (u/find-method methods {:method/name "<clinit>"})]
    (-> ctx
        (process-method-insns method))))

(defn process-init [{:keys [bc-for] :as ctx} {:class/keys [methods]}]
  (let [method (u/find-method methods {:method/name "<init>"})
        {:method/keys [arg-types]} method]
    (-> ctx
        (assoc :closed-overs (second (reduce (fn [[i c] a] [(+ i a) (conj c i)]) [1 #{0}]
                                             (map #({"long" 2 "double" 2} % 1)
                                                  arg-types))))
        (process-method-insns method))))

(defn decompile-fn-method [{:keys [fn-name] :as ctx} {:method/keys [local-variable-table flags name] :as method}]
  (let [{:keys [ast]} (process-method-insns ctx method)
        args (for [{:local-variable/keys [index name type start-label]} (->> local-variable-table
                                                                             (sort-by :local-variable/index))
                   :when (and (zero? start-label)
                              (or (:static flags)
                                  (not (zero? index))))]
               {:name name
                :type type})]

    {:op :fn-method
     :fn-name fn-name
     :var-args? (or (= "doInvoke" name)
                    (= "clojure.lang.ISeq" (-> args last :type)))
     :args args
     :body ast}))

(defn decompile-fn-methods [{:keys [fn-name] :as ctx} {:class/keys [methods]}]
  (let [invokes-static (u/find-methods methods {:method/name "invokeStatic"})
        invokes-prim (u/find-methods methods {:method/name "invokePrim"})
        invokes (u/find-methods methods {:method/name "invoke"})
        invoke-vararg (u/find-method methods {:method/name "doInvoke"})
        invoke-methods (-> invokes-static
                           (into (for [{:method/keys [arg-types] :as invoke} invokes-prim
                                       :let [argc (count arg-types)]
                                       :when (empty? (filter (fn [{:method/keys [arg-types]}]
                                                               (= (count arg-types) argc))
                                                             invokes-static))]
                                   invoke)))

        invoke-methods (-> invoke-methods
                           (into (for [{:method/keys [arg-types] :as invoke} (into invokes (when invoke-vararg
                                                                                             [invoke-vararg]))
                                       :let [argc (count arg-types)]
                                       :when (empty? (filter (fn [{:method/keys [arg-types]}]
                                                               (= (count arg-types) argc))
                                                             invoke-methods))]
                                   invoke)))
        methods-asts (mapv (partial decompile-fn-method ctx) invoke-methods)]
    {:op :fn
     :name fn-name
     :fn-methods methods-asts}))

(defn extract-fn-name [^String cname]
  (let [fname (subs cname (inc (.lastIndexOf cname "$")))
        pretty-fname (second (re-matches #"(.+)__[0-9]+$" fname))]
    (if (and pretty-fname
             (not= pretty-fname "fn"))
      pretty-fname
      fname)))

(defn decompile-fn [{class-name :class/name :as bc} {:keys [fn-name] :as ctx}]
  (-> ctx
      (assoc :fn-name (or fn-name (extract-fn-name class-name)))
      (assoc :class-name class-name)
      (process-static-init bc)
      (process-init bc)
      (decompile-fn-methods bc)))


(defn process-ns-inits [ctx {:class/keys [methods]}]
  (reduce (fn [ctx i]
            (if-let [method (u/find-method methods {:method/name (str "__init" i)})]
              (process-method-insns ctx method)
              (reduced ctx)))
          ctx (range)))

(defn process-ns-load [ctx {:class/keys [methods]}]
  (let [{:method/keys [bytecode jump-table]} (u/find-method methods {:method/name "load"})
        ctx (-> ctx
                (assoc
                 :terminate? (comp seq :statements)
                 :insns bytecode)
                (merge initial-local-ctx {:jump-table jump-table}))
        indicize (fn [s i]
                   (reduce (fn [[s i] insn]
                             (if (::idx insn)
                               [(conj s insn) i]
                               [(conj s (assoc insn ::idx i)) (inc i)]))
                           [[] i] s))]
    (loop [{:keys [stack statements] :as ctx} (process-insns ctx)
           init []
           i 0]
      (let [[stack i] (indicize stack i)
            [statements i] (indicize statements i)]
        (if (and (seq statements))
          (recur (process-insns (assoc ctx :statements [] :stack stack))
                 (into init statements)
                 i)
          (->do (sort-by ::idx (concat init stack statements))))))))

(defn decompile-ns [{class-name :class/name :as bc} {:keys [fn-name] :as ctx}]
  (-> ctx
      (assoc :class-name class-name)
      (process-ns-inits bc)
      (process-ns-load bc)))

(defn process-methods [ctx methods]
  (->> (for [{:method/keys [name local-variable-table] :as method} methods]
         {:op :method
          :name name
          :args (->> (for [{:local-variable/keys [name start-label type]} (->> local-variable-table
                                                                               (sort-by :local-variable/index))
                           :when (zero? start-label)]
                       {:name name
                        :type type})
                     (into []))
          :body (:ast (process-method-insns ctx method))})
       (into [])))

(defn decompile-deftype [{:class/keys [fields interfaces methods ^String name] :as bc} ctx]
  (let [fields (->> (for [{:field/keys [name flags]} fields]
                      {:name name
                       :mutable? (cond
                                   (:volatile flags) :volatile-mutable
                                   (:final flags) false
                                   :else :unsynchronized-mutable)})
                    (into []))
        instance-methods (->> methods
                              (remove (comp :static :method/flags))
                              ;; bridge
                              (remove (comp :volatile :method/flags))
                              (remove (comp #{"<init>"} :method/name)))
        ctx (-> ctx (process-static-init bc))]
    {:op :deftype
     :name name
     :tname (.replaceFirst name "\\." "/")
     :fields fields
     :methods (process-methods ctx instance-methods)
     :interfaces interfaces}))

(defn decompile-reify [{:class/keys [interfaces methods] :as bc} ctx]
  (let [instance-methods (->> methods
                              (remove (comp :static :method/flags))
                              ;; bridge
                              (remove (comp :volatile :method/flags))
                              (remove (comp #{"<init>" "meta" "withMeta"} :method/name)))
        ctx (-> ctx (process-static-init bc))]
    {:op :reify
     :methods (process-methods ctx instance-methods)
     :interfaces (vec (remove #{"clojure.lang.IObj"} interfaces))}))

(defn bc->ast [{:class/keys [interfaces super ^String name] :as bc} ctx]
  (let [ctx (merge ctx initial-ctx)]
    (cond
      (#{"clojure.lang.AFunction" "clojure.lang.RestFn"} super)
      (decompile-fn bc ctx)

      (.endsWith name "__init")
      (decompile-ns bc ctx)

      (some #{"clojure.lang.IType" "clojure.lang.IRecord"} interfaces)
      (decompile-deftype bc ctx)

      (and (some #{"clojure.lang.IObj"} interfaces)
           (.contains name "$reify__"))
      (decompile-reify bc ctx)

      :else
      (throw (Exception. ":(")))))

;;; genclass, proxy
;; WIP int -> booleans
