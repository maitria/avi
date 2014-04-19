(ns avi.compose)

(declare ^:private splice-form)

(defn- splice-normal-form
  [value form]
  (apply list (first form) value (rest form)))

(defn- splice-if-form
  [value form]
  (let [value-symbol (gensym)
        if-type (first form)
        condition (second form)
        then-form (nth form 2)
        else-form (if (= 4 (count form))
                    (nth form 3)
                    '(identity))]
    `(let [~value-symbol ~value]
       (~if-type ~condition 
         ~(splice-form value-symbol then-form)
         ~(splice-form value-symbol else-form)))))

(defn- splice-form
  [value form]
  (let [form (if (list? form)
               form
               (list form))
        position-1 (first form)]
    (cond
      ('#{if if-not} position-1)
      (splice-if-form value form)

      :else
      (splice-normal-form value form))))

(defmacro ->'
  "Threading macro like `->`, except that some forms are treated specially.
  
  `if` and `if-not` forms within the body are treated specially, in that
  the value is threaded through the then and else clauses separately (instead
  of being inserted as the condition)."
  [initial-value & forms]
  (loop [result initial-value
         forms forms]
    (if-not (seq forms)
      result
      (let [[form & forms] forms]
        (recur (splice-form result form) forms)))))

(defmacro in->
  "Thread a view of state through forms.

  `path` should be either a keyword or an accessor function.  An accessor
  function receives the original state and a function that receives the new view
  of the state.  It should update the original state with the response of the
  function."
  [state path & forms]
  (let [inner-fn `(fn [sub-state#]
                    (-> sub-state#
                        ~@forms))]
    `(let [state# ~state
           path# ~path]
       (if (keyword? path#)
         (update-in state# [path#] ~inner-fn)
         (path# state# ~inner-fn)))))
