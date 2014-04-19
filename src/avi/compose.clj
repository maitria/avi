(ns avi.compose)

(defn- splice-form
  [value form]
  (let [form (if (list? form)
               form
               (list form))
        spliced-form (apply list (first form) value (rest form))]
    spliced-form))

(defmacro ->'
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
