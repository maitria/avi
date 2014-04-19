(ns avi.compose)

(defmacro ->'
  [initial-value & forms]
  (loop [result initial-value
         forms forms]
    (if-not (seq forms)
      result
      (let [[form & forms] forms
            form (if (list? form)
                   form
                   (list form))
            combined-result (apply list (first form) result (rest form))]
        (recur combined-result forms)))))

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
