(ns avi.compose)

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
