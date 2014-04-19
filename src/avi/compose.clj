(ns avi.compose)

(defmacro ->*
  "Thread a subset of (or view of) state through forms."
  [state path & forms]
  (let [inner-fn `(fn [sub-state#]
                    (-> sub-state#
                        ~@forms))]
    `(update-in ~state [~path] ~inner-fn)))
