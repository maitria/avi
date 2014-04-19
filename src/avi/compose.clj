(ns avi.compose)

(defmacro ->*
  "Thread a subset of (or view of) state through forms."
  [state path & forms]
  state)
