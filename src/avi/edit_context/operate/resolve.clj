(ns avi.edit-context.operate.resolve)

(defmulti resolve-motion
  (fn [_ {[motion-type] :motion}]
    motion-type))
