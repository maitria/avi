(ns avi.buffer.operate.resolve)

(defmulti resolve-motion
  (fn [_ [motion-type]]
    motion-type))
