(ns avi.buffer.motion.resolve)

(defmulti resolve-motion
  (fn [_ [motion-type]]
    motion-type))
