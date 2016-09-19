(ns avi.xforms)

(defn annotate
  "A transducer which annotates inputs by stateful process.

  f accepts state and input and returns [state' input'].  To index inputs:

     (annotate (fn [input n] [(assoc input ::pos n) (inc n)]) 0)"
  [f init]
  (fn [rf]
    (let [state (volatile! init)]
      (fn annotate-rf
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [[state' input'] (f @state input)]
           (vreset! state state')
           (rf result input')))
        ([result input & inputs]
         (let [[state' input' & inputs'] (apply f @state input inputs)]
           (vreset! state state')
           (apply rf result input' inputs')))))))
