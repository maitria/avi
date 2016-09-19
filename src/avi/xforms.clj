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

(defn cata
  "A catamorphic transducing context.

  Each node in the tree has it's children processed by xform in depth-first
  order.

  xfmap takes xform and a node and rebuilds the nodes' children by passing
  them through xform, e.g. (update node ::children #(into [] xform %)).

  Returns a possibly empty sequence of results (the root node could have
  been filtered or duplicated by xform).

    (cata (fn [xf node]
            (if (coll? node)
              (let [[op & args] node]
                (cons op (sequence xf args)))
              node))
          (map (fn [node]
                 (if (coll? node)
                   (let [[op & args] node]
                     (apply (resolve op) args))
                   node)))
          '(* (+ 2 2) (/ 8 2))) ;;=> (16)"
  [xfmap xform tree]
  (letfn [(cata' [tree]
            (xfmap (comp (map cata') xform) tree))]
    (sequence (comp (map cata') xform) [tree])))
