(ns avi.core
  (:import [avi.terminal Terminal])
  (:require [packthread.core :refer :all]
            [avi.editor :as e]
            [clojure.stacktrace :as st]
            [avi.main]
            [avi.world :refer :all])
  (:gen-class))

(defn- event-stream
  ([world]
   (event-stream world (terminal-size world)))
  ([world current-size]
   (lazy-seq
     (let [keystroke (read-key world)
           new-size (terminal-size world)]
       (cond->> (event-stream world new-size)
         true
         (cons [:keystroke keystroke])

         (not= current-size new-size)
         (cons [:resize new-size]))))))

(defn- editor-stream
  [world args]
  (let [responder (avi.main/responder true)
        initial-editor (avi.main/initial-editor (terminal-size world) args)]
    (->> (event-stream world)
         (reductions responder initial-editor)
         (take-while (complement :finished?)))))

(defn- perform-effects!
  [editor]
  (when (:avi.editor/beep? editor)
    (beep *world*))
  (update-terminal *world* (:rendition editor)))

(defn- run
  [world args]
  (binding [*world* world]
    (setup *world*)
    (doseq [editor (editor-stream *world* args)]
      (perform-effects! editor))
    (cleanup *world*)))

(defn- clean-exit
  [world]
  (binding [*world* world]
    (cleanup *world*)))

(defn -main
  [& args]
  (let [world (reify
                World
                (setup [_] (Terminal/start))
                (cleanup [_] (Terminal/stop))
                (read-key [_] (Terminal/getKey))
                (beep [_] (Terminal/beep))
                (terminal-size [_]
                  (let [size (Terminal/size)]
                    [(get size 0) (get size 1)]))
                (update-terminal [_ {chars :chars,
                                     attrs :attrs,
                                     width :width,
                                     [i j] :point}]
                  (Terminal/refresh i j width chars attrs))
                (read-file [_ filename]
                  (slurp filename))
                (write-file [_ filename contents]
                  (spit filename contents)))]
    (try
      (run world args)
      (catch Exception e ((clean-exit world)
                          (println "============================================================")
                          (println "You have caught a bug in Avi")
                          (println "Stacktrace for the issue follows:")
                          (st/print-stack-trace e)
                          (println "============================================================")
                          (println "Issue can be logged for avi at:")
                          (println "https://github.com/maitria/avi/issues")
                          (println "Do also provide the exact steps to reproduce the issue there")
                          (println "============================================================"))))))
