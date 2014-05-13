(ns avi.core
  (:import [avi.terminal Terminal])
  (:require [packthread.core :refer :all]
            [avi.editor :as e]
            [avi.command-line-mode]
            [avi.normal-mode]
            [avi.insert-mode]
            [avi.render :as render])
  (:gen-class))

(defprotocol World
  "Avi's interface to the world."
  (setup [this])
  (cleanup [this])
  (read-key [this])
  (beep [this])
  (terminal-size [this])
  (update-terminal [this rendering]))

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
  (let [initial-editor (e/initial-editor (terminal-size world) args)]
    (->> (event-stream world)
         (reductions e/respond initial-editor)
         (take-while (complement e/finished?)))))

(defn- perform-effects!
  [editor world]
  (when (:beep? editor)
    (beep world))
  (update-terminal world (render/render editor)))

(defn- run
  [world args]
  (setup world)
  (doseq [editor (editor-stream world args)]
    (perform-effects! editor world))
  (cleanup world))

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
                                     [i j] :cursor}]
                  (Terminal/refresh i j width chars attrs)))]
    (run world args)))
