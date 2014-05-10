(ns avi.core
  (:import [avi.terminal Terminal])
  (:require [packthread.core :refer :all]
            [avi.buffer :as b]
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

(defmethod e/respond :resize
  [editor [_ size]]
  (+> editor
      (assoc-in [:viewport :size] size)
      (in e/current-buffer
          (b/resize (- (first size) 2)))))

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

(defn- run
  [world & args]
  (setup world)
  (doseq [editor (->> (event-stream world)
                      (reductions e/respond (apply e/initial-editor (terminal-size world) args))
                      (take-while #(not (= :finished (:mode %)))))]
    (when (:beep? editor)
      (beep world))
    (update-terminal world (render/render editor)))
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
    (apply run world args)))
