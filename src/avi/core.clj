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
  (beep [this])
  (terminal-size [this])
  (update-terminal [this rendering]))

(defmethod e/respond :resize
  [editor [_ size]]
  (+> editor
      (assoc-in [:viewport :size] size)
      (in e/current-buffer
          (b/resize (- (first size) 2)))))

(defn- run
  [world & args]
  (setup world)
  (loop [[height width] (terminal-size world)
         editor (apply e/initial-editor [height width] args)]
    (when (:beep? editor)
      (beep world))
    (let [editor (if (not= [height width] (:size (:viewport editor)))
                   (e/respond editor [:resize [height width]])
                   editor)]
      (update-terminal world (render/render editor))
      (if-not (= (:mode editor) :finished)
        (recur
          (terminal-size world)
          (e/respond editor [:keystroke (Terminal/getKey)])))))
  (cleanup world))

(defn -main
  [& args]
  (let [world (reify
                World
                (setup [_] (Terminal/start))
                (cleanup [_] (Terminal/stop))
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
