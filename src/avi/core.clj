(ns avi.core
  (:import [avi.terminal Terminal])
  (:require [avi.buffer :as b]
            [avi.editor :as e]
            [avi.command-line-mode]
            [avi.normal-mode]
            [avi.insert-mode]
            [avi.render :as render])
  (:gen-class))

(defmethod e/process :resize
  [editor [_ size]]
  (-> editor
      (assoc-in [:viewport :size] size)
      (e/update-current-buffer #(b/resize % (- (first size) 2)))))

(defn- update-screen
  [editor]
  (let [{chars :chars,
         attrs :attrs,
         width :width,
         [i j] :cursor} (render/render editor)]
    (Terminal/refresh i j width chars attrs)))

(defn- screen-size
  []
  (let [size (Terminal/size)]
    [(get size 0) (get size 1)]))

(defn -main
  [& args]
  (Terminal/start)
  (loop [[height width] (screen-size)
         editor (apply e/initial-editor [height width] args)]
    (if (:beep? editor)
      (Terminal/beep))
    (let [editor (if (not= [height width] (:size (:viewport editor)))
                   (e/process editor [:resize [height width]])
                   editor)]
      (update-screen editor)
      (if-not (= (:mode editor) :finished)
        (recur
          (screen-size)
          (e/process editor [:keystroke (Terminal/getKey)])))))
  (Terminal/stop))
