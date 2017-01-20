(ns ensemble-analyzer.core
  (:gen-class))

(import '(java.awt GridLayout)
        '(javax.swing JFrame))

(require 'ensemble-analyzer.fft-cl)
(alias 'fft 'ensemble-analyzer.fft-cl)

(require 'ensemble-analyzer.chromatic)
(alias 'chr 'ensemble-analyzer.chromatic)

(def color-map (ref nil))

(defn make-frame []
  (let [frame (JFrame. "Ensemble Analyzer")
        panel2 (chr/make-panel2)
        [status panel] (chr/make-panel panel2)]
    (chr/update-panel2 status panel2)
    (.. frame getContentPane (setLayout (GridLayout. 1 2)))
    (.. frame getContentPane (add panel))
    (.. frame getContentPane (add panel2))
    (.pack frame)
    (.setVisible frame true)
    frame))

(defn -main [& args]
  (fft/init)
  (let [frame (make-frame)]
    (fft/finalize)))
