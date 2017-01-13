(ns ensemble-analyzer.core
  (:gen-class))

(import '(java.awt BorderLayout)
        '(javax.swing JFrame))

(require 'ensemble-analyzer.fft-cl)
(alias 'fft 'ensemble-analyzer.fft-cl)

(require 'ensemble-analyzer.chromatic)
(alias 'chr 'ensemble-analyzer.chromatic)

(def color-map (ref nil))

(defn make-frame []
  (let [frame (JFrame. "Ensemble Analyzer")
        panel (chr/make-panel)]
    (.. frame getContentPane (add panel BorderLayout/CENTER))
    (.pack frame)
    (.setVisible frame true)
    frame))

(defn -main [& args]
  (fft/init)
  (let [frame (make-frame)]
    (fft/finalize)))
