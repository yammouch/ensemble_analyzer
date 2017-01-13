(ns ensemble-analyzer.core
  (:gen-class))

(import '(java.io File)
        '(javax.sound.sampled AudioSystem)
        '(java.awt Dimension BorderLayout Color)
        '(java.awt.image BufferedImage)
        '(javax.swing JFrame JPanel ImageIcon JLabel))

(require 'ensemble-analyzer.fft-cl)
(alias 'fft 'ensemble-analyzer.fft-cl)

(require 'ensemble-analyzer.chromatic)
(alias 'chr 'ensemble-analyzer.chromatic)

(defn read-file []
  (let [stream (AudioSystem/getAudioInputStream
                (File. "../data/pascal_20161217.wav"))
        len (.available stream)
        buf (byte-array len)]
    (.read stream buf 0 len)
    buf))

(def sampling-rate 44.1e3) ; s^(-1)
(def nfft 4096)
(def fa 442.0) ; Hz

(def color-map (ref nil))

(defn make-panel []
  (proxy [JPanel] []
    (getPreferredSize [] 
      (Dimension. 600 600)
      )))

(defn make-frame [img]
  (let [frame (JFrame. "Ensemble Analyzer")
        panel (make-panel)]
    (.add panel (JLabel. (ImageIcon. img)))
    (.. frame getContentPane (add panel BorderLayout/CENTER))
    (.pack frame)
    (.setVisible frame true)
    frame))

(require 'clojure.pprint)

(defn map2d [f vv]
  (map (fn [v] (map f v))
       vv))

(defn -main [& args]
  (fft/init)
  (let [waveform (read-file)
        status (chr/fft {:waveform waveform, :bit-resolution 15})
        status (chr/freq-to-pix (conj status
                                 {:freq-of-a4 442.0
                                  :pitch-range [-36 24]
                                  :image-height 600}))
        status (chr/pickuper status)
        status (chr/color-mapper (conj status {:db-range [-80.0 0.0]}))
        status (chr/horizon-handler (conj status {:image-dimension [600 600]}))
        frame (make-frame (:image status))]
    (fft/finalize)))
