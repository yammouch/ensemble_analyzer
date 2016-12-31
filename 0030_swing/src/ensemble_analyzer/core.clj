(ns ensemble-analyzer.core
  (:gen-class))

(import '(java.io File)
        '(javax.sound.sampled AudioSystem)
        '(java.awt Dimension BorderLayout Color)
        '(javax.swing JFrame JPanel))

(require 'ensemble-analyzer.fft)
(alias 'fft 'ensemble-analyzer.fft)

(require 'ensemble-analyzer.chromatic)
(alias 'chr 'ensemble-analyzer.chromatic)

(defn read-file []
  (let [stream (AudioSystem/getAudioInputStream
                (File. "../data/pascal_20161217.wav"))
        len (.available stream)
        buf (byte-array len)]
    (.read stream buf 0 len)
    (map (fn [[lsb msb]]
           (let [l (bit-and (int lsb) 0xFF)
                 m (bit-and (int msb) 0xFF)
                 x (bit-or (bit-shift-left m 8) l)]
             (if (= 0 (bit-and x 0x8000)) ; if plus
               x
               (- x 0x10000))))
         (partition 2 buf))))

(def sampling-rate 44.1e3) ; s^(-1)
(def nfft 4096)
(def fa 442.0) ; Hz

(def color-map (ref nil))

(defn paint-spectrum [g]
  (doseq [[y s] (map vector (range 199 -1 -1) @color-map)]
    (.setColor g (Color. s s s))
    (.fillRect g 0 y 5 1)))

(defn make-panel []
  (proxy [JPanel] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (paint-spectrum g))
    (getPreferredSize [] 
      (Dimension. 200 200)
      )))

(defn make-frame []
  (let [frame (JFrame. "Ensemble Analyzer")
        panel (make-panel)]
    (.. frame getContentPane (add panel BorderLayout/CENTER))
    (.pack frame)
    (.setVisible frame true)
    frame))

(require 'clojure.pprint)

(defn -main [& args]
  (let [spectrum (vec (fft/fft-mag-norm (take nfft (read-file))
                                        (bit-shift-left 1 15)))
        chromatic (chr/pickup (chr/index (* fa (Math/pow 2.0 -36.5)) ; A1
                                         (* fa (Math/pow 2.0  24.5)) ; A6
                                         200
                                         (/ sampling-rate nfft)
                                         (/ nfft 2))
                              spectrum)
        log10 (Math/log 10.0)
        db (map (fn [x] (/ (* 20.0 (Math/log (+ x 1e-10))) log10))
                chromatic)
        db-min -80.0 db-max 0.0
        coeff (/ 254.0 (- db-max db-min))
        visuals (map (fn [x]
                       (let [i (int (+ 1.0 (* (- x db-min) coeff)))]
                         (cond (<   i 0) 0
                               (< 255 i) 255
                               :else     i)))
                     db)]
    (spit "visuals.dat"
          (with-out-str (clojure.pprint/pprint visuals)))
    (dosync
      (ref-set color-map visuals))
    (spit "color-map.dat"
          (with-out-str (clojure.pprint/pprint color-map)))
    (let [frame (make-frame)]
      :done)))
