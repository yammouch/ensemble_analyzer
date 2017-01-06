(ns ensemble-analyzer.core
  (:gen-class))

(import '(java.io File)
        '(javax.sound.sampled AudioSystem)
        '(java.awt Dimension BorderLayout Color)
        '(java.awt.image BufferedImage)
        '(javax.swing JFrame JPanel ImageIcon JLabel))

;(require 'ensemble-analyzer.fft)
;(alias 'fft 'ensemble-analyzer.fft)

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
  (let [spectrum (time (doall
                  (map (fn [v] (vec (fft/fft-mag-norm v (bit-shift-left 1 15))))
                       (take 600 (drop 400 (partition nfft (read-file)))))))
        pickup-index (time (doall
                      (chr/index (* fa (Math/pow 2.0 (/ -36.5 12.0))) ; A1
                                 (* fa (Math/pow 2.0 (/  24.5 12.0))) ; A6
                                 600
                                 (/ sampling-rate nfft)
                                 (/ nfft 2))))
        chromatic (time (doall
                   (map (fn [v] (chr/pickup pickup-index v))
                        spectrum)))
        log10 (Math/log 10.0)
        db (time (doall
            (map2d (fn [x] (/ (* 20.0 (Math/log (+ x 1e-10)))
                              log10))
                   chromatic)))
        db-min -80.0 db-max 0.0
        coeff (/ 254.0 (- db-max db-min))
        visuals (time (doall
                 (map2d (fn [x]
                          (let [i (int (+ 1.0 (* (- x db-min) coeff)))]
                            (cond (<   i 0) 0
                                  (< 255 i) 255
                                  :else     i)))
                        db)))
        img (BufferedImage. 600 600 BufferedImage/TYPE_INT_ARGB)
        _ (.setRGB img 0 0 600 600
           (int-array (map (fn [i] (bit-or 0xFF000000
                                           (bit-shift-left i 16)
                                           (bit-shift-left i  8)
                                           i))
                           (apply concat
                            (reverse (apply map vector visuals)))))
           0 600)
        frame (make-frame img)]
    (fft/finalize)))
