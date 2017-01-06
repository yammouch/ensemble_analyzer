(ns image.core
  (:gen-class))

(ns image.core
  (:gen-class))

(import '(java.io File)
        '(java.awt Dimension BorderLayout Color)
        '(java.awt.image BufferedImage)
        '(javax.swing JFrame JPanel ImageIcon JLabel))

(defn make-panel []
  (proxy [JPanel] []
    (getPreferredSize [] 
      (Dimension. 200 200)
      )))

(defn make-frame []
  (let [frame (JFrame. "Ensemble Analyzer")
        panel (make-panel)
        img (BufferedImage. 200 200 BufferedImage/TYPE_INT_ARGB)]
    (.setRGB img 0 0 200 200
     (int-array (range 0xFF000000 (+ 0xFF000000 40000)))
     0 200)
    (.add panel (JLabel. (ImageIcon. img)))
    (.. frame getContentPane (add panel BorderLayout/CENTER))
    (.pack frame)
    (.setVisible frame true)
    frame))

(defn -main [& args]
  (let [frame (make-frame)]
    :done))
