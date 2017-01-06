(ns image.core
  (:gen-class))

(ns image.core
  (:gen-class))

(import '(java.io File)
        '(java.awt Dimension BorderLayout Color)
        '(java.awt.image BufferedImage)
        '(javax.swing JFrame JPanel ImageIcon JLabel))

(require 'clojure.pprint)

(defn make-panel []
  (proxy [JPanel] []
    (getPreferredSize [] 
      (Dimension. 600 600)
      )))

(def img (ref nil))
(def panel (ref nil))

(defn make-frame []
  (let [frame (JFrame. "Ensemble Analyzer")]
    (dosync
     (ref-set panel (make-panel))
     (ref-set img
      (BufferedImage. 600 600 BufferedImage/TYPE_INT_ARGB)))
    (.setRGB @img 0 0 600 600
     (int-array (range 0xFF000000 (+ 0xFF000000 360000)))
     0 600)
    (.add @panel (JLabel. (ImageIcon. @img)))
    (.. frame getContentPane (add @panel BorderLayout/CENTER))
    (.pack frame)
    (.setVisible frame true)
    frame))

(defn -main [& args]
  (let [frame (make-frame)]
    (Thread/sleep 2000)

    (println "2 seconds.")
    (.setRGB @img 0 0 600 600
     (int-array (range 0xFFFF0000 (- 0xFFFF0000 360000) -1))
     0 600)
    (.repaint @panel)
    (Thread/sleep 2000)

    (println "2 seconds.")
    (dosync 
      (ref-set img
       (BufferedImage. 200 200 BufferedImage/TYPE_INT_ARGB)))
    (.setRGB @img 0 0 200 200
     (int-array (range 0xFF000000 (+ 0xFF000000 40000)))
     0 200)
    (.removeAll @panel)
    (.add @panel (JLabel. (ImageIcon. @img)))
    (.validate @panel)
    (.repaint @panel)

    :done))
