(ns ensemble-analyzer.chromatic)

(import '(java.io File)
        '(java.awt Dimension Color)
        '(java.awt.image BufferedImage)
        '(java.awt.event MouseListener MouseMotionListener)
        '(javax.swing JPanel JLabel ImageIcon)
        '(javax.sound.sampled AudioSystem))

(require 'ensemble-analyzer.fft-cl)
(alias 'fft 'ensemble-analyzer.fft-cl)

; lo : low freq in chromatic plot
; hi : high freq in chromatic plot
; np : number of pixels
; fr : freq resolution
(defn index [lo hi np fr n-valid-fft-bins]
  (let [[llog hlog] (map #(Math/log %) [lo hi])
        f (map #(Math/exp (+ llog (/ (* (- hlog llog) %)
                                     np)))
               (range (inc np)))
        fnorm (map #(/ % fr) f)]
    (map (fn [[l h]]
           (cond (<= (- n-valid-fft-bins 0.5) l) []

                 (< (- h l) 1.0)
                 (let [m (int (+ 0.5 (* 0.5 (+ h l))))]
                   (if (<= n-valid-fft-bins m) [] [m]))

                 :else
                 (let [u (int (+ 0.5 h))]
                   (vec (range (int (+ 0.5 l))
                               (if (<= n-valid-fft-bins u) n-valid-fft-bins u)
                               )))))
         (partition 2 1 fnorm))))

(defn pickup [vv sm] ; vv, vector of vercors, sm : spectrum magnitude
  (map (fn [idxs]
         (if (empty? idxs)
           0.0
           (/ (apply + (map (fn [idx] (get sm idx))
                            idxs))
              (count idxs))))
       vv))

(defn fft [{br :bit-resolution w :waveform :as status}]
  ; The number of fft is now fixed to 4096 at the moment.
  (assoc status :mag-spectrum
         (let [factor (float (bit-shift-left 1 br))]
           (map (fn [ofs] (fft/fft-mag-norm w ofs factor))
                (range (*        400  4096 2)
                       (* (+ 600 400) 4096 2)
                       (*             4096 2)
                       )))))

(defn freq-to-pix
  [{fa :freq-of-a4 [pl ph] :pitch-range ih :image-height :as status}]
  (assoc status :pix-vs-fft-map
         (let [nfft    4096
               sr      44.1e3 ; sampling-rate
               ]
           (index (* fa (Math/pow 2.0 (/ (- pl 0.5) 12.0)))
                  (* fa (Math/pow 2.0 (/ (+ ph 0.5) 12.0)))
                  ih (/ sr nfft) (/ nfft 2)
                  ))))

(let [log10-inv (/ (Math/log 10.0))]
  (defn pickuper [{m :pix-vs-fft-map s :mag-spectrum :as status}]
    (assoc status :db
           (map (fn [v]
                  (map (fn [x]
                         (* 20.0 (Math/log (+ x 1e-10)) log10-inv))
                       (pickup m v)))
                s))))

(defn map2d [f vv]
  (map (fn [v] (map f v))
       vv))

(defn color-mapper [{[l h] :db-range db :db :as status}]
  (assoc status :vertical-pix
         (let [coeff (/ 254.0 (- h l))]
           (map2d (fn [x]
                    (let [i (int (+ 1.0 (* (- x l) coeff)))]
                      (cond (<   i 0)   0
                            (< 255 i) 255
                            :else       i)))
                  db))))

(defn horizon-handler [{[w h] :image-dimension p :vertical-pix :as status}]
  (let [img (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)]
    (.setRGB img 0 0 w h
     (int-array (map (fn [i] (bit-or 0xFF000000
                                     (bit-shift-left i 16)
                                     (bit-shift-left i  8)
                                     i))
                     (apply concat
                      (reverse (apply map vector p)))))
     0 w)
    (assoc status :image img)))

(defn horizon-handler2 [{[w h] :image-dimension p :vertical-pix :as status}]
  (let [img (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
        coeff (/ (float (count p)) w)]
    (doseq [j (range w)]
      (.setRGB img j 0 1 h
       (int-array (map (fn [i] (bit-or 0xFF000000
                                       (bit-shift-left i 16)
                                       (bit-shift-left i  8)
                                       i))
                       (reverse (nth p (* coeff (+ j 0.5))))))
       0 1))
    (assoc status :image img)))

(defn read-file []
  (let [stream (AudioSystem/getAudioInputStream
                (File. "../data/pascal_20161217.wav"))
        len (.available stream)
        buf (byte-array len)]
    (.read stream buf 0 len)
    buf))

(defn update-panel2
  [{{[x0 y0] :p0 [x1 y1] :p1} :select [l h] :pitch-range :as status}
   panel]
  (.removeAll panel)
  (let [status (assoc status :pitch-range
                [(/ (+ (* (- 599 y1) h) (* (+ 1 y1) l)) 600.0)
                 (/ (+ (* (- 600 y0) h) (*      y0  l)) 600.0)])
        _ (println (status :pitch-range))
        status (freq-to-pix status)
        status (pickuper status)
        status (color-mapper status)
        status (update-in status [:vertical-pix] #(drop x0 (take x1 %)))
        status (horizon-handler2 status)
        l (JLabel. (ImageIcon. (:image status)))]
    (.add panel l)))

(defn make-mouse-listener [rs panel label panel2]
  (proxy [MouseListener] []
    (mousePressed [e]
      (dosync
        (let [p (.getPoint e)
              x (int (.getX p))
              y (int (.getY p))
              {{[x0 y0] :p0 [x1 y1] :p1} :select} @rs]
          (ref-set rs
            (-> @rs
                (assoc :select-state :drawing)
                (assoc :select {:p0 [x y] :p1 [x y]})))
          (.repaint panel)
          (.repaint label))))
    (mouseReleased [e]
      (dosync
        (let [{{[x0 y0] :p0 [x1 y1] :p1} :select} @rs
              xmin (min x0 x1) xmax (max x0 x1)
              ymin (min y0 y1) ymax (max y0 y1)]
          (ref-set rs (assoc-in @rs [:select :p0] [xmin ymin]))
          (ref-set rs (assoc-in @rs [:select :p1] [xmax ymax])))
        (update-panel2 @rs panel2)
        (.validate panel2)
        (.repaint panel2)
        ))
    (mouseClicked [_])
    (mouseEntered [_])
    (mouseExited [_])))

(defn make-mouse-motion-listener [rs panel label]
  (proxy [MouseMotionListener] []
    (mouseDragged [e]
      (dosync
        (let [p (.getPoint e)
              x (.getX p)
              y (.getY p)]
          (ref-set rs (assoc-in @rs [:select :p1] [x y]))
          (.repaint panel)
          (.repaint label)
          )))
    (mouseMoved [_])))

(defn paint-body [g rs]
  (let [{[x0 y0] :p0 [x1 y1] :p1} (:select @rs)
        x (min x0 x1) y (min y0 y1)
        w (int (Math/abs (- x1 x0)))
        h (int (Math/abs (- y1 y0)))]
    (.setColor g Color/YELLOW)
    (.drawRect g x y w h)
    ))

(defn make-panel [panel2]
  (let [status {:waveform (read-file), :bit-resolution 15
                :select {:p0 [50 50] :p1 [100 100]}, :select-state :idle}
        status (fft status)
        status (freq-to-pix (conj status
                             {:freq-of-a4 442.0
                              :pitch-range [-36 24]
                              :image-height 600}))
        status (pickuper status)
        status (color-mapper (conj status {:db-range [-80.0 0.0]}))
        status (horizon-handler (conj status {:image-dimension [600 600]}))
        rs (ref status)
        p (proxy [JPanel] []
            (getPreferredSize []
              (Dimension. 600 600)))
        l (proxy [JLabel] [(ImageIcon. (:image status))]
            (paintComponent [g]
              (proxy-super paintComponent g)
              (paint-body g rs)))]
    (.addMouseListener l (make-mouse-listener rs p l panel2))
    (.addMouseMotionListener l (make-mouse-motion-listener rs p l))
    (.add p l)
    [status p]))

(defn make-panel2 []
  (proxy [JPanel] []
    (getPreferredSize []
      (Dimension. 600 600))))
