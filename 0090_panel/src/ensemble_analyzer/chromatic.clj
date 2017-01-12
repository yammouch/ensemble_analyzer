(ns ensemble-analyzer.chromatic)

(import '(java.awt.image BufferedImage))

(require 'ensemble-analyzer.fft)
(alias 'fft 'ensemble-analyzer.fft)

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

(defn fft [status]
  ; The number of fft is now fixed to 4096 at the moment.
  (assoc status :mag-spectrum
         (let [br (status :bit-resolution)
               factor (float (bit-shift-left 1 br))]
           (map (fn [v] (vec (fft/fft-mag-norm v factor)))
                (status :waveform)))))

(defn freq-to-pix [status]
  (assoc status :pix-vs-fft-map
         (let [fa      (status :freq-of-a4)
               [pl ph] (status :pitch-range)
               ih      (status :image-height)
               nfft    4096
               sr      44.1e3 ; sampling-rate
               ]
           (index (* fa (Math/pow 2.0 (/ (- pl 0.5) 12.0)))
                  (* fa (Math/pow 2.0 (/ (+ ph 0.5) 12.0)))
                  ih (/ sr nfft) (/ nfft 2)
                  ))))

(defn map2d [f vv]
  (map (fn [v] (map f v))
       vv))

(let [log10-inv (/ (Math/log 10.0))]
  (defn pickuper [status]
    (let [m (status :pix-vs-fft-map)]
      (assoc status :db
             (map2d (fn [x] (* 20.0 (Math/log (+ x 1e-10)) log10-inv))
                    (status :mag-spectrum))))))

(defn color-mapper [status]
  (assoc status :vertical-pix
         (let [[l h] (status :db-range)
               coeff (/ 254.0 (- h l))]
           (map2d (fn [x]
                    (let [i (int (+ 1.0 (* (- x l) coeff)))]
                      (cond (<   i 0)   0
                            (< 255 i) 255
                            :else       i)))
                  (status :db)))))

(defn horizon-handler [status]
  (let [[w h] (status :image-dimension)
        img (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)]
    (.setRGB img 0 0 w h
     (int-array (map (fn [i] (bit-or 0xFF000000
                                     (bit-shift-left i 16)
                                     (bit-shift-left i  8)
                                     i))
                (apply concat
                 (reverse (apply map vector (status :vertical-pix)))))
     0 w))
    (assoc status :image img)))
