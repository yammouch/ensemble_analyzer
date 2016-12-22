(ns ensemble-analyzer.chromatic)

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
                   (range (int (+ 0.5 l))
                          (if (<= n-valid-fft-bins u) n-valid-fft-bins u)
                          ))))
         (partition 2 1 fnorm))))

(defn pickup [vv sm] ; vv, vector of vercors, sm : spectrum magnitude
  (map (fn [idxs]
         (if (empty? idxs)
           0.0
           (/ (apply + (map (fn [idx] (get sm idx))
                            idxs))
              (count idxs))))
       vv))
