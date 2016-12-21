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
    (prn f)
    (prn f fnorm)
    (map (fn [[l h]]
           (cond (<= (- n-valid-fft-bins 0.5) l) nil

                 (< (- h l) 1.0)
                 (let [m (int (+ 0.5 (* 0.5 (+ h l))))]
                   (if (<= n-valid-fft-bins m) nil m))

                 :else
                 (let [u (int (+ 0.5 h))]
                   [(int (+ 0.5 l))
                    (if (<= n-valid-fft-bins u) n-valid-fft-bins u)])))
         (partition 2 1 fnorm))))
