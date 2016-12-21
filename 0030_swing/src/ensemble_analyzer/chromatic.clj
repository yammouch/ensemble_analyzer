(ns ensemble-analyzer.chromatic)

; hi : high note (A4 (440 or 442 Hz) = 0)
; lo : low note
; np : number of pixels
; sm : spectrum magnitude
; sr : sampling rate
; fa : frequency of A4 (440 or 442 Hz, typically)
;(defn lin-freq-to-chromatic [hi lo np sm sr fa]
;  (let [nf (count sm) ; number of FFT points
;        fr (/ sr nf) ; freq resolution
;        fl (* fa (Math/pow 2.0 (/ lo 12.0))) ; low freq in chromatic
;        fp (Math/pow 2.0 (/ (- hi lo) (* 12.0 npix))) ; freq ratio in a pixel
;        sw (Math/floor (/ (Math/log (/ fd (* (- fp 1.0) fl)))
;                          (Math/log fp)))
;        fp-rt (Math/sqrt fp)
;        end (min np sw)
;        [acc sm fc fs]
;        (loop [acc (repeat np 0.0), sm sm, i 0, fc (* fl fp-rt), fs fr]
;          (cond (or (<= sw end) (empty? sm)) [acc sm fc fs]
;                (<= fc fs) (recur (assoc acc i (first sm)) sm (inc i)
;                                  (* fc fp) fs)
;                :else (recur acc (next sm) i fc (+ fs fr))))]
;    (loop [acc acc, sm sm, i sw,
;           fc (/ fc fp-rt), fs (- fs (* 0.5 fs)), sum 0.0, nsum 0]
;      (cond (or (<= sw np) (empty? sm)) acc
;            (<= fc fs) (recur (assoc acc i (/ sum nsum)) sm (inc i)
;                              (* fc fp) fs 0.0 0)
;            :else (recur acc (next sm) i fc (+ fs fr)
;                         (+ sum (first sm)) (inc nsum)
;                         )))))

; lo : low freq of result
; hi : high freq of result
; np : number of pixels
; sm : spectrum magnitude
; fr : freq resolution
(defn lin-to-log [lo hi np sm fr]
  (let [fp (Math/pow (/ hi lo) (/ 1.0 np))
        sw (Math/floor (/ (Math/log (/ fr (* (- fp 1.0) lo)))
                          (Math/log fp)))
        fp-rt (Math/sqrt fp)
        end (min np sw)
        [acc sm fc fs]
        (loop [acc (repeat np 0.0), sm sm, i 0, fc (* lo fp-rt), fs fr]
          (cond (or (<= sw end) (empty? sm)) [acc sm fc fs]
                (<= fc fs) (recur (assoc acc i (first sm)) sm (inc i)
                                  (* fc fp) fs)
                :else (recur acc (next sm) i fc (+ fs fr))))]
    (loop [acc acc, sm sm, i sw,
           fc (/ fc fp-rt), fs (- fs (* 0.5 fs)), sum 0.0, nsum 0]
      (cond (or (<= sw np) (empty? sm)) acc
            (<= fc fs) (recur (assoc acc i (/ sum nsum)) sm (inc i)
                              (* fc fp) fs 0.0 0)
            :else (recur acc (next sm) i fc (+ fs fr)
                         (+ sum (first sm)) (inc nsum)
                         )))))
