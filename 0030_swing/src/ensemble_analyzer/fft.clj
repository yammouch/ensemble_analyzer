(ns ensemble-analyzer.fft)

(defn c* [[pre pim] [sre sim]]
  (list (- (* pre sre) (* pim sim))
        (+ (* pim sre) (* pre sim))))

(defn fftc [seq]
  (if (empty? (next seq))
    seq
    (let [n (count seq)
          [ev od] (apply map vector (partition 2 seq)) ; deinterleave
          evfft (fftc ev)
          odfft (fftc od)
          ws (let [n-div-inv-pi (/ (* 2 Math/PI) n)]
               (map (fn [i]
                      (let [phase (* n-div-inv-pi i)]
                        (list (Math/cos phase) (Math/sin phase))))
                    (range (/ n 2))))
          odws (map c* odfft ws)]
      (concat (map (fn [x y] (map + x y)) evfft odws)
              (map (fn [x y] (map - x y)) evfft odws)))))

(defn fft [seq]
  (fftc (map (fn [x] [x 0.0]) seq)))

(defn fft-mag [seq]
  (map (fn [[re im]] (Math/sqrt (+ (* re re) (* im im))))
       (fft seq)))

(defn fft-mag-norm [seq swing-0db]
  (let [factor (/ 2 swing-0db (count seq))]
    (map #(* % factor)
         (fft-mag seq)))) 

