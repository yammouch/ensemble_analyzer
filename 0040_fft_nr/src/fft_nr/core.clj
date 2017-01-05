(ns fft-nr.core
  (:gen-class))

(defn bit-reverse [i m]
  (loop [q i, acc []]
    (if (= q 0)
      (reduce #(+ (* %1 2) %2)
              (take m (concat acc (repeat 0))))
      (recur (quot q 2)
             (conj acc (rem q 2))
             ))))

(defn fftc1 [v w]
  (let [n-half (/ (count v) 2)
        v1 (take n-half v)
        v2 (drop n-half v)
        v2w (map (fn [[xr xi] [yr yi]]
                   [(- (* xr yr) (* xi yi))
                    (+ (* xr yi) (* xi yr))])
                 v2 w)]
    (apply concat
     (map (fn [c1 c2] [(map + c1 c2) (map - c1 c2)])
          v1 v2w))))

(defn fftc [v]
  (let [exp2 (loop [i 0, len (unsigned-bit-shift-right (count v) 1)]
               (if (= len 0)
                 i
                 (recur (inc i) (unsigned-bit-shift-right len 1))))
        n (bit-shift-left 1 exp2)
        w (map (fn [i]
                 (let [phase (/ (* 2 Math/PI (bit-reverse i (dec exp2))) n)]
                   [(Math/cos phase) (Math/sin phase)]))
               (range (/ n 2)))]
    ;(prn w)
    (loop [i 0, v v]
      ;(prn v)
      (if (<= exp2 i)
        (loop [i 0, acc []]
          (if (<= n i)
            acc
            (recur (inc i)
                   (conj acc (nth v (bit-reverse i exp2)))
                   )))
        (recur (inc i)
               (fftc1 v (cycle (take (bit-shift-left 1 i) w)))
               )))))

(defn -main [& args]
  ;(prn (fftc (map (fn [x] [(double x) 0.0])
  ;                [1 0 -1 0])))
  ;(prn (fftc (map (fn [x] [(double x) 0.0])
  ;                [1 1 1 1])))
  ;(prn (fftc (map (fn [x] [(Math/cos (/ (* 2 Math/PI x) 4096)) 0.0])
  ;                (range 4096))))
  (time (doall
   (fftc (map (fn [x] [(Math/cos (/ (* 2 Math/PI x) 4096)) 0.0])
              (range 4096)))))
  ;(prn (fftc (map (fn [x] [(Math/sin (/ (* 2 Math/PI x) 16)) 0.0])
  ;                (range 16))))
  ;(prn (fftc (map (fn [x] [(Math/cos (/ (* 2 Math/PI x)  8)) 0.0])
  ;                (range 8))))
                  )
