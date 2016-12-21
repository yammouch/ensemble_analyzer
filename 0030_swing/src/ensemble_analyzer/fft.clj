(ns ensemble-analyzer.fft)

(defn c+ [& seq]
  (if (empty? seq)
    (list 0.0 0.0)
    (letfn [(c+2 [[pre pim] [sre sim]]
              (list (+ pre sre) (+ pim sim)))]
      (reduce c+2 seq))))

(defn c- [& seq]
  (cond (empty? seq) (list 0.0 0.0)
        (empty? (rest seq)) (list (- (nth (first seq) 0))
                                  (- (nth (first seq) 1)))
        :else
        (letfn [(c-2 [[pre pim] [sre sim]]
                  (list (- pre sre) (- pim sim)))]
          (reduce c-2 seq))))

(defn c* [& seq]
  (if (empty? seq)
    (list 1.0 0.0)
    (letfn [(c*2 [[pre pim] [sre sim]]
              (list (- (* pre sre) (* pim sim))
                    (+ (* pim sre) (* pre sim))))]
      (reduce c*2 seq))))

(defn deinterleave [n seq]
  (if (empty? (nthrest seq n))
    (take n (lazy-cat (map list seq)
                      (repeat '())))
    (map cons (take n seq)
              (deinterleave n (nthrest seq n))
              )))

(defn fft [seq]
  (cond (empty? seq) '()
        (empty? (rest seq)) (list (list (first seq) 0.0)) ; '((re im))
        :else
        (let [n (count seq)
              [ev od] (deinterleave 2 seq)
              evfft (fft ev)
              odfft (fft od)
              ws (map (fn [i] (list (Math/cos (/ (* Math/PI i) (/ n 2)))
                                    (Math/sin (/ (* Math/PI i) (/ n 2)))))
                      (range (/ n 2)))
              odws (map c* odfft ws)]
        (concat (map c+ evfft odws)
    (map c- evfft odws)))))
