(in-package :nineveh.easing)

;; Note: it is know that these are really inefficient implementations for the
;;       GPU. This was a 5min coversions from the originals (which was super
;;       cool!) and will be fixed up in due course

(defmacro defeasing-f (name args &body body)
  (let ((ease-in (alexandria:symbolicate 'in- name))
        (ease-out (alexandria:symbolicate 'out- name))
        (ease-in-out (alexandria:symbolicate 'in-out- name)))
    `(progn
       ;; in
       (cepl.pipelines::defun-g-equiv ,ease-in ,args
         ,@body)
       ;; out
       (cepl.pipelines::defun-g-equiv ,ease-out ,args
         (let ((x (- 1s0 x)))
           (- 1s0 (progn ,@body))))
       ;; in-out
       (cepl.pipelines::defun-g-equiv ,ease-in-out ,args
         (if (<= x 0.5s0)
             (let ((x (* 2s0 x)))
               (/ (progn ,@body) 2s0))
             (let ((x (- 1s0 (* 2s0 (- x 0.5s0)))))
               (+ 0.5s0 (/ (- 1s0 (progn ,@body))
                           2s0))))))))

(defun-g linear ((x :float))
  ;; but why?
  x)

(defeasing-f sine ((x :float))
  (- 1s0 (cos (* x (/ +pi+ 2s0)))))

(defeasing-f quad ((x :float))
  (* x x))

(defeasing-f cubic ((x :float))
  (* x x x))

(defeasing-f quart ((x :float))
  (expt x 4s0))

(defeasing-f quint ((x :float))
  (expt x 5s0))

(defeasing-f exp ((x :float))
  (expt 2s0 (* 10s0 (- x 1s0))))

(defeasing-f circ ((x :float))
  (- (- (sqrt (- 1s0 (* x x)))
        1s0)))

(defeasing-f elastic ((x :float) (p :float) (s :float))
  (- (* (expt 2 (* 10 (- x 1s0)))
        (sin (/ (* (- (- x 1s0) s) (* 2 +pi+)) p)))))

(defeasing-f elastic ((x :float) (p :float))
  (let ((s (* #.(asin 1s0) (* p #.(/ 1s0 (* 2s0 +pi+))))))
    (- (* (expt 2 (* 10 (- x 1s0)))
          (sin (/ (* (- (- x 1s0) s) (* 2 +pi+)) p))))))

(defeasing-f elastic ((x :float))
  (let ((s (* #.(asin 1s0) (* 0.3f0 #.(/ 1s0 (* 2s0 +pi+))))))
    (- (* (expt 2 (* 10 (- x 1s0)))
          (sin (/ (* (- (- x 1s0) s) (* 2 +pi+)) 0.3f0))))))

(defeasing-f back ((x :float) (s :float))
  (* x x (- (* (+ 1s0 s) x) s)))

(defeasing-f back ((x :float))
  (* x x (- (* (+ 1s0 1.70158s0) x) 1.70158s0)))

(defeasing-f bounce ((x :float) (c1 :float))
  (let ((x (- 1s0 x)))
    (- 1s0 (cond ((< x (/ 1s0 2.75)) (* c1 x x))
                 ((< x (/ 2s0 2.75s0)) (let ((x (- x (/ 1.5s0 2.75s0))))
                                         (+ 0.75s0 (* c1 x x))))
                 ((< x (/ 2.5s0 2.75s0)) (let ((x (- x (/ 2.25 2.75))))
                                           (+ 0.9375s0 (* c1 x x))))
                 (t (let ((x (- x (/ 2.625s0 2.75s0))))
                      (+ 0.984375s0 (* c1 x x))))))))

(defeasing-f bounce ((x :float))
  (let ((x (- 1s0 x)))
    (- 1s0 (cond ((< x (/ 1s0 2.75)) (* 7.5625 x x))
                 ((< x (/ 2s0 2.75s0)) (let ((x (- x (/ 1.5s0 2.75s0))))
                                         (+ 0.75s0 (* 7.5625 x x))))
                 ((< x (/ 2.5s0 2.75s0)) (let ((x (- x (/ 2.25 2.75))))
                                           (+ 0.9375s0 (* 7.5625 x x))))
                 (t (let ((x (- x (/ 2.625s0 2.75s0))))
                      (+ 0.984375s0 (* 7.5625 x x))))))))
