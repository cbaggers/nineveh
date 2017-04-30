(in-package :nineveh.shaping-functions)

(defun-g almost-identity ((threshold :float) (min :float) (x :float))
  "Acts as identity above whilst x is above 'threshold' below this is smooths
   off to 'min'

   Valid when x>=0

   Credit:
   IQ
   http://www.iquilezles.org/www/articles/functions/functions.htm"
  (if (> x threshold)
      x
      (let* ((a (- (* 2f0 min) threshold))
             (b (- (* 2f0 threshold) (* 3f0 min)))
             (r (/ x threshold)))
        (+ (* r r (+ (* a r) b))
           min))))

(defun-g impulse ((squash :float) (x :float))
  "Grows fast to 1f0 and then slowly decays. Use 'squash' to control the
   squashing/stretching of the function.

   Note: It reaches 1f0 (its peak) at exactly x=1/squash. Thus higher
         squash values shorten the 'distance' to the curve peak

   Credit:
   IQ
   http://www.iquilezles.org/www/articles/functions/functions.htm"
  (let ((h (* squash x)))
    (* h (exp (- 1f0 h)))))

(defun-g cubic-pulse ((center :float) (half-width :float) (x :float))
  "A curve centered on 'center' (where y=1), where the length of the curve to
   y=0 is 'width'.

   Cheap replacement for a gaussian

   Credit:
   IQ
   http://www.iquilezles.org/www/articles/functions/functions.htm"
  (let ((x (abs (- x center))))
    (if (> x half-width)
        0f0
        (let ((x (/ x half-width)))
          (- 1f0 (* x x (- 3f0 (* 2f0 x))))))))

(defun-g exponential-step ((k :float) (exponent :float) (x :float))
  "A smoothstep with a control on sharpness

   Credit:
   IQ
   http://www.iquilezles.org/www/articles/functions/functions.htm"
  ;; {TODO} I'd like a better name for 'k'
  (exp (* (pow x exponent) (- k))))

(defun-g parabola ((k :float) (x :float))
  "Remaps the 0..1 interval into 0..1, such that the corners are remapped to 0
   and the center to 1.

   Credit:
   IQ
   http://www.iquilezles.org/www/articles/functions/functions.htm"
  ;; {TODO} I'd like a better name for 'k'
  (pow (* 4f0 x (- 1f0 x)) k))

(defun-g power-curve ((a :float) (b :float) (x :float))
  "Remaps the 0..1 interval into 0..1 such that the corners are remapped to 0
   and the point the curve reaches 1 is controllable via 'a' & 'b'

   Credit:
   IQ
   http://www.iquilezles.org/www/articles/functions/functions.htm"
  ;; {TODO} I'd like a better names for 'a' & 'b'
  (let ((k (/ (pow (+ a b) (+ a b))
              (* (pow a a) (pow b b)))))
    (* k (pow x a) (pow (- 1f0 x) b))))
