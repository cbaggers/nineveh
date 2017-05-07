(in-package :nineveh.shaping-functions)

;;
;;
;; Some polynomial functions for shaping, tweening, and easing signals in the
;; range [0..1]
;;
;;

(defun-g cos-raised-inverted-blinn-wybill ((x :float))
  "Approximation of 'raised inverted cosine'. Diverges from real function
   by less that 0.1% within the range [0..1].

   It also shares some of the Raised Inverted Cosine's key properties, having
   flat derivatives at 0 and 1, and the value 0.5 at x=0.5.

   Credit:
   Golan Levin and Collaborators: http://www.flong.com/texts/code/shapers_poly/
   "
  (let* ((x² (* x x))
         (x⁴ (* x² x²))
         (x⁶ (* x⁴ x²))
         ;;
         (fa (/ 4f0 9f0))
         (fb (/ 17f0 9f0))
         (fc (/ 22f0 9f0))
         (y (+ (- (* fa x⁶) (* fb x⁴))
               (* fc x²))))
    y))

(defun-g seat-double-cubic ((inflection-point :vec2) (x :float))
  "This seat-shaped function is formed by joining two 3rd-order polynomial
   (cubic) curves. The curves meet with a horizontal inflection point at the
   control coordinate specified by 'inflection-point' in the unit square.

   Credit:
   Golan Levin and Collaborators: http://www.flong.com/texts/code/shapers_poly/
   "
  (let* ((epsilon (glsl-expr "0.00001" :float))
         (min-a epsilon)
         (max-a (- 1f0 epsilon))
         (min-b 0f0)
         (max-b 1f0)
         (a (clamp (x inflection-point) min-a max-a))
         (b (clamp (y inflection-point) min-b max-b)))
    (if (<= x a)
        (- b (* b (pow (- 1 (/ x a)) 3f0)))
        (+ b (* (- 1 b) (pow (/ (- x a) (- 1 a)) 3f0))))))

(defun-g seat-double-cubic-with-linear-bend ((inflection-point :float)
                                             (amount-of-blend :float)
                                             (x :float))
  "This is a modified version of #'seat-double-cubic.

   It uses 'inflection-point' to control the location of its inflection point
   along the diagonal of the unit square.

   'amount-of-blend' is used to control the how much we blend this curve with
   the Identity Function (y=x). This has the effect of tilting the slope of
   the curve's plateau in the vicinity of its inflection point.

   The adjustable flattening around the inflection point makes this a useful
   shaping function for lensing or magnifying evenly-spaced data.

   Credit:
   Golan Levin and Collaborators: http://www.flong.com/texts/code/shapers_poly/
   "
  (let* ((epsilon (glsl-expr "0.00001" :float))
         (min-a epsilon)
         (max-a (- 1f0 epsilon))
         (min-b 0f0)
         (max-b 1f0)
         (a (clamp inflection-point min-a max-a))
         (b (clamp amount-of-blend min-b max-b))
         (b (- 1f0 b)))
    (if (<= x a)
        (+ (* b x) (* (- 1 b) a (- 1 (pow (- 1 (/ x a)) 3f0))))
        (+ (* b x)
           (* (- 1 b)
              (+ a (* (- 1 a) (pow (/ (- x a) (- 1 a)) 3f0))))))))

(defun-g seat-double-odd-exponent ((inflection-point :vec2)
                                   (exponent :int)
                                   (x :float))
  "This is the seat-double-cubic generalized to work with any odd exponent.
   The viable value for 'exponent' are the odd integers from 1 to 19.

   Credit:
   Golan Levin and Collaborators: http://www.flong.com/texts/code/shapers_poly/
   "
  (let* ((epsilon (glsl-expr "0.00001" :float))
         (min-a epsilon)
         (max-a (- 1f0 epsilon))
         (min-b 0f0)
         (max-b 1f0)
         (a (clamp (x inflection-point) min-a max-a))
         (b (clamp (y inflection-point) min-b max-b))
         (p (+ (* 2 exponent) 1)))
    (if (<= x a)
        (- b (* b (pow (- 1 (/ x a)) p)))
        (+ b (* (- 1 b) (pow (/ (- x a) (- 1 a)) p))))))

;; {TODO} Investifate switching away from conditions
