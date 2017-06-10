(in-package :nineveh.random)

(defun-g hammersley-nth-2d ((length-of-series :uint) (nth :uint))
  "http://holger.dammertz.org/stuff/notes_HammersleyOnHemisphere.html"
  (v! (/ (float nth) (float length-of-series))
      (radical-inverse-vdc nth)))

(defun-g hammersley-nth-hemisphere ((length-of-series :uint) (nth :uint))
  "http://holger.dammertz.org/stuff/notes_HammersleyOnHemisphere.html"
  (let* ((ham-sample (hammersley-nth-2d nth length-of-series))
         (u (x ham-sample))
         (v (y ham-sample))
         (φ (* v 2s0 +pi+))
         (cosθ (- 1s0 u))
         (sinθ (sqrt (- 1 (* cosθ cosθ)))))
    (v! (* (cos φ) sinθ)
        (* (sin φ) sinθ)
        cosθ)))
