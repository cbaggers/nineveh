(in-package :nineveh.vignette)

;;------------------------------------------------------------

(defun-g vignette ((uv :vec2)
                   (intensity :float)
                   (extent :float))
  (let* ((uv (* uv (- 1.0 (s~ uv :yx))))
         (vig0 (* (x uv) (y uv) intensity))
         (vig1 (expt vig0 extent)))
    vig1))

(defun-g vignette ((uv :vec2))
  (vignette uv 15.0 0.25))

;;------------------------------------------------------------

(defun-g natural-vignette ((uv :vec2)
                           (aspect-ratio :float)
                           (falloff :float))
  (let* ((coord (* (- uv 0.5)
                   aspect-ratio
                   2.0))
         (rf (* (length coord) falloff))
         (rf2-1 (+ (* rf rf) 1.0)))
    (/ 1f0 (* rf2-1 rf2-1))))

(defun-g natural-vignette ((uv :vec2)
                           (aspect-ratio :float))
  (natural-vignette uv aspect-ratio 0.5))

;;------------------------------------------------------------
