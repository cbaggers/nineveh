(in-package :nineveh.hashing)

(defun-g bbs-coord-prepare ((x :vec4))
  (- x (* (floor (* x (/ 1.0 61.0))) 61.0)))

(defun-g bbs-coord-prepare ((x :vec3))
  (- x (* (floor (* x (/ 1.0 61.0))) 61.0)))

(defun-g bbs-permute ((x :vec4)) (* (fract (* x (* x (/ 1.0 61.0)))) 61.0))

(defun-g bbs-permute-and-resolve ((x :vec4))
  (fract (* x (* x (/ 1.0 61.0)))))

(defun-g bbs-hash-2d ((grid-cell :vec2))
  (let* ((hash-coord
          (bbs-coord-prepare
           (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1.0)))))
         (p (bbs-permute (s~ hash-coord :xzxz))))
    (bbs-permute-and-resolve (+ p (s~ hash-coord :yyww)))))

(defun-g bbs-hash-hq-2d ((grid-cell :vec2))
  (let* ((hash-coord
          (bbs-coord-prepare
           (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1.0)))))
         (p (bbs-permute (s~ hash-coord :xzxz))))
    (setf p (bbs-permute (+ p (s~ hash-coord :yyww))))
    (bbs-permute-and-resolve (+ p (s~ hash-coord :xzxz)))))

(defun-g bbs-hash-3d ((grid-cell :vec3))
  (let (((lowz-hash :vec4))
        ((highz-hash :vec4))
        (domain 60.0))
    (setf (s~ grid-cell :xyz)
          (- (s~ grid-cell :xyz)
             (* (floor (* (s~ grid-cell :xyz) (/ 1.0 domain))) domain)))
    (let* ((grid-cell-inc1
            (* (step grid-cell (v3! (- domain 1.5))) (+ grid-cell (v3! 1.0))))
           (p
            (bbs-permute (s~ (v2! (x grid-cell) (x grid-cell-inc1)) :xyxy))))
      (setf p
            (bbs-permute
             (+ p (s~ (v2! (y grid-cell) (y grid-cell-inc1)) :xxyy))))
      (setf lowz-hash (bbs-permute-and-resolve (+ p (s~ grid-cell :zzzz))))
      (setf highz-hash
            (bbs-permute-and-resolve (+ p (s~ grid-cell-inc1 :zzzz)))))
    (values lowz-hash highz-hash)))

;;------------------------------------------------------------
