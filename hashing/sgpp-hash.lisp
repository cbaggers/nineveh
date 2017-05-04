(in-package :nineveh.hashing)

(defun-g sgpp-coord-prepare ((x :vec4))
  (mod-fixed-denominator x 289f0))

(defun-g sgpp-coord-prepare ((x :vec3))
  (mod-fixed-denominator x 289f0))

(defun-g sgpp-permute ((x :vec4))
  (* (fract (* x (+ (* (/ 34.0 289.0) x) (v4! (/ 1.0 289.0)))))
     289.0))

(defun-g sgpp-resolve ((x :vec4))
  (fract (* x (/ 7.0 288.0))))

(defun-g sgpp-hash-2d ((grid-cell :vec2))
  (let* ((hash-coord
          (sgpp-coord-prepare
           (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1.0))))))
    (sgpp-resolve
     (sgpp-permute
      (+ (sgpp-permute (s~ hash-coord :xzxz)) (s~ hash-coord :yyww))))))

(defun-g sgpp-hash-2d-v2 ((grid-cell :vec2))
  (let (((hash-0 :vec4)) ((hash-1 :vec4)))
    (let* ((hash-coord
            (sgpp-coord-prepare
             (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1.0))))))
      (setf hash-0
            (sgpp-permute
             (+ (sgpp-permute (s~ hash-coord :xzxz)) (s~ hash-coord :yyww))))
      (setf hash-1 (sgpp-resolve (sgpp-permute hash-0)))
      (setf hash-0 (sgpp-resolve hash-0)))
    (values hash-0 hash-1)))

(defun-g sgpp-hash-2d ((grid-cell :vec2))
  (let (((hash-0 :vec4)) ((hash-1 :vec4)))
    (let* ((hash-coord
            (sgpp-coord-prepare
             (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1.0))))))
      (setf hash-0
            (sgpp-permute
             (+ (sgpp-permute (s~ hash-coord :xzxz)) (s~ hash-coord :yyww))))
      (setf hash-1 (sgpp-resolve (sgpp-permute hash-0)))
      (setf hash-0 (sgpp-resolve hash-0)))
    (values hash-0 hash-1)))

(defun-g sgpp-hash-3d ((grid-cell :vec3))
  (let (((lowz-hash :vec4)) ((highz-hash :vec4)))
    (progn
      (setf grid-cell (sgpp-coord-prepare grid-cell))
      (let* ((grid-cell-inc1 (* (step grid-cell (v3! 287.5))
                                (+ grid-cell (v3! 1.0)))))
        (setf highz-hash
              (sgpp-permute
               (+
                (sgpp-permute
                 (s~ (v2! (x grid-cell) (x grid-cell-inc1)) :xyxy))
                (s~ (v2! (y grid-cell) (y grid-cell-inc1)) :xxyy))))
        (setf lowz-hash
              (sgpp-resolve
               (sgpp-permute (+ highz-hash (s~ grid-cell :zzzz)))))
        (setf highz-hash
              (sgpp-resolve
               (sgpp-permute (+ highz-hash (s~ grid-cell-inc1 :zzzz)))))))
    (values lowz-hash highz-hash)))

(defun-g sgpp-hash-3d ((grid-cell :vec3) (v1-mask :vec3) (v2-mask :vec3))
  (let (((hash-0 :vec4)) ((hash-1 :vec4)) ((hash-2 :vec4)))
    (let* ((coords0
            (- (s~ grid-cell :xyz)
               (* (floor (* (s~ grid-cell :xyz) (/ 1.0 289.0))) 289.0)))
           (coords3 (* (step coords0 (v3! 287.5)) (+ coords0 (v3! 1.0))))
           (coords1 (mix coords0 coords3 v1-mask))
           (coords2 (mix coords0 coords3 v2-mask)))
      (setf hash-2
            (sgpp-permute
             (+
              (sgpp-permute
               (+
                (sgpp-permute
                 (v4! (x coords0) (x coords1) (x coords2) (x coords3)))
                (v4! (y coords0) (y coords1) (y coords2) (y coords3))))
              (v4! (z coords0) (z coords1) (z coords2) (z coords3)))))
      (setf hash-0 (sgpp-resolve hash-2))
      (setf hash-1 (sgpp-resolve (setf hash-2 (sgpp-permute hash-2))))
      (setf hash-2 (sgpp-resolve (sgpp-permute hash-2))))
    (values hash-0 hash-1 hash-2)))

(defun-g sgpp-hash-3d ((grid-cell :vec3))
  (let (((lowz-hash-0 :vec4))
        ((lowz-hash-1 :vec4))
        ((lowz-hash-2 :vec4))
        ((highz-hash-0 :vec4))
        ((highz-hash-1 :vec4))
        ((highz-hash-2 :vec4)))
    (progn
      (setf grid-cell (sgpp-coord-prepare grid-cell))
      (let* ((grid-cell-inc1 (* (step grid-cell (v3! 287.5))
                                (+ grid-cell (v3! 1.0)))))
        (setf highz-hash-2
              (sgpp-permute
               (+
                (sgpp-permute
                 (s~ (v2! (x grid-cell) (x grid-cell-inc1)) :xyxy))
                (s~ (v2! (y grid-cell) (y grid-cell-inc1)) :xxyy))))
        (setf lowz-hash-2 (sgpp-permute (+ highz-hash-2 (s~ grid-cell :zzzz))))
        (setf lowz-hash-0 (sgpp-resolve lowz-hash-2))
        (setf highz-hash-0
              (sgpp-resolve
               (setf highz-hash-2
                     (sgpp-permute
                      (+ highz-hash-2 (s~ grid-cell-inc1 :zzzz))))))
        (setf lowz-hash-1
              (sgpp-resolve (setf lowz-hash-2 (sgpp-permute lowz-hash-2))))
        (setf highz-hash-1
              (sgpp-resolve (setf highz-hash-2 (sgpp-permute highz-hash-2))))
        (setf lowz-hash-2 (sgpp-resolve (sgpp-permute lowz-hash-2)))
        (setf highz-hash-2 (sgpp-resolve (sgpp-permute highz-hash-2)))
        (values lowz-hash-0 lowz-hash-1 lowz-hash-2 highz-hash-0 highz-hash-1
                highz-hash-2)))))

;;------------------------------------------------------------
