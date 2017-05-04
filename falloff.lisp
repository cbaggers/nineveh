(in-package :nineveh.hashing)

(defun-g falloff-xsq-c1 ((xsq :float))
  (setf xsq (- 1.0 xsq))
  (* xsq xsq))

(defun-g falloff-xsq-c2 ((xsq :float))
  (setf xsq (- 1.0 xsq))
  (* xsq (* xsq xsq)))

(defun-g falloff-xsq-c2 ((xsq :vec4))
  (setf xsq (- (v4! 1.0) xsq))
  (* xsq (* xsq xsq)))
