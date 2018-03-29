(in-package :nineveh.internals)

;;------------------------------------------------------------

(defun make-gpu-quad ()
  (make-buffer-stream
   (make-gpu-array
    (list (list (v! -0.5   0.5 0 0) (v!  0.0   1.0))
          (list (v! -0.5  -0.5 0 0) (v!  0.0   0.0))
          (list (v!  0.5  -0.5 0 0) (v!  1.0   0.0))
          (list (v! -0.5   0.5 0 0) (v!  0.0   1.0))
          (list (v!  0.5  -0.5 0 0) (v!  1.0   0.0))
          (list (v!  0.5   0.5 0 0) (v!  1.0   1.0)))
    :element-type 'g-pt
    :dimensions 6)
   :retain-arrays t))

(defvar *quad-cache*
  (make-hash-table :test #'eq))

(defun get-quad-for-context (context)
  (or (gethash context *quad-cache*)
      (setf (gethash context *quad-cache*)
            (make-gpu-quad))))

(defun get-gpu-quad ()
  (get-quad-for-context (cepl-context)))

;;------------------------------------------------------------
