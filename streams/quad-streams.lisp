(in-package :nineveh.streams)

(defvar *quad-stream-v2-data*
  (list (v! -1.0   1.0)
        (v! -1.0  -1.0)
        (v!  1.0  -1.0)
        (v! -1.0   1.0)
        (v!  1.0  -1.0)
        (v!  1.0   1.0)))

(let ((stream nil))
  (defun get-quad-stream-v2 ()
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (unless stream
      (setf stream (make-buffer-stream
                    (make-gpu-array *quad-stream-v2-data*
                                    :element-type :vec2
                                    :dimensions 6)
                    :retain-arrays t)))
    stream))
