(in-package #:nineveh.distortion)

;;------------------------------------------------------------

(defun-g radial-distort ((coord :vec2) (amount :vec2))
 (let* ((cc (* (- coord 0.5) 2.0)))
   (+ coord (* cc amount))))

(defun-g barrel-distortion ((p :vec2) (amount :vec2))
 (let* ((p (- (* 2.0 p) 1.0))
        (max-barrel-power (sqrt 5.0))
        (radius (dot p p))
        (p (* p (pow (v2! radius) (* max-barrel-power amount)))))
   (+ (* p 0.5) 0.5)))

(defun-g brown-conrady-distortion ((uv :vec2) (dist :float))
 (let* ((uv (- (* uv 2.0) 1.0))
        (barrel-distortion1 (* 0.1 dist))
        (barrel-distortion2 (* -0.025 dist))
        (r2 (dot uv uv))
        (uv (* uv (+ 1.0 (+ (* barrel-distortion1 r2)
                            (* barrel-distortion2 (* r2 r2)))))))
   (+ (* uv 0.5) 0.5)))

;;------------------------------------------------------------
