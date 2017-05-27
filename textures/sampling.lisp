(in-package :nineveh.textures)

(defun-g sample-equirectangular-tex ((tex :sampler-2d) (vec :vec3))
  (let* ((vec (normalize vec))
         (uv (v! (/ (atan (z vec) (x vec)) (* 2s0 +pi+))
                 (- (/ (+ (asin (y vec)) (/ +pi+ 2s0))
                       +pi+)))))
    (texture tex uv)))

(defun-g uv->cube-map-directions ((uv :vec2))
  (let ((scaled-uv (v! (* (- (x uv) 0.5) 2)
                       (* (- (y uv) 0.5) 2 -1s0))))
    (values
     (normalize (v! 1s0 (y scaled-uv) (- (x scaled-uv))))
     (normalize (v! -1s0 (y scaled-uv) (x scaled-uv)))
     (normalize (v! (x scaled-uv) 1s0 (- (y scaled-uv))))
     (normalize (v! (x scaled-uv) -1s0 (y scaled-uv)))
     (normalize (v! (x scaled-uv) (y scaled-uv) 1s0))
     (normalize (v! (- (x scaled-uv)) (y scaled-uv) -1s0)))))
