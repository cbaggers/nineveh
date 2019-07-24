(in-package :nineveh.normals)

(defun-g simple-sample-normals ((func (function (:vec2) :float))
                                (pos :vec2)
                                (offset :float)
                                (scale :float))
  (let* ((s0 (* (funcall func (+ pos (* offset (v!  0f0 -1f0))))
                scale))
         (s1 (* (funcall func (+ pos (* offset (v! -1f0  0f0))))
                scale))
         (s2 (* (funcall func (+ pos (* offset (v!  1f0  0f0))))
                scale))
         (s3 (* (funcall func (+ pos (* offset (v!  0f0  1f0))))
                scale)))
    (normalize
     (v! (- s1 s2)
         2f0
         (- s0 s3)))))

(defun-g simple-sample-normals ((func (function (:vec2) :float))
                                (pos :vec2)
                                (offset :float))
  (let ((scale 1f0)
        (s0 (funcall func (+ pos (v! (- offset)  (- offset)))))
        (s1 (funcall func (+ pos (v!         .0  (- offset)))))
        (s2 (funcall func (+ pos (v!     offset  (- offset)))))
        (s3 (funcall func (+ pos (v! (- offset) .0))))
        (s5 (funcall func (+ pos (v!     offset .0))))
        (s6 (funcall func (+ pos (v! (- offset)  offset))))
        (s7 (funcall func (+ pos (v!         .0  offset))))
        (s8 (funcall func (+ pos (v!     offset  offset)))))
    (normalize
     (v! (* scale (- (- s2 (- (+ s0 (+ (* 2 (- s5 s3)) s8)) s6))))
         1f0
         (* scale (- (- s6 (- (+ s0 (+ (* 2 (- s7 s1)) s8)) s2))))))))

(defun-g simple-sample-normals ((func (function (:vec2) :float))
                                (pos :vec2)
                                (offset :vec2))
  (let ((scale 1f0)
        (s0 (funcall func (+ pos (v! (- (x offset))  (- (y offset))))))
        (s1 (funcall func (+ pos (v!             .0  (- (y offset))))))
        (s2 (funcall func (+ pos (v!     (x offset)  (- (y offset))))))
        (s3 (funcall func (+ pos (v! (- (x offset)) .0))))
        (s5 (funcall func (+ pos (v!     (x offset) .0))))
        (s6 (funcall func (+ pos (v! (- (x offset))  (y offset)))))
        (s7 (funcall func (+ pos (v!             .0  (y offset)))))
        (s8 (funcall func (+ pos (v!     (x offset)  (y offset))))))
    (normalize
     (v! (* scale (- (- s2 (- (+ s0 (+ (* 2 (- s5 s3)) s8)) s6))))
         1f0
         (* scale (- (- s6 (- (+ s0 (+ (* 2 (- s7 s1)) s8)) s2))))))))


(defun-g simple-sample-normals ((func (function (:vec3) :float))
                                (pos :vec3)
                                (offset :float))
  (normalize
   (v! (- (funcall func (+ pos (v!     offset  0f0 0f0)))
          (funcall func (+ pos (v! (- offset)  0f0 0f0))))

       (- (funcall func (+ pos (v! 0f0      offset 0f0)))
          (funcall func (+ pos (v! 0f0  (- offset) 0f0))))

       (- (funcall func (+ pos (v! 0f0  0f0    offset)))
          (funcall func (+ pos (v! 0f0  0f0 (- offset))))))))
