(in-package :nineveh.graphing)

(defun-g plot ((uv :vec2) (val :float))
  (- (smoothstep (- val 0.02) val (y uv))
     (smoothstep val (+ val 0.02) (y uv))))

(defun-g plot ((uv :vec2) (val :float) (thickness :float))
  (let ((thickness (* thickness 0.5)))
    (- (smoothstep (- val thickness) val (y uv))
       (smoothstep val (+ val thickness) (y uv)))))
