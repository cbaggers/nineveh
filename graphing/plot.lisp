(in-package :nineveh.graphing)

(defun-g plot ((val :float)
               (uv :vec2)
               (xy-range :vec4) ;; x-min x-max y-min y-max
               (line-style :vec4)
               (axis-style :vec4))
  (let* (;;
         (line-thickness (* 0.5 (w line-style)))
         (line-color (v! (s~ line-style :xyz) 1))
         ;;
         (axis-thickness (w axis-style))
         (axis-color (v! (s~ axis-style :xyz) 1))
         ;;
         (x-diff (- (y xy-range) (x xy-range)))
         (y-diff (- (w xy-range) (z xy-range)))
         ;;
         (uv (+ (* uv (v! x-diff y-diff))
                (s~ xy-range :xz)))
         ;;
         (pval (* (- (smoothstep (- val (* x-diff line-thickness)) val (y uv))
                     (smoothstep val (+ val (* y-diff line-thickness)) (y uv)))
                  line-color)))
    (+ pval
       (* axis-color (smoothstep (* axis-thickness x-diff) 0 (abs (x uv))))
       (* axis-color (smoothstep (* axis-thickness y-diff) 0 (abs (y uv)))))))

;;------------------------------------------------------------

(defun-g plot ((val :float)
               (uv :vec2)
               (xy-range :vec4) ;; x-min x-max y-min y-max
               (line-style :vec4))
  (plot val uv xy-range line-style (v! 0.1 0.1 0.1 0.004)))

;;------------------------------------------------------------

(defun-g plot ((val :float)
               (uv :vec2)
               (xy-range :vec4)) ;; x-min x-max y-min y-max
  (plot val uv xy-range (v! 1 1 1 0.013) (v! 0.1 0.1 0.1 0.004)))

;;------------------------------------------------------------

(defun-g plot ((val :float)
               (uv :vec2))
  (plot val
        uv
        (v! 0 1 0 1) ;; range
        (v! 1 1 1 0.004) ;; line-style
        (v! 0.1 0.1 0.1 0.004))) ;; axis-style
