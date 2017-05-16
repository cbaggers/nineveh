(in-package :nineveh.graphing)

;; Based on Mikael Hvidtfeldt Christensen's excellent article here:
;; http://blog.hvidtfeldts.net/index.php/2011/07/plotting-high-frequency-functions-using-a-gpu/
;;
;; We don't use dithering on this version

(defun-g axis ((uv :vec2) (xy-range :vec4) (axis-style :vec4))
  (let* ((axis-thickness (w axis-style))
         (axis-color (v! (s~ axis-style :xyz) 1))
         (diff (/ (s~ xy-range :xz) (- (s~ xy-range :yw) (s~ xy-range :xz))))
         (uv (+ uv diff)))
    (+ (* axis-color (smoothstep axis-thickness 0 (abs (x uv))))
       (* axis-color (smoothstep axis-thickness 0 (abs (y uv)))))))

(defun-g graph ((func (function (:float) :float))
                (uv :vec2)
                (xy-range :vec4) ;; x-min x-max y-min y-max
                (line-style :vec4)
                (axis-style :vec4)
                (samples :int))
  (multiple-value-bind (g v)
      (graph-no-axis func uv xy-range line-style samples)
    (values (+ g (axis uv xy-range axis-style))
            v)))

;; {TODO} these can go away once varjo has &optional support

;;------------------------------------------------------------

(defun-g graph ((func (function (:float) :float))
                (uv :vec2)
                (xy-range :vec4) ;; x-min x-max y-min y-max
                (line-style :vec4)
                (axis-style :vec4))
  (graph func uv xy-range line-style axis-style 10))

;;------------------------------------------------------------

(defun-g graph ((func (function (:float) :float))
                (uv :vec2)
                (xy-range :vec4) ;; x-min x-max y-min y-max
                (line-style :float)
                (axis-style :float))
  (graph func uv xy-range (v! 1 1 1 line-style) (v! 0.1 0.1 0.1 axis-style)
         10))

;;------------------------------------------------------------

(defun-g graph ((func (function (:float) :float))
                (uv :vec2)
                (xy-range :vec4) ;; x-min x-max y-min y-max
                (line-style :vec4))
  (graph func uv xy-range line-style (v! 0.1 0.1 0.1 0.004) 10))

;;------------------------------------------------------------

(defun-g graph ((func (function (:float) :float))
                (uv :vec2)
                (xy-range :vec4) ;; x-min x-max y-min y-max
                (line-style :float))
  (graph func uv xy-range (v! 1 1 1 line-style) (v! 0.1 0.1 0.1 0.004) 10))

;;------------------------------------------------------------

(defun-g graph ((func (function (:float) :float))
                (uv :vec2)
                (xy-range :vec4)) ;; x-min x-max y-min y-max
  (graph func uv xy-range (v! 1 1 1 0.004) (v! 0.1 0.1 0.1 0.004) 10))

;;------------------------------------------------------------

(defun-g graph ((func (function (:float) :float))
                (uv :vec2))
  (graph func
         uv
         (v! 0 1 0 1) ;; range
         (v! 1 1 1 0.004) ;; line-style
         (v! 0.1 0.1 0.1 0.004) ;; axis-style
         10)) ;; samples

;;------------------------------------------------------------
