(in-package :nineveh.graphing)

;; Based on Mikael Hvidtfeldt Christensen's excellent article here:
;; http://blog.hvidtfeldts.net/index.php/2011/07/plotting-high-frequency-functions-using-a-gpu/
;;
;; We don't use dithering on this version


(defun-g graph ((func (function (:float) :float))
                (uv :vec2)
                (xy-range :vec4) ;; x-min x-max y-min y-max
                (line-style :vec4)
                (axis-style :vec4)
                (samples :int))
  (let* (;;
         (line-thickness (w line-style))
         (line-color (v! (s~ line-style :xyz) 1))
         ;;
         (axis-thickness (w axis-style))
         (axis-color (v! (s~ axis-style :xyz) 1))
         ;;
         (diff (v! (- (y xy-range) (x xy-range))
                   (- (w xy-range) (z xy-range))))
         ;;
         (uv (+ (* uv diff) (s~ xy-range :xz)))
         ;;
         (samples (float samples))
         (max-dist (* (v2! line-thickness) diff))
         (step (/ max-dist (v2! samples)))
         (count 0f0)
         (initial-offset (* step samples -0.5))
         (my-samples 0f0))
    (incf initial-offset uv)
    (for (i 0f0) (< i samples) (++ i)
         (let ((fx (funcall func (+ (x uv) (* i (x step))))))
           (for (j 0f0) (< j samples) (++ j)
                (when (> (+ (* i i) (* j j)) (* samples samples))
                  (continue))
                (incf my-samples 1f0)
                (let ((diff (- fx (+ (y uv) (* j (y step))))))
                  (incf count (- (* (step 0f0 diff) 2f0) 1))))))
    (+ (* (if (/= (abs count) my-samples)
              (- 1f0 (/ (abs (float count)) (float my-samples)))
              0f0)
          line-color)
       (* axis-color (smoothstep (* 0.5 axis-thickness (x diff)) 0
                                 (abs (x uv))))
       (* axis-color (smoothstep (* 0.5 axis-thickness (y diff)) 0
                                 (abs (y uv)))))))

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
