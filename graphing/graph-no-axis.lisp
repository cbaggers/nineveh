(in-package :nineveh.graphing)

;; Based on Mikael Hvidtfeldt Christensen's excellent article here:
;; http://blog.hvidtfeldts.net/index.php/2011/07/plotting-high-frequency-functions-using-a-gpu/
;;
;; We don't use dithering on this version

(defun-g graph-no-axis ((func (function (:float) :float))
                        (uv :vec2)
                        (xy-range :vec4) ;; x-min x-max y-min y-max
                        (line-style :vec4)
                        (samples :int))
  (let* (;;
         (line-thickness (w line-style))
         (line-color (v! (s~ line-style :xyz) 1))
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
    (values (* (if (/= (abs count) my-samples)
                   (- 1f0 (/ (abs (float count)) (float my-samples)))
                   0f0)
               line-color)
            (funcall func (x uv)))))

;; {TODO} these can go away once varjo has &optional support

;;------------------------------------------------------------

(defun-g graph-no-axis ((func (function (:float) :float))
                        (uv :vec2)
                        (xy-range :vec4) ;; x-min x-max y-min y-max
                        (line-style :vec4))
  (graph-no-axis func uv xy-range line-style 10))

;;------------------------------------------------------------

(defun-g graph-no-axis ((func (function (:float) :float))
                        (uv :vec2)
                        (xy-range :vec4) ;; x-min x-max y-min y-max
                        (line-style :vec4))
  (graph-no-axis func
                 uv
                 xy-range
                 line-style
                 10))

;;------------------------------------------------------------

(defun-g graph-no-axis ((func (function (:float) :float))
                        (uv :vec2)
                        (xy-range :vec4) ;; x-min x-max y-min y-max
                        (line-style :float))
  (graph-no-axis func
                 uv xy-range
                 (v! 1 1 1 line-style)
                 10))

;;------------------------------------------------------------

(defun-g graph-no-axis ((func (function (:float) :float))
                        (uv :vec2)
                        (xy-range :vec4)) ;; x-min x-max y-min y-max
  (graph-no-axis func
                 uv
                 xy-range
                 (v! 1 1 1 0.004)
                 10))

;;------------------------------------------------------------

(defun-g graph-no-axis ((func (function (:float) :float))
                        (uv :vec2))
  (graph-no-axis func
                 uv
                 (v! 0 1 0 1) ;; range
                 (v! 1 1 1 0.004) ;; line-style
                 10)) ;; samples

;;------------------------------------------------------------
