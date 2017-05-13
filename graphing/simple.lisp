(in-package :nineveh.graphing)

;; Based on Mikael Hvidtfeldt Christensen's excellent article here:
;; http://blog.hvidtfeldts.net/index.php/2011/07/plotting-high-frequency-functions-using-a-gpu/
;;
;; We don't use dithering on this version

;; {TODO} When inlining is implemented in Varjo make the implementation of this
;;        function: (graph func pos line-thickness 10)

(defun-g graph ((func (function (:float) :float))
                (pos :vec2)
                (line-thickness :float))
  ;; Usage
  ;; (v3! (graph #'cos uv 0.005))
  (let* ((samples 10)
         (samples (float samples))
         (max-dist (v2! line-thickness))
         (step (/ max-dist (v2! samples)))
         (count 0f0)
         (initial-offset (* step samples -0.5))
         (my-samples 0f0))
    (incf initial-offset pos)
    (for (i 0f0) (< i samples) (++ i)
         (let ((fx (funcall func (+ (x pos) (* i (x step))))))
           (for (j 0f0) (< j samples) (++ j)
                (when (> (+ (* i i) (* j j)) (* samples samples))
                  (continue))
                (incf my-samples 1f0)
                (let ((diff (- fx (+ (y pos) (* j (y step))))))
                  (incf count (- (* (step 0f0 diff) 2f0) 1))))))
    (if (/= (abs count) my-samples)
        (- 1f0 (/ (abs (float count)) (float my-samples)))
        0f0)))

(defun-g graph ((func (function (:float) :float))
                (pos :vec2)
                (line-thickness :float)
                (samples :int))
  ;; Usage
  ;; (v3! (graph #'cos uv 0.005 50))
  (let* ((samples (float samples))
         (max-dist (v2! line-thickness))
         (step (/ max-dist (v2! samples)))
         (count 0f0)
         (initial-offset (* step samples -0.5))
         (my-samples 0f0))
    (incf initial-offset pos)
    (for (i 0f0) (< i samples) (++ i)
         (let ((fx (funcall func (+ (x pos) (* i (x step))))))
           (for (j 0f0) (< j samples) (++ j)
                (when (> (+ (* i i) (* j j)) (* samples samples))
                  (continue))
                (incf my-samples 1f0)
                (let ((diff (- fx (+ (y pos) (* j (y step))))))
                  (incf count (- (* (step 0f0 diff) 2f0) 1))))))
    (if (/= (abs count) my-samples)
        (- 1f0 (/ (abs (float count)) (float my-samples)))
        0f0)))
