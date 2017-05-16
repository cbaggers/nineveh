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
