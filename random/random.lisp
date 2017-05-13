(in-package :nineveh.random)

(defun-g rand ((seed :vec2))
  (fract (* (sin (dot (s~ seed :xy) (v! "12.9898" "78.233")))
            "43758.5453")))

;;
;; Safe alternative for when Varjo supports precision modifiers
;;
;; http://byteblacksmith.com/improvements-to-the-canonical-one-liner-glsl-ran
;; d-for-opengl-es-2-0/
;;
;; highp float rand(vec2 co)
;; {
;;     highp float a = 12.9898;
;;     highp float b = 78.233;
;;     highp float c = 43758.5453;
;;     highp float dt= dot(co.xy ,vec2(a,b));
;;     highp float sn= mod(dt,3.14);
;;     return fract(sin(sn) * c);
;; }
