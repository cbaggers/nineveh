(in-package :nineveh.blending)


(defun-g blend-one-minus-src-color ((src :float) (b :float))
  (+ src (* b (- 1f0 src))))

(defun-g blend-one-minus-src-color ((src :vec2) (b :vec2))
  (+ src (* b (- (v2! 1f0) src))))

(defun-g blend-one-minus-src-color ((src :vec3) (b :vec3))
  (+ src (* b (- (v3! 1f0) src))))

(defun-g blend-one-minus-src-color ((src :vec3) (b :vec3))
  (+ src (* b (- (v4! 1f0) src))))

(defun-g blend-one-minus-src-color ((src :float) (b :float))
  (+ src (* b (- 1f0 src))))

(defun-g blend-one-minus-src-color ((src :vec2) (b :vec2))
  (+ src (* b (- (v2! 1f0) src))))

(defun-g blend-one-minus-src-color ((src :vec3) (b :vec3))
  (+ src (* b (- (v3! 1f0) src))))

(defun-g blend-one-minus-src-color ((src :vec3) (b :vec3))
  (+ src (* b (- (v4! 1f0) src))))


;; Simple Blend
;;
;; Least surprise is to blend by one-minus for float vec2, vec3.
;; For vec4 xyz & w components will be blended seperately and then
;; combined as a vec4.

(defun-g blend ((src :float) (dst :float))
  (+ src (* dst (- 1f0 src))))

(defun-g blend ((src :vec2) (dst :vec2))
  (+ src (* dst (- (v2! 1f0) src))))

(defun-g blend ((src :vec3) (dst :vec3))
  (+ src (* dst (- (v3! 1f0) src))))

(defun-g blend ((src :vec4) (dst :vec4))
  (let ((src-rgb (s~ src :xyz))
        (dst-rgb (s~ dst :xyz))
        (src-alpha (w src))
        (dst-alpha (w dst)))
    (v! (+ src-rgb (* dst-rgb (- (v3! 1) src-rgb)))
        (+ src-alpha (* dst-alpha (- 1f0 src-alpha))))))
