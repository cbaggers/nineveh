(in-package :nineveh.blending.gl)

;;
;; This is daft. It is an implementation of a chuck of OpenGL's blending api.
;;
;; Choosing (or even just understanding) GL's blending parameters can be tricky
;; so I decided to implement enough of it to be able to
;;

(defun-g opengl-blend ((src-rgb-transform (function (:vec4 :vec4) :vec3))
                       (src-alpha-transform (function (:float :float) :float))
                       (dest-rgb-transform (function (:vec4 :vec4) :vec3))
                       (dest-alpha-transform (function (:float :float) :float))
                       (mode-rgb-transform (function (:vec4 :vec4) :vec3))
                       (mode-alpha-transform (function (:float :float) :float))
                       (src :vec4)
                       (dest :vec4))
  ;; modify input params
  (let* ((src-rgb src)
         (dest-rgb dest)
         (src-alpha (w src))
         (dest-alpha (w dest))
         ;;
         (new-src-rgb (funcall src-rgb-transform src-rgb dest-rgb))
         (new-dest-rgb (funcall dest-rgb-transform src-rgb dest-rgb))
         (new-src-alpha (funcall src-alpha-transform src-alpha dest-alpha))
         (new-dest-alpha (funcall dest-alpha-transform src-alpha dest-alpha)))
    ;; compute final colors
    (v! (funcall mode-rgb-transform new-src-rgb new-dest-rgb)
        (funcall mode-alpha-transform new-src-alpha new-dest-alpha))))

;;------------------------------------------------------------
;; The component transform functions
;;
;; See comments at bottom of this file for info on what has been left out
;; and why

(defun-g zero ((src :vec4) (dst :vec4))
  (v3! 0f0))

(defun-g zero ((src :float) (dst :float))
  0f0)

(defun-g one ((src :vec4) (dst :vec4))
  (v3! 1f0))

(defun-g one ((src :float) (dst :float))
  1f0)

(defun-g src-color ((src :vec4) (dst :vec4))
  (s~ src :xyz))

(defun-g src-color ((src :float) (dst :float))
  src)

(defun-g dst-color ((src :vec4) (dst :vec4))
  (s~ dst :xyz))

(defun-g dst-color ((src :float) (dst :float))
  dst)

(defun-g one-minus-src-color ((src :vec4) (dst :vec4))
  (- (v3! 1f0) (s~ src :xyz)))

(defun-g one-minus-src-color ((src :float) (dst :float))
  (- 1f0 src))

(defun-g one-minus-dst-color ((src :vec4) (dst :vec4))
  (- (v3! 1f0) (s~ dst :xyz)))

(defun-g one-minus-dst-color ((src :float) (dst :float))
  (- 1f0 dst))

(defun-g src-alpha ((src :vec4) (dst :vec4))
  (v! (w src) (w src) (w src)))

(defun-g src-alpha ((src :float) (dst :float))
  src)

(defun-g dst-alpha ((src :vec4) (dst :vec4))
  (v! (w dst) (w dst) (w dst)))

(defun-g dst-alpha ((src :float) (dst :float))
  dst)

(defun-g one-minus-dst-alpha ((src :vec4) (dst :vec4))
  (- (v3! 1f0) (v! (w dst) (w dst) (w dst))))

(defun-g one-minus-dst-alpha ((src :float) (dst :float))
  (- 1f0 dst))

;; {TODO} Add src-alpha-saturate       | (v! i i i)                      | 1

;;--------------------------------------------------------------------------
;; mode functions
;;
;; We dont define min & max here are the regular glsl ones will work :)

(defun-g func-add ((src :vec3) (dst :vec3))
  (+ src dst))

(defun-g func-add ((src :float) (dst :float))
  (+ src dst))

(defun-g func-subtract ((src :vec3) (dst :vec3))
  (- src dst))

(defun-g func-subtract ((src :float) (dst :float))
  (- src dst))

(defun-g func-reverse-subtract ((src :vec3) (dst :vec3))
  (- dst src))

(defun-g func-reverse-subtract ((src :float) (dst :float))
  (- dst src))

;;--------------------------------------------------------------------------
;; Left out

;; I left out these as they are boring. Their behavior is easily understood
;; from the behaviour exhibited above.
;;
;; Parameter                 | RGB Factor                      | Alpha Factor
;; --------------------------------------------------------------------------
;; :constant-color           | (v! rc gc bc)                   | ac
;; :one-minus-constant-color | (- (v! 1 1 1) (v! rc gc bc))    | 1 - ac
;; :constant-alpha           | (v! ac ac ac)                   | ac
;; :one-minus-constant-alpha | (- (v! 1 1 1) (v! ac ac ac))    | 1 - ac
;; :src1-color               | (v! rs1 gs1 bs1)                | as1
;; :one-minus-src1-color     | (- (v! 1 1 1) (v! rs1 gs1 bs1)) | 1 - as1
;; :src1-alpha               | (v! as1 as1 as1)                | as1
;; :one-minus-src1-alpha     | (- (v! 1 1 1) (v! As1 As1 As1)) | 1 - As1
