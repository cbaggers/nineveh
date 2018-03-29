(in-package :nineveh.color)

;;------------------------------------------------------------
;; All props to:
;; - Franci Penov: Whos stackoverflow answers provided the bulk of
;;   functions here
;; - Maarten: Whos shadertoy code showed how to apply luminance
;;------------------------------------------------------------

;; Photometric/digital ITU BT.709: http://www.itu.int/rec/R-REC-BT.709
;; https://en.wikipedia.org/wiki/Luminance_%28relative%29
(defun-g rgb->luma-bt709 ((color :vec3))
  (+ (* (x color) "0.2126")
     (* (y color) "0.7152")
     (* (z color) "0.0722")))

(defun-g rgb->luma-bt709 ((color :vec4))
  (+ (* (x color) "0.2126")
     (* (y color) "0.7152")
     (* (z color) "0.0722")))

;; Y = 0.2126 R + 0.7152 G + 0.0722 B

;; Digital ITU BT.601 (gives more weight to the R and B components):
(defun-g rgb->luma-bt601 ((color :vec3))
  (+ (* (x color) "0.299")
     (* (y color) "0.587")
     (* (z color) "0.114")))

(defun-g rgb->luma-bt601 ((color :vec4))
  (+ (* (x color) "0.299")
     (* (y color) "0.587")
     (* (z color) "0.114")))

;; If you are willing to trade accuracy for perfomance, there are two
;; approximation formulas for this one:

(defun-g rgb->luma-low-accuracy-0 ((color :vec3))
  ;; Y = 0.33 R + 0.5 G + 0.16 B
  (/ (+ (x color) (x color)
        (y color)
        (z color) (z color) (z color))
     6))

(defun-g rgb->luma-low-accuracy-0 ((color :vec4))
  ;; Y = 0.33 R + 0.5 G + 0.16 B
  (/ (+ (x color) (x color)
        (y color)
        (z color) (z color) (z color))
     6))

;; (defun-g rgb->luma-low-accuracy-1 ((color :vec3))
;;   ;; Y = 0.375 R + 0.5 G + 0.125 B
;;   (>> (+ (x color) (x color) (x color)
;;          (y color)
;;          (z color) (z color) (z color) (z color))
;;       3))

;; (defun-g rgb->luma-low-accuracy-1 ((color :vec4))
;;   ;; Y = 0.375 R + 0.5 G + 0.125 B
;;   (>> (+ (x color) (x color) (x color)
;;          (y color)
;;          (z color) (z color) (z color) (z color))
;;       3))

(defun-g apply-luminance ((lum-function (function (:vec3) :float))
                          (lum :float)
                          (color :vec3))
  (let ((lum (/ lum (funcall lum-function color))))
    (* color lum)))

(defun-g apply-luminance ((lum-function (function (:vec4) :float))
                          (lum :float)
                          (color :vec4))
  (let ((lum (/ lum (funcall lum-function color))))
    (* color lum)))
