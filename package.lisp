;;;; package.lisp

(defpackage #:nineveh
  (:use #:cl #:cepl #:varjo-lang #:rtg-math :rtg-math.base-maths)
  (:export
   ;;------------------------------
   ;; log.lisp
   :log10
   ;;------------------------------
   ;; clamping.lisp
   :saturate
   ))
