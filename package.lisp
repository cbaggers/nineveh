;;;; package.lisp

(defpackage #:nineveh
  (:use #:cl #:cepl #:varjo-lang #:rtg-math :rtg-math.base-maths)
  (:export
   ;;
   ;; GPU
   ;;
   ;;------------------------------
   ;; log.lisp
   :log10
   ;;------------------------------
   ;; clamping.lisp
   :saturate

   ;;
   ;; CPU
   ;;
   ;;------------------------------
   :load-hdr-cross-image
   :load-hdr-cross-texture
   ))
