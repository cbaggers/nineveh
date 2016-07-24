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
   ;;------------------------------
   ;; sampling.lisp
   :sample-equirectangular-tex
   :uv->cube-map-directions
   ;;------------------------------
   ;; misc.lisp
   :radical-inverse-vdc
   ;;------------------------------
   ;; mipmaps.lisp
   :mipmap-level->grey
   :mipmap-level->color

   ;;
   ;; CPU
   ;;
   ;;------------------------------
   :load-hdr-cross-image
   :load-hdr-cross-texture
   :load-hdr-2d
   ;;
   ;; Both
   ;;
   ;;------------------------------
   :bind-vec
   ))
