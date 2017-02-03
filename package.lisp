;;;; package.lisp

(uiop:define-package #:nineveh
    (:use #:cl #:cepl #:varjo-lang #:rtg-math :rtg-math.base-maths)
  (:import-from :varjo
                :dbind :vbind :dbind* :vbind* :symb)
  (:import-from :cepl-utils
                :with-setf)
  (:export
   ;;------------------------------
   ;; GPU
   ;;------------------------------
   ;;
   ;; log
   :log10
   ;;
   ;; clamping
   :saturate
   ;;
   ;; sampling
   :sample-equirectangular-tex
   :uv->cube-map-directions
   ;;
   ;; misc
   :radical-inverse-vdc
   ;;
   ;; mipmaps
   :mipmap-level->grey
   :mipmap-level->color
   ;;
   ;; textures
   :draw-tex
   :draw-tex-tl
   :draw-tex-tr
   :draw-tex-bl
   :draw-tex-br
   :draw-tex-top-left
   :draw-tex-top-right
   :draw-tex-bottom-left
   :draw-tex-bottom-right

   ;;------------------------------
   ;; CPU
   ;;------------------------------
   ;;
   ;; hdr
   :load-hdr-cross-image
   :load-hdr-cross-texture
   :load-hdr-2d
   ;;
   ;; fbos.lisp
   :make-fbos-for-each-mipmap-of-cube-texture
   ;;
   ;; textures
   :cube-faces
   ;;
   ;; misc
   :as-frame
   :def-simple-main-loop

   ;;------------------------------
   ;; Both
   ;;------------------------------
   ;;
   :bind-vec))
