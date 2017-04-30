;;;; package.lisp

(uiop:define-package #:nineveh.math-primtives
    (:use #:cl #:cepl #:varjo-lang #:rtg-math :rtg-math.base-maths)
  (:import-from :varjo :dbind :vbind :dbind* :vbind* :symb)
  (:export :log10 :saturate))

(uiop:define-package #:nineveh.tonemapping
    (:use #:cl #:cepl #:varjo-lang #:rtg-math :rtg-math.base-maths
          :nineveh.math-primtives)
  (:import-from :varjo
                :dbind :vbind :dbind* :vbind* :symb)
  (:import-from :cepl-utils
                :with-setf)
  (:export :tone-map-linear
           :tone-map-reinhard
           :tone-map-haarm-peter-duiker
           :tone-map-hejl-burgess-dawson
           :tone-map-uncharted2))

(uiop:define-package #:nineveh.shaping-functions
    (:use #:cl #:cepl #:varjo-lang #:rtg-math :rtg-math.base-maths)
  (:import-from :varjo
                :dbind :vbind :dbind* :vbind* :symb)
  (:import-from :cepl-utils
                :with-setf)
  (:export :almost-identity :impulse :cubic-pulse
           :exponential-step :parabola :power-curve))

(uiop:define-package #:nineveh.shaping-functions.polynomial
    (:use #:cl #:cepl #:varjo-lang #:rtg-math :rtg-math.base-maths)
  (:import-from :varjo
                :dbind :vbind :dbind* :vbind* :symb)
  (:import-from :cepl-utils
                :with-setf)
  (:export :cos-raised-inverted-blinn-wybill
           :seat-double-cubic :seat-double-odd-exponent
           :seat-double-cubic-with-linear-bend))

(uiop:define-package #:nineveh
    (:use #:cl #:cepl #:varjo-lang #:rtg-math :rtg-math.base-maths)
  (:import-from :varjo
                :dbind :vbind :dbind* :vbind* :symb)
  (:import-from :cepl-utils
                :with-setf)
  (:reexport :nineveh.tonemapping
             :nineveh.shaping-functions.polynomial)
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
   ;; Quads
   ;;------------------------------
   :get-quad-stream-v2

   ;;------------------------------
   ;; Both
   ;;------------------------------
   ;;
   :bind-vec))
