;;;; package.lisp

(uiop:define-package #:nineveh.math-primitives
    (:use #:cl #:cepl #:varjo-lang #:rtg-math :rtg-math.base-maths
          :documentation-utils #:varjo)
  (:import-from :varjo :dbind :vbind :dbind* :vbind* :symb)
  (:export :log10
           :saturate
           :mod-fixed-denominator
           :mod-fixed-denominator-low-quality))

(uiop:define-package #:nineveh.hashing
    (:use #:cl #:cepl #:varjo-lang #:rtg-math :rtg-math.base-maths
          :nineveh.math-primitives
          :documentation-utils)
  (:import-from :varjo :dbind :vbind :dbind* :vbind* :symb)
  (:export :blum-blum-shub-hash))

(uiop:define-package #:nineveh.random
    (:use #:cl #:cepl #:varjo-lang #:rtg-math :rtg-math.base-maths
          :nineveh.math-primitives
          :documentation-utils)
  (:import-from :varjo :dbind :vbind :dbind* :vbind* :symb)
  (:export :rand))

(uiop:define-package #:nineveh.tonemapping
    (:use #:cl #:cepl #:varjo-lang #:rtg-math :rtg-math.base-maths
          :nineveh.math-primitives
          :documentation-utils)
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
    (:use #:cl #:cepl #:varjo-lang #:rtg-math :rtg-math.base-maths
          :documentation-utils)
  (:import-from :varjo
                :dbind :vbind :dbind* :vbind* :symb)
  (:import-from :cepl-utils
                :with-setf)
  (:export :almost-identity :impulse :cubic-pulse
           :exponential-step :parabola :power-curve))

(uiop:define-package #:nineveh.easing
    (:use #:cl #:cepl #:varjo-lang #:rtg-math :rtg-math.base-maths
          #:easing-f
          :documentation-utils)
  (:reexport :easing))

(uiop:define-package #:nineveh.shaping-functions.polynomial
    (:use #:cl #:cepl #:varjo-lang #:rtg-math :rtg-math.base-maths
          :documentation-utils)
  (:import-from :varjo
                :dbind :vbind :dbind* :vbind* :symb)
  (:import-from :cepl-utils
                :with-setf)
  (:export :cos-raised-inverted-blinn-wybill
           :seat-double-cubic :seat-double-odd-exponent
           :seat-double-cubic-with-linear-bend))

(uiop:define-package #:nineveh
    (:use #:cl #:cepl #:varjo-lang #:rtg-math :rtg-math.base-maths
          :nineveh.math-primitives
          :nineveh.random
          :nineveh.tonemapping
          :nineveh.shaping-functions.polynomial
          :documentation-utils)
  (:import-from :varjo
                :dbind :vbind :dbind* :vbind* :symb)
  (:import-from :cepl-utils
                :with-setf)
  (:reexport :nineveh.math-primitives
             :nineveh.random
             :nineveh.tonemapping
             :nineveh.shaping-functions.polynomial)
  (:export
   ;;------------------------------
   ;; GPU
   ;;------------------------------
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
