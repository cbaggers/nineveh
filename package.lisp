;;;; package.lisp

(uiop:define-package #:nineveh.math-primitives
    (:use #:cl :glsl-symbols #:cepl #:rtg-math :rtg-math.base-maths
          :documentation-utils #:vari)
  (:import-from :varjo :dbind :vbind :dbind* :vbind* :symb)
  (:export :log10
           :atan2
           :saturate
           :mod-fixed-denominator
           :mod-fixed-denominator-low-quality
           :remap
           :remap-uv
           :radical-inverse-vdc))

(uiop:define-package #:nineveh.conditionals
    (:use #:cl :glsl-symbols #:cepl #:vari #:rtg-math :rtg-math.base-maths
          :documentation-utils #:vari #:nineveh.math-primitives)
  (:import-from :varjo :dbind :vbind :dbind* :vbind* :symb)
  (:export :mix-step-if))

(uiop:define-package #:nineveh.color
    (:use #:cl :glsl-symbols #:cepl #:vari #:rtg-math :rtg-math.base-maths
          :documentation-utils #:nineveh.math-primitives
          :nineveh.conditionals)
  (:import-from :varjo :dbind :vbind :dbind* :vbind* :symb)
  (:export :rgb->greyscale
           :hue->rgb
           :rgb->hcv
           :rgb->hsv
           :hsv->rgb
           :rgb->hsl
           :hsl->rgb
           :rgb->hcy
           :hcy->rgb
           :rgb->hcl))

(uiop:define-package #:nineveh.hashing
    (:use #:cl :glsl-symbols #:cepl :vari #:rtg-math :rtg-math.base-maths
          :nineveh.math-primitives
          :documentation-utils)
  (:import-from :varjo :dbind :vbind :dbind* :vbind* :symb)
  (:export :blum-blum-shub-hash
           :blum-blum-shub-hash-low-quality
           ;;
           :bs-fast32-hash
           :bs-fast32-hash-2-per-corner
           :bs-fast32-hash-3-per-corner
           :bs-fast32-hash-4-per-corner
           :bs-fast32-hash-cell
           ;;
           :bs-quick32-hash
           :bs-quick32-hash-4-per-corner
           ;;
           :sgim-qpp-hash
           :sgim-qpp-hash-2-per-corner
           :sgim-qpp-hash-3-per-corner))

(uiop:define-package #:nineveh.shaping-functions
    (:use #:cl :glsl-symbols #:cepl :vari #:rtg-math :rtg-math.base-maths
          :documentation-utils)
  (:import-from :varjo
                :dbind :vbind :dbind* :vbind* :symb)
  (:import-from :cepl-utils
                :with-setf)
  (:export :cos-raised-inverted-blinn-wybill
           :seat-double-cubic
           :seat-double-cubic-with-linear-bend
           :seat-double-odd-exponent
           ;;
           :almost-identity
           :cubic-pulse
           :exponential-step
           :impulse
           :parabola
           :power-curve
           ;;
           :hermine
           :quintic
           :quintic-deriv
           :quintic-fast
           :quintic-interp-and-deriv
           :quintic-hermite
           :quintic-hermite-deriv
           ;;
           :falloff-xsq-c1
           :falloff-xsq-c2
           :falloff-xsq-c2))

(uiop:define-package #:nineveh.noise
    (:use #:cl :glsl-symbols #:cepl :vari #:rtg-math :rtg-math.base-maths
          :nineveh.math-primitives
          :nineveh.shaping-functions
          :nineveh.hashing
          :documentation-utils)
  (:import-from :varjo :dbind :vbind :dbind* :vbind* :symb)
  (:export :value-noise
           :value-noise-deriv
           ;;
           :perlin-noise
           :perlin-noise-surflet
           :perlin-noise-revised
           :perlin-noise-simplex
           :perlin-noise-deriv
           :perlin-noise-surflet-deriv
           :perlin-noise-simplex-deriv
           ;;
           :value-perlin-noise
           ;;
           :cubist-noise
           ;;
           :cellular-noise
           :cellular-noise-fast
           :cellular-noise-simplex
           ;;
           :polka-dot-noise
           :polka-dot-noise-simplex
           :stars-noise
           ;;
           :hermite-noise
           :hermite-noise-unnormalized-gradients
           :hermite-noise-deriv
           :hermite-noise-unnormalized-gradients-deriv
           ;;
           :value-hermite-noise))

(uiop:define-package #:nineveh.random
    (:use #:cl :glsl-symbols #:cepl :vari #:rtg-math :rtg-math.base-maths
          :nineveh.math-primitives
          :nineveh.shaping-functions
          :documentation-utils)
  (:import-from :varjo :dbind :vbind :dbind* :vbind* :symb)
  (:export :rand
           :hammersley-nth-2d
           :hammersley-nth-hemisphere))

(uiop:define-package :nineveh.mesh.data.primitives
    (:use #:cl :glsl-symbols #:cepl #:vari #:rtg-math #:rtg-math.base-maths
          #:nineveh.math-primitives
          #:documentation-utils
          #:dendrite.primitives)
  (:import-from :varjo
                :dbind :vbind :dbind* :vbind* :symb)
  (:import-from :cepl-utils
                :with-setf)
  (:export :latice-c-arrays
           :latice-gpu-arrays
           :box-c-arrays
           :box-gpu-arrays
           :cone-c-arrays
           :cone-gpu-arrays
           :cylinder-c-arrays
           :cylinder-gpu-arrays
           :plain-gpu-arrays
           :plain-c-arrays
           :cube-gpu-arrays
           :cube-c-arrays
           :sphere-gpu-arrays
           :sphere-c-arrays))

(uiop:define-package #:nineveh.easing
    (:use #:cl :glsl-symbols #:cepl :vari #:rtg-math :rtg-math.base-maths
          #:easing-f
          :documentation-utils)
  (:reexport :easing))

(uiop:define-package #:nineveh.graphing
    (:use #:cl :glsl-symbols #:cepl :vari #:rtg-math :rtg-math.base-maths
          :documentation-utils)
  (:export :graph
           :axis
           :plot))

(uiop:define-package #:nineveh.normals
    (:use #:cl :glsl-symbols #:cepl #:vari #:rtg-math :rtg-math.base-maths
          :documentation-utils)
  (:import-from :varjo :dbind :vbind :dbind* :vbind* :symb)
  (:export :simple-sample-normals))

(uiop:define-package #:nineveh.textures
    (:use #:cl :glsl-symbols #:cepl #:vari #:rtg-math :rtg-math.base-maths
          :documentation-utils)
  (:import-from :varjo :dbind :vbind :dbind* :vbind* :symb)
  (:export :sample-equirectangular-tex
           :uv->cube-map-directions
           ;;
           :draw-tex
           :draw-tex-tl
           :draw-tex-tr
           :draw-tex-bl
           :draw-tex-br
           :draw-tex-top-left
           :draw-tex-top-right
           :draw-tex-bottom-left
           :draw-tex-bottom-right
           ;;
           :load-hdr-cross-image
           :load-hdr-cross-texture
           :load-hdr-2d
           ;;
           :make-fbos-for-each-mipmap-of-cube-texture
           :cube-faces))

(uiop:define-package #:nineveh.streams
    (:use #:cl :glsl-symbols #:cepl #:vari #:rtg-math :rtg-math.base-maths
          :documentation-utils)
  (:import-from :varjo
                :dbind :vbind :dbind* :vbind* :symb)
  (:import-from :uiop
                :ensure-list)
  (:export :get-quad-stream-v2
           :buffer-streamer
           :make-buffer-streamer
           :buffer-streamer-push))

(uiop:define-package #:nineveh.tonemapping
    (:use #:cl :glsl-symbols #:cepl #:vari #:rtg-math :rtg-math.base-maths
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

(uiop:define-package #:nineveh
    (:use #:cl :glsl-symbols #:cepl :vari #:rtg-math :rtg-math.base-maths
          :nineveh.math-primitives
          :nineveh.conditionals
          :nineveh.random
          :nineveh.hashing
          :nineveh.noise
          :nineveh.tonemapping
          :nineveh.shaping-functions
          :nineveh.graphing
          :nineveh.normals
          :nineveh.textures
          :nineveh.streams
          :documentation-utils)
  (:import-from :varjo
                :dbind :vbind :dbind* :vbind* :symb)
  (:import-from :cepl-utils
                :with-setf)
  (:reexport :nineveh.math-primitives
             :nineveh.conditionals
             :nineveh.color
             :nineveh.hashing
             :nineveh.shaping-functions
             :nineveh.noise
             :nineveh.random
             :nineveh.graphing
             :nineveh.normals
             :nineveh.textures
             :nineveh.streams
             :nineveh.tonemapping)
  (:export
   :as-frame
   :def-simple-main-loop))
