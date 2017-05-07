;;;; nineveh.asd

(asdf:defsystem #:nineveh
  :description "A library of common gpu functions"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (#:cepl #:cl-soil #:livesupport #:easing
                      #:documentation-utils)
  :components ((:file "package")
               (:file "cpu/hdr-cross-cube-map-loader")
               (:file "cpu/bind")
               (:file "cpu/fbos")
               (:file "cpu/viewport")
               (:file "cpu/misc")
               ;;
               (:file "math-primitives/log")
               (:file "math-primitives/clamping")
               (:file "math-primitives/mod")
               (:file "math-primitives/docs")
               ;;
               (:file "hashing/blum-blum-shub-hash")
               (:file "hashing/permutation-polynomial-hash")
               (:file "hashing/bsharpe-fast-32-hash")
               (:file "hashing/bsharpe-quick32-hash")
               (:file "hashing/docs")
               ;;
               (:file "shaping-functions/to-sort")
               (:file "shaping-functions/interpolation")
               (:file "shaping-functions/polynominal")
               ;;
               (:file "random/random")
               (:file "random/docs")
               (:file "gpu/bind")
               (:file "gpu/sampling")
               (:file "gpu/mipmaps")
               (:file "textures/draw-texture")
               (:file "quad-streams")
               (:file "gpu/misc")
               (:file "tonemapping/operators")
               ))
