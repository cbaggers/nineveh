(in-package :nineveh.hashing)

;;
;; FAST32_hash
;; A very fast hashing function.  Requires 32bit support.
;; http://briansharpe.wordpress.com/2011/11/15/a-fast-and-simple-32bit-floating-point-hash-function/
;;
;; The 2D hash formula takes the form....
;; hash = mod( coord.x * coord.x * coord.y * coord.y, SOMELARGEFLOAT )
;;        / SOMELARGEFLOAT
;;
;; We truncate and offset the domain to the most interesting part of the noise.
;; SOMELARGEFLOAT should be in the range of 400.0->1000.0 and needs to be hand
;; picked.  Only some give good results. A 3D hash is achieved by offsetting
;; the SOMELARGEFLOAT value by the Z coordinate
;;

;;------------------------------------------------------------
;; 2D

(defun-g bs-fast32-hash ((grid-cell :vec2))
  (let* ((offset (v2! 26.0 161.0))
         (domain 71.0)
         (somelargefloat "951.135664")
         (p (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1.0)))))
    (setf p (- p (* (floor (* p (/ 1.0 domain))) domain)))
    (incf p (s~ offset :xyxy))
    (multf p p)
    (fract (* (s~ p :xzxz) (* (s~ p :yyww) (/ 1.0 somelargefloat))))))

(defun-g bs-fast32-hash-2-per-corner ((grid-cell :vec2))
  (let* ((offset (v2! 26.0 161.0))
         (domain 71.0)
         (some-large-floats (v! "951.135664" "642.949883"))
         (p (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1.0)))))
    (setf p (- p (* (floor (* p (/ 1.0 domain))) domain)))
    (incf p (s~ offset :xyxy))
    (multf p p)
    (setf p (* (s~ p :xzxz) (s~ p :yyww)))
    (values (fract (* p (/ 1.0 (x some-large-floats))))
            (fract (* p (/ 1.0 (y some-large-floats)))))))

(defun-g bs-fast32-hash-3-per-corner ((grid-cell :vec2))
  (let* ((offset (v2! 26.0 161.0))
         (domain 71.0)
         (some-large-floats (v! "951.135664" "642.949883" "803.202459"))
         (p (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1.0)))))
    (setf p (- p (* (floor (* p (/ 1.0 domain))) domain)))
    (incf p (s~ offset :xyxy))
    (multf p p)
    (setf p (* (s~ p :xzxz) (s~ p :yyww)))
    (values
     (fract (* p (/ 1.0 (x some-large-floats))))
     (fract (* p (/ 1.0 (y some-large-floats))))
     (fract (* p (/ 1.0 (z some-large-floats)))))))

(defun-g bs-fast32-hash-cell ((grid-cell :vec2))
  (let* ((offset (v2! 26.0 161.0))
         (domain 71.0)
         (some-large-floats (v! "951.135664"
                                "642.949883"
                                "803.202459"
                                "986.973274"))
         (p (- grid-cell (* (floor (* grid-cell (/ 1.0 domain))) domain))))
    (incf p (s~ offset :xy))
    (multf p p)
    (fract (* (* (x p) (y p)) (/ (v4! 1.0) (s~ some-large-floats :xyzw))))))

;;------------------------------------------------------------
;; 3D

(defun-g bs-fast32-hash ((grid-cell :vec3))
  (let* ((offset (v2! 50.0 161.0))
         (domain 69.0)
         (somelargefloat "635.298681")
         (zinc "48.500388"))
    (setf (s~ grid-cell :xyz)
          (- (s~ grid-cell :xyz)
             (* (floor (* (s~ grid-cell :xyz) (/ 1.0 domain))) domain)))
    (let* ((grid-cell-inc1
            (* (step grid-cell (v3! (- domain 1.5))) (+ grid-cell (v3! 1.0))))
           (p
            (+ (v! (s~ grid-cell :xy) (s~ grid-cell-inc1 :xy))
               (s~ offset :xyxy)))
           ((lowz-hash :vec4))
           (highz-hash (v4! 0)))
      (multf p p)
      (setf p (* (s~ p :xzxz) (s~ p :yyww)))
      (setf (s~ highz-hash :xy)
            (/ (v2! 1.0)
               (+ (v2! somelargefloat)
                  (* (v2! (z grid-cell) (z grid-cell-inc1)) zinc))))
      (setf lowz-hash (fract (* p (s~ highz-hash :xxxx))))
      (setf highz-hash (fract (* p (s~ highz-hash :yyyy))))
      (values lowz-hash highz-hash))))


(defun-g bs-fast32-hash ((grid-cell :vec3) (v1-mask :vec3) (v2-mask :vec3))
  (let* ((offset (v2! 50.0 161.0))
         (domain 69.0)
         (somelargefloat "635.298681")
         (zinc "48.500388"))
    (setf (s~ grid-cell :xyz)
          (- (s~ grid-cell :xyz)
             (* (floor (* (s~ grid-cell :xyz) (/ 1.0 domain))) domain)))
    (let* ((grid-cell-inc1
            (* (step grid-cell (v3! (- domain 1.5))) (+ grid-cell (v3! 1.0))))
           (p
            (+ (v! (s~ grid-cell :xy) (s~ grid-cell-inc1 :xy))
               (s~ offset :xyxy))))
      (multf p p)
      (let* ((v1xy-v2xy
              (mix (s~ p :xyxy) (s~ p :zwzw)
                   (v! (s~ v1-mask :xy) (s~ v2-mask :xy)))))
        (setf p
              (* (v! (x p) (s~ v1xy-v2xy :xz) (z p))
                 (v! (y p) (s~ v1xy-v2xy :yw) (w p))))
        (let* ((v1z-v2z
                (v2!
                 (if (< (z v1-mask) 0.5)
                     (z grid-cell)
                     (z grid-cell-inc1))
                 (if (< (z v2-mask) 0.5)
                     (z grid-cell)
                     (z grid-cell-inc1))))
               (mod-vals
                (/ (v4! 1.0)
                   (+ (v4! somelargefloat)
                      (* (v! (z grid-cell) v1z-v2z (z grid-cell-inc1))
                         zinc)))))
          (fract (* p mod-vals)))))))

(defun-g bs-fast32-hash-3-per-corner ((grid-cell :vec3))
  (let* ((offset (v2! 50.0 161.0))
         (domain 69.0)
         (some-large-floats (v! "635.298681" "682.357502" "668.926525"))
         (zinc (v! "48.500388" "65.294118" "63.934599")))
    (setf (s~ grid-cell :xyz)
          (- (s~ grid-cell :xyz)
             (* (floor (* (s~ grid-cell :xyz) (/ 1.0 domain))) domain)))
    (let* ((grid-cell-inc1 (* (step grid-cell (v3! (- domain 1.5)))
                              (+ grid-cell (v3! 1.0))))
           (p (+ (v! (s~ grid-cell :xy) (s~ grid-cell-inc1 :xy))
                 (s~ offset :xyxy))))
      (multf p p)
      (setf p (* (s~ p :xzxz) (s~ p :yyww)))
      (let* ((lowz-mod
              (/ (v3! 1.0)
                 (+ (s~ some-large-floats :xyz)
                    (* (s~ grid-cell :zzz) (s~ zinc :xyz)))))
             (highz-mod
              (/ (v3! 1.0)
                 (+ (s~ some-large-floats :xyz)
                    (* (s~ grid-cell-inc1 :zzz) (s~ zinc :xyz))))))
        (values
         (fract (* p (s~ lowz-mod :xxxx)))       ;; lowz-hash-0
         (fract (* p (s~ lowz-mod :yyyy)))       ;; lowz-hash-1
         (fract (* p (s~ lowz-mod :zzzz)))       ;; lowz-hash-2
         (fract (* p (s~ highz-mod :xxxx)))      ;; highz-hash-0
         (fract (* p (s~ highz-mod :yyyy)))      ;; highz-hash-1
         (fract (* p (s~ highz-mod :zzzz)))))))) ;; highz-hash-2


(defun-g bs-fast32-hash-3-per-corner ((grid-cell :vec3)
                                      (v1-mask :vec3)
                                      (v2-mask :vec3))
  (let* ((offset (v2! 50.0 161.0))
         (domain 69.0)
         (some-large-floats (v! "635.298681" "682.357502" "668.926525"))
         (zinc (v! "48.500388" "65.294118" "63.934599")))
    (setf (s~ grid-cell :xyz)
          (- (s~ grid-cell :xyz)
             (* (floor (* (s~ grid-cell :xyz) (/ 1.0 domain))) domain)))
    (let* ((grid-cell-inc1
            (* (step grid-cell (v3! (- domain 1.5))) (+ grid-cell (v3! 1.0))))
           (p
            (+ (v! (s~ grid-cell :xy) (s~ grid-cell-inc1 :xy))
               (s~ offset :xyxy))))
      (multf p p)
      (let* ((v1xy-v2xy
              (mix (s~ p :xyxy) (s~ p :zwzw)
                   (v! (s~ v1-mask :xy) (s~ v2-mask :xy)))))
        (setf p
              (* (v! (x p) (s~ v1xy-v2xy :xz) (z p))
                 (v! (y p) (s~ v1xy-v2xy :yw) (w p))))
        (let* ((lowz-mods
                (/ (v3! 1.0)
                   (+ (s~ some-large-floats :xyz)
                      (* (s~ grid-cell :zzz) (s~ zinc :xyz)))))
               (highz-mods
                (/ (v3! 1.0)
                   (+ (s~ some-large-floats :xyz)
                      (* (s~ grid-cell-inc1 :zzz) (s~ zinc :xyz))))))
          (setf v1-mask
                (if (< (z v1-mask) 0.5)
                    lowz-mods
                    highz-mods))
          (setf v2-mask
                (if (< (z v2-mask) 0.5)
                    lowz-mods
                    highz-mods))
          (values
           (fract
            (* p
               (v4! (x lowz-mods) (x v1-mask) (x v2-mask)
                    (x highz-mods))))
           (fract
            (* p
               (v4! (y lowz-mods) (y v1-mask) (y v2-mask)
                    (y highz-mods))))
           (fract
            (* p
               (v4! (z lowz-mods) (z v1-mask) (z v2-mask)
                    (z highz-mods))))))))))

(defun-g bs-fast32-hash-4-per-corner ((grid-cell :vec3))
  (let (((lowz-hash-0 :vec4))
        ((lowz-hash-1 :vec4))
        ((lowz-hash-2 :vec4))
        (lowz-hash-3 (v4! 0))
        ((highz-hash-0 :vec4))
        ((highz-hash-1 :vec4))
        ((highz-hash-2 :vec4))
        (highz-hash-3 (v4! 0)))
    (let* ((offset (v2! 50.0 161.0))
           (domain 69.0)
           (some-large-floats (v! "635.298681"
                                  "682.357502"
                                  "668.926525"
                                  "588.255119"))
           (zinc (v! "48.500388" "65.294118" "63.934599" "63.279683")))
      (setf (s~ grid-cell :xyz)
            (- (s~ grid-cell :xyz)
               (* (floor (* (s~ grid-cell :xyz) (/ 1.0 domain))) domain)))
      (let* ((grid-cell-inc1
              (* (step grid-cell (v3! (- domain 1.5))) (+ grid-cell (v3! 1.0))))
             (p
              (+ (v! (s~ grid-cell :xy) (s~ grid-cell-inc1 :xy))
                 (s~ offset :xyxy))))
        (multf p p)
        (setf p (* (s~ p :xzxz) (s~ p :yyww)))
        (setf (s~ lowz-hash-3 :xyzw)
              (/ (v4! 1.0)
                 (+ (s~ some-large-floats :xyzw)
                    (* (s~ grid-cell :zzzz) (s~ zinc :xyzw)))))
        (setf (s~ highz-hash-3 :xyzw)
              (/ (v4! 1.0)
                 (+ (s~ some-large-floats :xyzw)
                    (* (s~ grid-cell-inc1 :zzzz) (s~ zinc :xyzw)))))
        (setf lowz-hash-0 (fract (* p (s~ lowz-hash-3 :xxxx))))
        (setf highz-hash-0 (fract (* p (s~ highz-hash-3 :xxxx))))
        (setf lowz-hash-1 (fract (* p (s~ lowz-hash-3 :yyyy))))
        (setf highz-hash-1 (fract (* p (s~ highz-hash-3 :yyyy))))
        (setf lowz-hash-2 (fract (* p (s~ lowz-hash-3 :zzzz))))
        (setf highz-hash-2 (fract (* p (s~ highz-hash-3 :zzzz))))
        (setf lowz-hash-3 (fract (* p (s~ lowz-hash-3 :wwww))))
        (setf highz-hash-3 (fract (* p (s~ highz-hash-3 :wwww))))))
    (values lowz-hash-0 lowz-hash-1 lowz-hash-2 lowz-hash-3
            highz-hash-0 highz-hash-1 highz-hash-2 highz-hash-3)))

(defun-g bs-fast32-hash-cell ((grid-cell :vec3))
  (let* ((offset (v2! 50.0 161.0))
         (domain 69.0)
         (some-large-floats (v! "635.298681"
                                "682.357502"
                                "668.926525"
                                "588.255119"))
         (zinc (v! "48.500388" "65.294118" "63.934599" "63.279683")))
    (setf (s~ grid-cell :xyz)
          (- grid-cell (* (floor (* grid-cell (/ 1.0 domain))) domain)))
    (incf (s~ grid-cell :xy) (s~ offset :xy))
    (multf (s~ grid-cell :xy) (s~ grid-cell :xy))
    (fract
     (* (* (x grid-cell)
           (y grid-cell))
        (/ (v4! 1.0) (+ some-large-floats
                        (* (s~ grid-cell :zzzz)
                           zinc)))))))

;;------------------------------------------------------------
