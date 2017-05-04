(in-package :nineveh.hashing)

(defun-g fast32-hash-2d ((grid-cell :vec2))
  (let* ((offset (v2! 26.0 161.0))
         (domain 71.0)
         (somelargefloat 951.1357)
         (p (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1.0)))))
    (setf p (- p (* (floor (* p (/ 1.0 domain))) domain)))
    (incf p (s~ offset :xyxy))
    (multf p p)
    (fract (* (s~ p :xzxz) (* (s~ p :yyww) (/ 1.0 somelargefloat))))))

(defun-g fast32-hash-2d ((grid-cell :vec2))
  (let (((hash-0 :vec4)) ((hash-1 :vec4)))
    (let* ((offset (v2! 26.0 161.0))
           (domain 71.0)
           (some-large-floats (v2! 951.1357 642.9499))
           (p (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1.0)))))
      (setf p (- p (* (floor (* p (/ 1.0 domain))) domain)))
      (incf p (s~ offset :xyxy))
      (multf p p)
      (setf p (* (s~ p :xzxz) (s~ p :yyww)))
      (progn
        (setf hash-0 (fract (* p (/ 1.0 (x some-large-floats)))))
        (setf hash-1 (fract (* p (/ 1.0 (y some-large-floats)))))))
    (values hash-0 hash-1)))

(defun-g fast32-hash-2d ((grid-cell :vec2))
  (let (((hash-0 :vec4)) ((hash-1 :vec4)) ((hash-2 :vec4)))
    (let* ((offset (v2! 26.0 161.0))
           (domain 71.0)
           (some-large-floats (v3! 951.1357 642.9499 803.20245))
           (p (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1.0)))))
      (setf p (- p (* (floor (* p (/ 1.0 domain))) domain)))
      (incf p (s~ offset :xyxy))
      (multf p p)
      (setf p (* (s~ p :xzxz) (s~ p :yyww)))
      (progn
        (setf hash-0 (fract (* p (/ 1.0 (x some-large-floats)))))
        (setf hash-1 (fract (* p (/ 1.0 (y some-large-floats)))))
        (setf hash-2 (fract (* p (/ 1.0 (z some-large-floats)))))))
    (values hash-0 hash-1 hash-2)))

(defun-g fast32-hash-2d-cell ((grid-cell :vec2))
  (let* ((offset (v2! 26.0 161.0))
         (domain 71.0)
         (some-large-floats (v4! 951.1357 642.9499 803.20245 986.97327))
         (p (- grid-cell (* (floor (* grid-cell (/ 1.0 domain))) domain))))
    (incf p (s~ offset :xy))
    (multf p p)
    (fract (* (* (x p) (y p)) (/ (v4! 1.0) (s~ some-large-floats :xyzw))))))

(defun-g fast32-hash-3d-cell ((grid-cell :vec3))
  (let* ((offset (v2! 50.0 161.0))
         (domain 69.0)
         (some-large-floats (v4! 635.2987 682.3575 668.9265 588.2551))
         (zinc (v4! 48.50039 65.29412 63.9346 63.279682)))
    (setf (s~ grid-cell :xyz)
          (- grid-cell (* (floor (* grid-cell (/ 1.0 domain))) domain)))
    (incf (s~ grid-cell :xy) (s~ offset :xy))
    (multf (s~ grid-cell :xy) (s~ grid-cell :xy))
    (fract
     (* (* (x grid-cell) (y grid-cell))
        (/ (v4! 1.0) (+ some-large-floats (* (s~ grid-cell :zzzz) zinc)))))))

(defun-g fast32-hash-3d ((grid-cell :vec3))
  (let (((lowz-hash :vec4)) (highz-hash (v4! 0)))
    (let* ((offset (v2! 50.0 161.0))
           (domain 69.0)
           (somelargefloat 635.2987)
           (zinc 48.50039))
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
        (setf (s~ highz-hash :xy)
              (/ (v2! 1.0)
                 (+ (v2! somelargefloat)
                    (* (v2! (z grid-cell) (z grid-cell-inc1)) zinc))))
        (setf lowz-hash (fract (* p (s~ highz-hash :xxxx))))
        (setf highz-hash (fract (* p (s~ highz-hash :yyyy))))))
    (values lowz-hash highz-hash)))


(defun-g fast32-hash-3d ((grid-cell :vec3) (v1-mask :vec3) (v2-mask :vec3))
  (let (((hash-0 :vec4)) ((hash-1 :vec4)) ((hash-2 :vec4)))
    (let* ((offset (v2! 50.0 161.0))
           (domain 69.0)
           (some-large-floats (v3! 635.2987 682.3575 668.9265))
           (zinc (v3! 48.50039 65.29412 63.9346)))
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
            (setf hash-0
                  (fract
                   (* p
                      (v4! (x lowz-mods) (x v1-mask) (x v2-mask)
                           (x highz-mods)))))
            (setf hash-1
                  (fract
                   (* p
                      (v4! (y lowz-mods) (y v1-mask) (y v2-mask)
                           (y highz-mods)))))
            (setf hash-2
                  (fract
                   (* p
                      (v4! (z lowz-mods) (z v1-mask) (z v2-mask)
                           (z highz-mods)))))))))
    (values hash-0 hash-1 hash-2)))

(defun-g fast32-hash-3d ((grid-cell :vec3) (v1-mask :vec3) (v2-mask :vec3))
  (let* ((offset (v2! 50.0 161.0))
         (domain 69.0)
         (somelargefloat 635.2987)
         (zinc 48.50039))
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

(defun-g fast32-hash-3d-3 ((grid-cell :vec3))
  (let (((lowz-hash-0 :vec4))
        ((lowz-hash-1 :vec4))
        ((lowz-hash-2 :vec4))
        ((highz-hash-0 :vec4))
        ((highz-hash-1 :vec4))
        ((highz-hash-2 :vec4)))
    (let* ((offset (v2! 50.0 161.0))
           (domain 69.0)
           (some-large-floats (v3! 635.2987 682.3575 668.9265))
           (zinc (v3! 48.50039 65.29412 63.9346)))
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
        (let* ((lowz-mod
                (/ (v3! 1.0)
                   (+ (s~ some-large-floats :xyz)
                      (* (s~ grid-cell :zzz) (s~ zinc :xyz)))))
               (highz-mod
                (/ (v3! 1.0)
                   (+ (s~ some-large-floats :xyz)
                      (* (s~ grid-cell-inc1 :zzz) (s~ zinc :xyz))))))
          (setf lowz-hash-0 (fract (* p (s~ lowz-mod :xxxx))))
          (setf highz-hash-0 (fract (* p (s~ highz-mod :xxxx))))
          (setf lowz-hash-1 (fract (* p (s~ lowz-mod :yyyy))))
          (setf highz-hash-1 (fract (* p (s~ highz-mod :yyyy))))
          (setf lowz-hash-2 (fract (* p (s~ lowz-mod :zzzz))))
          (setf highz-hash-2 (fract (* p (s~ highz-mod :zzzz)))))))
    (values lowz-hash-0 lowz-hash-1 lowz-hash-2 highz-hash-0 highz-hash-1
            highz-hash-2)))

(defun-g fast32-hash-3d-4 ((grid-cell :vec3))
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
           (some-large-floats (v4! 635.2987 682.3575 668.9265 588.2551))
           (zinc (v4! 48.50039 65.29412 63.9346 63.279682)))
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
        (progn
          (setf lowz-hash-0 (fract (* p (s~ lowz-hash-3 :xxxx))))
          (setf highz-hash-0 (fract (* p (s~ highz-hash-3 :xxxx))))
          (setf lowz-hash-1 (fract (* p (s~ lowz-hash-3 :yyyy))))
          (progn
            (setf highz-hash-1 (fract (* p (s~ highz-hash-3 :yyyy))))
            (setf lowz-hash-2 (fract (* p (s~ lowz-hash-3 :zzzz))))
            (setf highz-hash-2 (fract (* p (s~ highz-hash-3 :zzzz))))
            (progn
              (setf lowz-hash-3 (fract (* p (s~ lowz-hash-3 :wwww))))
              (setf highz-hash-3 (fract (* p (s~ highz-hash-3 :wwww)))))))))
    (values lowz-hash-0 lowz-hash-1 lowz-hash-2 lowz-hash-3 highz-hash-0
            highz-hash-1 highz-hash-2 highz-hash-3)))

(defun-g fast32-2-hash-2d ((grid-cell :vec2))
  (let* ((offset (v2! 403.83917 377.2427))
         (domain 69.0)
         (somelargefloat 32745.709)
         (scale (v2! 2.009842 1.372549))
         (p (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1.0)))))
    (setf p (- p (* (floor (* p (/ 1.0 domain))) domain)))
    (setf p (+ (* p (s~ scale :xyxy)) (s~ offset :xyxy)))
    (multf p p)
    (fract (* (s~ p :xzxz) (* (s~ p :yyww) (/ 1.0 somelargefloat))))))

(defun-g fast32-2-hash-3d ((grid-cell :vec3))
  (let (((z0-hash :vec4)) ((z1-hash :vec4)))
    (let* ((offset (v3! 55.882355 63.167774 52.941177))
           (domain 69.0)
           (somelargefloat 69412.07)
           (scale (v3! 0.235142 0.20589 0.21644899)))
      (setf grid-cell
            (- grid-cell (* (floor (* grid-cell (/ 1.0 domain))) domain)))
      (let* ((grid-cell-inc1
              (* (step grid-cell (v3! (- domain 1.5))) (+ grid-cell (v3! 1.0)))))
        (setf grid-cell (+ (* grid-cell scale) offset))
        (setf grid-cell-inc1 (+ (* grid-cell-inc1 scale) offset))
        (multf grid-cell grid-cell)
        (multf grid-cell-inc1 grid-cell-inc1)
        (let* ((x0y0-x1y0-x0y1-x1y1
                (*
                 (v4! (x grid-cell) (x grid-cell-inc1) (x grid-cell)
                      (x grid-cell-inc1))
                 (v! (s~ grid-cell :yy) (s~ grid-cell-inc1 :yy)))))
          (setf z0-hash
                (fract
                 (* x0y0-x1y0-x0y1-x1y1
                    (* (s~ grid-cell :zzzz) (/ 1.0 somelargefloat)))))
          (setf z1-hash
                (fract
                 (* x0y0-x1y0-x0y1-x1y1
                    (* (s~ grid-cell-inc1 :zzzz)
                       (/ 1.0 somelargefloat))))))))
    (values z0-hash z1-hash)))

(defun-g fast32-2-hash-4d-4 ((grid-cell :vec4))
  (let (((z0w0-hash :vec4))
        ((z1w0-hash :vec4))
        ((z0w1-hash :vec4))
        ((z1w1-hash :vec4)))
    (let* ((offset (v4! 16.84123 18.774548 16.873274 13.664607))
           (domain 69.0)
           (somelargefloat 47165.637)
           (scale (v4! 0.102007 0.114473 0.139651 0.08455)))
      (setf grid-cell
            (- grid-cell (* (floor (* grid-cell (/ 1.0 domain))) domain)))
      (let* ((grid-cell-inc1
              (* (step grid-cell (v4! (- domain 1.5))) (+ grid-cell (v4! 1.0)))))
        (setf grid-cell (+ (* grid-cell scale) offset))
        (setf grid-cell-inc1 (+ (* grid-cell-inc1 scale) offset))
        (multf grid-cell grid-cell)
        (multf grid-cell-inc1 grid-cell-inc1)
        (let* ((x0y0-x1y0-x0y1-x1y1
                (*
                 (v4! (x grid-cell) (x grid-cell-inc1) (x grid-cell)
                      (x grid-cell-inc1))
                 (v! (s~ grid-cell :yy) (s~ grid-cell-inc1 :yy))))
               (z0w0-z1w0-z0w1-z1w1
                (*
                 (v4! (z grid-cell) (z grid-cell-inc1) (z grid-cell)
                      (z grid-cell-inc1))
                 (* (v! (s~ grid-cell :ww) (s~ grid-cell-inc1 :ww))
                    (/ 1.0 somelargefloat)))))
          (setf z0w0-hash
                (fract
                 (* x0y0-x1y0-x0y1-x1y1 (s~ z0w0-z1w0-z0w1-z1w1 :xxxx))))
          (setf z1w0-hash
                (fract
                 (* x0y0-x1y0-x0y1-x1y1 (s~ z0w0-z1w0-z0w1-z1w1 :yyyy))))
          (setf z0w1-hash
                (fract
                 (* x0y0-x1y0-x0y1-x1y1 (s~ z0w0-z1w0-z0w1-z1w1 :zzzz))))
          (setf z1w1-hash
                (fract
                 (* x0y0-x1y0-x0y1-x1y1 (s~ z0w0-z1w0-z0w1-z1w1 :wwww)))))))
    (values z0w0-hash z1w0-hash z0w1-hash z1w1-hash)))

(defun-g fast32-2-hash-4d-16 ((grid-cell :vec4))
  (let (((z0w0-hash-0 :vec4))
        ((z0w0-hash-1 :vec4))
        ((z0w0-hash-2 :vec4))
        ((z0w0-hash-3 :vec4))
        ((z1w0-hash-0 :vec4))
        ((z1w0-hash-1 :vec4))
        ((z1w0-hash-2 :vec4))
        ((z1w0-hash-3 :vec4))
        ((z0w1-hash-0 :vec4))
        ((z0w1-hash-1 :vec4))
        ((z0w1-hash-2 :vec4))
        ((z0w1-hash-3 :vec4))
        ((z1w1-hash-0 :vec4))
        ((z1w1-hash-1 :vec4))
        ((z1w1-hash-2 :vec4))
        ((z1w1-hash-3 :vec4)))
    (let* ((offset (v4! 16.84123 18.774548 16.873274 13.664607))
           (domain 69.0)
           (some-large-floats (v4! 56974.746 47165.637 55049.668 49901.273))
           (scale (v4! 0.102007 0.114473 0.139651 0.08455)))
      (setf grid-cell
            (- grid-cell (* (floor (* grid-cell (/ 1.0 domain))) domain)))
      (let* ((grid-cell-inc1
              (* (step grid-cell (v4! (- domain 1.5))) (+ grid-cell (v4! 1.0)))))
        (setf grid-cell (+ (* grid-cell scale) offset))
        (setf grid-cell-inc1 (+ (* grid-cell-inc1 scale) offset))
        (multf grid-cell grid-cell)
        (multf grid-cell-inc1 grid-cell-inc1)
        (let* ((x0y0-x1y0-x0y1-x1y1
                (*
                 (v4! (x grid-cell) (x grid-cell-inc1) (x grid-cell)
                      (x grid-cell-inc1))
                 (v! (s~ grid-cell :yy) (s~ grid-cell-inc1 :yy))))
               (z0w0-z1w0-z0w1-z1w1
                (*
                 (v4! (z grid-cell) (z grid-cell-inc1) (z grid-cell)
                      (z grid-cell-inc1))
                 (v! (s~ grid-cell :ww) (s~ grid-cell-inc1 :ww))))
               (hashval (* x0y0-x1y0-x0y1-x1y1 (s~ z0w0-z1w0-z0w1-z1w1 :xxxx))))
          (setf z0w0-hash-0 (fract (* hashval (/ 1.0 (x some-large-floats)))))
          (setf z0w0-hash-1 (fract (* hashval (/ 1.0 (y some-large-floats)))))
          (setf z0w0-hash-2 (fract (* hashval (/ 1.0 (z some-large-floats)))))
          (setf z0w0-hash-3 (fract (* hashval (/ 1.0 (w some-large-floats)))))
          (setf hashval
                (* x0y0-x1y0-x0y1-x1y1 (s~ z0w0-z1w0-z0w1-z1w1 :yyyy)))
          (setf z1w0-hash-0 (fract (* hashval (/ 1.0 (x some-large-floats)))))
          (setf z1w0-hash-1 (fract (* hashval (/ 1.0 (y some-large-floats)))))
          (setf z1w0-hash-2
                (fract (* hashval (/ 1.0 (z some-large-floats)))))
          (setf z1w0-hash-3
                (fract (* hashval (/ 1.0 (w some-large-floats)))))
          (setf hashval
                (* x0y0-x1y0-x0y1-x1y1 (s~ z0w0-z1w0-z0w1-z1w1 :zzzz)))
          (setf z0w1-hash-0
                (fract (* hashval (/ 1.0 (x some-large-floats)))))
          (setf z0w1-hash-1
                (fract (* hashval (/ 1.0 (y some-large-floats)))))
          (setf z0w1-hash-2
                (fract (* hashval (/ 1.0 (z some-large-floats)))))
          (setf z0w1-hash-3
                (fract (* hashval (/ 1.0 (w some-large-floats)))))
          (setf hashval
                (* x0y0-x1y0-x0y1-x1y1 (s~ z0w0-z1w0-z0w1-z1w1 :wwww)))
          (setf z1w1-hash-0
                (fract (* hashval (/ 1.0 (x some-large-floats)))))
          (setf z1w1-hash-1
                (fract (* hashval (/ 1.0 (y some-large-floats)))))
          (setf z1w1-hash-2
                (fract (* hashval (/ 1.0 (z some-large-floats)))))
          (setf z1w1-hash-3
                (fract
                 (* hashval (/ 1.0 (w some-large-floats))))))))
    (values z0w0-hash-0 z0w0-hash-1 z0w0-hash-2 z0w0-hash-3 z1w0-hash-0
            z1w0-hash-1 z1w0-hash-2 z1w0-hash-3 z0w1-hash-0 z0w1-hash-1
            z0w1-hash-2 z0w1-hash-3 z1w1-hash-0 z1w1-hash-1 z1w1-hash-2
            z1w1-hash-3)))
