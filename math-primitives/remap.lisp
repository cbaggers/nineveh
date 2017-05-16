(in-package #:nineveh.math-primitives)

;;------------------------------------------------------------
;; Remap

(defun-g remap ((val :float)
                (original-min :float) (original-max :float)
                (new-min :float) (new-max :float))
  (+ new-min
     (* (- val original-min)
        (/ (- new-max new-min)
           (- original-max original-min)))))

(defun-g remap ((val :vec2)
                (original-min :vec2) (original-max :vec2)
                (new-min :vec2) (new-max :vec2))
  (+ new-min
     (* (- val original-min)
        (/ (- new-max new-min)
           (- original-max original-min)))))

(defun-g remap ((val :vec3)
                (original-min :vec3) (original-max :vec3)
                (new-min :vec3) (new-max :vec3))
  (+ new-min
     (* (- val original-min)
        (/ (- new-max new-min)
           (- original-max original-min)))))

(defun-g remap ((val :vec4)
                (original-min :vec4) (original-max :vec4)
                (new-min :vec4) (new-max :vec4))
  (+ new-min
     (* (- val original-min)
        (/ (- new-max new-min)
           (- original-max original-min)))))

;;------------------------------------------------------------
;; Remap Vec Ranges

(defun-g remap ((val :float) (original-range :vec2) (new-range :vec2))
  (+ (x new-range)
     (* (- val (x original-range))
        (/ (- (y new-range) (x new-range))
           (- (y original-range) (x original-range))))))

(defun-g remap ((val :vec2) (original-range :vec4) (new-range :vec4))
  (+ (s~ new-range :xz)
     (* (- val (s~ original-range :xz))
        (/ (- (s~ new-range :yw) (s~ new-range :xz))
           (- (s~ original-range :yw) (s~ original-range :xz))))))

(defun-g remap-uv ((uv :vec2) (range :vec4))
  (+ (* uv (- (s~ range :yw) (s~ range :xz)))
     (s~ range :xz)))
