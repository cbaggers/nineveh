(in-package :nineveh.graphing)

;;
;; This is a simple 3d graph that just uses instanced particles
;; as the points. Not the best for rapidly changing values but
;; could be handy for getting a general idea of what a function
;; is about.
;;

;;------------------------------------------------------------
;;

(defvar *pgraph-blend-params*
  (make-blending-params
   :mode-rgb :func-add
   :mode-alpha :func-add
   :source-rgb :one
   :destination-rgb :one
   :source-alpha :one
   :destination-alpha :one))

;;------------------------------------------------------------

(defun-g pgraph-dot-frag ((uv :vec2)
                          &uniform
                          (point-color :vec4))
  (let ((sdf-scale 25f0))
    (mix
     (v! 0 0 0 0)
     point-color
     (nineveh.sdf.2d:mask-fill
      (nineveh.sdf.2d:circle (- (* uv 2 sdf-scale) (vec2 sdf-scale))
                             sdf-scale)))))

(defpipeline-g pgraph-billboard-range ()
  :vertex (pgraph-billboard-range-vert g-pt)
  :fragment (pgraph-dot-frag :vec2))

(defun-g vert-transform ((fn (function (:float) :vec3))
                         (vert g-pt)
                         (min :float)
                         (by :float)
                         (world->view :mat4)
                         (proj :mat4)
                         (point-size :float))
  (with-slots (position texture) vert
    (let* ((input (+ min (* by (float gl-instance-id))))
           (func-result (funcall fn input))
           (world-pos (vec4 func-result 1.0))
           (view-pos (+ (* world->view world-pos)
                        (vec4 (* position point-size) 0)))
           (clip-pos (* proj view-pos)))
      (values
       clip-pos
       texture))))

;;------------------------------------------------------------

(defmacro define-pgraph (name
                         (kind &rest options &key &allow-other-keys) (arg-name)
                         &body body)
  (declare (ignore kind options))
  (assert (and (symbolp arg-name) (not (keywordp arg-name))))
  (let ((vert-name (intern (format nil "%~a-VERT" name) *package*))
        (pline-name (intern (format nil "%~a-PIPELINE" name) *package*)))
    `(progn
       (defun-g ,vert-name ((vert g-pt)
                            &uniform
                            (min :float)
                            (by :float)
                            (world->view :mat4)
                            (proj :mat4)
                            (point-size :float))
         (flet ((graph-func ((,arg-name :float))
                  (the :vec3 (progn ,@body))))
           (vert-transform #'graph-func
                           vert
                           min
                           by
                           world->view
                           proj
                           point-size)))
       (defpipeline-g ,pline-name ()
         :vertex (,vert-name g-pt)
         :fragment (pgraph-dot-frag :vec2))
       (defun ,name (position-vec3
                     direction-vec3
                     &key (min 0f0) (max 100f0) (by 1f0)
                       (point-color (vec4 0.7 0.7 0.8 0.0))
                       (point-size 1f0))
         (let* ((min (float min 0f0))
                (max (float max 0f0))
                (by (float by 0f0))
                (point-size (float point-size 0f0))
                (count (floor (/ (- max min) by)))
                (vp (current-viewport))
                (world->view
                 (m4:* (q:to-mat4 (q:inverse (q:from-direction (v! 0 1 0) direction-vec3)))
                       (m4:translation (v3:negate position-vec3))))
                (proj (rtg-math.projection:perspective
                       (float (viewport-resolution-x vp) 0f0)
                       (float (viewport-resolution-y vp) 0f0)
                       1f0
                       5000f0
                       45f0)))
           (with-setf (depth-test-function) nil
             (with-blending *pgraph-blend-params*
               (with-instances count
                 (map-g #',pline-name (nineveh.internals:get-gpu-quad)
                        :world->view world->view
                        :proj proj
                        :min min
                        :by by
                        :point-color point-color
                        :point-size point-size)))))))))

;;------------------------------------------------------------

#+nul
(define-pgraph woop (:range) (i)
  (let* ((ang (* i 0.1))
         (disp (* (vec2 (sin ang) (cos ang))
                  (* i 0.01))))
    (vec3 (x disp) (* 20 (sin i)) (y disp))))
