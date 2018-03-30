(in-package :nineveh.graphing)

;;
;; This is a simple 3d graph that just uses instanced particles
;; as the points. Not the best for rapidly changing values but
;; could be handy for getting a general idea of what a function
;; is about.
;;

;; NOTE: Not working, this is where we want to get to, however
;;       we need baking to allow recompilation which it doesnt
;;       yet (as it turns the stages into lambda)

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

(defun-g pgraph-billboard-range-vert ((vert g-pt)
                                      &uniform
                                      (func (function (:float) :vec3))
                                      (min :float)
                                      (by :float)
                                      (projection :mat4)
                                      (point-size :float))
  (with-slots (position texture) vert
    (let* ((input (+ min (* by (float gl-instance-id))))
           (func-result (funcall func input))
           (world-pos (vec4 (+ (* position point-size)
                               func-result)
                            1.0)))
      (values
       (* projection world-pos)
       texture))))

(defun-g pgraph-dot-frag ((uv :vec2)
                          &uniform
                          (point-color :vec4))
  (let ((sdf-scale 5f0))
    (mix
     (v! 0 0 0 0)
     point-color
     (nineveh.sdf.2d:mask-fill
      (nineveh.sdf.2d:circle (* uv 2 sdf-scale) sdf-scale))))
  (v! 1 0 0 0))

(defpipeline-g pgraph-billboard-range ()
  :vertex (pgraph-billboard-range-vert g-pt)
  :fragment (pgraph-dot-frag :vec2))

;;------------------------------------------------------------

(defgeneric pgraph (pipeline position-vec3 direction-vec3 &key))

;; {TODO} when rtg-math has desctructive projection funcs store
;;        a mat4 in this struct
(defstruct (pgraph-base-pipeline (:constructor nil))
  (pipeline (error "BUG: pgraph-range with no inner pipeline")
            :type function)
  (quad (error "BUG: pgraph-range with no inner quad")
        :type buffer-stream))

(defmethod free ((obj pgraph-base-pipeline))
  ;; quad is owned by nineveh internals so we dont free it
  (free (pgraph-base-pipeline-pipeline obj)))

;;------------------------------------------------------------

(defstruct (pgraph-range-pipeline (:include pgraph-base-pipeline)
                                  (:constructor %pgraph-range)))

(defun make-pgraph-range-pipeline (gpu-function)
  (let ((quad (nineveh.internals:get-gpu-quad))
        (cpipeline (bake-uniforms 'pgraph-billboard-range
                                  :func gpu-function)))
    (%pgraph-range
     :pipeline cpipeline
     :quad quad)))

(defmethod pgraph ((pipeline pgraph-range-pipeline)
                   position-vec3
                   direction-vec3
                   &key (min 0f0) (max 100f0) (by 1f0)
                     (point-color (vec4 0.7 0.7 0.8 0.0))
                     (point-size 1f0))
  (let* ((cpipeline (pgraph-range-pipeline-pipeline pipeline))
         (min (float min 0f0))
         (max (float max 0f0))
         (by (float by 0f0))
         (point-size (float point-size 0f0))
         (count (floor (/ (- max min) by)))
         (vp (current-viewport))
         (proj (rtg-math.projection:perspective
                (float (viewport-resolution-x vp) 0f0)
                (float (viewport-resolution-y vp) 0f0)
                1f0
                1000f0
                45f0))
         (stream (pgraph-range-pipeline-quad pipeline)))
    (declare (type function cpipeline))
    (with-setf (depth-test-function) nil
      (with-blending *pgraph-blend-params*
        (with-instances count
          (map-g cpipeline stream
                 :projection proj
                 :min min
                 :by by
                 :point-color point-color
                 :point-size point-size))))))

;;------------------------------------------------------------

#+nil
(defun-g blep ((i :float))
  (let ((ang (* i 0.1)))
    (* (vec3 (sin ang) (cos ang) -5)
       (* i 0.001))))

#+nil
(defun-g glap ((i :float))
  (let ((ang (* i 0.1)))
    (* (vec2 (sin ang) (cos ang))
       (* i 0.001))))

;;
