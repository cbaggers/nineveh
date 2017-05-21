(in-package :nineveh.mesh.data.primitives)

(defun latice-c-arrays (&key (width 1.0) (height 1.0) (x-segments 30)
                          (y-segments 30) (normals t) (tex-coords t))
  (dbind (data-ptr indices-ptr) (latice-foreign
                                 :width width :height height
                                 :x-segments x-segments :y-segments y-segments
                                 :normals normals :tex-coords tex-coords)
    (let ((elem-type (cond
                       ((and normals tex-coords) 'g-pnt)
                       (tex-coords 'g-pt)
                       (normals 'g-pn)))
          (vert-width (1+ x-segments))
          (vert-height (1+ y-segments)))
      (list (make-c-array-from-pointer
             (* vert-width vert-height) elem-type data-ptr)
            (make-c-array-from-pointer
             (* x-segments y-segments 6) :uint indices-ptr)))))

(defun latice-gpu-arrays (&key (width 1.0) (height 1.0) (x-segments 30)
                            (y-segments 30) (normals t) (tex-coords t))
  (dbind (data-c-array indices-c-array)
      (latice-c-arrays :width width :height height
                       :x-segments x-segments :y-segments y-segments
                       :normals normals :tex-coords tex-coords)
    (prog1 (list (make-gpu-array data-c-array)
                 (make-gpu-array indices-c-array))
      (free data-c-array)
      (free indices-c-array))))
