(in-package :nineveh.mesh.data.primitives)

;;------------------------------------------------------------

(defmacro %c-array-internals ((index-type normals-p tex-coords-p)
                              &body call)
  (assert (= (length call) 1))
  (alexandria:with-gensyms (indx-type norms-p uvs-p)
    `(dbind (data-ptr data-len indices-ptr index-len) ,(first call)
       (let* ((,uvs-p ,tex-coords-p)
              (,norms-p ,normals-p)
              (elem-type (cond
                           ((and ,norms-p ,uvs-p) 'g-pnt)
                           (,uvs-p 'g-pt)
                           (,norms-p 'g-pn)))
              (,indx-type ,index-type))
         (list (make-c-array-from-pointer
                data-len elem-type data-ptr)
               (make-c-array-from-pointer
                index-len ,indx-type indices-ptr))))))

(defmacro %gpu-array-internals (call)
  `(dbind (data-c-array indices-c-array) ,call
     (prog1 (list (make-gpu-array data-c-array)
                  (make-gpu-array indices-c-array))
       (free data-c-array)
       (free indices-c-array))))

;;------------------------------------------------------------

(defun latice-c-arrays (&key (width 1.0) (height 1.0) (x-segments 30)
                          (y-segments 30) (normals t) (tex-coords t))
  (%c-array-internals (:uint normals tex-coords)
    (latice-foreign
     :width width :height height
     :x-segments x-segments :y-segments y-segments
     :normals normals :tex-coords tex-coords)))

(defun latice-gpu-arrays (&key (width 1.0) (height 1.0) (x-segments 30)
                            (y-segments 30) (normals t) (tex-coords t))
  (%gpu-array-internals
   (latice-c-arrays :width width :height height
                    :x-segments x-segments :y-segments y-segments
                    :normals normals :tex-coords tex-coords)))

;;------------------------------------------------------------

(defun box-c-arrays (&key (width 1.0) (height 1.0) (depth 1.0)
                       (normals t) (tex-coords t))
  (%c-array-internals (:ushort normals tex-coords)
    (box-foreign
     :width width :height height :depth depth
     :normals normals :tex-coords tex-coords)))

(defun box-gpu-arrays (&key (width 1.0) (height 1.0) (depth 1.0)
                         (normals t) (tex-coords t))
  (%gpu-array-internals
   (box-c-arrays
    :width width :height height :depth depth
    :normals normals :tex-coords tex-coords)))

;;------------------------------------------------------------

(defun cone-c-arrays (&key (segments 10) (height 1) (radius 0.5f0)
                        (normals t) (tex-coords t) (cap t))
  (%c-array-internals (:ushort normals tex-coords)
    (cone-foreign :segments segments :height height :radius radius
                  :normals normals :tex-coords tex-coords :cap cap)))

(defun cone-gpu-arrays (&key (segments 10) (height 1) (radius 0.5f0)
                          (normals t) (tex-coords t) (cap t))
  (%gpu-array-internals
   (cone-c-arrays :segments segments :height height :radius radius
                  :normals normals :tex-coords tex-coords :cap cap)))

;;------------------------------------------------------------

(defun cylinder-c-arrays (&key (segments 10) (height 1) (radius 0.5f0)
                            (normals t) (tex-coords t) (cap t))
  (%c-array-internals (:ushort normals tex-coords)
    (cylinder-foreign :segments segments :height height :radius radius
                      :normals normals :tex-coords tex-coords :cap cap)))

(defun cylinder-gpu-arrays (&key (segments 10) (height 1) (radius 0.5f0)
                              (normals t) (tex-coords t) (cap t))
  (%gpu-array-internals
   (cylinder-c-arrays :segments segments :height height :radius radius
                      :normals normals :tex-coords tex-coords :cap cap)))

;;------------------------------------------------------------

(defun plain-gpu-arrays (&key (width 1.0) (height 1.0) (normals t)
                           (tex-coords t))
  (latice-gpu-arrays :width width :height height
                     :x-segments 1 :y-segments 1
                     :normals normals :tex-coords tex-coords))

(defun plain-c-arrays (&key (width 1.0) (height 1.0) (normals t)
                         (tex-coords t))
  (latice-c-arrays :width width :height height
                   :x-segments 1 :y-segments 1
                   :normals normals :tex-coords tex-coords))

;;------------------------------------------------------------

(defun cube-gpu-arrays (&key (size 1.0) (normals t) (tex-coords t))
  (box-gpu-arrays :width size :height size :depth size :normals normals
                  :tex-coords tex-coords))


(defun cube-c-arrays (&key (size 1.0) (normals t) (tex-coords t))
  (box-c-arrays :width size :height size :depth size :normals normals
                :tex-coords tex-coords))

;;------------------------------------------------------------
