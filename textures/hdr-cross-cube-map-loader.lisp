(in-package :nineveh.textures)

(defun load-hdr-cross-texture (filepath)
  (with-c-arrays (c-arrays (load-hdr-cross-image filepath))
    (make-texture c-arrays :element-type :rgb16f :cubes t)))

(defun load-hdr-cross-image (filepath)
  (destructuring-bind (ptr width height components-per-pixel)
      (stbi:loadf filepath)
    (unwind-protect
         (%load-hdr-cross-image ptr width height components-per-pixel)
      (cffi:foreign-free ptr))))

(defun %load-hdr-cross-image (ptr width height components-per-pixel)
  (let* ((face-width (/ width 3))
         (face-height (/ height 4))
         (dim (list face-width face-height))
         (line-float-width (* width components-per-pixel))
         (offsets `((,face-width 0)
                    (0 ,face-height)
                    (,face-width ,face-height)
                    (,(* face-width 2) ,face-height)
                    (,face-width ,(* face-height 2))
                    (,face-width ,(* face-height 3)))))
    (assert (and (= (mod width 3) 0)
                 (= (mod height 4) 0)
                 (= face-width face-height)
                 (= components-per-pixel 3)))
    (let ((arrays
           (loop :for f :below 6 :collect
              (let ((arr
                     (make-c-array nil :dimensions dim :element-type :vec3))
                    (offset (elt offsets f)))
                (destructuring-bind (x-offset y-offset) offset
                  (loop :for y :below face-height :do
                     (loop :for x :below face-width :do
                        (let* ((line-offset (* line-float-width (+ y y-offset)))
                               (face-offset (+ line-offset
                                               (* x-offset components-per-pixel)))
                               (index (+ face-offset (* x components-per-pixel))))
                          (if (= f 5)
                              (setf (aref-c arr
                                            (- face-width (1+ x))
                                            (- face-height (1+ y)))
                                    (v! (cffi:mem-aref ptr :float index)
                                        (cffi:mem-aref ptr :float (+ index 1))
                                        (cffi:mem-aref ptr :float (+ index 2))))
                              (setf (aref-c arr x y)
                                    (v! (cffi:mem-aref ptr :float index)
                                        (cffi:mem-aref ptr :float (+ index 1))
                                        (cffi:mem-aref ptr :float (+ index 2)))))))))
                arr))))
      ;; 0        GL_TEXTURE_CUBE_MAP_POSITIVE_X
      ;; 1        GL_TEXTURE_CUBE_MAP_NEGATIVE_X
      ;; 2        GL_TEXTURE_CUBE_MAP_POSITIVE_Y
      ;; 3        GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
      ;; 4        GL_TEXTURE_CUBE_MAP_POSITIVE_Z
      ;; 5        GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
      (mapcar (lambda (x) (elt arrays x))
              '(3 1 0 4 2 5)))))


(defun load-hdr-2d (filepath)
  (destructuring-bind (ptr width height components-per-pixel)
      (stbi:loadf filepath)
    (assert (and (= components-per-pixel 3)))
    (with-c-array (c-array (make-c-array-from-pointer
                            (list width height) :vec3 ptr))
      (make-texture c-array :element-type :rgb16f))))
