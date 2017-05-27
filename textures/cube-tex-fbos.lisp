(in-package :nineveh.textures)

(defun cube-texture-p (texture)
  (and (texture-p texture)
       (eq (texture-type texture) :texture-cube-map)))

(defun cube-faces (cube-texture &optional (level-num 0))
  (assert (cube-texture-p cube-texture))
  (loop :for face :below 6 :collect
     (texref cube-texture :mipmap-level level-num :cube-face face)))

(defun make-fbos-for-each-mipmap-of-cube-texture (cube-texture
                                                  &key (with-depth t))
  (loop :for level :below (texture-mipmap-levels cube-texture) :collect
     (let* ((arrays (cube-faces cube-texture level))
            (color-specs (loop :for x :in arrays :for i :from 0 :collect
                            (list i x)))
            (spec (append color-specs
                          (when with-depth
                            `((:d :dimensions ,(dimensions (first arrays))))))))

       (apply #'make-fbo spec))))
