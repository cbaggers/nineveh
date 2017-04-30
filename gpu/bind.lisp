(in-package :nineveh)

(defmacro-g bind-vec (vars vec &body body)
  (labels ((get-func (i var)
             (let* ((var (if (listp var) var (list var)))
                    (f (second var))
                    (var (first var)))
               `(,var ,(if f
                           (ecase f (:x 'x) (:y 'y) (:z 'z) (:w 'w))
                           (elt '(x y z w) i))))))
    (let ((v (gensym "vec")))
      `(let* ((,v ,vec)
              ,@(loop :for p :in vars :for i :from 0
                   :for (c f) := (get-func i p)
                   :collect `(,c (,f ,v))))
         ,@body))))
