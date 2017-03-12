(in-package :nineveh)

(defmacro as-frame (&body body)
  `(progn
     (clear)
     (prog1 (progn ,@body)
       (swap))))

(defmacro def-simple-main-loop (name (&key on-start) &body body)
  (let ((frame-var-name (symb :* name :-frame-counter*))
        (step-func-name (symb :% name :-step-func*)))
    `(progn
       ,(when (or (not (boundp frame-var-name))
                  (= (or (symbol-value frame-var-name) -1) -1))
              `(progn
                 (defparameter ,frame-var-name -1)
                 (defun ,name (action &optional frames)
                   (ecase action
                     (:start
                      (setf ,frame-var-name (or frames -1))
                      (format t "~%- starting ~a -" ',name)
                      (unless *gl-context*
                        (cepl:repl))
                      (let ((on-start ,on-start))
                        (when on-start
                          (funcall on-start)))
                      (unwind-protect
                           (loop :until (= ,frame-var-name 0) :do
                              (progn
                                (decf ,frame-var-name 1)
                                ;; update swank
                                (livesupport:continuable
                                  (livesupport:update-repl-link))
                                ;; update event system
                                (livesupport:continuable
                                  (cepl:step-host))
                                ;; update temporal pool
                                ,(when (find-package :temporal-functions)
                                       `(livesupport:continuable
                                          (,(intern "UPDATE" :ttm))))
                                ;; run step function
                                (livesupport:continuable
                                  (,step-func-name))))
                        (setf ,frame-var-name -1)
                        (format t "~%- stopping ~a -" ',name)))
                     (:stop
                      (setf ,frame-var-name (max 0 (or frames 0))))))))
       (defun ,step-func-name ()
          ,@body))))
