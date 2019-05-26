(in-package :nineveh)

(defmacro as-frame (&body body)
  `(progn
     (clear)
     (prog1 (progn ,@body)
       (swap))))

(defmacro def-simple-main-loop (name (&key on-start) &body body)
  `(define-simple-main-loop ,name (:on-start ,on-start) ,@body))

(defmacro define-simple-main-loop (name (&key on-start) &body body)
  (let ((frame-var-name (symb :* name :-frame-counter*))
        (step-func-name (symb :% name :-step-func*)))
    `(progn
       (defvar ,frame-var-name 0)
       (defun ,name (action &optional frames)
         (ecase action
           (:start
            (if (= ,frame-var-name 0)
                (progn
                  (setf ,frame-var-name (or frames -1))
                  (format t "~%- starting ~a -" ',name)
                  (unwind-protect
                       (progn
                         (when (cepl.lifecycle:uninitialized-p)
                           (cepl:repl))
                         (let ((on-start ,on-start))
                           (when on-start
                             (funcall on-start)))
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
                               (,step-func-name)))))
                    (unless (= ,frame-var-name 0)
                      (as-frame
                       (with-setf (clear-color) (v! 0 1 0 1)
                                  (cls))))
                    (setf ,frame-var-name 0)
                    (format t "~%~%- stopping ~a -~%" ',name)))
                (format t "~%~%- ~a is already running -~%" ',name)))
           (:stop
            (setf ,frame-var-name (max 0 (or frames 0))))))
       (defun ,step-func-name ()
         ,@body))))

(defun set-viewport-dimensions-to-same-as-window
    (&key (viewport (current-viewport))
       (window (cepl.context:current-surface
                (cepl.context:cepl-context)))
       (step-host nil))
  (when window
    (when step-host
      (cepl:step-host))
    (let ((win-dim (cepl.host:window-size window)))
      (setf (viewport-dimensions viewport) win-dim))))
