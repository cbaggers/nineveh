(in-package :nineveh)

(defun set-viewport-dimensions-to-same-as-window
    (&key (viewport (current-viewport))
       (window (cepl.context:current-surface
                cepl.context:*cepl-context*))
       (step-host nil))
  (when window
    (when step-host
      (cepl:step-host))
    (let ((win-dim (cepl.host:window-size window)))
      (setf (viewport-dimensions viewport) win-dim))))
