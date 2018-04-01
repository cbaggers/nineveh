(in-package :nineveh.math-primitives)

;;------------------------------------------------------------

(defun-g vmax ((vec :vec2))
  "Returns the `max` of the vectors components"
  (max (x vec) (y vec)))

(defun-g vmax ((vec :vec3))
  "Returns the `max` of the vectors components"
  (max (x vec) (y vec) (z vec)))

(defun-g vmax ((vec :vec4))
  "Returns the `max` of the vectors components"
  (max (x vec) (y vec) (z vec) (w vec)))

;;------------------------------------------------------------
