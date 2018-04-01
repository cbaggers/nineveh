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

(defun-g vmin ((vec :vec2))
  "Returns the `min` of the vectors components"
  (min (x vec) (y vec)))

(defun-g vmin ((vec :vec3))
  "Returns the `min` of the vectors components"
  (min (x vec) (y vec) (z vec)))

(defun-g vmin ((vec :vec4))
  "Returns the `min` of the vectors components"
  (min (x vec) (y vec) (z vec) (w vec)))

;;------------------------------------------------------------
