(in-package :nineveh.math-primitives)

(defun-g radical-inverse-vdc ((bits :uint))
  "Given a uint (e.g 5) it takes the binary representation of the
  number (0101.0) and mirrors it the decimal (0.1010) and
  returns it as a float (0.625)

  vdc stand for Van Der Corput. For more details see:
  http://holger.dammertz.org/stuff/notes_HammersleyOnHemisphere.html"
  (let* ((bits (logior (<< bits (uint 16)) (>> bits (uint 16))))
         (bits (logior (<< (logand bits (uint #x55555555)) (uint 1))
                       (>> (logand bits (uint #xAAAAAAAA)) (uint 1))))
         (bits (logior (<< (logand bits (uint #x33333333)) (uint 2))
                       (>> (logand bits (uint #xCCCCCCCC)) (uint 2))))
         (bits (logior (<< (logand bits (uint #x0F0F0F0F)) (uint 4))
                       (>> (logand bits (uint #xF0F0F0F0)) (uint 4))))
         (bits (logior (<< (logand bits (uint #x00FF00FF)) (uint 8))
                       (>> (logand bits (uint #xFF00FF00)) (uint 8)))))
    ;;                              ↓ 0x100000000 as a float ↓
    (* (float bits) (glsl-expr "2.3283064365386963e-10" :float))))
