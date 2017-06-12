(in-package :nineveh)

;;------------------------------------------------------------

(defgeneric draw-tex (tex &key scale flip-uvs-vertically))
(defgeneric draw-tex-tl (sampler &key flip-uvs-vertically))
(defgeneric draw-tex-bl (sampler &key flip-uvs-vertically))
(defgeneric draw-tex-tr (sampler &key flip-uvs-vertically))
(defgeneric draw-tex-br (sampler &key flip-uvs-vertically))
(defgeneric draw-tex-at (sampler &key pos centered flip-uvs-vertically))

;;------------------------------------------------------------

(defun make-gpu-quad ()
  (make-buffer-stream
   (make-gpu-array
    (list (list (v! -0.5   0.5 0 0) (v!  0.0   1.0))
          (list (v! -0.5  -0.5 0 0) (v!  0.0   0.0))
          (list (v!  0.5  -0.5 0 0) (v!  1.0   0.0))
          (list (v! -0.5   0.5 0 0) (v!  0.0   1.0))
          (list (v!  0.5  -0.5 0 0) (v!  1.0   0.0))
          (list (v!  0.5   0.5 0 0) (v!  1.0   1.0)))
    :element-type 'g-pt
    :dimensions 6)
   :retain-arrays t))

(defvar *quad* nil)

(defun get-gpu-quad ()
  (or *quad* (setf *quad* (make-gpu-quad))))

;;------------------------------------------------------------

(defun-g draw-texture-vert ((vert g-pt) &uniform (transform :mat4)
                            (uv-y-mult :float))
  (values (* transform (v! (pos vert) 1s0))
          (* (tex vert) (v! 1 uv-y-mult))))

(defun-g draw-texture-frag ((tc :vec2) &uniform (tex :sampler-2d))
  (texture tex tc))

(def-g-> draw-texture-pipeline ()
  #'(draw-texture-vert g-pt) #'(draw-texture-frag :vec2))

;;------------------------------------------------------------

(defun-g draw-cube-face-vert ((vert g-pt) &uniform (transform :mat4)
                              (uv-mult :vec2))
  (values (* transform (v! (pos vert) 1s0))
          (* (s~ (pos vert) :xy) uv-mult 2)))

(defun-g draw-cube-face-frag ((tc :vec2) &uniform (tex :sampler-cube)
                              (mat :mat3))
  (texture tex (* mat (v! tc -1))))

(def-g-> draw-cube-face-pipeline ()
  #'(draw-cube-face-vert g-pt) #'(draw-cube-face-frag :vec2))

;;------------------------------------------------------------

(defun %draw-cube-face (sampler pos-vec2 rotation scale)
  (let* ((tex (sampler-texture sampler))
         (tex-res (resolution tex))
         (win-res (resolution (current-viewport)))
         (rect-res (rotated-rect-size (v2:* tex-res (v! 3 4)) rotation))
         (fit-scale (* (get-fit-to-rect-scale win-res rect-res) 2)))
    (labels ((calc-trans (pos)
               (m4:*
                (m4:translation (v! pos-vec2 0))
                (m4:scale (v3:*s (v! (/ 1 (x win-res))
                                     (/ 1 (y win-res))
                                     0)
                                 (* scale fit-scale)))
                (m4:rotation-z rotation)
                (m4:scale (v! tex-res 0))
                (m4:translation pos))))
      (map-g #'draw-cube-face-pipeline (get-gpu-quad)
             :mat (m3:rotation-x (radians 90))
             :tex sampler
             :uv-mult (v! 1 -1)
             :transform (calc-trans (v! 0 1.5 0)))
      (map-g #'draw-cube-face-pipeline (get-gpu-quad)
             :mat (m3:rotation-y (radians 90))
             :tex sampler
             :uv-mult (v! -1 1)
             :transform (calc-trans (v! -1 0.5 0)))
      (map-g #'draw-cube-face-pipeline (get-gpu-quad)
             :mat (m3:rotation-y (radians -90))
             :tex sampler
             :uv-mult (v! -1 1)
             :transform (calc-trans (v! 1 0.5 0)))
      (map-g #'draw-cube-face-pipeline (get-gpu-quad)
             :mat (m3:rotation-x (radians 180))
             :tex sampler
             :uv-mult (v! 1 -1)
             :transform (calc-trans (v! 0 0.5 0)))
      (map-g #'draw-cube-face-pipeline (get-gpu-quad)
             :mat (m3:rotation-x (radians -90))
             :tex sampler
             :uv-mult (v! 1 -1)
             :transform (calc-trans (v! 0 -0.5 0)))
      (map-g #'draw-cube-face-pipeline (get-gpu-quad)
             :mat (m3:rotation-x (radians 0))
             :tex sampler
             :uv-mult (v! 1 -1)
             :transform (calc-trans (v! 0 -1.5 0))))))

;;------------------------------------------------------------

(defun %draw-sampler (sampler pos-vec2 rotation scale flip-uvs-vertically)
  (let* ((tex (sampler-texture sampler))
         (tex-res (resolution tex))
         (win-res (resolution (current-viewport)))
         (rect-res (rotated-rect-size tex-res rotation))
         (fit-scale (* (get-fit-to-rect-scale win-res rect-res) 2))
         (transform
          (m4:*
           (m4:translation (v! pos-vec2 0))
           (m4:scale (v3:*s (v! (/ 1 (x win-res))
                                (/ 1 (y win-res))
                                0)
                            (float scale)))
           (m4:rotation-z rotation)
           (m4:scale (v3:*s (v! tex-res 0) fit-scale)))))
    (map-g #'draw-texture-pipeline (get-gpu-quad)
           :tex sampler
           :transform transform
           :uv-y-mult (if flip-uvs-vertically -1s0 1s0)))) ;; this is a bug as it samples outside of texture

(defun rotated-rect-size (size-v2 φ)
  ;; returns width & height
  (let ((w (x size-v2))
        (h (y size-v2)))
    (v! (+ (* w (abs (cos φ)))
           (* h (abs (sin φ))))
        (+ (* w (abs (sin φ)))
           (* h (abs (cos φ)))))))

(defun get-fit-to-rect-scale (target-v2 to-fit-v2)
  (min (/ (x target-v2) (x to-fit-v2))
       (/ (y target-v2) (y to-fit-v2))))

;;------------------------------------------------------------

(defmethod draw-tex ((tex texture)
                     &key (scale 0.9) (flip-uvs-vertically nil))
  (with-sampling (s tex)
    (draw-tex s :scale scale :flip-uvs-vertically flip-uvs-vertically)))

(defmethod draw-tex ((sampler sampler)
                     &key (scale 0.9) (flip-uvs-vertically nil))
  (cepl-utils:with-setf (depth-test-function *cepl-context*) nil
    (if (eq (sampler-type sampler) :sampler-cube)
        (%draw-cube-face sampler (v! -0 0) 1.5707 scale)
        (%draw-sampler sampler (v! 0 0) 0s0 scale flip-uvs-vertically))))

;;------------------------------------------------------------

(defun draw-tex-top-left (sampler/tex &key (flip-uvs-vertically nil))
  (draw-tex-tl sampler/tex flip-uvs-vertically))

(defmethod draw-tex-tl ((sampler sampler) &key (flip-uvs-vertically nil))
  (if (eq (sampler-type sampler) :sampler-cube)
      (%draw-cube-face sampler (v! -0.5 0.5) 1.5707 0.5)
      (%draw-sampler sampler (v! -0.5 0.5) 0s0 0.5 flip-uvs-vertically)))

;;------------------------------------------------------------

(defun draw-tex-bottom-left (sampler/tex &key (flip-uvs-vertically nil))
  (draw-tex-bl sampler/tex :flip-uvs-vertically flip-uvs-vertically))

(defmethod draw-tex-bl ((sampler sampler) &key (flip-uvs-vertically nil))
  (if (eq (sampler-type sampler) :sampler-cube)
      (%draw-cube-face sampler (v! -0.5 -0.5) 1.5707 0.5)
      (%draw-sampler sampler (v! -0.5 -0.5) 0s0 0.5 flip-uvs-vertically)))

;;------------------------------------------------------------

(defun draw-tex-top-right (sampler/tex &key (flip-uvs-vertically nil))
  (draw-tex-tr sampler/tex :flip-uvs-vertically flip-uvs-vertically))

(defmethod draw-tex-tr ((sampler sampler) &key (flip-uvs-vertically nil))
  (if (eq (sampler-type sampler) :sampler-cube)
      (%draw-cube-face sampler (v! 0.5 0.5) 1.5707 0.5)
      (%draw-sampler sampler (v! 0.5 0.5) 0s0 0.5 flip-uvs-vertically)))

;;------------------------------------------------------------

(defun draw-tex-bottom-right (sampler/tex &key (flip-uvs-vertically nil))
  (draw-tex-br sampler/tex :flip-uvs-vertically flip-uvs-vertically))

(defmethod draw-tex-br ((sampler sampler) &key (flip-uvs-vertically nil))
  (if (eq (sampler-type sampler) :sampler-cube)
      (%draw-cube-face sampler (v! 0.5 -0.5) 1.5707 0.5)
      (%draw-sampler sampler (v! 0.5 -0.5) 0s0 0.5 flip-uvs-vertically)))

;;------------------------------------------------------------

(defun-g draw-texture-at-vert ((vert g-pt) &uniform (pos :vec2)
                               (size :vec2)
                               (viewport-size :vec2)
                               (uv-flip :bool))
  (let* ((vpos (* (s~ (pos vert) :xy) 2))
         (scaled-pos (/ (* vpos size) viewport-size))
         (final (v! (+ pos scaled-pos) 0f0 1f0))
         (uv-y (if uv-flip
                   (- 1 (y (tex vert)))
                   (y (tex vert)))))
    (values final (v! (x (tex vert)) uv-y))))

(defun-g draw-texture-at-frag ((tc :vec2) &uniform (tex :sampler-2d))
  (texture tex tc))

(def-g-> draw-texture-at-pipeline ()
  #'(draw-texture-at-vert g-pt)
  #'(draw-texture-at-frag :vec2))

(defmethod draw-tex-at ((sampler sampler)
                        &key
                          (pos (v! 0 0))
                          (centered t)
                          (flip-uvs-vertically nil))
  (let* ((size (v! (texture-base-dimensions (sampler-texture sampler))))
         (vp-size (viewport-resolution (current-viewport)))
         (ratio (v2:/ size vp-size))
         (pos (if centered
                  pos
                  (v2:+ pos (v! (x ratio) 0)))))
    (map-g #'draw-texture-at-pipeline (get-gpu-quad)
           :tex sampler
           :size size
           :pos pos
           :viewport-size vp-size
           :uv-flip (if flip-uvs-vertically 1 0))))
