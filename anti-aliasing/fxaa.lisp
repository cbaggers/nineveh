(in-package :nineveh)

(defun make-fxaa-quad ()
  (make-buffer-stream
   (make-gpu-array (list (v! -1.0   1.0)
                         (v! -1.0  -1.0)
                         (v!  1.0  -1.0)
                         (v! -1.0   1.0)
                         (v!  1.0  -1.0)
                         (v!  1.0   1.0))
                   :element-type :vec2
                   :dimensions 6)
   :retain-arrays t))

(defvar *fxaa-quad* nil)

(defun get-fxaa-quad ()
  (or *fxaa-quad*
      (setf *fxaa-quad* (make-fxaa-quad))))

(defun-g fxxa-vert ((vert-pos :vec2))
  (let ((tex-coord (+ (* vert-pos 0.5) (v2! 0.5))))
    (values (v! vert-pos 0f0 1f0)
            tex-coord)))

(defun-g test-frag ((tc :vec2) &uniform (src-tex :sampler-2d))
  (texture src-tex tc))

(def-glsl-stage fxaa-frag (("tex_coord" :vec2)
                           &uniform ("src_tex" :sampler-2d)
                           ("resolution_x" :float)
                           ("resolution_y" :float)
                           &context :330 :fragment)
  (:file :nineveh "anti-aliasing/fxaa-dm.glsl")
  (("color_out" :vec4)))

(def-g-> fxaa ()
  (fxxa-vert :vec2)
  (fxaa-frag :vec2))

(def-g-> fookit ()
  (fxxa-vert :vec2)
  (test-frag :vec2))

(defun apply-fxaa (sampler)
  (let ((tex (sampler-texture sampler)))
    (destructuring-bind (w h) (texture-base-dimensions tex)
      (print (list :size w h))
      (map-g #'fxaa (get-fxaa-quad)
             :resolution_x w
             :resolution_y h
             :src_tex sampler))))
