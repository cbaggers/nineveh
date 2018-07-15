(in-package :nineveh.textures)

(defun-g dirty-blit-v ((vert :vec2))
  (values
   (v! vert 0 1)
   (+ (* vert 0.5) 0.5)))

(defun-g dirty-blit-f ((uv :vec2) &uniform (sam :sampler-2d))
  (texture sam uv ))

(defpipeline-g dirty-blit ()
  (dirty-blit-v :vec2)
  (dirty-blit-f :vec2))

(defun dirty-blit-sampler (sampler)
  (map-g #'dirty-blit (nineveh:get-quad-stream-v2)
         :sam sampler))
