(in-package #:nineveh.anti-aliasing)

(defun-g fxaa2 ((uv :vec4) (tex :sampler-2d) (one-over-resolution :vec2))
  (let* ((fxaa-span-max 8.0)
         (fxaa-reduce-mul (/ 1.0 fxaa-span-max))
         (fxaa-reduce-min (/ 1.0 128.0))
         (fxaa-subpix-shift (/ 1.0 4.0))
         (rgb-nw
          (rtg-math.vectors:s~
           (texture-lod tex (rtg-math.vectors:s~ uv :zw) 0.0) :xyz))
         (rgb-ne
          (rtg-math.vectors:s~
           (texture-lod tex
                        (+ (rtg-math.vectors:s~ uv :zw)
                           (* (rtg-math.base-vectors:v2! 1 0)
                              (rtg-math.vectors:s~ one-over-resolution :xy)))
                        0.0)
           :xyz))
         (rgb-sw
          (rtg-math.vectors:s~
           (texture-lod tex
                        (+ (rtg-math.vectors:s~ uv :zw)
                           (* (rtg-math.base-vectors:v2! 0 1)
                              (rtg-math.vectors:s~ one-over-resolution :xy)))
                        0.0)
           :xyz))
         (rgb-se
          (rtg-math.vectors:s~
           (texture-lod tex
                        (+ (rtg-math.vectors:s~ uv :zw)
                           (* (rtg-math.base-vectors:v2! 1 1)
                              (rtg-math.vectors:s~ one-over-resolution :xy)))
                        0.0)
           :xyz))
         (rgb-m
          (rtg-math.vectors:s~
           (texture-lod tex (rtg-math.vectors:s~ uv :xy) 0.0) :xyz))
         (luma (rtg-math.base-vectors:v3! 0.29900002 0.587 0.11400001))
         (luma-nw (dot rgb-nw luma))
         (luma-ne (dot rgb-ne luma))
         (luma-sw (dot rgb-sw luma))
         (luma-se (dot rgb-se luma))
         (luma-m (dot rgb-m luma))
         (luma-min
          (min luma-m (min (min luma-nw luma-ne) (min luma-sw luma-se))))
         (luma-max
          (max luma-m (max (max luma-nw luma-ne) (max luma-sw luma-se))))
         (dir (vec2 (- (- (+ luma-nw luma-ne) (+ luma-sw luma-se)))
                    (- (+ luma-nw luma-sw) (+ luma-ne luma-se)))))
    (let* ((dir-reduce
            (max
             (* (+ luma-nw (+ luma-ne (+ luma-sw luma-se)))
                (* 0.25 fxaa-reduce-mul))
             fxaa-reduce-min))
           (rcp-dir-min
            (/ 1.0 (+ (min (abs (x dir)) (abs (y dir))) dir-reduce))))
      (setf dir
            (*
             (min (rtg-math.base-vectors:v2! fxaa-span-max fxaa-span-max)
                  (max
                   (rtg-math.base-vectors:v2! (- fxaa-span-max)
                                              (- fxaa-span-max))
                   (* dir rcp-dir-min)))
             (rtg-math.vectors:s~ one-over-resolution :xy)))
      (let* ((rgb-a
              (* (/ 1.0 2.0)
                 (+
                  (rtg-math.vectors:s~
                   (texture-lod tex
                                (+ (rtg-math.vectors:s~ uv :xy) (* dir (- (/ 1.0 3.0) 0.5)))
                                0.0)
                   :xyz)
                  (rtg-math.vectors:s~
                   (texture-lod tex
                                (+ (rtg-math.vectors:s~ uv :xy) (* dir (- (/ 2.0 3.0) 0.5)))
                                0.0)
                   :xyz))))
             (rgb-b
              (+ (* rgb-a (/ 1.0 2.0))
                 (* (/ 1.0 4.0)
                    (+
                     (rtg-math.vectors:s~
                      (texture-lod tex
                                   (+ (rtg-math.vectors:s~ uv :xy)
                                      (* dir (- (/ 0.0 3.0) 0.5)))
                                   0.0)
                      :xyz)
                     (rtg-math.vectors:s~
                      (texture-lod tex
                                   (+ (rtg-math.vectors:s~ uv :xy)
                                      (* dir (- (/ 3.0 3.0) 0.5)))
                                   0.0)
                      :xyz)))))
             (luma-b (dot rgb-b luma)))
        (if (or (< luma-b luma-min) (> luma-b luma-max))
            rgb-a
            rgb-b)))))
