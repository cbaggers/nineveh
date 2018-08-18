(in-package #:nineveh.anti-aliasing)

;;------------------------------------------------------------

(defun-g fxaa-luma ((rgba :vec4))
  ;; only used in fxaa3 for getting component storing luma
  (w rgba))

(defun-g fxaa3 ((pos :vec2)
                (tex :sampler-2d)
                (one-over-resolution :vec2)
                (amount-of-sub-pixel-aliasing-removal :float)
                (minimum-local-contrast-required-to-apply :float)
                (edge-threshold-min :float))
  "
# Fxaa3

Assumes you have already stored the luma in the w component of the
texture.
See nineveh.color for functions that will give you a luma value.
rgb->luma-bt601 is recommended, but remember to pass it the color
value post tone-mapping and gamma correction.

## pos
Use noperspective interpolation here (turn off perspective interpolation).
{xy} = center of pixel

## tex
Input color texture.
{rgb_} = color in linear or perceptual color space
if (FXAA_GREEN_AS_LUMA == 0)
    {___a} = luma in perceptual color space (not linear)

## one-over-resolution
This must be from a constant/uniform.
{x_} = 1.0/screenWidthInPixels
{_y} = 1.0/screenHeightInPixels

## amount-of-sub-pixel-aliasing-removal
Choose the amount of sub-pixel aliasing removal.
This can effect sharpness.
  1.00 - upper limit (softer)
  0.75 - default amount of filtering
  0.50 - lower limit (sharper, less sub-pixel aliasing removal)
  0.25 - almost off
  0.00 - completely off

## minimum-local-contrast-required-to-apply
The minimum amount of local contrast required to apply algorithm.
  0.333 - too little (faster)
  0.250 - low quality
  0.166 - default
  0.125 - high quality
  0.063 - overkill (slower)

## edge-threshold-min
Trims the algorithm from processing darks.
  0.0833 - upper limit (default, the start of visible unfiltered edges)
  0.0625 - high quality (faster)
  0.0312 - visible limit (slower)
"
 (let* ((pos-m pos)
        (rgby-m (texture-lod tex pos-m 0.0))
        (luma-s (fxaa-luma (texture-lod-offset tex pos-m 0.0 (ivec2 0 1))))
        (luma-e (fxaa-luma (texture-lod-offset tex pos-m 0.0 (ivec2 1 0))))
        (luma-n
         (fxaa-luma (texture-lod-offset tex pos-m 0.0 (ivec2 0 (- 1)))))
        (luma-w
         (fxaa-luma (texture-lod-offset tex pos-m 0.0 (ivec2 (- 1) 0))))
        (max-sm (max luma-s (w rgby-m)))
        (min-sm (min luma-s (w rgby-m)))
        (max-esm (max luma-e max-sm))
        (min-esm (min luma-e min-sm))
        (max-wn (max luma-n luma-w))
        (min-wn (min luma-n luma-w))
        (range-max (max max-wn max-esm))
        (range-min (min min-wn min-esm))
        (range-max-scaled (* range-max
                             minimum-local-contrast-required-to-apply))
        (range (- range-max range-min))
        (range-max-clamped (max edge-threshold-min range-max-scaled))
        (early-exit (< range range-max-clamped)))
   (if early-exit
       rgby-m
       (let* ((luma-nw
               (fxaa-luma
                (texture-lod-offset tex pos-m 0.0 (ivec2 (- 1) (- 1)))))
              (luma-se
               (fxaa-luma (texture-lod-offset tex pos-m 0.0 (ivec2 1 1))))
              (luma-ne
               (fxaa-luma (texture-lod-offset tex pos-m 0.0 (ivec2 1 (- 1)))))
              (luma-sw
               (fxaa-luma (texture-lod-offset tex pos-m 0.0 (ivec2 (- 1) 1))))
              (luma-ns (+ luma-n luma-s))
              (luma-we (+ luma-w luma-e))
              (subpix-rcp-range (/ 1.0 range))
              (subpix-nswe (+ luma-ns luma-we))
              (edge-horz1 (+ (* (- 2.0) (w rgby-m)) luma-ns))
              (edge-vert1 (+ (* (- 2.0) (w rgby-m)) luma-we))
              (luma-nese (+ luma-ne luma-se))
              (luma-nwne (+ luma-nw luma-ne))
              (edge-horz2 (+ (* (- 2.0) luma-e) luma-nese))
              (edge-vert2 (+ (* (- 2.0) luma-n) luma-nwne))
              (luma-nwsw (+ luma-nw luma-sw))
              (luma-swse (+ luma-sw luma-se))
              (edge-horz4 (+ (* (abs edge-horz1) 2.0) (abs edge-horz2)))
              (edge-vert4 (+ (* (abs edge-vert1) 2.0) (abs edge-vert2)))
              (edge-horz3 (+ (* (- 2.0) luma-w) luma-nwsw))
              (edge-vert3 (+ (* (- 2.0) luma-s) luma-swse))
              (edge-horz (+ (abs edge-horz3) edge-horz4))
              (edge-vert (+ (abs edge-vert3) edge-vert4))
              (subpix-nwswnese (+ luma-nwsw luma-nese))
              (length-sign (x one-over-resolution))
              (horz-span (>= edge-horz edge-vert))
              (subpix-a (+ (* subpix-nswe 2.0) subpix-nwswnese)))
         (when (not horz-span) (setf luma-n luma-w))
         (when (not horz-span) (setf luma-s luma-e))
         (when horz-span (setf length-sign (y one-over-resolution)))
         (let* ((subpix-b (- (* subpix-a (/ 1.0 12.0)) (w rgby-m)))
                (gradient-n (- luma-n (w rgby-m)))
                (gradient-s (- luma-s (w rgby-m)))
                (luma-nn (+ luma-n (w rgby-m)))
                (luma-ss (+ luma-s (w rgby-m)))
                (pair-n (>= (abs gradient-n) (abs gradient-s)))
                (gradient (max (abs gradient-n) (abs gradient-s))))
           (when pair-n (setf length-sign (- length-sign)))
           (let* ((subpix-c
                   (clamp (* (abs subpix-b) subpix-rcp-range)
                          0.0 1.0))
                  (pos-b pos-m)
                  (off-np (vec2 (if (not horz-span)
                                    0.0
                                    (x one-over-resolution))
                                (if horz-span
                                    0.0
                                    (y one-over-resolution)))))
             (when (not horz-span) (incf (x pos-b) (* length-sign 0.5)))
             (when horz-span (incf (y pos-b) (* length-sign 0.5)))
             (let* ((pos-n (vec2 (- (x pos-b) (* (x off-np) 1.0))
                                 (- (y pos-b) (* (y off-np) 1.0))))
                    (pos-p (vec2 (+ (x pos-b) (* (x off-np) 1.0))
                                 (+ (y pos-b) (* (y off-np) 1.0))))
                    (subpix-d (+ (* (- 2.0) subpix-c) 3.0))
                    (luma-end-n (fxaa-luma (texture-lod tex pos-n 0.0)))
                    (subpix-e (* subpix-c subpix-c))
                    (luma-end-p (fxaa-luma (texture-lod tex pos-p 0.0))))
               (when (not pair-n) (setf luma-nn luma-ss))
               (let* ((gradient-scaled (/ (* gradient 1.0) 4.0))
                      (luma-mm (- (w rgby-m) (* luma-nn 0.5)))
                      (subpix-f (* subpix-d subpix-e))
                      (luma-mltzero (< luma-mm 0.0)))
                 (decf luma-end-n (* luma-nn 0.5))
                 (decf luma-end-p (* luma-nn 0.5))
                 (let* ((done-n (>= (abs luma-end-n) gradient-scaled))
                        (done-p (>= (abs luma-end-p) gradient-scaled)))
                   (when (not done-n) (decf (x pos-n) (* (x off-np) 1.0)))
                   (when (not done-n) (decf (y pos-n) (* (y off-np) 1.0)))
                   (let* ((done-np (or (not done-n) (not done-p))))
                     (when (not done-p)
                       (incf (x pos-p) (* (x off-np) 1.0)))
                     (when (not done-p)
                       (incf (y pos-p) (* (y off-np) 1.0)))
                     (when done-np
                       (when (not done-n)
                         (setf luma-end-n
                               (fxaa-luma
                                (texture-lod tex pos-n 0.0))))
                       (when (not done-p)
                         (setf luma-end-p
                               (fxaa-luma
                                (texture-lod tex pos-p 0.0))))
                       (when (not done-n)
                         (setf luma-end-n (- luma-end-n (* luma-nn 0.5))))
                       (when (not done-p)
                         (setf luma-end-p
                               (- luma-end-p (* luma-nn 0.5))))
                       (setf done-n
                             (>= (abs luma-end-n) gradient-scaled))
                       (setf done-p
                             (>= (abs luma-end-p) gradient-scaled))
                       (when (not done-n)
                         (decf (x pos-n) (* (x off-np) 1.0)))
                       (when (not done-n)
                         (decf (y pos-n) (* (y off-np) 1.0)))
                       (setf done-np (or (not done-n) (not done-p)))
                       (when (not done-p)
                         (incf (x pos-p) (* (x off-np) 1.0)))
                       (when (not done-p)
                         (incf (y pos-p) (* (y off-np) 1.0)))
                       (when done-np
                         (when (not done-n)
                           (setf luma-end-n
                                 (fxaa-luma
                                  (texture-lod tex pos-n
                                               0.0))))
                         (when (not done-p)
                           (setf luma-end-p
                                 (fxaa-luma
                                  (texture-lod tex pos-p
                                               0.0))))
                         (when (not done-n)
                           (setf luma-end-n
                                 (- luma-end-n (* luma-nn 0.5))))
                         (when (not done-p)
                           (setf luma-end-p
                                 (- luma-end-p (* luma-nn 0.5))))
                         (setf done-n
                               (>= (abs luma-end-n)
                                   gradient-scaled))
                         (setf done-p
                               (>= (abs luma-end-p)
                                   gradient-scaled))
                         (when (not done-n)
                           (decf (x pos-n) (* (x off-np) 1.0)))
                         (when (not done-n)
                           (decf (y pos-n) (* (y off-np) 1.0)))
                         (setf done-np
                               (or (not done-n) (not done-p)))
                         (when (not done-p)
                           (incf (x pos-p) (* (x off-np) 1.0)))
                         (when (not done-p)
                           (incf (y pos-p) (* (y off-np) 1.0)))
                         (when done-np
                           (when (not done-n)
                             (setf luma-end-n
                                   (fxaa-luma
                                    (texture-lod tex
                                                 pos-n 0.0))))
                           (when (not done-p)
                             (setf luma-end-p
                                   (fxaa-luma
                                    (texture-lod tex
                                                 pos-p 0.0))))
                           (when (not done-n)
                             (setf luma-end-n
                                   (- luma-end-n
                                      (* luma-nn 0.5))))
                           (when (not done-p)
                             (setf luma-end-p
                                   (- luma-end-p
                                      (* luma-nn 0.5))))
                           (setf done-n
                                 (>= (abs luma-end-n)
                                     gradient-scaled))
                           (setf done-p
                                 (>= (abs luma-end-p)
                                     gradient-scaled))
                           (when (not done-n)
                             (decf (x pos-n)
                                   (* (x off-np) 1.0)))
                           (when (not done-n)
                             (decf (y pos-n)
                                   (* (y off-np) 1.0)))
                           (setf done-np
                                 (or (not done-n)
                                     (not done-p)))
                           (when (not done-p)
                             (incf (x pos-p)
                                   (* (x off-np) 1.0)))
                           (when (not done-p)
                             (incf (y pos-p)
                                   (* (y off-np) 1.0)))
                           (when done-np
                             (when (not done-n)
                               (setf luma-end-n
                                     (fxaa-luma
                                      (texture-lod tex
                                                   pos-n
                                                   0.0))))
                             (when (not done-p)
                               (setf luma-end-p
                                     (fxaa-luma
                                      (texture-lod tex
                                                   pos-p
                                                   0.0))))
                             (when (not done-n)
                               (setf luma-end-n
                                     (- luma-end-n
                                        (* luma-nn 0.5))))
                             (when (not done-p)
                               (setf luma-end-p
                                     (- luma-end-p
                                        (* luma-nn 0.5))))
                             (setf done-n
                                   (>= (abs luma-end-n)
                                       gradient-scaled))
                             (setf done-p
                                   (>= (abs luma-end-p)
                                       gradient-scaled))
                             (when (not done-n)
                               (decf (x pos-n)
                                     (* (x off-np) 1.5)))
                             (when (not done-n)
                               (decf (y pos-n)
                                     (* (y off-np) 1.5)))
                             (setf done-np
                                   (or (not done-n)
                                       (not done-p)))
                             (when (not done-p)
                               (incf (x pos-p)
                                     (* (x off-np) 1.5)))
                             (when (not done-p)
                               (incf (y pos-p)
                                     (* (y off-np) 1.5)))
                             (when done-np
                               (when (not done-n)
                                 (setf luma-end-n
                                       (fxaa-luma
                                        (texture-lod tex pos-n 0.0))))
                               (when (not done-p)
                                 (setf luma-end-p
                                       (fxaa-luma
                                        (texture-lod tex pos-p 0.0))))
                               (when (not done-n)
                                 (setf luma-end-n
                                       (- luma-end-n
                                          (* luma-nn 0.5))))
                               (when (not done-p)
                                 (setf luma-end-p
                                       (- luma-end-p
                                          (* luma-nn 0.5))))
                               (setf done-n
                                     (>= (abs luma-end-n) gradient-scaled))
                               (setf done-p
                                     (>= (abs luma-end-p) gradient-scaled))
                               (when (not done-n)
                                 (decf (x pos-n) (* (x off-np) 2.0)))
                               (when (not done-n)
                                 (decf (y pos-n) (* (y off-np) 2.0)))
                               (setf done-np (or (not done-n) (not done-p)))
                               (when (not done-p)
                                 (incf (x pos-p)
                                       (* (x off-np)
                                          2.0)))
                               (when (not done-p)
                                 (incf (y pos-p)
                                       (* (y off-np)
                                          2.0)))
                               (when done-np
                                 (when (not done-n)
                                   (setf luma-end-n
                                         (fxaa-luma
                                          (texture-lod
                                           tex
                                           pos-n
                                           0.0))))
                                 (when (not done-p)
                                   (setf luma-end-p
                                         (fxaa-luma
                                          (texture-lod
                                           tex
                                           pos-p
                                           0.0))))
                                 (when (not done-n)
                                   (setf luma-end-n
                                         (- luma-end-n (* luma-nn 0.5))))
                                 (when (not done-p)
                                   (setf luma-end-p
                                         (- luma-end-p (* luma-nn 0.5))))
                                 (setf done-n
                                       (>= (abs luma-end-n) gradient-scaled))
                                 (setf done-p
                                       (>= (abs luma-end-p) gradient-scaled))
                                 (when (not done-n)
                                   (decf (x pos-n)
                                         (* (x off-np) 2.0)))
                                 (when (not done-n)
                                   (decf (y pos-n)
                                         (* (y off-np) 2.0)))
                                 (setf done-np
                                       (or (not done-n)
                                           (not done-p)))
                                 (when (not done-p)
                                   (incf (x pos-p)
                                         (* (x off-np) 2.0)))
                                 (when (not done-p)
                                   (incf (y pos-p)
                                         (* (y off-np) 2.0)))
                                 (when done-np
                                   (when (not done-n)
                                     (setf luma-end-n
                                           (fxaa-luma
                                            (texture-lod
                                             tex
                                             pos-n
                                             0.0))))
                                   (when (not done-p)
                                     (setf luma-end-p
                                           (fxaa-luma
                                            (texture-lod
                                             tex
                                             pos-p
                                             0.0))))
                                   (when (not done-n)
                                     (setf luma-end-n
                                           (-
                                            luma-end-n
                                            (*
                                             luma-nn
                                             0.5))))
                                   (when (not done-p)
                                     (setf luma-end-p
                                           (-
                                            luma-end-p
                                            (*
                                             luma-nn
                                             0.5))))
                                   (setf done-n
                                         (>=
                                          (abs
                                           luma-end-n)
                                          gradient-scaled))
                                   (setf done-p
                                         (>=
                                          (abs
                                           luma-end-p)
                                          gradient-scaled))
                                   (when (not done-n)
                                     (decf
                                      (x pos-n)
                                      (*
                                       (x
                                        off-np)
                                       2.0)))
                                   (when (not done-n)
                                     (decf
                                      (y pos-n)
                                      (*
                                       (y
                                        off-np)
                                       2.0)))
                                   (setf done-np
                                         (or
                                          (not
                                           done-n)
                                          (not
                                           done-p)))
                                   (when (not done-p)
                                     (incf
                                      (x pos-p)
                                      (*
                                       (x
                                        off-np)
                                       2.0)))
                                   (when (not done-p)
                                     (incf
                                      (y pos-p)
                                      (*
                                       (y
                                        off-np)
                                       2.0)))
                                   (when done-np
                                     (when (not done-n)
                                       (setf luma-end-n
                                             (fxaa-luma
                                              (texture-lod
                                               tex
                                               pos-n
                                               0.0))))
                                     (when (not done-p)
                                       (setf luma-end-p
                                             (fxaa-luma
                                              (texture-lod
                                               tex
                                               pos-p
                                               0.0))))
                                     (when (not done-n)
                                       (setf luma-end-n
                                             (-
                                              luma-end-n
                                              (*
                                               luma-nn
                                               0.5))))
                                     (when (not done-p)
                                       (setf luma-end-p
                                             (-
                                              luma-end-p
                                              (*
                                               luma-nn
                                               0.5))))
                                     (setf done-n
                                           (>=
                                            (abs
                                             luma-end-n)
                                            gradient-scaled))
                                     (setf done-p
                                           (>=
                                            (abs
                                             luma-end-p)
                                            gradient-scaled))
                                     (when (not done-n)
                                       (decf
                                        (x
                                         pos-n)
                                        (*
                                         (x
                                          off-np)
                                         2.0)))
                                     (when (not done-n)
                                       (decf
                                        (y
                                         pos-n)
                                        (*
                                         (y
                                          off-np)
                                         2.0)))
                                     (setf done-np
                                           (or
                                            (not
                                             done-n)
                                            (not
                                             done-p)))
                                     (when (not done-p)
                                       (incf
                                        (x
                                         pos-p)
                                        (*
                                         (x
                                          off-np)
                                         2.0)))
                                     (when (not done-p)
                                       (incf
                                        (y
                                         pos-p)
                                        (*
                                         (y
                                          off-np)
                                         2.0)))
                                     (when done-np
                                       (when (not done-n)
                                         (setf luma-end-n
                                               (fxaa-luma
                                                (texture-lod
                                                 tex
                                                 pos-n
                                                 0.0))))
                                       (when (not done-p)
                                         (setf luma-end-p
                                               (fxaa-luma
                                                (texture-lod
                                                 tex
                                                 pos-p
                                                 0.0))))
                                       (when (not done-n)
                                         (setf luma-end-n
                                               (-
                                                luma-end-n
                                                (*
                                                 luma-nn
                                                 0.5))))
                                       (when (not done-p)
                                         (setf luma-end-p
                                               (-
                                                luma-end-p
                                                (*
                                                 luma-nn
                                                 0.5))))
                                       (setf done-n
                                             (>=
                                              (abs
                                               luma-end-n)
                                              gradient-scaled))
                                       (setf done-p
                                             (>=
                                              (abs
                                               luma-end-p)
                                              gradient-scaled))
                                       (when (not done-n)
                                         (decf
                                          (x
                                           pos-n)
                                          (*
                                           (x
                                            off-np)
                                           4.0)))
                                       (when (not done-n)
                                         (decf
                                          (y
                                           pos-n)
                                          (*
                                           (y
                                            off-np)
                                           4.0)))
                                       (setf done-np
                                             (or
                                              (not
                                               done-n)
                                              (not
                                               done-p)))
                                       (when (not done-p)
                                         (incf
                                          (x
                                           pos-p)
                                          (*
                                           (x
                                            off-np)
                                           4.0)))
                                       (when (not done-p)
                                         (incf
                                          (y
                                           pos-p)
                                          (*
                                           (y
                                            off-np)
                                           4.0)))
                                       (when done-np
                                         (when (not done-n)
                                           (setf luma-end-n
                                                 (fxaa-luma
                                                  (texture-lod
                                                   tex
                                                   pos-n
                                                   0.0))))
                                         (when (not done-p)
                                           (setf luma-end-p
                                                 (fxaa-luma
                                                  (texture-lod
                                                   tex
                                                   pos-p
                                                   0.0))))
                                         (when (not done-n)
                                           (setf luma-end-n
                                                 (- luma-end-n
                                                    (* luma-nn 0.5))))
                                         (when (not done-p)
                                           (setf luma-end-p
                                                 (- luma-end-p
                                                    (* luma-nn 0.5))))
                                         (setf done-n
                                               (>=
                                                (abs
                                                 luma-end-n)
                                                gradient-scaled))
                                         (setf done-p
                                               (>=
                                                (abs
                                                 luma-end-p)
                                                gradient-scaled))
                                         (when (not done-n)
                                           (decf
                                            (x
                                             pos-n)
                                            (*
                                             (x
                                              off-np)
                                             8.0)))
                                         (when (not done-n)
                                           (decf
                                            (y
                                             pos-n)
                                            (*
                                             (y
                                              off-np)
                                             8.0)))
                                         (setf done-np
                                               (or
                                                (not
                                                 done-n)
                                                (not
                                                 done-p)))
                                         (when (not done-p)
                                           (incf (x pos-p)
                                                 (* (x off-np) 8.0)))
                                         (when (not done-p)
                                           (incf (y pos-p)
                                                 (* (y off-np)
                                                    8.0)))))))))))))
                     (let* ((dst-n (- (x pos-m) (x pos-n)))
                            (dst-p (- (x pos-p) (x pos-m))))
                       (when (not horz-span)
                         (setf dst-n (- (y pos-m) (y pos-n))))
                       (when (not horz-span)
                         (setf dst-p (- (y pos-p) (y pos-m))))
                       (let* ((good-span-n
                               (/= (< luma-end-n 0.0) luma-mltzero))
                              (span-length (+ dst-p dst-n))
                              (good-span-p
                               (/= (< luma-end-p 0.0) luma-mltzero))
                              (span-length-rcp (/ 1.0 span-length))
                              (direction-n (< dst-n dst-p))
                              (dst (min dst-n dst-p))
                              (good-span
                               (if direction-n
                                   good-span-n
                                   good-span-p))
                              (subpix-g (* subpix-f subpix-f))
                              (pixel-offset
                               (+ (* dst (- span-length-rcp)) 0.5))
                              (subpix-h (* subpix-g
                                           amount-of-sub-pixel-aliasing-removal))
                              (pixel-offset-good
                               (if good-span
                                   pixel-offset
                                   0.0))
                              (pixel-offset-subpix
                               (max pixel-offset-good subpix-h)))
                         (when (not horz-span)
                           (incf (x pos-m)
                                 (* pixel-offset-subpix length-sign)))
                         (when horz-span
                           (incf (y pos-m)
                                 (* pixel-offset-subpix length-sign)))
                         (v! (s~ (texture-lod tex pos-m 0.0) :xyz)
                             (w rgby-m))))))))))))))

(defun-g fxaa3 ((pos :vec2)
                (tex :sampler-2d)
                (one-over-resolution :vec2))
  (fxaa3 pos tex one-over-resolution 0.75 0.166 0.0833))

;;------------------------------------------------------------

(defun-g fxaa3-pass-v ((vert :vec2))
  (values (v! vert 0 1)
          (:noperspective (+ (* vert 0.5) 0.5))))

(defun-g fxaa3-pass-f ((uv :vec2)
                       &uniform
                       (sampler :sampler-2d)
                       (one-over-viewport-res :vec2))
  (fxaa3 uv sampler one-over-viewport-res))

(defpipeline-g fxaa3-pass-pline ()
  (fxaa3-pass-v :vec2)
  (fxaa3-pass-f :vec2))

(defun fxaa3-pass (sampler)
  "
Assumes you have already stored the luma in the w component.
See nineveh.color for functions that will give you a luma value.
rgb->luma-bt601 is recommended, but remember to pass it the color
value post tone-mapping and gamma correction."
  (let* ((vp (current-viewport))
         (recip-res (vec2 (/ 1f0 (viewport-resolution-x vp))
                          (/ 1f0 (viewport-resolution-y vp)))))
    (map-g #'fxaa3-pass-pline (nineveh:get-quad-stream-v2)
           :sampler sampler
           :one-over-viewport-res recip-res)))

;;------------------------------------------------------------
