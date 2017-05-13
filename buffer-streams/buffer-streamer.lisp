(in-package :nineveh)

;; Based on the wonderful article from here:  Jonathan Dupuy
;;
;; http://onrendering.blogspot.no/2011/10/buffer-object-streaming-in-opengl.html

(defstruct (buffer-streamer (:include buffer-stream)
                            (:constructor %make-buffer-streamer))
  (arr (error "buffer-streamer: bug[0]")
       :type cepl.types::gpu-array-bb))

(defmethod print-object ((object buffer-stream) stream)
  (let ((arr (buffer-streamer-arr object)))
    (format stream "#<~a (~s) ~s :LENGTH ~s>"
            'buffer-streamer
            (buffer-stream-vao object)
            (gpu-array-element-type arr)
            (first (gpu-array-dimensions arr)))))

(defun make-buffer-streamer (dimensions element-type)
  (when (listp dimensions)
    (assert (= (length dimensions) 1)))
  (assert element-type)
  (let* ((len (first (ensure-list dimensions)))
         (array (make-gpu-array nil :element-type element-type
                                :dimensions len))
         (gpu-arrays (list array)))
    (cepl.context::if-gl-context
     (%init-streamer
      (cepl.streams::init-buffer-stream-from-id
       %pre% (cepl.vaos:make-vao gpu-arrays nil)
       gpu-arrays nil 0 len t))
     (make-uninitialized-streamer)
     gpu-arrays)))

(defun %init-streamer (streamer)
  (setf (buffer-streamer-arr streamer)
        (caar (buffer-stream-gpu-arrays streamer)))
  streamer)

(defun make-uninitialized-streamer ()
  (%make-buffer-streamer
   :vao nil
   :%start 0
   :%start-byte 0
   :length 0
   :%index-type :uninitialized
   :%index-type-size 0
   :managed t
   :arr cepl.types::+null-buffer-backed-gpu-array+
   :gpu-arrays nil))

(defun buffer-streamer-push (c-array streamer)
  (assert (= (length (c-array-dimensions c-array)) 1))
  (let* ((g-arr (buffer-streamer-arr streamer))
         (g-len (first (gpu-array-dimensions g-arr)))
         (c-len (first (c-array-dimensions c-array)))
         (s-start (cepl.streams::buffer-stream-start streamer))
         (s-len (buffer-stream-length streamer))
         (old-tail-pos (+ s-start s-len))
         (doesnt-wrap (< (+ old-tail-pos c-len) g-len))
         (new-start-pos (if doesnt-wrap
                            old-tail-pos
                            0)))
    (assert (equal (c-array-element-type c-array)
                   (gpu-array-element-type g-arr)))
    (unless doesnt-wrap
      (reallocate-gpu-array g-arr)
      (cepl.vaos:make-vao-from-id (buffer-stream-vao streamer) (list g-arr)))

    (setf (cepl.streams::buffer-stream-start streamer) new-start-pos
          (cepl.streams:buffer-stream-length streamer) c-len)

    (cepl.gpu-arrays::with-gpu-array-range-as-pointer
        (g-ptr g-arr new-start-pos c-len
               :access-set '(:map-write :map-unsynchronized))
      (cepl.types::%memcpy g-ptr (c-array-pointer c-array)
                           (* (cepl.c-arrays:element-byte-size c-array)
                              c-len)))
    streamer))

(defmethod push-g ((c-arr c-array) (destination buffer-streamer))
  (buffer-streamer-push c-arr destination))

(defmethod push-g ((object list) (destination buffer-streamer))
  (let ((garr (buffer-streamer-arr destination)))
    (with-c-array (tmp (make-c-array object
                                     :dimensions (length object)
                                     :element-type (element-type garr)))
      (push-g tmp destination))))

(defmethod push-g ((object array) (destination buffer-streamer))
  (let ((garr (buffer-streamer-arr destination)))
    (with-c-array (tmp (make-c-array object
                                     :dimensions (length object)
                                     :element-type (element-type garr)))
      (push-g tmp destination))))
