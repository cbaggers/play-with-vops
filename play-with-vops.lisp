(in-package :play-with-vops)

;;------------------------------------------------------------

(defun assert-aligned (ptr)
  (assert (= (mod (cffi:pointer-address ptr) 16) 0)))

;;------------------------------------------------------------

(defun foreign-arr-cycling-floats (count cycle-max &optional (step 1f0))
  (let ((cycle-max (float cycle-max 0f0))
        (step (float step 0f0))
        (arr (cffi:foreign-alloc :float :count count))
        (val 0f0))
    (loop :for i :below count :do
       (setf (cffi:mem-aref arr :float i) (+ 1f0 val))
       (setf val (mod (+ val step) cycle-max)))
    arr))

(declaim (ftype (function (t t &optional t) (simple-array single-float (*)))
                arr-cycling-floats))
(defun arr-cycling-floats (count cycle-max &optional (step 1f0))
  (let ((cycle-max (float cycle-max 0f0))
        (step (float step 0f0))
        (arr (make-array count :element-type 'single-float))
        (val 0f0))
    (loop :for i :below count :do
       (setf (aref arr i) (+ 1f0 val))
       (setf val (mod (+ val step) cycle-max)))
    arr))

;;------------------------------------------------------------

(defun test ()
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0)))
  (let* ((at-least-times 10000)
         (times (* (ceiling at-least-times 4) 4))
         (arr0 (foreign-arr-cycling-floats times 1000 0.35))
         (arr1 (foreign-arr-cycling-floats times 1000 1.2))
         (arr-res (foreign-arr-cycling-floats times 1000 0f0))
         (run 0))
    (assert-aligned arr0)
    (assert-aligned arr1)
    (assert-aligned arr-res)
    (unwind-protect
         (progn
           (time
            (loop :for test-run :below 100000 :do
               (setf run test-run)
               (loop :for i :below times :by 4
                  :do
                  (%vec/ (* i 4)
                         arr-res
                         arr0
                         arr1))))
           (cffi:foreign-array-to-lisp arr-res '(:array :float 100)))
      (cffi:foreign-free arr0)
      (cffi:foreign-free arr1)
      (cffi:foreign-free arr-res))))

(defun test1 ()
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0)))
  (let* ((at-least-times 10000)
         (times (* (ceiling at-least-times 4) 4))
         (arr0 (foreign-arr-cycling-floats times 1000 0.35))
         (arr1 (foreign-arr-cycling-floats times 1000 1.2))
         (arr-res (foreign-arr-cycling-floats times 1000 0f0))
         (run 0))
    (unwind-protect
         (progn
           (time
            (dotimes (test-run 100000)
              (setf run test-run)
              (dotimes (i times)
                (declare (type (unsigned-byte 32) i))
                (setf (cffi:mem-aref arr-res :float i)
                      (/ (cffi:mem-aref arr0 :float i)
                         (cffi:mem-aref arr1 :float i))))))
           (cffi:foreign-array-to-lisp arr-res '(:array :float 100)))
      (cffi:foreign-free arr0)
      (cffi:foreign-free arr1)
      (cffi:foreign-free arr-res))))

(defun test3 ()
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0)))
  (let* ((at-least-times 10000)
         (times (* (ceiling at-least-times 4) 4))
         (arr0 (arr-cycling-floats times 1000 0.35))
         (arr1 (arr-cycling-floats times 1000 1.2))
         (arr-res (arr-cycling-floats times 1000 0f0))
         (run 0))
    (time
     (dotimes (test-run 100000)
       (setf run test-run)
       (dotimes (i times)
         (declare (type (unsigned-byte 32) i))
         (let ((x (/ (aref arr0 i)
                     (aref arr1 i))))
           (setf (aref arr-res i) x)))))
    (loop :for i :below 100 :collect (aref arr-res i))))

(defun test2 ()
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0)))
  (let* ((at-least-times 10000)
         (times (* (ceiling at-least-times 4) 4))
         (arr0 (foreign-arr-cycling-floats times 1000 0.35))
         (arr1 (foreign-arr-cycling-floats times 1000 1.2))
         (arr-res (foreign-arr-cycling-floats times 1000 0f0))
         (run 0))
    (unwind-protect
         (progn
           (time
            (dotimes (test-run 100000)
              (setf run test-run)
              (loop :for i :below times :by 4 :do
                 (setf (cffi:mem-aref arr-res :float (+ 0 i))
                       (/ (cffi:mem-aref arr0 :float (+ 0 i))
                          (cffi:mem-aref arr1 :float (+ 0 i))))
                 (setf (cffi:mem-aref arr-res :float (+ 1 i))
                       (/ (cffi:mem-aref arr0 :float (+ 1 i))
                          (cffi:mem-aref arr1 :float (+ 1 i))))
                 (setf (cffi:mem-aref arr-res :float (+ 2 i))
                       (/ (cffi:mem-aref arr0 :float (+ 2 i))
                          (cffi:mem-aref arr1 :float (+ 2 i))))
                 (setf (cffi:mem-aref arr-res :float (+ 3 i))
                       (/ (cffi:mem-aref arr0 :float (+ 3 i))
                          (cffi:mem-aref arr1 :float (+ 3 i)))))))
           (cffi:foreign-array-to-lisp arr-res '(:array :float 100)))
      (cffi:foreign-free arr0)
      (cffi:foreign-free arr1)
      (cffi:foreign-free arr-res))))
