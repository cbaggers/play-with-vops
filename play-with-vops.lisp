(in-package :play-with-vops)

;;------------------------------------------------------------

(defknown (%vec+) ((unsigned-byte 32)
                   sb-vm::system-area-pointer
                   sb-vm::system-area-pointer
                   sb-vm::system-area-pointer)
    (values)
    (any)
  :overwrite-fndb-silently t)

(define-vop (%vec+)
  (:translate %vec+)
  (:policy :fast-safe)
  (:args (index :scs (sb-vm::unsigned-reg))
         (result :scs (sb-vm::sap-reg))
         (vector1 :scs (sb-vm::sap-reg))
         (vector2 :scs (sb-vm::sap-reg)))
  (:arg-types sb-vm::unsigned-num
              sb-vm::system-area-pointer
              sb-vm::system-area-pointer
              sb-vm::system-area-pointer)
  (:results)
  (:temporary (:sc sb-vm::single-sse-reg) tmp)
  (:temporary (:sc sb-vm::unsigned-reg) idx)
  (:generator
   4
   (sb-c:move idx index)
   (sb-assem:inst sb-vm::movaps
                  tmp
                  (sb-vm::make-ea :dword :base vector1 :disp 0 :index idx))
   (sb-assem:inst sb-vm::addps
                  tmp
                  (sb-vm::make-ea :dword :base vector2 :disp 0 :index idx))
   (sb-assem:inst sb-vm::movaps
                  (sb-x86-64-asm::make-ea :dword :base result :disp 0 :index idx)
                  tmp)))

(defun %vec+ (i r x y)
  (declare (type (unsigned-byte 32) i)
           (type sb-vm::system-area-pointer r x y)
           (optimize (speed 3) (debug 0) (safety 0)))
  (%vec+ i r x y))

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
       (setf (cffi:mem-aref arr :float i) val)
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
       (setf (aref arr i) val)
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
                  (%vec+ (* i 4)
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
                      (+ (cffi:mem-aref arr0 :float i)
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
         (let ((x (+ (aref arr0 i)
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
                       (+ (cffi:mem-aref arr0 :float (+ 0 i))
                          (cffi:mem-aref arr1 :float (+ 0 i))))
                 (setf (cffi:mem-aref arr-res :float (+ 1 i))
                       (+ (cffi:mem-aref arr0 :float (+ 1 i))
                          (cffi:mem-aref arr1 :float (+ 1 i))))
                 (setf (cffi:mem-aref arr-res :float (+ 2 i))
                       (+ (cffi:mem-aref arr0 :float (+ 2 i))
                          (cffi:mem-aref arr1 :float (+ 2 i))))
                 (setf (cffi:mem-aref arr-res :float (+ 3 i))
                       (+ (cffi:mem-aref arr0 :float (+ 3 i))
                          (cffi:mem-aref arr1 :float (+ 3 i)))))))
           (cffi:foreign-array-to-lisp arr-res '(:array :float 100)))
      (cffi:foreign-free arr0)
      (cffi:foreign-free arr1)
      (cffi:foreign-free arr-res))))

