(in-package :play-with-vops)

;;------------------------------------------------------------

(defknown (%vec+) ((unsigned-byte 32)
                   system-area-pointer
                   system-area-pointer
                   system-area-pointer)
    (values)
    (any)
  :overwrite-fndb-silently t)

(define-vop (%vec+)
  (:translate %vec+)
  (:policy :fast-safe)
  (:args (index :scs (unsigned-reg))
         (result :scs (sap-reg))
         (vector1 :scs (sap-reg))
         (vector2 :scs (sap-reg)))
  (:arg-types unsigned-num
              system-area-pointer
              system-area-pointer
              system-area-pointer)
  (:results)
  (:temporary (:sc single-sse-reg) tmp)
  (:temporary (:sc unsigned-reg) idx)
  (:generator
   4
   (move idx index)
   (inst movaps
         tmp
         (make-ea :dword :base vector1 :disp 0 :index idx))
   (inst addps
         tmp
         (make-ea :dword :base vector2 :disp 0 :index idx))
   (inst movaps
         (make-ea :dword :base result :disp 0 :index idx)
         tmp)))

(defun %vec+ (i r x y)
  (declare (type (unsigned-byte 32) i)
           (type system-area-pointer r x y)
           (optimize (speed 3) (debug 0) (safety 0)))
  (%vec+ i r x y))

;;------------------------------------------------------------

(defknown (%vec/) ((unsigned-byte 32)
                   system-area-pointer
                   system-area-pointer
                   system-area-pointer)
    (values)
    (any)
  :overwrite-fndb-silently t)

(define-vop (%vec/)
  (:translate %vec/)
  (:policy :fast-safe)
  (:args (index :scs (unsigned-reg))
         (result :scs (sap-reg))
         (vector1 :scs (sap-reg))
         (vector2 :scs (sap-reg)))
  (:arg-types unsigned-num
              system-area-pointer
              system-area-pointer
              system-area-pointer)
  (:results)
  (:temporary (:sc single-sse-reg) tmp)
  (:temporary (:sc unsigned-reg) idx)
  (:generator
   4
   (move idx index)
   (inst movaps
         tmp
         (make-ea :dword :base vector1 :disp 0 :index idx))
   (inst divps
         tmp
         (make-ea :dword :base vector2 :disp 0 :index idx))
   (inst movaps
         (make-ea :dword :base result :disp 0 :index idx)
         tmp)))

(defun %vec/ (i r x y)
  (declare (type (unsigned-byte 32) i)
           (type system-area-pointer r x y)
           (optimize (speed 3) (debug 0) (safety 0)))
  (%vec/ i r x y))

;;------------------------------------------------------------
