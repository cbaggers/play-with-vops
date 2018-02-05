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

(defknown (%vec/) ((unsigned-byte 32)
                   sb-vm::system-area-pointer
                   sb-vm::system-area-pointer
                   sb-vm::system-area-pointer)
    (values)
    (any)
  :overwrite-fndb-silently t)

(define-vop (%vec/)
  (:translate %vec/)
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
   (sb-assem:inst sb-vm::divps
                  tmp
                  (sb-vm::make-ea :dword :base vector2 :disp 0 :index idx))
   (sb-assem:inst sb-vm::movaps
                  (sb-x86-64-asm::make-ea :dword :base result :disp 0 :index idx)
                  tmp)))

(defun %vec/ (i r x y)
  (declare (type (unsigned-byte 32) i)
           (type sb-vm::system-area-pointer r x y)
           (optimize (speed 3) (debug 0) (safety 0)))
  (%vec/ i r x y))

;;------------------------------------------------------------
