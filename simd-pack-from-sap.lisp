(in-package :play-with-vops)


;; make from values
;; (sb-kernel:%make-simd-pack-single 1f0 2f0 3f0 4f0)

;; and our equiv to make from ptr
(defknown make-simd-pack-single-from-sap
    ((unsigned-byte 32) system-area-pointer)
    (simd-pack single-float)
    (flushable movable foldable)
  :overwrite-fndb-silently t)

(define-vop (make-simd-pack-single-from-sap)
  (:translate make-simd-pack-single-from-sap)
  (:policy :fast-safe)
  (:args (index :scs (unsigned-reg) :target idx)
         (ptr :scs (sap-reg)))
  (:arg-types unsigned-num system-area-pointer)
  (:temporary (:sc unsigned-reg :from (:argument 0)) idx)
  (:results (dst :scs (single-sse-reg)))
  (:result-types simd-pack-single)
  (:generator 3
	      (move idx index)
	      (inst movaps
		    dst
		    (make-ea :dword :base ptr :disp 0 :index idx))))

(defun make-simd-pack-single-from-sap (i ptr)
  (declare (type (unsigned-byte 32) i)
           (type system-area-pointer ptr)
           (optimize (speed 3) (debug 0) (safety 0)))
  (make-simd-pack-single-from-sap i ptr))


(defun test (ptr)
  (declare (optimize speed))
  (let ((x (make-simd-pack-single-from-sap 0 ptr)))
    (print x)
    10))
