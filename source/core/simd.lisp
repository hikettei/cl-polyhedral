
(in-package :cl-polyhedral)

(defgeneric codegen-write-packed-simd-type (backend kernel buffer)
  (:documentation "writes a dtype e.g.: float32x4_t"))

(defgeneric codegen-write-simd-pack (backend kernel buffer index scalar-p)
  (:documentation "Corresponds with:
simd_pack(buffer + index)"))

(defgeneric codegen-write-simd-unpack (backend kernel buffer index body scalar-p)
  (:documentation ""))

(defgeneric codegen-write-simd-intrinsics (backend callexpr body target-buffer source-buffers kernel)
  (:documentation "Writes an binary ops"))

(defgeneric codegen-write-simd-stride (backend buffer kernel)
  (:documentation "Computes the step of simdified loop"))

