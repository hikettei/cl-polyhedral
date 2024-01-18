
(in-package :cl-polyhedral)

(defgeneric codegen-write-packed-simd-type (backend buffer simd-stride)
  (:documentation "writes a dtype e.g.: float32x4_t"))

(defgeneric codegen-write-simd-pack (backend buffer index simd-stride)
  (:documentation "Corresponds with:
simd_pack(buffer + index)"))

(defgeneric codegen-write-simd-unpack (backend buffer index simd-stride)
  (:documentation ""))

