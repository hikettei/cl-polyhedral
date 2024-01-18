
(in-package :cl-polyhedral)

;; General Interfaces for generating simd-oritened operations

;; #include <arm_neon.h>
;; Ignore generics?

(defgeneric device-write-simd-packed-simd-type (arch kernel buffer))
(defgeneric device-write-simd-pack             (arch kernel buffer index scalar-p))
(defgeneric device-write-simd-unpack           (arch kernel buffer index body scalar-p))

(defgeneric device-write-simd-intrinsics (arch op target-buffer source-buffers lhs rhs kernel))
(defgeneric device-write-simd-related-headers (arch kernel))
