
(in-package :cl-polyhedral)

;; Abstract Code Generator from Kernel -> Lisp

;; A Function is consisted from two parts:
;;  Headers (writing a function args etc, ref: AbstractNode.lisp)
;;  For
;;  Body

;; (defgeneric write-operation
;; (defgeneric write-header

;; TODO: FuseOp Scheduler
;; TODO: Codegen Header部分とBody部分に分ける
;; Since the goal is to work on any backends
