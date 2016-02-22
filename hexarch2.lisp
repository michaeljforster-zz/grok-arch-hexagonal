;;;; hexarch2.lisp

;;; The MIT License (MIT)
;;;
;;; Copyright (c) 2016 Michael J. Forster
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

(in-package #:grok-arch-hexagonal-hexarch2)

;;; Inner Hexagon - Domain

(defclass account ()
  ((balance :initarg :balance :accessor balance)))

(defmethod withdraw ((account account) amount)
  (decf (balance account) amount))

;;; Inner Hexagon - Storage Port

(defclass storage ()
  ())

(defgeneric stored-account (account-id storage))

(defgeneric (setf stored-account) (account account-id storage))

;;; Inner Hexagon - Application

(defclass bank ()
  ((storage :initarg :storage :reader storage)))

(defmethod create-account ((bank bank) account-id opening-balance)
  (setf (stored-account account-id (storage bank))
        (make-instance 'account :balance opening-balance))
  bank)

(defmethod account-balance ((bank bank) account-id)
  (let ((account (stored-account account-id (storage bank))))
    (balance account)))

(defmethod withdraw-from-account ((bank bank) account-id amount)
  (let ((account (stored-account account-id (storage bank))))
    (withdraw account amount)))

;;; Outer Hexagon - Storage Adapter

(defmethod stored-account (account-id (storage hash-table))
  (gethash account-id storage))

(defmethod (setf stored-account) (account account-id (storage hash-table))
  (setf (gethash account-id storage) account))

;;; Outer Hexagon - Test Adapter

(setf lisp-unit:*print-failures* t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (wu-decimal:enable-reader-macro))

(defparameter *account-id* 'account-01)

(defparameter *opening-balance* #$42.05)

(lisp-unit:define-test test-bank-create-account
  (let ((storage (make-hash-table)))
    (let ((bank (make-instance 'bank :storage storage)))
      (create-account bank *account-id* *opening-balance*)
      (lisp-unit:assert-equal *opening-balance*
                              (account-balance bank *account-id*)))))

(lisp-unit:define-test test-bank-withdraw-from-account
  (let ((storage (make-hash-table)))
    (let ((bank (make-instance 'bank :storage storage))
          (amount #$40.00))
      (create-account bank *account-id* *opening-balance*)
      (withdraw-from-account bank *account-id* amount)
      (lisp-unit:assert-equal (- *opening-balance* amount)
                              (account-balance bank *account-id*)))))

(defun run-test ()
  (lisp-unit:run-tests :all :grok-arch-hexagonal-hexarch2))
