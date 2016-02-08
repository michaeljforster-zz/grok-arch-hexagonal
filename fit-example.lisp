;;;; fit-example.lisp

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

(in-package #:grok-arch-hexagonal-fit-example)

;;; Database Adaptor Interface

(defclass rate-repository ()
  ())

(defgeneric rate (object amount))

;;; Application

(defclass discounter ()
  ((rate-repository :initarg :rate-repository :reader rate-repository)))

(defmethod discount ((object discounter) amount)
  (let ((rate (rate (rate-repository object) amount)))
    (* amount rate)))

;;; Database Adaptor Implementations

(defclass mock-rate-repository (rate-repository)
  ())

(defmethod rate ((object mock-rate-repository) amount)
  (cond ((<= amount 100) 1/100)
        ((<= amount 1000) 1/50)
        (t 1/20)))

;;; Test Adaptor

(setf *print-failures* t)

(lisp-unit:define-test test-discounter
  (let ((mock-rate-repository (make-instance 'mock-rate-repository)))
    (let ((discounter (make-instance 'discounter :rate-repository mock-rate-repository)))
      (lisp-unit:assert-equal 1 (discount discounter 100)) ; NOTE reference doc stated 5
      (lisp-unit:assert-equal 4 (discount discounter 200))))) ; NOTE reference doc stated 10

(defun run-test ()
  (lisp-unit:run-tests :all :grok-arch-hexagonal-fit-example))

;;; UI Adaptor

(defun prompt-for-amount ()
  (wu-decimal:parse-decimal (grok-arch-hexagonal-utils:prompt-read "Amount")
                            :junk-allowed t))

(defun run-ui ()
  (let ((amount (prompt-for-amount)))
    (if (null amount)
        (grok-arch-hexagonal-utils:inform "No amount entered. Exiting.")
        (let ((mock-rate-repository (make-instance 'mock-rate-repository)))
          (let ((discounter (make-instance 'discounter
                                           :rate-repository mock-rate-repository)))
            (let ((discount (discount discounter amount))
                  (wu-decimal:*print-precision-loss* :round))
              (grok-arch-hexagonal-utils:inform (format nil
                                                        "Discount: $~,2/wu-decimal:F/"
                                                        discount))))))))
