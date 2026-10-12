;;;; benchmark/package.lisp

(defpackage #:cl-xml-kit/benchmark
  (:use #:cl)
  (:export #:main #:benchmark-api-error))

(in-package #:cl-xml-kit/benchmark)

(define-condition benchmark-api-error (error)
  ((operation :initarg :operation :reader benchmark-api-error-operation)
   (symbol :initarg :symbol :reader benchmark-api-error-symbol))
  (:report
   (lambda (condition stream)
     (format stream
             "Benchmark operation ~A requires the implemented public API ~A."
             (benchmark-api-error-operation condition)
             (benchmark-api-error-symbol condition)))))

