;;;; src/resource-limits.lisp

(in-package #:cl-xml-kit)

(define-resource-limits xml-resource-limits make-xml-resource-limits
    +resource-limit-specs+
  :default-instance *default-xml-resource-limits*
  :default-instance-doc
  "The default immutable resource limits used by parser and serializer entry points."
  :record-doc "An immutable set of parser and serializer resource limits."
  :predicate-doc "Return true when VALUE is XML resource limits."
  :constructor-doc
  "Return an immutable XML-RESOURCE-LIMITS value after validating each bound.")
