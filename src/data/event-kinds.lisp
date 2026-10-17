;;;; src/data/event-kinds.lisp

(in-package #:cl-xml-kit)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +xml-event-types+
    '((:start-document :position)
      (:doctype :text :position)
      (:start-element :name :attributes :position)
      (:characters :text :position)
      (:comment :text :position)
      (:processing-instruction :text :target :position)
      (:end-element :name :position)
      (:skipped-external-entity :name :position :external-p)
      (:end-document :position))
    "The event types of the pull reader contract and the fields each one sets.

A field not listed for a type is NIL in its events; a listed :EXTERNAL-P is
always true."))
