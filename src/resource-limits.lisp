;;;; src/resource-limits.lisp

(in-package #:cl-xml-kit)

(defconstant +default-max-input-octets+ 16777216)
(defconstant +default-max-document-characters+ 16777216)
(defconstant +default-max-output-characters+ 16777216)
(defconstant +default-max-depth+ 512)
(defconstant +default-max-elements+ 1000000)
(defconstant +default-max-attributes-per-element+ 1024)
(defconstant +default-max-name-length+ 1024)
(defconstant +default-max-text-node-length+ 1048576)
(defconstant +default-max-namespace-bindings+ 1024)
(defconstant +default-max-entity-expansions+ 10000)
(defconstant +default-max-expanded-characters+ 16777216)
(defconstant +default-max-entity-depth+ 32)

(defstruct (xml-resource-limits
            (:constructor %make-xml-resource-limits
                (max-input-octets max-document-characters max-output-characters
                 max-depth max-elements max-attributes-per-element
                 max-name-length max-text-node-length max-namespace-bindings
                 max-entity-expansions max-expanded-characters max-entity-depth))
            (:copier nil)
            (:predicate xml-resource-limits-p))
  "An immutable set of parser and serializer resource limits."
  (max-input-octets +default-max-input-octets+ :read-only t)
  (max-document-characters +default-max-document-characters+ :read-only t)
  (max-output-characters +default-max-output-characters+ :read-only t)
  (max-depth +default-max-depth+ :read-only t)
  (max-elements +default-max-elements+ :read-only t)
  (max-attributes-per-element +default-max-attributes-per-element+ :read-only t)
  (max-name-length +default-max-name-length+ :read-only t)
  (max-text-node-length +default-max-text-node-length+ :read-only t)
  (max-namespace-bindings +default-max-namespace-bindings+ :read-only t)
  (max-entity-expansions +default-max-entity-expansions+ :read-only t)
  (max-expanded-characters +default-max-expanded-characters+ :read-only t)
  (max-entity-depth +default-max-entity-depth+ :read-only t))

(defun %valid-limit-p (value)
  (and (integerp value) (plusp value)))

(defun %check-limit (kind value)
  (unless (%valid-limit-p value)
    (error 'xml-invalid-resource-limit :kind kind :value value))
  value)

(defun make-xml-resource-limits
    (&key
       (max-input-octets +default-max-input-octets+)
       (max-document-characters +default-max-document-characters+)
       (max-output-characters +default-max-output-characters+)
       (max-depth +default-max-depth+)
       (max-elements +default-max-elements+)
       (max-attributes-per-element +default-max-attributes-per-element+)
       (max-name-length +default-max-name-length+)
       (max-text-node-length +default-max-text-node-length+)
       (max-namespace-bindings +default-max-namespace-bindings+)
       (max-entity-expansions +default-max-entity-expansions+)
       (max-expanded-characters +default-max-expanded-characters+)
       (max-entity-depth +default-max-entity-depth+))
  "Return an immutable XML-RESOURCE-LIMITS value after validating each bound."
  (dolist (entry `((:max-input-octets . ,max-input-octets)
                   (:max-document-characters . ,max-document-characters)
                   (:max-output-characters . ,max-output-characters)
                   (:max-depth . ,max-depth)
                   (:max-elements . ,max-elements)
                   (:max-attributes-per-element . ,max-attributes-per-element)
                   (:max-name-length . ,max-name-length)
                   (:max-text-node-length . ,max-text-node-length)
                   (:max-namespace-bindings . ,max-namespace-bindings)
                   (:max-entity-expansions . ,max-entity-expansions)
                   (:max-expanded-characters . ,max-expanded-characters)
                   (:max-entity-depth . ,max-entity-depth)))
    (%check-limit (car entry) (cdr entry)))
  (%make-xml-resource-limits max-input-octets max-document-characters
                             max-output-characters max-depth max-elements
                             max-attributes-per-element max-name-length
                             max-text-node-length max-namespace-bindings
                             max-entity-expansions max-expanded-characters
                             max-entity-depth))

(defparameter *default-xml-resource-limits* (make-xml-resource-limits)
  "The default immutable resource limits used by parser and serializer entry points.")

(loop for (name . text) in
      '((xml-resource-limits-p . "Return true when VALUE is XML resource limits.")
        (xml-resource-limits-max-input-octets . "Return the maximum input octets.")
        (xml-resource-limits-max-document-characters . "Return the maximum document characters.")
        (xml-resource-limits-max-output-characters . "Return the maximum output characters.")
        (xml-resource-limits-max-depth . "Return the maximum nesting depth.")
        (xml-resource-limits-max-elements . "Return the maximum element count.")
        (xml-resource-limits-max-attributes-per-element . "Return the maximum attributes per element.")
        (xml-resource-limits-max-name-length . "Return the maximum XML name length.")
        (xml-resource-limits-max-text-node-length . "Return the maximum text-node length.")
        (xml-resource-limits-max-namespace-bindings . "Return the maximum namespace bindings.")
        (xml-resource-limits-max-entity-expansions . "Return the maximum entity expansions.")
        (xml-resource-limits-max-expanded-characters . "Return the maximum expanded characters.")
        (xml-resource-limits-max-entity-depth . "Return the maximum entity expansion depth."))
      do (setf (documentation name 'function) text))

