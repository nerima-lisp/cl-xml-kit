;;;; src/data/resource-limits.lisp

(in-package #:cl-xml-kit)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +resource-limit-specs+
    '((:max-input-octets max-input-octets +default-max-input-octets+ 16777216
       xml-resource-limits-max-input-octets
       "Return the maximum input octets.")
      (:max-document-characters max-document-characters
       +default-max-document-characters+ 16777216
       xml-resource-limits-max-document-characters
       "Return the maximum document characters.")
      (:max-output-characters max-output-characters
       +default-max-output-characters+ 16777216
       xml-resource-limits-max-output-characters
       "Return the maximum output characters.")
      (:max-depth max-depth +default-max-depth+ 512
       xml-resource-limits-max-depth
       "Return the maximum nesting depth.")
      (:max-elements max-elements +default-max-elements+ 1000000
       xml-resource-limits-max-elements
       "Return the maximum element count.")
      (:max-attributes-per-element max-attributes-per-element
       +default-max-attributes-per-element+ 1024
       xml-resource-limits-max-attributes-per-element
       "Return the maximum attributes per element.")
      (:max-name-length max-name-length +default-max-name-length+ 1024
       xml-resource-limits-max-name-length
       "Return the maximum XML name length.")
      (:max-text-node-length max-text-node-length
       +default-max-text-node-length+ 1048576
       xml-resource-limits-max-text-node-length
       "Return the maximum text-node length.")
      (:max-namespace-bindings max-namespace-bindings
       +default-max-namespace-bindings+ 1024
       xml-resource-limits-max-namespace-bindings
       "Return the maximum namespace bindings.")
      (:max-entity-expansions max-entity-expansions
       +default-max-entity-expansions+ 10000
       xml-resource-limits-max-entity-expansions
       "Return the maximum entity expansions.")
      (:max-expanded-characters max-expanded-characters
       +default-max-expanded-characters+ 16777216
       xml-resource-limits-max-expanded-characters
       "Return the maximum expanded characters.")
      (:max-entity-depth max-entity-depth +default-max-entity-depth+ 32
       xml-resource-limits-max-entity-depth
       "Return the maximum entity expansion depth."))
    "Parser and serializer resource limits.

Each row is (KEYWORD SLOT DEFAULT-CONSTANT DEFAULT READER READER-DOC); the row
order is the constructor's keyword and validation order."))
