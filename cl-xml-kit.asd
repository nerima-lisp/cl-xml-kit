;;;; cl-xml-kit.asd

;;; This form comes FIRST, before any defsystem. ASDF binds *PACKAGE* to
;;; ASDF-USER only for a file it loads itself; read any other way -- a REPL
;;; `load`, an editor evaluating the buffer, flake.nix parsing :version -- the
;;; file is read in whatever package happens to be current. Saying it makes
;;; the file self-contained.
(in-package #:asdf-user)

(asdf:defsystem "cl-xml-kit"
  :description "XML 1.0 and Namespaces toolkit for Common Lisp"
  :long-description "An SBCL-oriented Common Lisp foundation for XML tooling."
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "1.0.0"
  :homepage "https://github.com/nerima-lisp/cl-xml-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-xml-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-xml-kit.git")
  :depends-on ("cl-codec-kit") ; UTF-8 octet decoding at the input boundary.
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "macros")
               (:file "data/error-messages")
               (:file "data/character-classes")
               (:file "data/resource-limits")
               (:file "data/token-kinds")
               (:file "data/event-kinds")
               (:file "data/parser-token-rules")
               (:file "data/serializer-node-rules")
               (:file "resource-limits")
               (:file "nodes")
               (:file "events")
               (:file "char-class")
               (:file "input")
               (:file "tokenizer-core")
               (:file "tokenizer-content")
               (:file "tokenizer-doctype")
               (:file "tokenizer-tags")
               (:file "tokenizer")
               (:file "namespace")
               (:file "entity-core")
               (:file "entity-expansion")
               (:file "entity-subset")
               (:file "parser-state")
               (:file "parser")
               (:file "parser-prolog")
               (:file "parser-api")
               (:file "dom-builder")
               (:file "serializer-core")
               (:file "serializer-element")
               (:file "serializer-node")
               (:file "serializer")
               (:file "spans-records")
               (:file "spans-tag-node-scan")
               (:file "spans-entity-doctype-scan")
               (:file "spans-api")
               (:file "path")
               (:file "verify")
               (:file "fragment")
               (:file "edit-support")
               (:file "edit-operations")
               (:file "edit")
               (:file "format"))
  :in-order-to ((test-op (test-op "cl-xml-kit/test"))))

(asdf:defsystem "cl-xml-kit/test"
  :description "Test system for cl-xml-kit"
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "1.0.0"
  :homepage "https://github.com/nerima-lisp/cl-xml-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-xml-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-xml-kit.git")
  :depends-on ("cl-xml-kit" "cl-weave")
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "helpers-matchers")
               (:file "helpers-mutation")
               (:file "helpers-tokenizer")
               (:file "helpers-input")
               (:file "helpers-entities")
               (:file "helpers-namespaces")
               (:file "helpers-parser")
               (:file "helpers-serializer")
               (:file "helpers-format")
               (:file "helpers-spans")
               (:file "helpers-corpus")
               (:file "helpers-path")
               (:file "helpers-edit")
               (:file "package-test")
               (:file "conditions-test")
               (:file "macros-test")
               (:file "resource-limits-test")
               (:file "nodes-test")
               (:file "events-test")
               (:file "char-class-test")
               (:file "input-test")
               (:file "input-encoding-test")
               (:file "tokenizer-core-test")
               (:file "tokenizer-content-test")
               (:file "tokenizer-doctype-test")
               (:file "tokenizer-tags-test")
               (:file "tokenizer-test")
               (:file "namespace-test")
               (:file "entity-core-test")
               (:file "entity-expansion-test")
               (:file "entity-expansion-limits-test")
               (:file "entity-subset-test")
               (:file "entity-subset-names-test")
               (:file "parser-test")
               (:file "parser-prolog-test")
               (:file "parser-api-test")
               (:file "dom-builder-test")
               (:file "parser-conformance-test")
               (:file "parser-property-test")
               (:file "serializer-core-test")
               (:file "serializer-element-test")
               (:file "serializer-node-test")
               (:file "serializer-test")
               (:file "serializer-round-trip-test")
               (:file "serializer-property-test")
               (:file "spans-tag-node-scan-test")
               (:file "spans-entity-doctype-scan-test")
               (:file "spans-api-test")
               (:file "spans-api-dom-agreement-test")
               (:file "spans-api-generated-test")
               (:file "path-test")
               (:file "path-syntax-test")
               (:file "path-conditions-test")
               (:file "verify-test")
               (:file "fragment-test")
               (:file "edit-support-test")
               (:file "edit-operations-test")
               (:file "edit-operations-replace-test")
               (:file "edit-operations-delete-test")
               (:file "edit-operations-append-test")
               (:file "edit-test")
               (:file "edit-refusals-test")
               (:file "edit-generated-test")
               (:file "format-test")
               (:file "suite"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (funcall (symbol-function
                               (find-symbol "RUN-TESTS" "CL-XML-KIT/TEST")))
               (error "cl-xml-kit test suite failed"))))

(asdf:defsystem "cl-xml-kit/benchmark"
  :description "Benchmarks for cl-xml-kit public APIs"
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "1.0.0"
  :homepage "https://github.com/nerima-lisp/cl-xml-kit"
  :source-control (:git "https://github.com/nerima-lisp/cl-xml-kit.git")
  :depends-on ("cl-xml-kit" (:require "sb-md5"))
  :pathname "benchmark"
  :serial t
  :components ((:file "package")
               (:file "run")))

