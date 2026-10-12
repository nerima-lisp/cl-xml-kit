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
               (:file "resource-limits")
               (:file "nodes")
               (:file "events")
               (:file "char-class")
               (:file "input")
               (:file "tokenizer")
               (:file "namespace")
               (:file "entities")
               (:file "parser-state")
               (:file "parser")
               (:file "serializer")
               (:file "spans")
               (:file "path")
               (:file "verify")
               (:file "fragment")
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
               (:file "runner")
               (:file "api-surface-test")
               (:file "conditions-test")
               (:file "resource-limits-test")
               (:file "char-class-test")
               (:file "input-test")
               (:file "tokenizer-test")
               (:file "namespace-test")
               (:file "entities-test")
               (:file "security-test")
               (:file "parser-test")
               (:file "dom-test")
               (:file "serializer-test")
               (:file "round-trip-test")
               (:file "spans-test")
               (:file "path-test")
               (:file "verify-test")
               (:file "fragment-test")
               (:file "edit-test")
               (:file "format-test")
               (:file "property-test")
               (:file "fuzz-test")
               (:file "xml-conformance-test"))
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
  :depends-on ("cl-xml-kit")
  :pathname "benchmark"
  :serial t
  :components ((:file "package")
               (:file "run")))

