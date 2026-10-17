;;;; run-tests.lisp
;;;;
;;;; Bootstrap script: register this checkout, inherit the caller's ASDF
;;;; configuration for dependencies, and run the test system.

(require :asdf)

(load (merge-pathnames "scripts/script-support.lisp" *load-truename*))

(cl-xml-kit/script-support:register-repository)

(asdf:test-system "cl-xml-kit")

