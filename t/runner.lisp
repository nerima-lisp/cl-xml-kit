;;;; t/runner.lisp

(in-package #:cl-xml-kit/test)

(it "loads the public CL-XML-KIT package"
  (expect (not (null (find-package "CL-XML-KIT"))) :to-equal t))

(defun run-tests ()
  "Run the cl-xml-kit test suite and fail when any spec fails."
  (unless (run-all :reporter :spec :timeout-ms 10000)
    (error "cl-xml-kit test suite failed"))
  (format t "~&cl-xml-kit/test: successful completion~%")
  t)

