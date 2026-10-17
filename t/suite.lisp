(in-package #:cl-xml-kit/test)

(defun run-tests ()
  "Run the cl-xml-kit test suite and fail when any spec fails."
  (unless (run-all :reporter :spec :timeout-ms 10000)
    (error "cl-xml-kit test suite failed"))
  (format t "~&cl-xml-kit/test: successful completion~%")
  t)
