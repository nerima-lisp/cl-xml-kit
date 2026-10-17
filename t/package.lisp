(defpackage #:cl-xml-kit/test
  (:use #:cl #:cl-xml-kit)
  ;; CL:DESCRIBE is a standard function; the suite DSL takes its name.
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave
   #:it #:it-each #:it-property #:it-fuzz #:it-sequential
   #:expect #:expect-not #:signals #:finishes #:fail #:defmatcher
   #:with-soft-assertions #:run-all
   #:gen-boolean #:gen-character #:gen-integer #:gen-list #:gen-map
   #:gen-member #:gen-one-of #:gen-recursive #:gen-string #:gen-tuple
   #:gen-vector
   #:run-mutations #:assert-mutation-score #:mutation-result-status)
  (:export #:run-tests))

(in-package #:cl-xml-kit/test)
