;;;; Mutate character-class predicates and the xml-digit-weight helper with cl-weave.

(require :asdf)
(require :sb-cltl2)

(load (merge-pathnames "script-support.lisp" *load-truename*))

(progn
  (cl-xml-kit/script-support:load-project-definition)
  (asdf:load-system "cl-weave")
  (asdf:load-system "cl-xml-kit/test" :force t)
  ; Mutants compile serially; prevent prior inline expansions from leaking.
  (proclaim
   (cons (quote notinline)
         (mapcar (lambda (name) (find-symbol name "CL-XML-KIT"))
                 (quote ("XML-CHAR-P" "XML-SPACE-CHAR-P"
                         "XML-NAME-START-CHAR-P" "XML-NAME-CHAR-P"
                         "XML-PUBID-CHAR-P" "XML-NCNAME-START-CHAR-P"
                         "XML-NCNAME-CHAR-P" "XML-SPACE-P" "XML-NCNAME-P"
                         "XML-DIGIT-WEIGHT"))))))

(progn
  (defparameter *mutation-usage*
    "Usage: sbcl --script scripts/run-mutation.lisp

Run mutations of character-class predicates and the xml-digit-weight helper.
Require every mutant to be killed by the existing focused tests. The script takes
no options.")
  (defparameter *generated-character-class-names*
    '("xml-char-p" "xml-space-char-p" "xml-name-start-char-p"
      "xml-name-char-p" "xml-pubid-char-p"
      "xml-ncname-start-char-p" "xml-ncname-char-p"))
  (defparameter *mutation-targets*
    '(("xml-char-p" ("XML characters and whitespace"))
      ("xml-space-char-p" ("XML characters and whitespace"))
      ("xml-name-start-char-p" ("name characters"))
      ("xml-name-char-p" ("name characters"))
      ("xml-ncname-start-char-p" ("name characters"))
      ("xml-ncname-char-p" ("name characters"))
      ("xml-pubid-char-p" ("public identifier characters"))
      ("xml-space-p" ("XML characters and whitespace"))
      ("xml-ncname-p" ("name characters"))
      ("xml-digit-weight" ("weighs " "gives code #x"))))
  (defun mutation-symbol (name)
    (let ((symbol (find-symbol (string-upcase name) "CL-XML-KIT")))
      (unless (and symbol (fboundp symbol))
        (error "Mutation target function ~A is not defined" name))
      symbol))
  (defun generated-character-class-form (name)
    (let* ((*package* (find-package "CL-XML-KIT"))
           (expansion
             (macroexpand-1
              '(cl-xml-kit::define-char-classes
                cl-xml-kit::+xml-character-class-specs+)))
           (symbol (mutation-symbol name)))
      (unless (and (consp expansion) (eq (first expansion) 'cl:progn))
        (error "Character-class macro did not expand to a PROGN"))
      (or (find symbol (rest expansion)
                :key (lambda (form)
                       (and (consp form)
                            (eq (first form) 'cl:defun)
                            (second form)))
                :test #'eq)
          (error "Character-class expansion has no function named ~A" name))))
  (progn
  (defun normalize-discarded-loop-nil (form)
    (if (consp form)
        (let ((form (mapcar (function normalize-discarded-loop-nil) form)))
          (if (and (eq (first form) (quote if))
                   (= (length form) 4)
                   (null (third form))
                   (equal (fourth form)
                          (list (quote return-from) nil nil)))
            ; LOOP ALWAYS discards NIL when its predicate succeeds.
              (list (quote if)
                    (list (quote not) (second form))
                    (fourth form))
            form))
      form))
  (defun mutation-target-form (name)
    (let ((form
           (sb-cltl2:macroexpand-all
            (if (member name *generated-character-class-names*
                        :test (function string=))
                (generated-character-class-form name)
              (cl-xml-kit/test::source-definition-form
               "src/char-class.lisp" (mutation-symbol name))))))
      (if (member name (list "xml-space-p" "xml-ncname-p")
                  :test (function string=))
          (normalize-discarded-loop-nil form)
        form))))
  (defun mutation-selected-test-count (filters)
    (loop for filter in filters
          for plan = (cl-weave:collect-test-plan
                      (cl-weave:root-suite) :name-filter filter)
          do (unless plan
               (error "No tests selected for mutation filter ~S" filter))
          sum (length plan)))
  (defun run-focused-mutation-tests (filters)
    (let ((all-passed t))
      (dolist (filter filters)
        (let* ((plan (cl-weave:collect-test-plan
                      (cl-weave:root-suite) :name-filter filter))
               (test-count (length plan))
               (passed nil)
               (report
                (with-output-to-string (stream)
                  (setf passed
                        (cl-weave:run-all
                         :reporter :spec
                         :stream stream
                         :name-filter filter
                         :timeout-ms 10000
                         :max-workers 1
                         :pass-with-no-tests nil))))
               (complete-p
                (and (search ", 0 skipped, 0 todo, " report :from-end t)
                     (search (format nil ", ~D total" test-count)
                             report :from-end t))))
          (unless (plusp test-count)
            (error "No tests selected for mutation filter ~S" filter))
          (unless complete-p
            (write-string report)
            (error "Focused mutation tests did not complete without skips"))
          (cond
           ((and passed
                 (search (format nil
                                 "~D passed, 0 skipped, 0 todo, 0 failed, 0 errored, ~D total"
                                 test-count test-count)
                         report :from-end t)))
           ((and (not passed)
                 (or (search "[FAIL] " report)
                     (search "[ERROR] " report)))
            ; A completed failed test run kills the mutant; runner errors escape.
            (setf all-passed nil))
           (t
            (write-string report)
            (error "Focused mutation run returned an inconsistent summary")))))
      all-passed))
  (defun mutation-results-for-target (name filters)
    (let* ((symbol (mutation-symbol name))
           (form (mutation-target-form name))
           (original (fdefinition symbol))
           (test-count (mutation-selected-test-count filters)))
      (unless (plusp test-count)
        (error "No tests selected for mutation target ~A" name))
      (unless (run-focused-mutation-tests filters)
        (error "Focused tests failed before mutation target ~A" name))
      (let ((results
             (unwind-protect
                 (progn
                   (handler-bind ((warning (function muffle-warning)))
                     (eval form))
                   (unless (run-focused-mutation-tests filters)
                     (error "Unmutated target form ~A failed focused tests" name))
                   (cl-weave:run-mutations
                    form
                    (lambda (mutated mutation)
                      (declare (ignore mutation))
                      (handler-bind ((warning (function muffle-warning)))
                        (eval mutated))
                      (run-focused-mutation-tests filters))
                    :timeout-ms 10000))
               (setf (fdefinition symbol) original))))
        (unless results
          (error "Mutation target ~A produced no mutants after macroexpansion" name))
        (format t "mutation-target name=~A tests=~D mutants=~D killed=~D~%"
                name test-count (length results)
                (count :killed results :key (function cl-weave:mutation-result-status)))
        results)))
  (defun mutation-report (results pathname)
  (host-kit:write-file-string
   (with-output-to-string (stream)
     (cl-weave:report-mutations-sexp results stream))
   pathname))
  (defun mutation-report-gaps (targets results)
    (loop for target in targets
          for target-results in results
          do (loop for result in target-results
                   unless (eq (cl-weave:mutation-result-status result) :killed)
                     do (let ((mutation
                                (cl-weave:mutation-result-mutation result)))
                          (format t "mutation-gap target=~A status=~S operator=~S form=~S~%"
                                  (first target)
                                  (cl-weave:mutation-result-status result)
                                  (cl-weave:mutation-operator mutation)
                                  (cl-weave:mutation-form mutation))))))
  (defun mutation-main (arguments)
    (when arguments
      (error 'cl-xml-kit/script-support:script-usage-error
             :message (format nil "Unexpected argument ~A" (first arguments))))
    (let* ((targets *mutation-targets*)
           (target-results
             (mapcar (lambda (target)
                       (mutation-results-for-target (first target) (second target)))
                     targets))
           (results (apply #'append target-results))
           (summary (cl-weave:mutation-summary results))
           (report-path "mutation-results.sexp"))
      (mutation-report results report-path)
      (mutation-report-gaps targets target-results)
      (format t "mutation-summary total=~D killed=~D survived=~D errored=~D report=~A~%"
              (getf summary :total) (getf summary :killed)
              (getf summary :survived) (getf summary :errored)
              report-path)
      (if (zerop (getf summary :total))
          (error "Mutation run produced no mutants")
          (if (and (zerop (getf summary :survived))
                   (zerop (getf summary :errored)))
              0
              (error "Mutation gate failed")))))
  (cl-xml-kit/script-support:run-script
   "run-mutation" *mutation-usage* #'mutation-main))

