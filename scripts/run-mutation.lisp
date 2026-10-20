(require :asdf)
(require :sb-cltl2)

(load (merge-pathnames "script-support.lisp" *load-truename*))

(progn
  (cl-xml-kit/script-support:load-project-definition)
  (asdf:load-system "cl-weave")
  (asdf:load-system "cl-xml-kit/test" :force t)
  ; Keep the mutated function from being inlined into other mutation targets.
  (proclaim
   (cons (quote notinline)
         (mapcar (lambda (name) (find-symbol name "CL-XML-KIT"))
                 (quote ("XML-CHAR-P" "XML-SPACE-CHAR-P"
                         "XML-NAME-START-CHAR-P" "XML-NAME-CHAR-P"
                         "XML-PUBID-CHAR-P" "XML-NCNAME-START-CHAR-P"
                         "XML-NCNAME-CHAR-P" "XML-SPACE-P" "XML-NCNAME-P"
                         "XML-DIGIT-WEIGHT" "%MARKUP-CHARACTER-P"
                         "%ESCAPE-TEXT" "%ESCAPE-ATTRIBUTE"))))))

(progn
  (defparameter *mutation-usage* "Usage: sbcl --script scripts/run-mutation.lisp

Run mutations of XML character classes, XML whitespace and NCName validation,
digit weighting, and serializer escaping for text, canonical text, and
attributes. Require every mutant to be killed by focused tests.
No mutation-target options are accepted. Run from the repository root; the
report is written to mutation-results.sexp.")
  (defparameter *generated-character-class-names*
    '("xml-char-p" "xml-space-char-p" "xml-name-start-char-p"
      "xml-name-char-p" "xml-pubid-char-p"
      "xml-ncname-start-char-p" "xml-ncname-char-p"))
  (defparameter *mutation-targets*
    ; Name filters match substrings of test titles.
    '(("xml-char-p" "src/char-class.lisp" ("XML characters and whitespace"))
      ("xml-space-char-p" "src/char-class.lisp" ("XML characters and whitespace"))
      ("xml-name-start-char-p" "src/char-class.lisp" ("name characters"))
      ("xml-name-char-p" "src/char-class.lisp" ("name characters"))
      ("xml-ncname-start-char-p" "src/char-class.lisp" ("name characters"))
      ("xml-ncname-char-p" "src/char-class.lisp" ("name characters"))
      ("xml-pubid-char-p" "src/char-class.lisp" ("public identifier characters"))
      ("xml-space-p" "src/char-class.lisp" ("XML characters and whitespace"))
      ("xml-ncname-p" "src/char-class.lisp" ("name characters"))
      ("xml-digit-weight" "src/char-class.lisp" ("weighs " "gives code #x"))
      ("%markup-character-p" "src/serializer-core.lisp"
       ("characters that need markup escaping"))
      ("%escape-text" "src/serializer-core.lisp" ("writes the text" "writes canonical text"))
      ("%escape-attribute" "src/serializer-core.lisp"
       ("writes the attribute value"))))
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
            ; Expose LOOP ALWAYS's failure test to mutation operators.
              (list (quote if)
                    (list (quote not) (second form))
                    (fourth form))
            form))
      form))
  (defun mutation-target-form (name source-path)
    ; For serializer targets, expand CASE and COND to expose their source branches.
    ; Leave LOOP and WITH-OUTPUT-TO-STRING intact so generated implementation
    ; forms stay out of the mutation set. Other targets still use full expansion.
    (labels ((expand-control-forms (form)
               (cond
                 ((atom form) form)
                 ((member (first form) (list (quote quote) (quote function))
                          :test (function eq))
                  form)
                 ((member (first form) (list (quote case) (quote cond))
                          :test (function eq))
                  (expand-control-forms (macroexpand-1 form)))
                 (t (mapcar (function expand-control-forms) form)))))
      (let* ((source-form
               (if (member name *generated-character-class-names*
                           :test (function string=))
                   (generated-character-class-form name)
                 (cl-xml-kit/test::source-definition-form
                  source-path (mutation-symbol name))))
             (form
               (if (member name
                           (list "%markup-character-p"
                                 "%escape-text"
                                 "%escape-attribute")
                           :test (function string=))
                   (expand-control-forms source-form)
                 (sb-cltl2:macroexpand-all source-form))))
        (if (member name (list "xml-space-p" "xml-ncname-p")
                    :test (function string=))
            (normalize-discarded-loop-nil form)
          form)))))
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
  (defun mutation-results-for-target (name source-path filters)
    (let* ((symbol (mutation-symbol name))
           (form (mutation-target-form name source-path))
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
                       (mutation-results-for-target
                        (first target) (second target) (third target)))
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

