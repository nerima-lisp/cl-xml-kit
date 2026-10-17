;;;; XMLConf case execution against the cl-xml-kit parser.

(in-package #:cl-xml-kit/xmlconf)

(defun read-octets-file (path)
  (with-open-file (stream path :direction :input
                          :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream)
                              :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

(defun bom-decoded-text (octets)
  "Return OCTETS decoded as UTF-16 when a UTF-16 byte order mark selects it,
and as UTF-8 otherwise."
  (flet ((starts-with-p (first second)
           (and (>= (length octets) 2)
                (= first (aref octets 0))
                (= second (aref octets 1)))))
    (cond
      ((starts-with-p #xFE #xFF)
       (sb-ext:octets-to-string octets :external-format :utf-16be :start 2))
      ((starts-with-p #xFF #xFE)
       (sb-ext:octets-to-string octets :external-format :utf-16le :start 2))
      (t
       (sb-ext:octets-to-string octets :external-format :utf-8)))))

(defun xmlconf-external-resource-declaration-p (text)
  (labels ((resource-keyword-after-p (marker)
             (and marker
                  (or (search "SYSTEM" text :start2 marker :test #'char-equal)
                      (search "PUBLIC" text :start2 marker :test #'char-equal)))))
    (or (resource-keyword-after-p (search "<!DOCTYPE" text :test #'char-equal))
        (resource-keyword-after-p (search "<!ENTITY" text :test #'char-equal)))))

(defun xmlconf-profile-skip-reason (source)
  (handler-case
      (let ((text (bom-decoded-text (read-octets-file source))))
        (cond
          ((search "version=\"1.1\"" text :test #'char-equal)
           "XML 1.1 is outside the XML 1.0 profile")
          ((xmlconf-external-resource-declaration-p text)
           "external DTD or entity resolution is disabled by policy")
          ((or (search "<!ELEMENT" text :test #'char-equal)
               (search "<!ATTLIST" text :test #'char-equal)
               (search "<!NOTATION" text :test #'char-equal))
           "DTD validation declarations are outside the non-validating profile")))
    (error () nil)))

(defun xmlconf-namespace-invalid-case-p (case)
  (let ((test (xmlconf-case-result-test case)))
    (and (string= "invalid" (xmlconf-test-type test))
         (string= "NS1.0" (xmlconf-test-recommendation test)))))

(defun xmlconf-test-path (test relative-path)
  (let* ((source-directory (directory-pathname (xmlconf-test-source test)))
         (base-directory (if (xmlconf-test-base test)
                             (directory-pathname
                              (merge-pathnames (xmlconf-test-base test)
                                               source-directory))
                             source-directory)))
    (probe-file (merge-pathnames relative-path base-directory))))


(defparameter *default-case-timeout-seconds* 10)

(defun load-parser-api ()
  (or (find-package "CL-XML-KIT")
      (progn
        (cl-xml-kit/script-support:load-project-definition)
        (asdf:load-system "cl-xml-kit")
        (or (find-package "CL-XML-KIT")
            (error "ASDF loaded cl-xml-kit without creating its package")))))

(defun parser-function (name)
  (let ((symbol (find-symbol name (load-parser-api))))
    (unless (and symbol (fboundp symbol))
      (error "The cl-xml-kit API does not export ~A" name))
    (symbol-function symbol)))

(defun parse-file (path)
  (funcall (parser-function "PARSE-XML")
           (read-octets-file path)
           :source-name (namestring path)))

(defun output-mismatch (test document)
  "Return why DOCUMENT differs from the expected output of TEST, or NIL."
  (let ((expected-path (xmlconf-test-path test (xmlconf-test-output test)))
        (canonical (parser-function "XML->CANONICAL-STRING")))
    (cond
      ((null expected-path)
       "expected output file was not found")
      ((string/= (funcall canonical document)
                 (funcall canonical (parse-file expected-path)))
       "canonical output differs from XMLConf output"))))

(defun parse-case-source (case source)
  (let ((test (xmlconf-case-result-test case))
        (id (xmlconf-case-result-id case))
        (rejected (eq (xmlconf-case-result-category case) :rejected)))
    (handler-case
        (let* ((document (parse-file source))
               (mismatch (and (xmlconf-test-output test)
                              (output-mismatch test document))))
          (cond
            (mismatch (values :failed id mismatch))
            (rejected (values :failed id "not-wf input was accepted"))
            (t (values :passed id "document parsed"))))
      (error (condition)
        (if (or rejected (xmlconf-namespace-invalid-case-p case))
            (values :passed id "document was rejected")
            (values :failed id (princ-to-string condition)))))))

(defun execute-xmlconf-case (case)
  (let* ((test (xmlconf-case-result-test case))
         (id (xmlconf-case-result-id case))
         (uri (xmlconf-test-uri test)))
    (cond
      ((null uri)
       (values :skipped id "TEST has no URI"))
      ((not (string= "none" (xmlconf-test-entities test)))
       (values :skipped id "external entity resolution is disabled by policy"))
      (t
       (let ((source (xmlconf-test-path test uri)))
         (if (null source)
             (values :failed id (format nil "input URI was not found: ~A" uri))
             (let ((profile-reason (xmlconf-profile-skip-reason source)))
               (if profile-reason
                   (values :skipped id profile-reason)
                   (parse-case-source case source)))))))))

(defun execute-case-within (case seconds)
  "Execute CASE and report it as failed when it runs longer than SECONDS."
  (handler-case (sb-ext:with-timeout seconds
                  (execute-xmlconf-case case))
    (sb-ext:timeout ()
      (values :failed
              (xmlconf-case-result-id case)
              (format nil "exceeded the ~D second case time limit" seconds)))))

(defun execute-xmlconf (analysis case-timeout)
  ;; Load the parser before the first case starts its time limit; a timeout
  ;; that interrupts ASDF leaves a partly loaded system for every later case.
  (load-parser-api)
  (let ((passed 0)
        (failed 0)
        (skipped 0))
    (dolist (case (xmlconf-analysis-results analysis))
      (when (member (xmlconf-case-result-category case) '(:accepted :rejected))
        (multiple-value-bind (status id reason)
            (execute-case-within case case-timeout)
          (ecase status
            (:passed (incf passed))
            (:failed (incf failed))
            (:skipped (incf skipped)))
          (print-case-outcome status id reason))))
    (print-execution-summary passed failed skipped)
    (when (plusp failed)
      (error "XMLConf execution failed for ~D case~:P" failed))
    (values passed failed skipped)))

(defun run-xmlconf (directory &key (exclusions-file *default-exclusions-file*)
                                   (mode :catalog)
                                   (case-timeout *default-case-timeout-seconds*))
  "Analyze the XMLConf catalog below DIRECTORY; with MODE :EXECUTE, also parse
each selected case, failing any case that runs longer than CASE-TIMEOUT seconds."
  (let ((analysis (analyze-xmlconf directory :exclusions-file exclusions-file)))
    (ecase mode
      (:catalog)
      (:execute (execute-xmlconf analysis case-timeout)))
    analysis))
