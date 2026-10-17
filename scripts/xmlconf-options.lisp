;;;; XMLConf runner command line.

(in-package #:cl-xml-kit/xmlconf)

(defparameter *runner-usage*
  "Usage: sbcl --script scripts/xmlconf-runner.lisp [--run] [--catalog-dir DIR]
                                                   [--report FILE]

Analyze the XMLConf catalog and print its counts. With --run, also parse each
selected case, print its outcome and the totals, and exit with status 1 when
any case fails. With --report, write that output to FILE instead of standard
output.

Environment:
  XML_KIT_XMLCONF_DIR           catalog directory when --catalog-dir is omitted
  XML_KIT_XMLCONF_EXCLUSIONS    exclusion file
                                (default: scripts/xmlconf-exclusions.sexp)
  XML_KIT_XMLCONF_CASE_TIMEOUT  seconds one case may run under --run before it
                                is reported as failed (default: 10)
")

(defun usage-error (control &rest arguments)
  (error 'cl-xml-kit/script-support:script-usage-error
         :message (apply #'format nil control arguments)))

(defun parse-options (arguments)
  "Return the execution mode and the --catalog-dir and --report values in
ARGUMENTS."
  (let ((mode :catalog)
        (directory nil)
        (report nil))
    (flet ((option-value (option)
             (unless arguments
               (usage-error "Missing value after ~A" option))
             (pop arguments)))
      (loop while arguments do
        (let ((argument (pop arguments)))
          (cond
            ((string= argument "--run")
             (setf mode :execute))
            ((string= argument "--catalog-dir")
             (setf directory (option-value argument)))
            ((string= argument "--report")
             (setf report (option-value argument)))
            (t
             (usage-error "Unknown option ~A" argument))))))
    (values mode directory report)))

(defun run-command (mode directory)
  (let ((analysis
          (analyze-xmlconf directory
                           :exclusions-file
                           (or (uiop:getenv "XML_KIT_XMLCONF_EXCLUSIONS")
                               *default-exclusions-file*))))
    (print-analysis analysis)
    (when (eq mode :execute)
      (execute-xmlconf analysis
                       (cl-xml-kit/script-support:positive-integer-setting
                        "XML_KIT_XMLCONF_CASE_TIMEOUT"
                        *default-case-timeout-seconds*)))))

(defun command-line-main (arguments)
  (multiple-value-bind (mode directory report) (parse-options arguments)
    (let ((directory (or directory (uiop:getenv "XML_KIT_XMLCONF_DIR"))))
      (unless directory
        (usage-error "XML_KIT_XMLCONF_DIR is not set; pass --catalog-dir DIR"))
      (if report
          (with-open-file (*standard-output* report :direction :output
                                                    :if-exists :supersede)
            (run-command mode directory))
          (run-command mode directory))
      0)))
