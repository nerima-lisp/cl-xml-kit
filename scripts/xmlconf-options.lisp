;;;; XMLConf runner command line.

(in-package #:cl-xml-kit/xmlconf)

(defparameter *runner-usage*
  "Usage: sbcl --script scripts/xmlconf-runner.lisp [--run] [--catalog-dir DIR]

Without --run, analyze the XMLConf catalog and print its counts. With --run,
parse each selected case instead and exit with status 1 when any case fails.

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
  "Return the execution mode and the --catalog-dir value in ARGUMENTS."
  (let ((mode :catalog)
        (directory nil))
    (loop while arguments do
      (let ((argument (pop arguments)))
        (cond
          ((string= argument "--run")
           (setf mode :execute))
          ((string= argument "--catalog-dir")
           (unless arguments
             (usage-error "Missing value after --catalog-dir"))
           (setf directory (pop arguments)))
          (t
           (usage-error "Unknown option ~A" argument)))))
    (values mode directory)))

(defun command-line-main (arguments)
  (multiple-value-bind (mode directory) (parse-options arguments)
    (let ((directory (or directory (uiop:getenv "XML_KIT_XMLCONF_DIR"))))
      (unless directory
        (usage-error "XML_KIT_XMLCONF_DIR is not set; pass --catalog-dir DIR"))
      (let ((analysis
              (run-xmlconf directory
                           :exclusions-file
                           (or (uiop:getenv "XML_KIT_XMLCONF_EXCLUSIONS")
                               *default-exclusions-file*)
                           :mode mode
                           :case-timeout
                           (cl-xml-kit/script-support:positive-integer-setting
                            "XML_KIT_XMLCONF_CASE_TIMEOUT"
                            *default-case-timeout-seconds*))))
        (when (eq mode :catalog)
          (print-analysis analysis))
        0))))
