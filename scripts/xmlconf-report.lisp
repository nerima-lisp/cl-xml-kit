;;;; XMLConf report output.

(in-package #:cl-xml-kit/xmlconf)

(defun print-analysis (analysis)
  (format t "XMLConf catalog analysis~%")
  (format t "  profile: XML 1.0 Fifth Edition + Namespaces 1.0~%")
  (format t "  catalog-root: ~A~%" (xmlconf-analysis-catalog-root analysis))
  (format t "  catalogs: ~D~%" (xmlconf-analysis-catalog-count analysis))
  (format t "  tests: ~D~%" (xmlconf-analysis-test-count analysis))
  (format t "  accepted: ~D~%" (xmlconf-analysis-accepted analysis))
  (format t "  rejected: ~D~%" (xmlconf-analysis-rejected analysis))
  (format t "  skipped: ~D~%" (xmlconf-analysis-skipped analysis))
  (format t "  excluded: ~D~%" (xmlconf-analysis-excluded analysis))
  (format t "  output-cases: ~D~%" (xmlconf-analysis-output-count analysis))
  (format t "  unmatched-exclusions: ~D~%"
          (length (xmlconf-analysis-unmatched-exclusions analysis)))
  analysis)

(defun print-case-outcome (status id reason)
  (format t "XMLConf ~A ~A: ~A~%" status id reason))

(defun print-execution-summary (passed failed skipped)
  (format t "XMLConf execution~%  passed: ~D~%  failed: ~D~%  skipped: ~D~%"
          passed failed skipped))
