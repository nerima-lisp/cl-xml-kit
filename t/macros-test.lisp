(in-package #:cl-xml-kit/test)

;;; The definition macros run only while the library compiles, where no test can
;;; observe them, so these tests expand and evaluate them at run time over probe tables.

(defvar *check-argument-evaluations* 0)

(defparameter *probe-limit-specs*
  '((:max-probe max-probe +probe-default-max+ 7 probe-limits-max-probe
     "Return the probe maximum.")
    (:min-probe min-probe +probe-default-min+ 2 probe-limits-min-probe
     "Return the probe minimum."))
  "A resource-limit table in the layout DEFINE-RESOURCE-LIMITS reads.")

(defparameter *probe-char-class-specs*
  '((probe-digit-p "Return true for an ASCII digit." :ranges ((#x30 #x39)))
    (probe-name-p "Return true for a probe name character." :include probe-digit-p
     :codes (#x5F) :chars (#\: #\a) :ranges ((#x41 #x42)))
    (probe-bare-p "Return true for a probe name character other than a colon."
     :include probe-name-p :exclude-chars (#\:)))
  "A character-class table whose third class includes a class that includes another.")

(defparameter *probe-missing-include-specs*
  '((probe-orphan-p "Return true for nothing." :include probe-missing-p))
  "A character-class table whose only class includes an undefined class.")

(defparameter *probe-range-exclusion-specs*
  '((probe-range-p "Return true for a digit." :ranges ((#x30 #x39)) :exclude-chars (#\5)))
  "A character-class table that excludes a character only a range lists.")

(defparameter *probe-code-exclusion-specs*
  '((probe-code-p "Return true for an underscore." :codes (#x5F) :exclude-chars (#\_)))
  "A character-class table that excludes a character only a code lists.")

(defun evaluate-quietly (form)
  "Evaluate FORM in the test package with redefinition warnings muffled and
return its value. DEFSTRUCT interns its accessors in *PACKAGE*, so the package
must be the one the record names live in, as it is when the library compiles."
  (let ((*package* (find-package '#:cl-xml-kit/test)))
    (handler-bind ((warning #'muffle-warning))
      (eval form))))

(defun expansion-error-message (form)
  "Return the report of the SIMPLE-ERROR that expanding FORM once signals, or
:EXPANDED when it expands. Other errors propagate and fail the test. Names in
the report print relative to the test package."
  (let ((*package* (find-package '#:cl-xml-kit/test)))
    (handler-case (progn (macroexpand-1 form) :expanded)
      (simple-error (condition) (princ-to-string condition)))))

(defparameter *probe-record-definition*
  '(cl-xml-kit::define-immutable-record probe-record
     (:constructor make-probe-record :conc-name %probe-record- :predicate probe-record-p
      :predicate-doc "Return true for a probe record." :structure-doc "A probe record.")
     ((id 0 probe-record-id "Return the id." :identity)
      (items nil probe-record-items "Return the items." :copy-list)
      (label "" probe-record-label "Return the label." :copy-seq)
      (secret nil nil nil :identity)))
  "A record definition with one slot of each copy policy and one private slot.")

(defparameter *probe-limits-definition*
  '(cl-xml-kit::define-resource-limits probe-limits make-probe-limits *probe-limit-specs*
     :default-instance *default-probe-limits*
     :default-instance-doc "The default probe limits."
     :record-doc "Probe limits." :predicate-doc "Return true for probe limits."
     :constructor-doc "Return validated probe limits."))

(describe "message macros"
  (it "expands a message without arguments to its literal text"
    (expect '(cl-xml-kit::xml-message :expected-name)
            :to-expand-to "Expected an XML Name."))

  (it "expands a message with arguments to a FORMAT call on its control string"
    (expect '(cl-xml-kit::xml-message :label-must-be-string label)
            :to-expand-to '(format nil "~A must be a string." label)))

  (it-each (((cl-xml-kit::xml-message :no-such-message)
             "Unknown XML error message ID :NO-SUCH-MESSAGE.")
            ((cl-xml-kit::xml-message :label-must-be-string)
             "Message :LABEL-MUST-BE-STRING needs format arguments."))
      "refuses to expand ~S"
      (form message)
    (expect (expansion-error-message form) :to-equal message))

  (it "expands an error to ERROR with the selected message and the further initargs"
    (expect '(cl-xml-kit::xml-error 'xml-input-error :label-must-be-list
              :format-arguments (label) :condition-initargs (:source source))
            :to-expand-to '(error 'xml-input-error
                            :message (cl-xml-kit::xml-message :label-must-be-list label)
                            :source source)))

  (it "signals the named condition with its message and initargs"
    (expect (lambda ()
              (evaluate-quietly
               '(cl-xml-kit::xml-error 'xml-namespace-error :prefix-not-bound
                 :condition-initargs (:prefix "p"))))
            :to-signal-xml '(xml-namespace-error
                             (xml-namespace-error-prefix "p")
                             (xml-condition-message
                              "A prefixed name has no in-scope namespace binding.")))))

(describe "argument checks"
  (it "returns the accepted value after evaluating its form once"
    (setf *check-argument-evaluations* 0)
    (expect (list (evaluate-quietly
                   '(cl-xml-kit::check-argument
                     (progn (incf *check-argument-evaluations*) 5)
                     integerp xml-input-error :label-must-be-string
                     :format-arguments ("COUNT")))
                  *check-argument-evaluations*)
            :to-equal '(5 1)))

  (it "signals the condition with formatted message and initargs for a refused value"
    (expect (lambda ()
              (evaluate-quietly
               '(cl-xml-kit::check-argument "five" integerp xml-input-error
                 :label-must-be-string :format-arguments ("COUNT")
                 :condition-initargs (:source "probe.xml"))))
            :to-signal-xml '(xml-input-error
                             (xml-condition-source "probe.xml")
                             (xml-condition-message "COUNT must be a string.")))))

(describe "immutable records"
  (it "returns the record name and reads every public slot from positional construction"
    (expect (evaluate-quietly *probe-record-definition*) :to-be 'probe-record)
    (let ((record (funcall 'make-probe-record 7 (list 1 2) "abc" :hidden)))
      (expect (list (funcall 'probe-record-p record) (funcall 'probe-record-id record)
                    (funcall 'probe-record-items record) (funcall 'probe-record-label record))
              :to-equal '(t 7 (1 2) "abc"))))

  (it "returns fresh copies from copy-list and copy-seq readers and the object itself otherwise"
    (evaluate-quietly *probe-record-definition*)
    (let* ((id (list :id))
           (record (funcall 'make-probe-record id (list 1 2) "abc" nil)))
      (setf (cdr (funcall 'probe-record-items record)) nil
            (char (funcall 'probe-record-label record) 0) #\X)
      (expect (list (funcall 'probe-record-items record) (funcall 'probe-record-label record))
              :to-equal '((1 2) "abc"))
      (expect (funcall 'probe-record-id record) :to-be id)))

  (it "defines no reader for a private slot, no writers, and no copier"
    (evaluate-quietly *probe-record-definition*)
    (expect (remove-if-not #'fboundp '(probe-record-secret (setf %probe-record-id)
                                       (setf probe-record-id) copy-probe-record))
            :to-equal nil))

  (it "answers the record predicate false for other objects"
    (evaluate-quietly *probe-record-definition*)
    (expect (mapcar (lambda (value) (funcall 'probe-record-p value)) '(nil 7 "record"))
            :to-equal '(nil nil nil))))

(describe "resource-limit definitions"
  (it "defines default constants and a default instance from the table defaults"
    (evaluate-quietly *probe-limits-definition*)
    (expect (list (symbol-value '+probe-default-max+) (symbol-value '+probe-default-min+)
                  (funcall 'probe-limits-max-probe (symbol-value '*default-probe-limits*))
                  (funcall 'probe-limits-min-probe (symbol-value '*default-probe-limits*)))
            :to-equal '(7 2 7 2)))

  (it "defines a keyword constructor and a predicate for the record"
    (evaluate-quietly *probe-limits-definition*)
    (let ((limits (funcall 'make-probe-limits :min-probe 3)))
      (expect (list (funcall 'probe-limits-p limits) (funcall 'probe-limits-p 3)
                    (funcall 'probe-limits-max-probe limits)
                    (funcall 'probe-limits-min-probe limits))
              :to-equal '(t nil 7 3))))

  (it-each ((:max-probe 0) (:min-probe -1) (:min-probe nil))
      "validates the generated ~S keyword, refusing ~S"
      (kind value)
    (evaluate-quietly *probe-limits-definition*)
    (expect (lambda () (funcall 'make-probe-limits kind value))
            :to-signal-xml `(xml-invalid-resource-limit
                             (xml-invalid-resource-limit-kind ,kind)
                             (xml-invalid-resource-limit-value ,value)))))

(describe "character-class definitions"
  (it-each ((probe-digit-p #\0 t) (probe-digit-p #\9 t) (probe-digit-p #\/ nil)
            (probe-digit-p #\a nil) (probe-name-p #\5 t) (probe-name-p #\_ t)
            (probe-name-p #\: t) (probe-name-p #\a t) (probe-name-p #\B t)
            (probe-name-p #\C nil) (probe-name-p #\b nil) (probe-bare-p #\5 t)
            (probe-bare-p #\_ t) (probe-bare-p #\a t) (probe-bare-p #\A t)
            (probe-bare-p #\: nil))
      "generates ~S answering ~S with ~S"
      (predicate character expected)
    (evaluate-quietly '(cl-xml-kit::define-char-classes *probe-char-class-specs*))
    (expect (funcall predicate character) :to-be expected))

  (it-each ((*probe-missing-include-specs* "Unknown XML character class PROBE-MISSING-P.")
            (*probe-range-exclusion-specs*
             "Character class PROBE-RANGE-P can exclude only listed characters.")
            (*probe-code-exclusion-specs*
             "Character class PROBE-CODE-P can exclude only listed characters."))
      "refuses to expand character classes from ~S"
      (specs-variable message)
    (expect (expansion-error-message (list 'cl-xml-kit::define-char-classes specs-variable))
            :to-equal message)))
