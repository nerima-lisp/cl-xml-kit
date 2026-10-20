(in-package #:cl-xml-kit/test)

(describe "pull parser"
  (it "returns the events before a late syntax error, then signals it"
    (let ((parser (make-xml-parser "<a><b/></a"))
          (taken nil))
      (expect (lambda ()
                (loop for event = (next-xml-event parser)
                      while event do (push (event-summary event) taken)))
              :to-signal-xml '(xml-well-formedness-error
                               (xml-well-formedness-error-constraint :well-formedness)
                               (xml-well-formedness-error-expected nil)
                               (xml-condition-message "Unexpected end of XML input.")
                               (condition-offset 10)))
      (expect (nreverse taken)
              :to-equal '((:start-document nil nil) (:start-element "a" nil)
                          (:start-element "b" nil) (:end-element "b" nil)))
      (expect (xml-parser-eof-p parser) :to-equal nil)))

  (it "snapshots a mixed prolog and element event sequence"
    (let ((events (mapcar (function xml-event-type)
                          (pull-events
                           "<!--b--><?p d?><!DOCTYPE r><r>a<![CDATA[x]]><x/>b</r><!--e-->"))))
      (let ((cl-weave:*snapshot-directory*
              (asdf:system-relative-pathname
               "cl-xml-kit/test" "t/__snapshots__/")))
        (expect events :to-match-snapshot-sequence "parser/mixed-content"))))

  (it "keeps a parser fatal after malformed input"
    (let ((parser (make-xml-parser "<r>")))
      (expect (lambda () (loop while (next-xml-event parser)))
              :to-signal-xml '(xml-well-formedness-error
                               (xml-well-formedness-error-expected :end-tag)))
      (expect (lambda () (next-xml-event parser))
              :to-signal-xml '(xml-parser-state-error
                               (xml-condition-message "The parser is in a fatal state.")))))

  (it "sets end of input only once end-document has been returned"
    (let ((parser (make-xml-parser "<a/>")))
      (loop repeat 3 do (next-xml-event parser))
      (expect (list (xml-parser-eof-p parser)
                    (xml-event-type (next-xml-event parser))
                    (xml-parser-eof-p parser)
                    (next-xml-event parser))
              :to-equal '(nil :end-document t nil))))

  (it "reports end-document at the end of the input"
    (let ((end (xml-event-position (car (last (pull-events (format nil "<a/>~%")))))))
      (expect (list (xml-position-offset end) (xml-position-line end)
                    (xml-position-column end))
              :to-equal '(5 2 1))))

  (it "names the caller's source in the conditions it signals"
    (expect (lambda () (parse-xml "<r>" :source-name "in.xml"))
            :to-signal-xml '(xml-well-formedness-error
                             (xml-condition-source "in.xml")
                             (condition-offset 3))))
  (it "uses default limits and rejects external DTDs when options are omitted"
    (let ((*default-xml-resource-limits*
            (make-xml-resource-limits :max-elements 1)))
      (let ((parser (make-xml-parser "<a><b/></a>")))
        (expect (lambda () (loop while (next-xml-event parser)))
                :to-signal-xml
                (quote (xml-resource-limit-exceeded
                        (xml-resource-limit-exceeded-kind :max-elements)
                        (xml-resource-limit-exceeded-value 2)
                        (xml-resource-limit-exceeded-limit 1)
                        (xml-condition-source nil)))))
      (let ((parser (make-xml-parser "<!DOCTYPE r SYSTEM \"remote.dtd\"><r/>")))
        (expect (lambda () (loop while (next-xml-event parser)))
                :to-signal-xml
                (quote (xml-external-resource-disabled
                        (xml-external-resource-disabled-kind :external-dtd)
                        (xml-condition-source nil))))))))

(describe "event mapping"
  (it "maps the same events as the pull reader and returns nil"
    (let ((input "<!--c--><r a=\"1\">t<?p d?><x/></r>"))
      (expect (mapcar #'event-summary (mapped-events input))
              :to-equal (mapcar #'event-summary (pull-events input)))
      (expect (map-xml-events #'identity input) :to-equal nil)))

  (it "maps each event before parsing the rest of the input"
    (let ((types nil))
      (expect (lambda ()
                (map-xml-events (lambda (event) (push (xml-event-type event) types))
                                "<a><b/></a"))
              :to-signal-xml '(xml-well-formedness-error
                               (xml-condition-message "Unexpected end of XML input.")))
      (expect (nreverse types)
              :to-equal '(:start-document :start-element :start-element :end-element))))

  (it "passes its parser options to the parser it makes"
    (expect (mapcar #'event-summary
                    (mapped-events (concatenate 'string +skip-doctype+ "<r>&x;</r>")
                                   :external-entity-policy :skip))
            :to-equal '((:start-document nil nil) (:doctype nil nil)
                        (:start-element "r" nil) (:skipped-external-entity "x" nil)
                        (:end-element "r" nil) (:end-document nil nil))))

  (it "iterates events like DOLIST with a result form and an early return"
    (let ((count 0))
      (expect (do-xml-events (event "<r><x/></r>" (list count event))
                (declare (ignorable event))
                (incf count))
              :to-equal '(6 nil)))
    (expect (do-xml-events (event "<r><x/></r>")
              (when (eq (xml-event-type event) :start-element)
                (return (xml-name-local-name (xml-event-name event)))))
            :to-equal "r")))

(describe "argument validation"
  ;; NEXT-XML-EVENT given an event, not a parser, proves that MAP-XML-EVENTS
  ;; called the function a symbol designates.
  (it-each ((make-xml-parser ("<r/>" :source-name 1)
             "SOURCE-NAME must be a string or NIL.")
            (parse-xml ("<r/>" :source-name :file)
             "SOURCE-NAME must be a string or NIL.")
            (make-xml-parser ("<r/>" :resource-limits 3)
             "RESOURCE-LIMITS must be XML-RESOURCE-LIMITS.")
            (make-xml-parser ("<r/>" :external-entity-policy :allow)
             "EXTERNAL-ENTITY-POLICY must be :REJECT or :SKIP.")
            (make-xml-parser ("<r/>" :external-entity-policy :resource-limits)
             "EXTERNAL-ENTITY-POLICY must be :REJECT or :SKIP.")
            (parse-xml ("<r/>" :external-entity-policy :resource-limits)
             "EXTERNAL-ENTITY-POLICY must be :REJECT or :SKIP.")
            (parse-xml ("<r/>" :external-entity-policy nil)
             "EXTERNAL-ENTITY-POLICY must be :REJECT or :SKIP.")
            (map-xml-events (3 "<r/>") "FUNCTION must be a function designator.")
            (map-xml-events ("not a function" "<r/>")
             "FUNCTION must be a function designator.")
            (map-xml-events (next-xml-event "<r/>") "PARSER must be an XML parser.")
            (next-xml-event (3) "PARSER must be an XML parser.")
            (xml-parser-eof-p ("<r/>") "PARSER must be an XML parser."))
      "rejects ~S applied to ~S" (operator arguments message)
    (expect (lambda () (apply operator arguments))
            :to-signal-xml `(xml-input-error (xml-condition-message ,message))))

  (it "reads a fresh parser as not at end of input"
    (expect (xml-parser-eof-p (make-xml-parser "<r/>")) :to-equal nil)))
