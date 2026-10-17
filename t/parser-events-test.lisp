(in-package #:cl-xml-kit/test)

(defparameter +skip-doctype+
  "<!DOCTYPE r [<!ENTITY x SYSTEM \"urn:x\"><!ENTITY i \"[&x;]\">]>")

(defun event-summary (event)
  (list (xml-event-type event)
        (let ((name (xml-event-name event))) (and name (xml-name-local-name name)))
        (and (eq (xml-event-type event) :characters) (xml-event-text event))))

(defun mapped-events (input &rest options)
  (let ((events nil))
    (apply #'map-xml-events (lambda (event) (push event events)) input options)
    (nreverse events)))

(it "returns the events before a late syntax error, then signals it"
  (let ((parser (make-xml-parser "<a><b/></a"))
        (taken nil))
    (expect (typep (handler-case
                       (loop for event = (next-xml-event parser)
                             while event do (push (event-summary event) taken))
                     (xml-well-formedness-error (caught) caught))
                   'xml-well-formedness-error)
            :to-be-truthy)
    (expect (nreverse taken)
            :to-equal '((:start-document nil nil) (:start-element "a" nil)
                        (:start-element "b" nil) (:end-element "b" nil)))
    (expect (xml-parser-eof-p parser) :to-equal nil)))

(it "reports end-document at the end of the input"
  (let* ((input (format nil "<a/>~%"))
         (events (parser-events input))
         (end (xml-event-position (car (last events)))))
    (expect (list (xml-position-offset end) (xml-position-line end)
                  (xml-position-column end))
            :to-equal '(5 2 1))
    (expect (xml-position-offset (xml-document-end-position (parse-xml input)))
            :to-equal 5)))

(it "sets end of input only once end-document has been returned"
  (let ((parser (make-xml-parser "<a/>")))
    (loop repeat 3 do (next-xml-event parser))
    (expect (xml-parser-eof-p parser) :to-equal nil)
    (expect (xml-event-type (next-xml-event parser)) :to-equal :end-document)
    (expect (xml-parser-eof-p parser) :to-be-truthy)
    (expect (next-xml-event parser) :to-equal nil)))

(it "emits one skipped-entity event where a skipped external entity is referenced"
  (let* ((input (concatenate 'string +skip-doctype+ "<r>a&x;b</r>"))
         (events (parser-events input :external-entity-policy :skip))
         (skipped (remove-if-not (lambda (event)
                                         (eq (xml-event-type event) :skipped-external-entity))
                                       events)))
    (expect (mapcar #'event-summary events)
            :to-equal '((:start-document nil nil) (:doctype nil nil)
                        (:start-element "r" nil) (:characters nil "a")
                        (:skipped-external-entity "x" nil) (:characters nil "b")
                        (:end-element "r" nil) (:end-document nil nil)))
    (expect (length skipped) :to-equal 1)
    (expect (xml-event-external-p (first skipped)) :to-be-truthy)
    (expect (xml-position-offset (xml-event-position (first skipped)))
            :to-equal (search "&x;" input :from-end t))))

(it "emits a skipped-entity event for a skipped reference after an empty root"
  (expect (mapcar #'event-summary
                  (parser-events (concatenate 'string +skip-doctype+ "<r/>&x;")
                                 :external-entity-policy :skip))
          :to-equal '((:start-document nil nil) (:doctype nil nil)
                      (:start-element "r" nil) (:end-element "r" nil)
                      (:skipped-external-entity "x" nil) (:end-document nil nil))))

(it "emits one skipped-entity event for a skipped entity in replacement text"
  (let* ((input (concatenate 'string +skip-doctype+ "<r>&i;</r>"))
         (events (parser-events input :external-entity-policy :skip))
         (position (xml-event-position
                    (find :skipped-external-entity events :key #'xml-event-type))))
    (expect (mapcar #'event-summary events)
            :to-equal '((:start-document nil nil) (:doctype nil nil)
                        (:start-element "r" nil) (:characters nil "[]")
                        (:skipped-external-entity "x" nil)
                        (:end-element "r" nil) (:end-document nil nil)))
    (expect (list (xml-position-offset position) (xml-position-line position)
                  (xml-position-column position))
            :to-equal '(1 1 2))
    (expect (mapcar #'xml-text-data
                    (xml-element-children
                     (first (last (xml-document-children
                                   (parse-xml input :external-entity-policy :skip))))))
            :to-equal '("[]"))))

(it "emits skipped-entity events at their places in nested replacement text"
  (let* ((input (concatenate 'string
                             "<!DOCTYPE r [<!ENTITY x SYSTEM \"urn:x\">"
                             "<!ENTITY j \"<q>&x;</q>\"><!ENTITY k \"a&j;&#10;&x;b\">]>"
                             "<r>&k;</r>"))
         (events (parser-events input :external-entity-policy :skip)))
    (expect (mapcar #'event-summary events)
            :to-equal `((:start-document nil nil) (:doctype nil nil)
                        (:start-element "r" nil) (:characters nil "a")
                        (:start-element "q" nil) (:skipped-external-entity "x" nil)
                        (:end-element "q" nil) (:characters nil ,(format nil "~%b"))
                        (:skipped-external-entity "x" nil)
                        (:end-element "r" nil) (:end-document nil nil)))
    (expect (mapcar (lambda (event)
                      (let ((position (xml-event-position event)))
                        (list (xml-position-offset position) (xml-position-line position)
                              (xml-position-column position))))
                    (remove-if-not (lambda (event)
                                     (eq (xml-event-type event) :skipped-external-entity))
                                   events))
            :to-equal '((4 1 5) (9 2 1)))))

(it "rejects a colon in a PI target or DOCTYPE name while pulling, as PARSE-XML does"
  (dolist (input '("<?a:b data?><r/>" "<!DOCTYPE a:r><a:r xmlns:a=\"urn:a\"/>"))
    (expect (handler-case (progn (parser-events input) nil)
              (xml-input-error (caught) (xml-condition-message caught)))
            :to-equal (handler-case (progn (parse-xml input) nil)
                        (xml-input-error (caught) (xml-condition-message caught))))
    (expect (handler-case (progn (parser-events input) nil)
              (xml-input-error () t))
            :to-be-truthy)))

(it "maps the same events as the pull reader and returns nil"
  (let ((input "<!--c--><r a=\"1\">t<?p d?><x/></r>"))
    (expect (mapcar #'event-summary (mapped-events input))
            :to-equal (mapcar #'event-summary (parser-events input)))
    (expect (map-xml-events #'identity input) :to-equal nil)))

(it "maps each event before parsing the rest of the input"
  (let ((types nil))
    (expect (typep (handler-case
                       (map-xml-events (lambda (event)
                                         (push (xml-event-type event) types))
                                       "<a><b/></a")
                     (xml-well-formedness-error (caught) caught))
                   'xml-well-formedness-error)
            :to-be-truthy)
    (expect (nreverse types)
            :to-equal '(:start-document :start-element :start-element :end-element))))

(it "iterates events like DOLIST with a result form and an early return"
  (let ((count 0))
    (expect (do-xml-events (event "<r><x/></r>" (list count event))
              (declare (ignorable event))
              (incf count))
            :to-equal '(6 nil)))
  (expect (do-xml-events (event "<r><x/></r>")
            (when (eq (xml-event-type event) :start-element)
              (return (xml-name-local-name (xml-event-name event)))))
          :to-equal "r"))

(it "parses nesting that recursive descent would exhaust the default stack on"
  (let* ((depth 60000)
         (input (with-output-to-string (stream)
                  (loop repeat depth do (write-string "<a>" stream))
                  (loop repeat depth do (write-string "</a>" stream))))
         (limits (make-xml-resource-limits :max-depth depth))
         (element (first (xml-document-children
                          (parse-xml input :resource-limits limits))))
         (levels 1))
    (loop for children = (xml-element-children element)
          while children
          do (setf element (first children))
             (incf levels))
    (expect levels :to-equal depth)
    (expect (handler-case (progn (parse-xml (concatenate 'string "<a>" input "</a>")
                                            :resource-limits limits)
                                 nil)
              (xml-resource-limit-exceeded (caught)
                (xml-resource-limit-exceeded-kind caught)))
            :to-equal :max-depth)))
