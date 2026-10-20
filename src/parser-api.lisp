(in-package #:cl-xml-kit)

(defun make-xml-parser
    (input &rest options
           &key source-name resource-limits external-entity-policy)
  "Create a forward-only parser for XML input.

The input is decoded and line-end normalized here; tokenizing and parsing
happen one event at a time as the events are taken."
  (unless (member :resource-limits options)
    (setf resource-limits *default-xml-resource-limits*))
  (unless (member :external-entity-policy options)
    (setf external-entity-policy :reject))
  (unless (or (null source-name) (stringp source-name))
    (xml-error 'xml-input-error :label-must-be-string-or-nil
               :format-arguments ("SOURCE-NAME")))
  (check-argument resource-limits xml-resource-limits-p xml-input-error
                  :label-must-be-resource-limits
                  :format-arguments ("RESOURCE-LIMITS"))
  (unless (member external-entity-policy '(:reject :skip))
    (xml-error 'xml-input-error :external-entity-policy-invalid))
  (let* ((xml-input (make-xml-input input :source source-name
                                          :limits resource-limits))
         (text (xml-input-text xml-input))
         (source (xml-input-source xml-input)))
    (%make-xml-parser
     text source resource-limits external-entity-policy
     (make-xml-tokenizer text :source source :limits resource-limits)
     (make-xml-position :source source :offset 0 :line 1 :column 1)
     (make-xml-entity-context :resource-limits resource-limits
                              :external-entity-policy external-entity-policy
                              :source source))))

(defun %parser-resume (parser emit)
  "Continue PARSER, calling EMIT with PARSER and each event until EMIT returns :YIELD.

Return the yielded event, or NIL once the document has ended. EMIT returns
:CONTINUE to take the next event. PARSER itself holds the rest of the parse,
so neither the events nor the host stack grow with the document. The first
XML-KIT-ERROR makes PARSER fatal."
  (when (xml-parser-fatal parser)
    (xml-error 'xml-parser-state-error :parser-fatal
               :condition-initargs (:parser parser)))
  (handler-bind ((xml-kit-error
                   (lambda (condition)
                     (setf (xml-parser-fatal parser) condition))))
    (loop for event = (%parser-step parser)
          when (or (null event) (eq (funcall emit parser event) :yield))
            return event)))

(defun %yield-event (parser event)
  (declare (ignore parser event))
  :yield)

(defun %continue-event (parser event)
  (declare (ignore parser event))
  :continue)

(defun next-xml-event (parser)
  "Return the next pull event, or NIL after END-DOCUMENT.

Each call parses only as far as the event it returns, so the events before a
well-formedness error are returned before the call that signals it."
  (check-argument parser xml-parser-p xml-input-error :parser-required)
  (%parser-resume parser #'%yield-event))

(defun xml-parser-eof-p (parser)
  "Return true when PARSER has emitted END-DOCUMENT."
  (check-argument parser xml-parser-p xml-input-error :parser-required)
  (eq (xml-parser-phase parser) :done))

(defun map-xml-events (function input &rest parser-options)
  "Call FUNCTION on each event of INPUT in document order and return NIL.

PARSER-OPTIONS are the keyword arguments of MAKE-XML-PARSER. FUNCTION sees
each event before the rest of the document is parsed."
  (unless (or (functionp function) (symbolp function))
    (xml-error 'xml-input-error :label-must-be-function
               :format-arguments ("FUNCTION")))
  (let ((parser (apply #'make-xml-parser input parser-options)))
    (%parser-resume parser (lambda (parser event)
                             (declare (ignore parser))
                             (funcall function event)
                             :continue))
    nil))

(defmacro do-xml-events ((event input &optional result) &body body)
  "Evaluate BODY with EVENT bound to each event of INPUT, then return RESULT.

Like DOLIST, the iteration is in a block named NIL and EVENT is NIL while
RESULT is evaluated."
  `(block nil
     (map-xml-events (lambda (,event) ,@body) ,input)
     (let ((,event nil))
       (declare (ignorable ,event))
       ,result)))
