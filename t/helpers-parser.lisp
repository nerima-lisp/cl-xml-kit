(in-package #:cl-xml-kit/test)

(defparameter +skip-doctype+
  "<!DOCTYPE r [<!ENTITY x SYSTEM \"urn:x\"><!ENTITY i \"[&x;]\">]>"
  "A DOCTYPE declaring the external entity x and the internal entity i, whose
replacement text references x.")

(defun pull-events (input &rest parser-options)
  "Return every event of INPUT, taken with NEXT-XML-EVENT until it returns NIL.
PARSER-OPTIONS are passed to MAKE-XML-PARSER. A condition signalled while pulling
propagates, so a test that expects a complete event list fails on it."
  (let ((parser (apply #'make-xml-parser input parser-options)))
    (loop for event = (next-xml-event parser)
          while event collect event)))

(defun mapped-events (input &rest parser-options)
  "Return the events MAP-XML-EVENTS passes for INPUT, in the order it passes them.
A condition signalled by MAP-XML-EVENTS propagates."
  (let ((events nil))
    (apply #'map-xml-events (lambda (event) (push event events)) input parser-options)
    (nreverse events)))

(defun event-summary (event)
  "Return (TYPE LOCAL-NAME TEXT) for EVENT.
LOCAL-NAME is NIL for an event without a name, and TEXT is NIL for any event
other than :CHARACTERS, so a summary list compares under EQUAL."
  (list (xml-event-type event)
        (let ((name (xml-event-name event)))
          (and name (xml-name-local-name name)))
        (and (eq (xml-event-type event) :characters) (xml-event-text event))))

(defun document-element (input &rest parse-options)
  "Return the root element of the document PARSE-XML makes from INPUT.
Signals whatever PARSE-XML signals; returns NIL when the document has no element
child, which makes the element accessors of the calling test signal a type error."
  (find-if #'xml-element-p
           (xml-document-children (apply #'parse-xml input parse-options))))

(defun condition-offset (condition)
  "Return the offset of the position CONDITION carries, or NIL when it carries none.
Used as a reader in :TO-SIGNAL-XML rows, where a position object would never be
EQUAL to a literal."
  (let ((position (xml-condition-position condition)))
    (and position (xml-position-offset position))))

(defun position-offsets (&rest positions)
  "Return the offset of each XML-POSITION in POSITIONS.
A NIL or non-position argument signals a type error, which fails the calling test."
  (mapcar #'xml-position-offset positions))

(defun text-content (element)
  "Return the concatenated data of the text children of ELEMENT, ignoring other children.
A non-element argument signals a type error, which fails the calling test."
  (apply #'concatenate 'string
         (mapcar #'xml-text-data (remove-if-not #'xml-text-p (xml-element-children element)))))
