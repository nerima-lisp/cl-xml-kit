;;;; src/events.lisp

(in-package #:cl-xml-kit)

(defparameter +xml-event-types+
  '(:start-document :doctype :start-element :characters :comment
    :processing-instruction :end-element :skipped-external-entity :end-document)
  "The event type designators defined by the pull reader contract.")

(defstruct (xml-event
            (:constructor %make-xml-event
                (type name attributes text target position external-p))
            (:conc-name %xml-event-)
            (:copier nil))
  "An immutable pull-parser event snapshot."
  (type nil :read-only t)
  (name nil :read-only t)
  (attributes nil :read-only t)
  (text nil :read-only t)
  (target nil :read-only t)
  (position nil :read-only t)
  (external-p nil :read-only t))

(defun make-xml-event (&key type name attributes text target position external-p)
  "Return an immutable pull event with fields not applicable to TYPE set to NIL."
  (declare (ignore external-p))
  (unless (member type +xml-event-types+)
    (error 'xml-input-error
           :message (format nil "Unknown XML event type ~S." type)))
  (when (and name (not (xml-name-p name)))
    (error 'xml-input-error :message "EVENT NAME must be an XML-NAME."))
  (when (and text (not (stringp text)))
    (error 'xml-input-error :message "EVENT TEXT must be a string or NIL."))
  (when (and target (not (stringp target)))
    (error 'xml-input-error :message "EVENT TARGET must be a string or NIL."))
  (when (and position (not (xml-position-p position)))
    (error 'xml-input-error :message "EVENT POSITION must be an XML-POSITION."))
  (%list-value attributes "ATTRIBUTES")
  (dolist (attribute attributes)
    (unless (xml-attribute-p attribute)
      (error 'xml-input-error :message "EVENT ATTRIBUTES must contain XML-ATTRIBUTE values.")))
  (case type
    ((:start-document :end-document)
     (%make-xml-event type nil nil nil nil position nil))
    (:doctype
     (%make-xml-event type nil nil (%copy-string text) nil position nil))
    (:start-element
     (%make-xml-event type name (copy-list attributes) nil nil position nil))
    ((:characters :comment)
     (%make-xml-event type nil nil (%copy-string text) nil position nil))
    (:processing-instruction
     (%make-xml-event type nil nil (%copy-string text) (%copy-string target)
                      position nil))
    (:end-element
     (%make-xml-event type name nil nil nil position nil))
    (:skipped-external-entity
     (%make-xml-event type name nil nil nil position t))))

(defun xml-event-type (event) (%xml-event-type event))
(defun xml-event-name (event) (%xml-event-name event))
(defun xml-event-attributes (event) (copy-list (%xml-event-attributes event)))
(defun xml-event-text (event) (%xml-event-text event))
(defun xml-event-target (event) (%xml-event-target event))
(defun xml-event-position (event) (%xml-event-position event))
(defun xml-event-external-p (event) (%xml-event-external-p event))

(loop for (name . text) in
      '((xml-event-p . "Return true when VALUE is an XML pull event.")
        (xml-event-type . "Return an XML pull event's type designator.")
        (xml-event-name . "Return an event's expanded name, or NIL.")
        (xml-event-attributes . "Return a fresh snapshot of event attributes.")
        (xml-event-text . "Return event text, or NIL.")
        (xml-event-target . "Return a processing-instruction target, or NIL.")
        (xml-event-position . "Return an event's source position, or NIL.")
        (xml-event-external-p . "Return true only for a skipped external entity event."))
      do (setf (documentation name 'function) text))

