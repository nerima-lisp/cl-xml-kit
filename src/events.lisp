;;;; src/events.lisp

(in-package #:cl-xml-kit)

(define-immutable-record xml-event
    (:constructor %make-xml-event
     :conc-name %xml-event-
     :predicate xml-event-p
     :predicate-doc "Return true when VALUE is an XML pull event."
     :structure-doc "An immutable pull-parser event snapshot.")
  ((type nil xml-event-type "Return an XML pull event's type designator." :identity)
   (name nil xml-event-name "Return an event's expanded name, or NIL." :identity)
   (attributes nil xml-event-attributes "Return a fresh snapshot of event attributes."
               :copy-list)
   (text nil xml-event-text "Return event text, or NIL." :identity)
   (target nil xml-event-target "Return a processing-instruction target, or NIL."
           :identity)
   (position nil xml-event-position "Return an event's source position, or NIL."
             :identity)
   (external-p nil xml-event-external-p
               "Return true only for a skipped external entity event." :identity)))

(defun make-xml-event (&key type name attributes text target position external-p)
  "Return an immutable pull event with fields not applicable to TYPE set to NIL."
  (declare (ignore external-p))
  (unless (assoc type +xml-event-types+)
    (xml-error 'xml-input-error :unknown-event-type :format-arguments (type)))
  (when (and name (not (xml-name-p name)))
    (xml-error 'xml-input-error :label-must-be-xml-name :format-arguments ("EVENT NAME")))
  (when (and text (not (stringp text)))
    (xml-error 'xml-input-error :label-must-be-string-or-nil
               :format-arguments ("EVENT TEXT")))
  (when (and target (not (stringp target)))
    (xml-error 'xml-input-error :label-must-be-string-or-nil
               :format-arguments ("EVENT TARGET")))
  (when (and position (not (xml-position-p position)))
    (xml-error 'xml-input-error :label-must-be-xml-position
               :format-arguments ("EVENT POSITION")))
  (%list-value attributes "ATTRIBUTES")
  (dolist (attribute attributes)
    (unless (xml-attribute-p attribute)
      (xml-error 'xml-input-error :event-attributes-invalid)))
  (macrolet ((construct-by-type ()
               `(ecase type
                  ,@(mapcar
                     (lambda (spec)
                       (flet ((field (field form)
                                (and (member field (rest spec)) form)))
                         `(,(first spec)
                           (%make-xml-event type
                                            ,(field :name 'name)
                                            ,(field :attributes '(copy-list attributes))
                                            ,(field :text '(%copy-string text))
                                            ,(field :target '(%copy-string target))
                                            ,(field :position 'position)
                                            ,(field :external-p t)))))
                     +xml-event-types+))))
    (construct-by-type)))

