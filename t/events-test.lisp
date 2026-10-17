(in-package #:cl-xml-kit/test)

(defun event-with-every-field (type)
  "Return an event of TYPE built with every field supplied, so that the fields the
event keeps show which ones the constructor treats as applicable to TYPE."
  (make-xml-event :type type
                  :name (make-xml-name :local-name "root")
                  :attributes (list (make-xml-attribute
                                     :name (make-xml-name :local-name "id") :value "1"))
                  :text "text" :target "pi"
                  :position (make-xml-position :source "events.xml" :offset 4)
                  :external-p t))

(defun event-field-values (event)
  "Return the name, attributes, text, target, position, and external flag of EVENT,
each reduced to a value a literal row can state."
  (list (and (xml-event-name event) (xml-name-local-name (xml-event-name event)))
        (mapcar #'xml-attribute-value (xml-event-attributes event))
        (xml-event-text event)
        (xml-event-target event)
        (and (xml-event-position event) (xml-position-offset (xml-event-position event)))
        (xml-event-external-p event)))

;; The fields each type keeps, from the event table in docs/design/architecture.md
;; section 1; the external flag is true only for a skipped external entity, as
;; XML-EVENT-EXTERNAL-P documents. Columns: name attributes text target position flag.
(it-each ((:start-document (nil nil nil nil 4 nil))
          (:doctype (nil nil "text" nil 4 nil))
          (:start-element ("root" ("1") nil nil 4 nil))
          (:characters (nil nil "text" nil 4 nil))
          (:comment (nil nil "text" nil 4 nil))
          (:processing-instruction (nil nil "text" "pi" 4 nil))
          (:end-element ("root" nil nil nil 4 nil))
          (:skipped-external-entity ("root" nil nil nil 4 t))
          (:end-document (nil nil nil nil 4 nil)))
    "keeps only the fields applicable to a ~S event"
    (type fields)
  (let ((event (event-with-every-field type)))
    (expect (list (xml-event-p event) (xml-event-type event)) :to-equal (list t type))
    (expect (event-field-values event) :to-equal fields)))

(it "marks a skipped external entity as external even when the caller says otherwise"
  (expect (xml-event-external-p
           (make-xml-event :type :skipped-external-entity
                           :name (make-xml-name :local-name "e") :external-p nil))
          :to-be t))

(it "leaves every optional field NIL when only the type is given"
  (expect (event-field-values (make-xml-event :type :start-element))
          :to-equal '(nil nil nil nil nil nil)))

(it "returns a fresh attribute list on every read"
  (let* ((event (event-with-every-field :start-element))
         (attributes (xml-event-attributes event)))
    (setf (cdr attributes) (list :extra))
    (expect (length (xml-event-attributes event)) :to-equal 1)))

(it "copies the text and target strings it is given"
  (let* ((text (copy-seq "data"))
         (target (copy-seq "pi"))
         (event (make-xml-event :type :processing-instruction :text text :target target)))
    (setf (char text 0) #\X (char target 0) #\X)
    (expect (list (xml-event-text event) (xml-event-target event))
            :to-equal '("data" "pi"))))

(it-each ((:unknown :text nil "Unknown XML event type :UNKNOWN.")
          (nil :text nil "Unknown XML event type NIL.")
          (:characters :text 1 "EVENT TEXT must be a string or NIL.")
          (:processing-instruction :target 1 "EVENT TARGET must be a string or NIL.")
          (:start-document :position :start "EVENT POSITION must be an XML-POSITION.")
          (:start-element :name "root" "EVENT NAME must be an XML-NAME.")
          (:start-element :attributes :not-a-list "ATTRIBUTES must be a list.")
          (:start-element :attributes (:not-an-attribute)
           "EVENT ATTRIBUTES must contain XML-ATTRIBUTE values."))
    "refuses a ~S event whose ~S is ~S"
    (type key value message)
  (expect (lambda () (make-xml-event :type type key value))
          :to-signal-xml `(xml-input-error (xml-condition-message ,message))))
