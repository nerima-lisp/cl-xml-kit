(in-package #:cl-xml-kit)

(defstruct (dom-frame
            (:constructor %make-dom-frame (event namespace-declarations))
            (:copier nil))
  "An element whose start event the DOM builder has taken and whose end it awaits."
  event
  namespace-declarations
  children)

(defstruct (dom-builder
            (:constructor %make-dom-builder ())
            (:copier nil))
  "The consumer that builds a document from parser events.

CHILDREN holds the document's children, newest first. TEXT holds the pieces of
the pending character run, newest first, from TEXT-START to TEXT-END."
  frames
  children
  text
  text-start
  text-end
  end-position)

(defun %dom-add-child (builder node)
  (let ((frame (car (dom-builder-frames builder))))
    (if frame
        (push node (dom-frame-children frame))
        (push node (dom-builder-children builder)))))

(defun %dom-join-pieces (pieces)
  "Return the concatenation of the strings PIECES, which are newest first."
  (let ((result (make-string (reduce #'+ pieces :key #'length)))
        (end 0))
    (dolist (piece (reverse pieces) result)
      (replace result piece :start1 end)
      (incf end (length piece)))))

(defun %dom-flush-text (builder)
  (let ((pieces (dom-builder-text builder)))
    (when pieces
      (%dom-add-child builder
                      (%make-xml-text (if (rest pieces)
                                          (%dom-join-pieces pieces)
                                          (first pieces))
                                      (dom-builder-text-start builder)
                                      (dom-builder-text-end builder)))
      (setf (dom-builder-text builder) nil))))

(defun %dom-add-text (builder event)
  (unless (dom-builder-text builder)
    (setf (dom-builder-text-start builder) (xml-event-position event)))
  (push (xml-event-text event) (dom-builder-text builder))
  (setf (dom-builder-text-end builder) (xml-event-position event)))

(defun %dom-close-element (builder event)
  (let* ((frame (pop (dom-builder-frames builder)))
         (start (dom-frame-event frame)))
    (%dom-add-child builder
                    (%make-xml-element (xml-event-name start)
                                       (%xml-event-attributes start)
                                       (dom-frame-namespace-declarations frame)
                                       (nreverse (dom-frame-children frame))
                                       (xml-event-position start)
                                       (xml-event-position event)))))

(defun %dom-doctype (event)
  (let ((body (xml-event-text event))
        (position (xml-event-position event)))
    (multiple-value-bind (start end subset-start subset-end) (%doctype-body-layout body)
      (make-xml-doctype :root-name (subseq body start end)
                        :internal-subset (and subset-start
                                              (subseq body subset-start subset-end))
                        :position position :end-position position))))

(defun %dom-node (event)
  "Return the leaf node for a comment, processing-instruction, or doctype EVENT."
  (let ((position (xml-event-position event)))
    (ecase (xml-event-type event)
      (:comment
       (make-xml-comment :data (xml-event-text event)
                         :position position :end-position position))
      (:processing-instruction
       (make-xml-processing-instruction :target (xml-event-target event)
                                        :data (xml-event-text event)
                                        :position position :end-position position))
      (:doctype (%dom-doctype event)))))

(defun %dom-emit (builder parser event)
  "Take one parser EVENT into BUILDER and return :CONTINUE."
  (let ((type (xml-event-type event)))
    (case type
      (:characters (%dom-add-text builder event))
      ;; A skipped entity adds no node, so the text around it stays one run.
      ((:skipped-external-entity :start-document))
      (t
       (%dom-flush-text builder)
       (case type
         (:start-element
          (push (%make-dom-frame event (xml-parser-frame-namespace-declarations
                                        (car (xml-parser-stack parser))))
                (dom-builder-frames builder)))
         (:end-element (%dom-close-element builder event))
         (:end-document
          (setf (dom-builder-end-position builder) (xml-event-position event)))
         (otherwise (%dom-add-child builder (%dom-node event))))))
    :continue))

(defun %parser-document (parser)
  "Run PARSER to the end through a DOM builder and return the document."
  (let ((builder (%make-dom-builder)))
    (%parser-resume parser (lambda (parser event) (%dom-emit builder parser event)))
    (make-xml-document :children (nreverse (dom-builder-children builder))
                       :position (xml-parser-start-position parser)
                       :end-position (dom-builder-end-position builder))))

(defun parse-xml
    (input &key source-name
               (resource-limits *default-xml-resource-limits*)
               (external-entity-policy :reject))
  "Parse INPUT into an immutable XML document."
  (%parser-document (make-xml-parser input :source-name source-name
                                           :resource-limits resource-limits
                                           :external-entity-policy
                                           external-entity-policy)))
