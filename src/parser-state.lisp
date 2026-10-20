(in-package #:cl-xml-kit)

(defstruct (xml-parser-frame
            (:constructor %make-xml-parser-frame
                (lexical-name name namespace-declarations context end-position))
            (:constructor %make-xml-parser-frame-defaults ())
            (:copier nil))
  "One open element, or the boundary of an entity replacement when NAME is NIL.

END-POSITION is the position of `/>` for an empty element and NIL otherwise."
  (lexical-name nil :read-only t)
  (name nil :read-only t)
  (namespace-declarations nil :read-only t)
  (context nil :read-only t)
  (end-position nil :read-only t))

(defstruct (xml-parser
            (:constructor %make-xml-parser-structure
                (input source-name limits external-entity-policy tokenizer
                 tokenizer-stack start-position phase stack depth fatal root-seen
                 root-closed doctype-root doctype-seen element-count text-length
                 entities token-observer entity-skips))
            (:constructor %make-xml-parser-defaults ())
            (:copier nil))
  "Forward-only parser state; it is also the continuation of the parse.

PHASE is :START before the start-document event, :RUNNING while tokens are
read, :EMPTY-END between the start and end events of an empty element, and
:DONE after the end-document event. STACK holds the open frames, innermost
first, and DEPTH counts the element frames among them. TEXT-LENGTH is the
length of the character run the current events continue. TOKEN-OBSERVER, when
set, is called with each token read from the document itself, before the
parser interprets it. ENTITY-SKIPS lists, in order, the (OFFSET . NAME) of the
external entities skipped in the replacement text being read whose events are
still due."
  (input "" :read-only t)
  (source-name nil :read-only t)
  (limits nil :read-only t)
  (external-entity-policy :reject :read-only t)
  tokenizer
  tokenizer-stack
  (start-position nil :read-only t)
  phase
  stack
  depth
  fatal
  root-seen
  root-closed
  doctype-root
  doctype-seen
  element-count
  text-length
  entities
  token-observer
  entity-skips)

(defun %make-xml-parser
    (input source-name limits external-entity-policy tokenizer
     start-position entities)
  (%make-xml-parser-structure
   input source-name limits external-entity-policy tokenizer
   nil start-position :start nil 0 nil nil nil nil nil 0 0 entities nil nil))

(defun %parser-current-position (parser)
  (%xml-tokenizer-current-position (xml-parser-tokenizer parser)))

(defun %parser-limit-error (parser kind value limit &optional position)
  (error 'xml-resource-limit-exceeded
         :message (xml-message :resource-limit-exceeded kind)
         :kind kind :value value :limit limit
         :source (xml-parser-source-name parser)
         :position (or position (%parser-current-position parser))))

(defun %parser-well-formedness-error (parser message constraint expected position)
  (error 'xml-well-formedness-error
         :message message :constraint constraint :expected expected
         :source (xml-parser-source-name parser)
         :position position))

(defun %parser-token-error (parser token message constraint expected)
  (%parser-well-formedness-error parser message constraint expected
                                  (xml-token-position token)))

(defun %parser-event (parser type &key name attributes text target position)
  "Return the event of TYPE that the parser hands to its consumer.

Every parser event is made here. An event other than characters or a skipped
entity ends the current character run."
  (unless (or (eq type :characters) (eq type :skipped-external-entity))
    (setf (xml-parser-text-length parser) 0))
  (%make-xml-event type name attributes text target position
                   (eq type :skipped-external-entity)))

(defun %parser-next-token (parser)
  "Return the next token, leaving an entity replacement at its EOF token."
  (let ((token (next-xml-token (xml-parser-tokenizer parser)))
        (observer (xml-parser-token-observer parser)))
    (if (xml-parser-tokenizer-stack parser)
        (when (eq (xml-token-kind token) :eof)
          (setf (xml-parser-tokenizer parser)
                (pop (xml-parser-tokenizer-stack parser))))
        (when observer
          (funcall observer token)))
    token))

(defun %parser-push-tokenizer (parser source)
  (push (xml-parser-tokenizer parser) (xml-parser-tokenizer-stack parser))
  (setf (xml-parser-tokenizer parser)
        (make-xml-tokenizer source
                            :source (xml-parser-source-name parser)
                            :limits (xml-parser-limits parser))))
