(in-package #:cl-xml-kit)

(defstruct (xml-parser-frame
            (:constructor %make-xml-parser-frame
                (lexical-name name attributes namespace-declarations context
                 children position empty-p text-length element end-position))
            (:copier nil))
  "Mutable state for one open XML element."
  (lexical-name "" :read-only t)
  (name nil :read-only t)
  (attributes nil :read-only t)
  (namespace-declarations nil :read-only t)
  (context nil :read-only t)
  (children nil)
  (position nil :read-only t)
  (empty-p nil :read-only t)
  (text-length 0)
  (element nil)
  (end-position nil))

(defstruct (xml-parser
            (:constructor %make-xml-parser
                (input source-name limits external-entity-policy tokenizer
                 position line column start-position phase stack document-children
                 queue eof fatal root-seen root-closed doctype-root doctype-seen
                 element-count entities entity-expansions expanded-characters
                 entity-depth tokenizer-stack))
            (:copier nil))
  "Mutable forward-only parser state owned by the pull reader."
  (input "" :read-only t)
  (source-name nil :read-only t)
  (limits nil :read-only t)
  (external-entity-policy :reject :read-only t)
  (tokenizer nil)
  (tokenizer-stack nil)
  (position 0)
  (line 1)
  (column 1)
  (start-position nil :read-only t)
  (phase :prolog)
  (stack nil)
  (document-children nil)
  (queue nil)
  (eof nil)
  (fatal nil)
  (root-seen nil)
  (root-closed nil)
  (doctype-root nil)
  (doctype-seen nil)
  (element-count 0)
  (entities nil)
  (entity-expansions 0)
  (expanded-characters 0)
  (entity-depth 0))

(defun %parser-current-position (parser)
  (make-xml-position :source (xml-parser-source-name parser)
                     :offset (xml-parser-position parser)
                     :line (xml-parser-line parser)
                     :column (xml-parser-column parser)))

(defun %parser-limit-error (parser kind value limit &optional position)
  (error 'xml-resource-limit-exceeded
         :message (format nil "XML resource limit ~S exceeded." kind)
         :kind kind :value value :limit limit
         :source (xml-parser-source-name parser)
         :position (or position (%parser-current-position parser))))

(defun %parser-well-formedness-error (parser message constraint expected
                                      &optional position)
  (error 'xml-well-formedness-error
         :message message :constraint constraint :expected expected
         :source (xml-parser-source-name parser)
         :position (or position (%parser-current-position parser))))

(defun %parser-input-error (parser message &optional position)
  (error 'xml-input-error :message message
         :source (xml-parser-source-name parser)
         :position (or position (%parser-current-position parser))))

