;;;; src/edit.lisp
;;;;
;;;; Source-preserving edits. Each edit replaces one range of the caller's
;;;; string and leaves every other character as it was; `%edit-finish` verifies
;;;; the result before it is returned.

(in-package #:cl-xml-kit)

(defun edit-xml-set (source path value &key (resource-limits *default-xml-resource-limits*)
                                            (external-entity-policy :reject))
  "Return SOURCE with the target of PATH set to VALUE, changing nothing else.

PATH must select exactly one target.
`.../@name`: an existing attribute keeps its quote character and only its value
text changes; a missing attribute is inserted after the last one.
`.../text()`: the whole content of the element is replaced by VALUE as character
data, or as a CDATA section when the content was exactly one; content that holds
elements, comments, or processing instructions is refused.
An element path: VALUE is a fragment of exactly one element that replaces the
element; it must parse where it is written, with the namespaces in scope there.
The result is parsed again and must equal the original document with the change
applied, otherwise XML-UNSUPPORTED-EDIT with reason :VERIFICATION-FAILED."
  (let ((parsed (%path-parse path)))
    (when (eq (parsed-path-target parsed) :attribute)
      (%edit-refuse-namespace-declaration path parsed))
    (%edit-check-value path value)
    (let ((context (%path-context source path
                                  :resource-limits resource-limits
                                  :external-entity-policy external-entity-policy)))
      (ecase (parsed-path-target parsed)
        (:attribute (%edit-set-attribute context parsed value))
        (:text (%edit-set-text context parsed value))
        (:element (%edit-replace-element context parsed value))))))

(defun edit-xml-delete (source path &key (resource-limits *default-xml-resource-limits*)
                                         (external-entity-policy :reject))
  "Return SOURCE without the target of PATH, changing nothing else.

PATH must select exactly one target. For `.../@name` the attribute and the
whitespace before it are removed. For an element path the element is removed,
with the whitespace and line break before it when nothing else is on its line;
the document element cannot be removed. A `text()` path is not a deletable
target and signals XML-PATH-ERROR with reason :WRONG-TARGET."
  (let ((parsed (%path-parse path)))
    (when (eq (parsed-path-target parsed) :text)
      (xml-error 'xml-path-error :text-path-not-deletable
                 :condition-initargs (:path path :reason :wrong-target)))
    (when (eq (parsed-path-target parsed) :attribute)
      (%edit-refuse-namespace-declaration path parsed))
    (let ((context (%path-context source path
                                  :resource-limits resource-limits
                                  :external-entity-policy external-entity-policy)))
      (ecase (parsed-path-target parsed)
        (:attribute (%edit-delete-attribute context parsed))
        (:element (%edit-delete-element context parsed))))))

(defun edit-xml-append (source path fragment
                        &key (resource-limits *default-xml-resource-limits*)
                             (external-entity-policy :reject))
  "Return SOURCE with the element FRAGMENT added as the last child of the element at PATH.

PATH must select exactly one element, otherwise XML-PATH-ERROR with reason
:WRONG-TARGET for an attribute or text() path, :NO-MATCH, or :MULTIPLE-MATCHES.
FRAGMENT must parse as exactly one element where it is written, with the
namespaces in scope there; surrounding XML whitespace is dropped. It goes after
the last child that is not whitespace, preceded by the line break and indentation
that precede the last indented element child, unless that child is character data,
which the indentation would change; an empty element gets it between a new start
tag end and end tag, and content that is only whitespace keeps it before the
fragment. The result is parsed again and must equal the original document
with the element added."
  (let ((parsed (%path-parse path)))
    (unless (eq (parsed-path-target parsed) :element)
      (xml-error 'xml-path-error :append-needs-element
                 :condition-initargs (:path path :reason :wrong-target)))
    (%edit-check-value path fragment)
    (%edit-append-element (%path-context source path
                                         :resource-limits resource-limits
                                         :external-entity-policy external-entity-policy)
                          parsed fragment)))
