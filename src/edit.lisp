;;;; src/edit.lisp
;;;;
;;;; Source-preserving edits replace one range of the caller's string and leave
;;;; every other character as it was. `%edit-finish` verifies a candidate before
;;;; passing it to the success callback.

(in-package #:cl-xml-kit)

(defun edit-xml-set (source path value success failure
                       &key (resource-limits *default-xml-resource-limits*)
                         (external-entity-policy :reject))
  "Call SUCCESS once with the edited source, or FAILURE once with the condition.

PATH must select exactly one target. For a missing attribute, the element steps
in PATH must resolve exactly one owner even though its final attribute need not
exist. For an attribute path, an existing attribute keeps its quote character
and only its value changes; a missing attribute is inserted after the last
attribute, or after the element name if there are none. Namespace declarations
cannot be edited. VALUE must be a string containing only XML characters. A text
path replaces the element content with character data. It uses a CDATA section
only when the original content was exactly one CDATA section and VALUE contains
neither ]]> nor a carriage return; otherwise it writes escaped character data.
The edit is refused when the content includes an element, comment, processing
instruction, or markup entity. An element path replaces that element with
exactly one element parsed from VALUE using the namespaces in scope there.
Verification ignores source positions and merges adjacent text nodes. A
structural mismatch or parse error during verification is reported as
XML-UNSUPPORTED-EDIT with reason :VERIFICATION-FAILED; exceeding a resource
limit is reported as XML-EDIT-VALUE-ERROR with reason :RESOURCE-LIMIT."
  (%path-parse
   path
   (lambda (parsed)
     (%call-with-callbacks
      (lambda ()
        (when (eq (parsed-path-target parsed) :attribute)
          (%edit-refuse-namespace-declaration path parsed))
        (%edit-check-value path value)
        parsed)
      (lambda (parsed)
        (%path-context
         source path
         (lambda (context)
           (ecase (parsed-path-target parsed)
             (:attribute (%edit-set-attribute context parsed value success failure))
             (:text (%edit-set-text context parsed value success failure))
             (:element (%edit-replace-element context parsed value success failure))))
         failure
         :resource-limits resource-limits
         :external-entity-policy external-entity-policy))
      failure))
   failure))

(defun edit-xml-delete (source path success failure
                          &key (resource-limits *default-xml-resource-limits*)
                            (external-entity-policy :reject))
  "Call SUCCESS once with the source after deletion, or FAILURE once with the
condition.

PATH must select exactly one target. For an attribute path, the attribute and
its preceding whitespace are removed; namespace declarations cannot be edited.
For an element path, the element is removed. If the immediately preceding
sibling is a text run containing only XML whitespace written as ordinary text
in SOURCE (not in CDATA or a reference), and the next sibling is absent,
non-text, or another run with the same properties, deletion also removes spaces
and tabs immediately before the element and the preceding line break when
present. The document element cannot be removed. A text path fails with
XML-PATH-ERROR with reason :WRONG-TARGET. Verification ignores source positions,
merges adjacent text nodes, and ignores whitespace-only text among the children
of the deleted element parent. A structural mismatch or parse error during
verification is reported as XML-UNSUPPORTED-EDIT with reason
:VERIFICATION-FAILED; exceeding a resource limit is reported as
XML-EDIT-VALUE-ERROR with reason :RESOURCE-LIMIT."
  (%path-parse
   path
   (lambda (parsed)
     (%call-with-callbacks
      (lambda ()
        (when (eq (parsed-path-target parsed) :text)
          (xml-error 'xml-path-error :text-path-not-deletable
                     :condition-initargs (:path path :reason :wrong-target)))
        (when (eq (parsed-path-target parsed) :attribute)
          (%edit-refuse-namespace-declaration path parsed))
        parsed)
      (lambda (parsed)
        (%path-context
         source path
         (lambda (context)
           (ecase (parsed-path-target parsed)
             (:attribute (%edit-delete-attribute context parsed success failure))
             (:element (%edit-delete-element context parsed success failure))))
         failure
         :resource-limits resource-limits
         :external-entity-policy external-entity-policy))
      failure))
   failure))

(defun edit-xml-append (source path fragment success failure
                          &key (resource-limits *default-xml-resource-limits*)
                            (external-entity-policy :reject))
  "Call SUCCESS once with the source after the append, or FAILURE once with the
condition.

PATH must select exactly one element; an attribute or text path fails with
XML-PATH-ERROR with reason :WRONG-TARGET. FRAGMENT must parse as exactly one
element using the namespaces in scope there; surrounding XML whitespace is
removed. The insertion point follows the last child that is not a text run
containing only XML whitespace written as ordinary text in SOURCE (not in
CDATA or a reference). When the child before the insertion point is not text,
the fragment reuses the line break and indentation before the last element
child with such indentation, when present. When that child is text, no existing
indentation is reused. An empty element is expanded. If the element contains
only whitespace text, that text stays before the fragment. Verification ignores
source positions, merges adjacent text nodes, and ignores whitespace-only text
among the children of the element receiving the appended element. A structural
mismatch or parse error during verification is reported as
XML-UNSUPPORTED-EDIT with reason :VERIFICATION-FAILED; exceeding a resource
limit is reported as XML-EDIT-VALUE-ERROR with reason :RESOURCE-LIMIT."
  (%path-parse
   path
   (lambda (parsed)
     (%call-with-callbacks
      (lambda ()
        (unless (eq (parsed-path-target parsed) :element)
          (xml-error 'xml-path-error :append-needs-element
                     :condition-initargs (:path path :reason :wrong-target)))
        (%edit-check-value path fragment)
        parsed)
      (lambda (parsed)
        (%path-context
         source path
         (lambda (context)
           (%edit-append-element context parsed fragment success failure))
         failure
         :resource-limits resource-limits
         :external-entity-policy external-entity-policy))
      failure))
   failure))
