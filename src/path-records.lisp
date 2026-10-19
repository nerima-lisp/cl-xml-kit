(in-package #:cl-xml-kit)
(defun %call-with-callbacks (thunk success failure)
  (multiple-value-bind (result condition)
      (handler-case
          (values (multiple-value-list (funcall thunk)) nil)
        (error (condition) (values nil condition)))
    (if condition
        (funcall failure condition)
        (apply success result))))

(defstruct (path-step
            (:constructor %make-path-step (name index))
            (:copier nil))
  "One element step. NAME is a lexical qualified name or :ANY, INDEX a 1-based
position among the children passing the name test, or NIL."
  (name nil :read-only t)
  (index nil :read-only t))

(defstruct (parsed-path
            (:constructor %make-parsed-path (steps target attribute-name))
            (:copier nil))
  "A parsed path: its element STEPS, then a TARGET of :ELEMENT, :ATTRIBUTE, or
:TEXT. ATTRIBUTE-NAME is the lexical name of an :ATTRIBUTE target."
  (steps nil :read-only t)
  (target :element :read-only t)
  (attribute-name nil :read-only t))

(defstruct (path-context
            (:constructor %make-path-context
                (source path dom spans resource-limits external-entity-policy))
            (:copier nil))
  "A parsed and scanned SOURCE that paths resolve against.

PATH is the caller's path string, kept for conditions. DOM is the PARSE-XML
document of SOURCE without a leading U+FEFF, and SPANS the span tree of SOURCE
itself. The options are those the caller passed."
  (source "" :read-only t)
  (path "" :read-only t)
  (dom nil :read-only t)
  (spans nil :read-only t)
  (resource-limits nil :read-only t)
  (external-entity-policy :reject :read-only t))

(defstruct (path-hit
            (:constructor %make-path-hit (element parent chain ordinals steps))
            (:copier nil))
  "An element reached by a path.

ELEMENT is the span element and PARENT the span element that contains it, NIL for
the document element. CHAIN lists the DOM elements from the matching one outward
to the document element. ORDINALS lists the 1-based positions among all element
children that lead from the document element to it, and STEPS the canonical step
strings, both innermost first."
  (element nil :read-only t)
  (parent nil :read-only t)
  (chain nil :read-only t)
  (ordinals nil :read-only t)
  (steps nil :read-only t))

(define-immutable-record xml-match
    (:constructor %make-xml-match
     :conc-name %xml-match-
     :predicate xml-match-p
     :predicate-doc "Return true when VALUE is an XML-MATCH."
     :structure-doc "One element, attribute, or text target that a path selected in a source string.

START and END are indices into that string, END exclusive.")
  ((kind :element xml-match-kind "Return :ELEMENT, :ATTRIBUTE, or :TEXT." :identity)
   (path "" xml-match-path
         "Return the canonical path of a match, with an index on every element step."
         :identity)
   (name nil xml-match-name
         "Return the lexical qualified name of an element or attribute match, or NIL."
         :identity)
   (value nil xml-match-value
          "Return the decoded value of an attribute or text match, or NIL." :identity)
   (start 0 xml-match-start "Return the index in the source where a match begins."
          :identity)
   (end 0 xml-match-end "Return the index in the source after a match." :identity)
   (element nil xml-match-element
            "Return the DOM element that a match is, or belongs to." :identity)))
