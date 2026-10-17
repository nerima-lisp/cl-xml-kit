;;;; src/data/token-kinds.lisp

(in-package #:cl-xml-kit)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +xml-token-kinds+
    '((:text :string) (:name :string) (:quoted-value :string)
      (:entity-reference :string) (:comment :string) (:cdata :string)
      (:processing-instruction :string) (:doctype :string)
      (:start-tag-open nil) (:end-tag-open nil) (:tag-close nil)
      (:empty-element-close nil) (:equals nil) (:eof nil))
    "Allowed XML token kinds, each with :STRING or NIL as its value form.

TEXT is a contiguous character-data run. NAME is one XML Name. QUOTED-VALUE
is the content of one attribute value with its surrounding quote removed;
entity references in it remain lexical (for example, \"a &amp; b\").
ENTITY-REFERENCE is the content between `&` and `;` (for example, \"amp\"
or \"#x26\"). COMMENT, CDATA, PROCESSING-INSTRUCTION, and DOCTYPE are the
contents of their corresponding XML delimiters removed. A processing
instruction value therefore contains its target and data, while a doctype
value contains the declaration body after `<!DOCTYPE` (including its
internal subset when present). START-TAG-OPEN, END-TAG-OPEN, TAG-CLOSE,
EMPTY-ELEMENT-CLOSE, EQUALS, and EOF have NIL values. Token positions are the
zero-based normalized input offset and one-based line and column at the first
character of the lexical construct. EOF is returned once at the final
position; subsequent calls return NIL."))
