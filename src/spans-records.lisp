;;;; src/spans-records.lisp
;;;;
;;;; Lexical span scanner for tools that edit XML without rewriting it. DOM
;;;; positions cannot serve: attributes and end tags have none, end positions
;;;; are not exclusive, and offsets count characters of the line-end-normalized
;;;; text. This scanner drives the tokenizer over that text and records where
;;;; each construct lies in the caller's original string.

(in-package #:cl-xml-kit)

(defstruct (span-node
            (:constructor %make-span-node (start end))
            (:copier nil))
  "A lexical construct and its extent in the caller's original string.

START and END are indices into that string, END exclusive."
  (start 0 :read-only t)
  (end 0 :read-only t))

(defstruct (span-comment
            (:include span-node)
            (:constructor %make-span-comment (start end))
            (:copier nil))
  "A comment, delimiters included.")

(defstruct (span-processing-instruction
            (:include span-node)
            (:constructor %make-span-processing-instruction (start end))
            (:copier nil))
  "A processing instruction other than the XML declaration, delimiters included.")

(defstruct (span-doctype
            (:include span-node)
            (:constructor %make-span-doctype
                (start end root-name internal-subset subset-start subset-end))
            (:copier nil))
  "A DOCTYPE declaration, `<!DOCTYPE` through `>`.

INTERNAL-SUBSET is the subset text between the brackets as the DOM reports it
(line ends normalized). SUBSET-START and SUBSET-END bound that text in the
original string. All three are NIL when there is no internal subset."
  (root-name "" :read-only t)
  (internal-subset nil :read-only t)
  (subset-start nil :read-only t)
  (subset-end nil :read-only t))

(defstruct (span-text
            (:include span-node)
            (:constructor %make-span-text
                (start end cdata-p single-cdata-p char-ref-p predefined-ref-p
                 entity-names))
            (:copier nil))
  "A maximal run of character data, CDATA sections, and references.

CDATA-P, CHAR-REF-P, and PREDEFINED-REF-P tell whether the run contains a CDATA
section, a character reference, or a reference to one of the five predefined
entities. SINGLE-CDATA-P is true when the run is exactly one CDATA section.
ENTITY-NAMES lists the other general entities referenced, once each, in order
of first appearance."
  (cdata-p nil :read-only t)
  (single-cdata-p nil :read-only t)
  (char-ref-p nil :read-only t)
  (predefined-ref-p nil :read-only t)
  (entity-names nil :read-only t))

(defstruct (span-attribute
            (:constructor %make-span-attribute
                (qname declaration-p space-start name-start name-end value-start
                 value-end quote-char))
            (:copier nil))
  "An attribute or namespace declaration inside a start tag.

QNAME is the name as written. DECLARATION-P marks `xmlns` and `xmlns:*`.
SPACE-START is where the whitespace before the name begins; NAME-START and
NAME-END bound the name. VALUE-START is the opening quote and VALUE-END the
index after the closing quote, so the value text lies between them exclusive of
the quotes. QUOTE-CHAR is the quote character used."
  (qname "" :read-only t)
  (declaration-p nil :read-only t)
  (space-start 0 :read-only t)
  (name-start 0 :read-only t)
  (name-end 0 :read-only t)
  (value-start 0 :read-only t)
  (value-end 0 :read-only t)
  (quote-char #\" :read-only t))

(defstruct (span-element
            (:include span-node)
            (:constructor %make-span-element)
            (:copier nil))
  "An element, `<` through the end of its end tag or `/>`.

QNAME is the name as written and NAME-END the index after it. ATTRIBUTES lists
SPAN-ATTRIBUTE values in source order. START-TAG-END is the index after `>` or
`/>`. CONTENT-START and CONTENT-END bound the content; for an empty element both
are the index of the `/`. END-TAG-START and END-TAG-END bound the end tag and are
NIL for an empty element. CHILDREN lists SPAN-ELEMENT, SPAN-TEXT, SPAN-COMMENT,
and SPAN-PROCESSING-INSTRUCTION values in source order."
  (qname "" :read-only t)
  (name-end 0 :read-only t)
  (attributes nil :read-only t)
  (empty-p nil :read-only t)
  (start-tag-end 0 :read-only t)
  (content-start 0 :read-only t)
  (content-end 0 :read-only t)
  (end-tag-start nil :read-only t)
  (end-tag-end nil :read-only t)
  (children nil :read-only t))

(defstruct (span-document
            (:constructor %make-span-document)
            (:copier nil))
  "The lexical tree of one document, with spans in the caller's original string.

DECLARATION is the XML declaration node or NIL. DOCTYPE is the SPAN-DOCTYPE or
NIL. CHILDREN lists, in source order, the DOCTYPE, comments, processing
instructions, and ROOT; whitespace between them is not recorded. LINE-BREAK is
the style of the first line break, :CRLF, :LF, or :CR, and :LF when the source
has none. MARKUP-ENTITY-NAMES lists the internal general entities whose
replacement text contains markup, and ATTLIST-ELEMENT-NAMES the element names
that appear in ATTLIST declarations of the internal subset."
  (declaration nil :read-only t)
  (doctype nil :read-only t)
  (children nil :read-only t)
  (root nil :read-only t)
  (line-break :lf :read-only t)
  (markup-entity-names nil :read-only t)
  (attlist-element-names nil :read-only t))

(defstruct (span-run
            (:constructor %make-span-run (start))
            (:copier nil))
  "Mutable accumulator for one text run, in normalized offsets."
  (start 0)
  (end 0)
  (token-count 0)
  (cdata-count 0)
  (char-ref-p nil)
  (predefined-ref-p nil)
  (entity-names nil))

(defstruct (span-frame
            (:constructor %make-span-frame
                (qname start name-end attributes start-tag-end))
            (:copier nil))
  "Mutable state for one open element, in original-string offsets."
  (qname "" :read-only t)
  (start 0 :read-only t)
  (name-end 0 :read-only t)
  (attributes nil :read-only t)
  (start-tag-end 0 :read-only t)
  (children nil)
  (run nil))

(defstruct (span-scanner
            (:constructor %make-span-scanner
                (tokenizer text offsets shift limits external-entity-policy))
            (:copier nil))
  "Mutable state for one scan.

TEXT is the normalized text. OFFSETS maps each normalized index, and the text
length, to an index in the original string; it is NIL when the two differ only
by the SHIFT of a skipped leading U+FEFF."
  (tokenizer nil :read-only t)
  (text "" :read-only t)
  (offsets nil :read-only t)
  (shift 0 :read-only t)
  (limits nil :read-only t)
  (external-entity-policy :reject :read-only t)
  (stack nil)
  (nodes nil)
  (declaration nil)
  (doctype nil)
  (root nil)
  (attlist-element-names nil)
  (markup-entity-names nil))
