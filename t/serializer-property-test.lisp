;;;; t/serializer-property-test.lisp

(in-package #:cl-xml-kit/test)

(it-property "parses generated source, serializes it, and parses the output to the same tree"
    ((raw (gen-document-spec)))
  (let* ((spec (generated-document raw :source t))
         (parsed (parse-xml (render-spec spec))))
    (expect (round-trip-signature parsed)
            :to-equal (round-trip-signature (node-from-spec spec)))
    (expect parsed :to-round-trip :ordinary)))

(it-property "parses generated source, writes it canonically, and parses the output to the same tree"
    ((raw (gen-document-spec)))
  (let* ((spec (generated-document raw :source t))
         (parsed (parse-xml (render-spec spec))))
    (expect (round-trip-signature parsed :comments nil)
            :to-equal (round-trip-signature (node-from-spec spec) :comments nil))
    (expect parsed :to-round-trip :canonical)))

(it-property "writes every generated tree as XML that parses back to that tree"
    ((raw (gen-document-spec)))
  (let ((document (node-from-spec (generated-document raw))))
    (expect document :to-round-trip :ordinary)
    (expect document :to-round-trip :canonical)))

(it-property "writes the canonical form of its own canonical output unchanged"
    ((raw (gen-document-spec)))
  (let ((canonical (xml->canonical-string (node-from-spec (generated-document raw)))))
    (expect (xml->canonical-string (parse-xml canonical)) :to-equal canonical)))

(it-each (("<root/>" "<root></root>")
          ("<root a=\"1\">text &amp; more</root>" "<root a=\"1\">text &amp; more</root>")
          ("<root xmlns:p=\"urn:p\"><p:item p:id=\"1\"/></root>"
           "<root xmlns:p=\"urn:p\"><p:item p:id=\"1\"></p:item></root>")
          ("<?xml version=\"1.0\"?><root><!--note--><?done value?></root>"
           "<root><?done value?></root>"))
    "writes ~S in canonical form as ~S, a fixed point of canonicalization"
    (source expected)
  (let ((canonical (xml->canonical-string (parse-xml source))))
    (expect canonical :to-equal expected)
    (expect (xml->canonical-string (parse-xml canonical)) :to-equal expected)))
