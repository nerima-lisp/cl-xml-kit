;;;; src/data/character-classes.lisp

(in-package #:cl-xml-kit)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +xml-character-class-specs+
    '((xml-char-p
       "Return true when CHARACTER matches XML 1.0 Char [2]."
       :codes (#x9 #xA #xD)
       :ranges ((#x20 #xD7FF) (#xE000 #xFFFD) (#x10000 #x10FFFF)))
      (xml-space-char-p
       "Return true when CHARACTER is one XML S [3] whitespace character."
       :codes (#x20 #x9 #xD #xA))
      (xml-name-start-char-p
       "Return true when CHARACTER matches XML NameStartChar [4]."
       :chars (#\: #\_)
       :ranges ((#x41 #x5A) (#x61 #x7A) (#xC0 #xD6) (#xD8 #xF6) (#xF8 #x2FF)
                (#x370 #x37D) (#x37F #x1FFF) (#x200C #x200D) (#x2070 #x218F)
                (#x2C00 #x2FEF) (#x3001 #xD7FF) (#xF900 #xFDCF) (#xFDF0 #xFFFD)
                (#x10000 #xEFFFF)))
      (xml-name-char-p
       "Return true when CHARACTER matches XML NameChar [4a]."
       :include xml-name-start-char-p
       :codes (#xB7)
       :chars (#\- #\.)
       :ranges ((#x30 #x39) (#x300 #x36F) (#x203F #x2040)))
      (xml-pubid-char-p
       "Return true when CHARACTER matches XML PubidChar [13]."
       :codes (#x20 #xD #xA)
       :chars (#\- #\' #\( #\) #\+ #\, #\. #\/ #\: #\= #\? #\; #\! #\* #\#
               #\@ #\$ #\_ #\%)
       :ranges ((#x41 #x5A) (#x61 #x7A) (#x30 #x39)))
      (xml-ncname-start-char-p
       "Return true when CHARACTER is an XML Namespaces NCName start character."
       :include xml-name-start-char-p
       :exclude-chars (#\:))
      (xml-ncname-char-p
       "Return true when CHARACTER is an XML Namespaces NCName character."
       :include xml-name-char-p
       :exclude-chars (#\:)))
    "XML 1.0 Fifth Edition and Namespaces character classes.

Each entry is (PREDICATE DOCSTRING &key INCLUDE CODES CHARS RANGES
EXCLUDE-CHARS). INCLUDE names a class whose members also belong; RANGES are
inclusive (LOW HIGH) code pairs; EXCLUDE-CHARS removes characters that an
included class lists in CHARS."))
