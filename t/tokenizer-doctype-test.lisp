(in-package #:cl-xml-kit/test)

(it-each (("<!DOCTYPE root>" " root")
          ("<!DOCTYPE root [<!ENTITY gt '>'><!ELEMENT root (#PCDATA)>]>"
           " root [<!ENTITY gt '>'><!ELEMENT root (#PCDATA)>]")
          ("<!DOCTYPE r SYSTEM 'a.dtd'>" " r SYSTEM 'a.dtd'")
          ("<!DOCTYPE r SYSTEM \"a]b.dtd\">" " r SYSTEM \"a]b.dtd\"")
          ("<!DOCTYPE r PUBLIC \"-//p\" 'u'>" " r PUBLIC \"-//p\" 'u'")
          ("<!DOCTYPE r PUBLIC '-//p' \"u\" [<!ENTITY a 'b'>]>"
           " r PUBLIC '-//p' \"u\" [<!ENTITY a 'b'>]")
          ("<!DOCTYPE r [<!ENTITY a \"[\">]>" " r [<!ENTITY a \"[\">]")
          ("<!DOCTYPE r [<!ENTITY a ']'>] >" " r [<!ENTITY a ']'>] "))
    "keeps the body of ~S, quoted delimiters included"
    (input body)
  (expect input :to-tokenize-as `((:doctype ,body) (:eof nil))))

(it-each (("<!DOCTYPE root ]>" "Unmatched ']' in DOCTYPE." nil)
          ("<!DOCTYPE root [" "Unterminated DOCTYPE declaration." #\>)
          ("<!DOCTYPE r SYSTEM 'a>" "Unterminated DOCTYPE declaration." #\>)
          ("<!DOCTYPE>" "DOCTYPE must name a root element." nil)
          ("<!DOCTYPE 1r>" "DOCTYPE must name a root element." nil)
          ("<!DOCTYPE r SYSTEM>" "SYSTEM identifiers need a quoted system literal." nil)
          ("<!DOCTYPE r PUBLIC>" "PUBLIC identifiers need a quoted public literal." nil)
          ("<!DOCTYPE r PUBLIC 'p'>" "PUBLIC identifiers need a quoted system literal." nil)
          ("<!DOCTYPE r junk>" "Unexpected content in DOCTYPE declaration." nil)
          ("<!DOCTYPE r [] x>" "Unexpected content in DOCTYPE declaration." nil))
    "refuses the DOCTYPE ~S"
    (input message expected)
  (expect (lambda () (drain-tokens input))
          :to-signal-xml `(xml-well-formedness-error
                           (xml-well-formedness-error-constraint :doctype)
                           (xml-well-formedness-error-expected ,expected)
                           (xml-condition-message ,message))))
