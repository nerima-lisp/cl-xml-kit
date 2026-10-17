(in-package #:cl-xml-kit/test)

(defparameter *mixed-content-sample*
  (concatenate 'string "text&amp;<![CDATA[c<d]]><!--ok--><?pi data?>"
               "<!DOCTYPE root [<!ELEMENT root ANY>]><root a=\"v&amp;x\"/>tail</root>")
  "One instance of every content construct, each followed directly by the next.")

(describe "content constructs"
  (it "tokenizes every content construct with delimiter-free values"
    (expect *mixed-content-sample*
            :to-tokenize-as '((:text "text") (:entity-reference "amp") (:cdata "c<d")
                              (:comment "ok") (:processing-instruction "pi data")
                              (:doctype " root [<!ELEMENT root ANY>]")
                              (:start-tag-open nil) (:name "root") (:name "a")
                              (:equals nil) (:quoted-value "v&amp;x")
                              (:empty-element-close nil) (:text "tail")
                              (:end-tag-open nil) (:name "root") (:tag-close nil)
                              (:eof nil))))

  (it "starts every token at the offset of its first character"
    (expect (mapcar (lambda (token) (third (token-summary token)))
                    (drain-tokens *mixed-content-sample*))
            :to-equal '(0 4 9 24 33 44 81 82 87 88 89 98 100 104 106 110 111)))

  (it "ends every token at the exclusive offset after its lexeme"
    (expect (token-lexemes "text&amp;<![CDATA[c]]><!--k--><?pi d?><root  a = \"v\" >t</root>")
            :to-equal '("text" "&amp;" "<![CDATA[c]]>" "<!--k-->" "<?pi d?>"
                        "<" "root" "a" "=" "\"v\"" ">" "t" "</" "root" ">" "")))

  (it-each (("<!---->" ((:comment "") (:eof nil)))
            ("<![CDATA[]]>" ((:cdata "") (:eof nil)))
            ("<?pi?>" ((:processing-instruction "pi") (:eof nil)))
            ("<?pi  d?>" ((:processing-instruction "pi  d") (:eof nil)))
            ("<![CDATA[]]]]>" ((:cdata "]]") (:eof nil)))
            ("<!--a-b-->" ((:comment "a-b") (:eof nil)))
            ("<!---a-->" ((:comment "-a") (:eof nil))))
      "tokenizes the edge construct ~S"
      (input tokens)
    (expect input :to-tokenize-as tokens)))

(describe "references"
  (it "tokenizes decimal, hexadecimal, and named references"
    (expect "&#65;&#x42;&name;"
            :to-tokenize-as '((:entity-reference "#65") (:entity-reference "#x42")
                              (:entity-reference "name") (:eof nil))))

  (it "accepts hexadecimal references in either letter case"
    (expect "&#xaF;&#xAf;&#x41;"
            :to-tokenize-as '((:entity-reference "#xaF") (:entity-reference "#xAf")
                              (:entity-reference "#x41") (:eof nil))))

  (it "accepts a NameChar that is not a NameStartChar after the first name character"
    (expect (xml-text-of '("&a" #xB7 "-.9;"))
            :to-tokenize-as `((:entity-reference ,(xml-text-of '("a" #xB7 "-.9")))
                              (:eof nil))))

  (it-each ((("&#1z;") "Malformed numeric character reference.")
            (("&#4a;") "Malformed numeric character reference.")
            (("&#x4G;") "Malformed numeric character reference.")
            (("&#x41 ;") "Malformed numeric character reference.")
            (("&#;") "Malformed numeric character reference.")
            (("&#x;") "Malformed numeric character reference.")
            (("&#X41;") "Malformed numeric character reference.")
            (("&#" #x666 #x665 ";") "Malformed numeric character reference.")
            (("&#x" #xFF14 #xFF11 ";") "Malformed numeric character reference.")
            (("&#x" #xFF21 ";") "Malformed numeric character reference.")
            (("<a b=\"&#1z;\">") "Malformed numeric character reference.")
            (("<a b='&#" #x666 #x665 ";'>") "Malformed numeric character reference.")
            (("&#0;") "Numeric character reference names an illegal XML character.")
            (("&#x110000;") "Numeric character reference names an illegal XML character.")
            (("&#xD800;") "Numeric character reference names an illegal XML character.")
            (("&#xFFFE;") "Numeric character reference names an illegal XML character."))
      "refuses the character reference ~S"
      (parts message)
    (expect (lambda () (drain-tokens (xml-text-of parts)))
            :to-signal-xml `(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :character-reference)
                             (xml-condition-message ,message))))

  ;; The last four rows: #xD7 and #xF7 sit inside the Latin-1 block that
  ;; NameStartChar [4] otherwise admits, so an entity name holding them is refused.
  (it-each ((("&") "Unterminated entity reference." #\;)
            (("&abc") "Unterminated entity reference." #\;)
            (("&;") "An entity reference must not be empty." nil)
            (("&bad name;") "Malformed entity reference." nil)
            (("&1a;") "Malformed entity reference." nil)
            (("&-a;") "Malformed entity reference." nil)
            (("&" #xD7 ";") "Malformed entity reference." nil)
            (("&" #xF7 ";") "Malformed entity reference." nil)
            (("&a" #xD7 ";") "Malformed entity reference." nil)
            (("&a" #xF7 "b;") "Malformed entity reference." nil))
      "refuses the entity reference ~S"
      (parts message expected)
    (expect (lambda () (drain-tokens (xml-text-of parts)))
            :to-signal-xml `(xml-well-formedness-error
                             (xml-well-formedness-error-constraint :entity-reference)
                             (xml-well-formedness-error-expected ,expected)
                             (xml-condition-message ,message)))))

(describe "lexical terminators"
  (it-each (("abc]]>" :character-data "The character sequence ]]> is not allowed in text.")
            ("<![CDATA[a]]>b]]>" :character-data
             "The character sequence ]]> is not allowed in text.")
            ("<!--a--b-->" :comment "XML comments must not contain '--'.")
            ("<!--a--->" :comment "XML comments must not contain '--'.")
            ("<??>" :processing-instruction "A processing instruction needs a target.")
            ("<?1?>" :processing-instruction
             "A processing instruction target must be an XML Name.")
            ("<?pi=?>" :processing-instruction
             "Processing-instruction target must be separated from its data.")
            ("<a x='<'>" :attribute-value "A '<' is not allowed in an attribute value."))
      "refuses ~S under the ~S constraint"
      (input constraint message)
    (expect (lambda () (drain-tokens input))
            :to-signal-xml `(xml-well-formedness-error
                             (xml-well-formedness-error-constraint ,constraint)
                             (xml-condition-message ,message))))

  (it-each (("<!--" :comment "-->" "Unterminated XML comment.")
            ("<!--a-" :comment "-->" "Unterminated XML comment.")
            ("<![CDATA[x" :cdata "]]>" "Unterminated CDATA section.")
            ("<![CDATA[x]]" :cdata "]]>" "Unterminated CDATA section.")
            ("<?pi" :processing-instruction "?>" "Unterminated processing instruction.")
            ("<?pi ?" :processing-instruction "?>" "Unterminated processing instruction.")
            ("<a x=\"unterminated" :attribute-value #\" "Unterminated quoted attribute value.")
            ("<a x='v\"" :attribute-value #\' "Unterminated quoted attribute value."))
      "refuses the unterminated construct ~S"
      (input constraint expected message)
    (expect (lambda () (drain-tokens input))
            :to-signal-xml `(xml-well-formedness-error
                             (xml-well-formedness-error-constraint ,constraint)
                             (xml-well-formedness-error-expected ,expected)
                             (xml-condition-message ,message)))))
