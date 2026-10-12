(in-package #:cl-xml-kit/test)

(it "rejects a deterministic malformed-input corpus"
  (dolist (source '("<"
                    "<root>"
                    "</root>"
                    "<root a=\"unterminated></root>"
                    "<root>&unknown;</root>"
                    "<root><!--bad--comment--></root>"
                    "<root><![CDATA[bad]]></root>tail"))
    (expect (handler-case
                (progn (parse-xml source) nil)
              (xml-kit-error () t)
              (error () nil))
            :to-be-truthy)))

