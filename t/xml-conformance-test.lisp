(in-package #:cl-xml-kit/test)

(it "accepts XML declarations, doctypes, namespaces, CDATA, and PIs"
  (let ((document
          (parse-xml
           "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><!DOCTYPE root><root xmlns:p=\"urn:p\"><p:item><![CDATA[x<y]]></p:item><?done value?></root>")))
    (expect (xml-document-p document) :to-be-truthy)
    (expect (length (xml-document-children document)) :to-equal 2)))

(it "rejects XML declarations outside the UTF-8 profile"
  (let ((condition nil))
    (handler-case
        (parse-xml "<?xml version=\"1.0\" encoding=\"UTF-16\"?><root/>")
      (xml-unsupported-encoding (caught) (setf condition caught)))
    (expect (typep condition 'xml-unsupported-encoding) :to-be-truthy)
    (expect (xml-unsupported-encoding-encoding condition) :to-equal "UTF-16")
    (expect (xml-unsupported-encoding-reason condition)
            :to-equal :declaration-not-utf-8)))

(it "rejects malformed XML declaration ordering"
  (dolist (source '(" <?xml version=\"1.0\"?><root/>"
                    "<?xml encoding=\"UTF-8\" version=\"1.0\"?><root/>"
                    "<?xml version=\"1.1\"?><root/>"
                    "<?xml version=\"1.0\" standalone=\"maybe\"?><root/>"))
    (expect (handler-case
                (progn (parse-xml source) nil)
              (xml-kit-error () t)
              (error () t))
            :to-be-truthy)))

(it "rejects XML well-formedness violations"
  (dolist (source '("<root><child></root>"
                    "<root a=\"1\" a=\"2\"/>"
                    "<root><p:item/></root>"))
    (expect (handler-case
                (progn (parse-xml source) nil)
              (xml-kit-error () t)
              (error () nil))
            :to-be-truthy)))

