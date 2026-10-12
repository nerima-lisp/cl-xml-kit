(in-package #:cl-xml-kit/test)

(it "keeps canonical serialization idempotent for representative documents"
  (dolist (source '("<root/>"
                    "<root a=\"1\">text &amp; more</root>"
                    "<root xmlns:p=\"urn:p\"><p:item p:id=\"1\"/></root>"
                    "<?xml version=\"1.0\"?><root><!--note--><?done value?></root>"))
    (let* ((first (xml->canonical-string (parse-xml source)))
           (second (xml->canonical-string (parse-xml first))))
      (expect second :to-equal first))))

