;;;; t/edit-operations-test.lisp
;;;;
;;;; Setting the text content of an element through EDIT-XML-SET with a
;;;; text() path.

(in-package #:cl-xml-kit/test)

(it-each (("/root/bare/text()" "hi" "<bare/>" ">hi</bare>" 5 2)
          ("/root/spaced/text()" "hi" "<spaced />" ">hi</spaced>" 8 2)
          ("/root/item/text()" "a<b>&c" "text &amp; &#65; &e; <![CDATA[<raw>]]>" "a&lt;b&gt;&amp;c" 0 nil)
          ("/root/cdata/text()" "x < y & z" "<![CDATA[a < b]]>" "<![CDATA[x < y & z]]>" 0 nil)
          ("/root/cdata/text()" "a]]>b" "<![CDATA[a < b]]>" "a]]&gt;b" 0 nil)
          ("/root/cdata/text()" "a{cr}b" "<![CDATA[a < b]]>" "a&#13;b" 0 nil)
          ("/root/cdata/text()" "a{lf}b" "<![CDATA[a < b]]>" "<![CDATA[a~%b]]>" 0 nil)
          ("/root/refs/text()" "plain" "&#65;&amp;&e;" "plain" 0 nil)
          ("/root/blank/text()" "x" "<blank>   </blank>" "x" 7 3)
          ("/root/item/text()" "" "text &amp; &#65; &e; <![CDATA[<raw>]]>" "" 0 nil)
          ("/root/refs/text()" "a{lf}b" "&#65;&amp;&e;" "a~%b" 0 nil)
          ("/root/refs/text()" "a{cr}{lf}b" "&#65;&amp;&e;" "a&#13;~%b" 0 nil))
    "sets ~S to ~S by replacing ~S with ~S"
    (path value needle replacement offset length)
  (expect (edit-xml-set +edit-source+ path (spans-lines value))
          :to-equal (edit-expected needle replacement :offset offset :length length)))

(it "keeps a document without carriage returns free of them when setting text"
  (expect (edit-xml-set (spans-lines "<r>~%  <a>x</a>~%</r>") "/r/a/text()" (spans-lines "p~%q"))
          :to-equal (spans-lines "<r>~%  <a>p~%q</a>~%</r>")))

(it "sets text within the text node limit of the caller"
  (expect (edit-xml-set "<r><a>x</a></r>" "/r/a/text()" "abcde"
                        :resource-limits (make-xml-resource-limits :max-text-node-length 5))
          :to-equal "<r><a>abcde</a></r>"))

(it-each (("plain") ("") ("  padded  ") ("a&b<c>d") ("a]]>b") ("<![CDATA[x]]>") ("&#65;")
          ("l1{lf}l2") ("cr{cr}x") ("crlf{cr}{lf}x"))
    "sets the text ~S so that it selects back as that value in empty, CDATA, and mixed-run content"
    (value)
  (expect (loop for path in '("/root/bare/text()" "/root/cdata/text()" "/root/item/text()")
                for edited = (edit-xml-set +edit-source+ path (spans-lines value))
                for selected = (xml-match-value (first (select-xml edited path)))
                unless (equal selected (spans-lines value))
                  collect (list path selected))
          :to-equal nil))

(it-property "sets generated text so that it selects back as the value that was set"
    ((path (gen-member '("/root/bare/text()" "/root/cdata/text()" "/root/refs/text()")))
     (value (gen-string :max-length 12 :alphabet (format nil "t &<>]~C~C~C~C"
                                                        #\Tab #\Linefeed #\Return
                                                        (code-char #xE9)))))
  (expect (edit-xml-set +edit-source+ path value) :to-select-value path value))

(it-each (("<a>x</a>" "x") ("<a>&#65;<![CDATA[b]]></a>" "&#65;<![CDATA[b]]>")
          ("<a/>" nil) ("<a>x<b/></a>" nil) ("<a><b/></a>" nil))
    "finds in ~S the single text run that is all the content: ~S"
    (source run)
  (let ((content (cl-xml-kit::%edit-text-content (spans-root source))))
    (expect (and content (spans-slice source content)) :to-equal run)))
