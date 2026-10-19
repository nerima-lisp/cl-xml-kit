;;;; t/edit-support-test.lisp
;;;;
;;;; Attribute edits through EDIT-XML-SET and EDIT-XML-DELETE: replacing a
;;;; value between its quotes, inserting a new attribute, and removing one.

(in-package #:cl-xml-kit/test)

(describe "replacing an attribute value"
  (it-each (("/root/@b" "x" "b=\"double\"" "x" 3 6)
            ("/root/@a" "new value" "a='single'" "new value" 3 6)
            ("/root/item/@name" "it's \"a\" & b" "name='n'" "it&apos;s \"a\" &amp; b" 6 1)
            ("/root/item/@id" "say \"hi\" & 'bye'" "id=\"1\"" "say &quot;hi&quot; &amp; 'bye'" 4 1)
            ("/root/item/@name" "a{tab}b{lf}c{cr}d" "name='n'" "a&#9;b&#10;c&#13;d" 6 1)
            ("/root/@b" "a>b ]]> é日" "b=\"double\"" "a>b ]]> é日" 3 6)
            ("/root/@b" "a<b<" "b=\"double\"" "a&lt;b&lt;" 3 6)
            ("/root/@a" "<'" "a='single'" "&lt;&apos;" 3 6)
            ("/root/@a" "" "a='single'" "" 3 6)
            ("/root/p:item/@p:code" "8" "p:code=\"7\"" "8" 8 1)
            ("/root/list/entry/@label" "second" "label='first'" "second" 7 5)
            ("/root/holder/@x" "2" "x=\"1\"" "2" 3 1))
      "sets ~S to ~S by replacing only the characters between the quotes of ~S with ~S"
      (path value needle replacement offset length)
    (expect (edit-apply :set +edit-source+ path (spans-lines value))
            :to-equal (edit-expected needle replacement :offset offset :length length)))

  (it-each (("plain") ("") ("a&b") ("a<b>c") ("&#65;") ("say \"hi\" 'there'")
            ("t{tab}l{lf}r{cr}x"))
      "writes the attribute value ~S so that it selects back as that value"
      (value)
    (expect (edit-apply :set +edit-source+ "/root/@b" (spans-lines value))
            :to-select-value "/root/@b" (spans-lines value)))

  (it-property "writes generated attribute values so that they select back as the value set"
      ((value (gen-string :max-length 12 :alphabet (format nil "a \"'&<>]~C~C~C~C"
                                                          #\Tab #\Linefeed #\Return
                                                          (code-char #x65E5)))))
    (expect (edit-apply :set +edit-source+ "/root/item/@name" value)
            :to-select-value "/root/item/@name" value)))

(describe "adding an attribute"
  (it-each (("/root/bare/@id" "5" "<bare/>" " id=\"5\"" 5)
            ("/root/spaced/@id" "5" "<spaced />" " id=\"5\"" 7)
            ("/root/item/@new" "v" "name='n'" " new='v'" 8)
            ("/root/item/@new" "it's \"q\"" "name='n'" " new='it&apos;s \"q\"'" 8)
            ("/root/@c" "it's \"q\"" "b=\"double\"" " c=\"it's &quot;q&quot;\"" 10)
            ("/root/@c" "v" "b=\"double\"" " c=\"v\"" 10)
            ("/root/list/entry/@extra" "x" "state=\"on\"" "~%        extra=\"x\"" 10)
            ("/root/bare/@p:id" "5" "<bare/>" " p:id=\"5\"" 5)
            ("/root/p:item/@p:other" "v" "p:code=\"7\"" " p:other=\"v\"" 10)
            ("/root/p:item/@q:note" "n" "p:code=\"7\"" " q:note=\"n\"" 10)
            ("/root/bare/@xml:lang" "en" "<bare/>" " xml:lang=\"en\"" 5)
            ("/root/bare/@id" "1<2" "<bare/>" " id=\"1&lt;2\"" 5))
      "adds ~S with the value ~S by inserting, after ~S, the text ~S"
      (path value needle insertion offset)
    (expect (edit-apply :set +edit-source+ path value)
            :to-equal (edit-expected needle insertion :offset offset :length 0)))

  (it-each (("<root a=\"1\"~%      b=\"2\">x</root>" "/root/@c" "3"
             "<root a=\"1\"~%      b=\"2\"~%      c=\"3\">x</root>")
            ("<r a=\"1\" b=\"2\"~%    c=\"3\"/>" "/r/@d" "v"
             "<r a=\"1\" b=\"2\"~%    c=\"3\"~%    d=\"v\"/>")
            ("<r a=\"1\"{cr}{lf}    b=\"2\"/>" "/r/@d" "v"
             "<r a=\"1\"{cr}{lf}    b=\"2\"{cr}{lf}    d=\"v\"/>")
            ("<r a=\"1\"~%    b=\"2\" c=\"3\"/>" "/r/@d" "v"
             "<r a=\"1\"~%    b=\"2\" c=\"3\" d=\"v\"/>")
            ("<r><a/><b x='1'/></r>" "/r/a/@y" "v" "<r><a y='v'/><b x='1'/></r>")
            ("<r><a/><b><c x='1'/></b></r>" "/r/a/@y" "v" "<r><a y='v'/><b><c x='1'/></b></r>")
            ("<r><a/></r>" "/r/a/@y" "v" "<r><a y=\"v\"/></r>"))
      "lays out the new attribute in ~S, set at ~S to ~S, as ~S"
      (source path value expected)
    (expect (edit-apply :set (spans-lines source) path value) :to-equal (spans-lines expected)))

  (it "adds an attribute that selects back as the value that was set"
    (expect (edit-apply :set +edit-source+ "/root/bare/@id" "a&b")
            :to-select-value "/root/bare/@id" "a&b"))

  (it-property "restores a generated document when an added attribute is deleted again"
      ((document (corpus-gen-document))
       (choice (gen-integer :min 0 :max 40))
       (value (gen-string :max-length 6 :alphabet (format nil "v&<\"'~C" #\Linefeed))))
    (let* ((source (corpus-render document))
           (matches (edit-element-matches source))
           (match (nth (mod choice (length matches)) matches))
           (path (concatenate 'string (xml-match-path match) "/@zz"))
           (edited (edit-apply :set source path value)))
      (expect (edit-within-p source edited (xml-match-start match) (xml-match-end match))
              :to-be-truthy)
      (expect edited :to-select-value path value)
      (expect (edit-apply :delete edited path nil) :to-equal source))))

(describe "deleting an attribute"
  (it-each (("/root/item/@id" " id=\"1\"")
            ("/root/@a" " a='single'")
            ("/root/@b" " b=\"double\"")
            ("/root/holder/@x" " x=\"1\"")
            ("/root/dup/@p:flag" " p:flag=\"1\"")
            ("/root/list/entry/@label" "~%        label='first'")
            ("/root/p:item/@p:code" " p:code=\"7\""))
      "deletes ~S together with the whitespace before it, ~S"
      (path text)
    (expect (edit-apply :delete +edit-source+ path nil)
            :to-equal (edit-expected (spans-lines text :crlf) "")))

  (it "no longer selects a deleted attribute"
    (expect (path-select (edit-apply :delete +edit-source+ "/root/@a" nil) "/root/@a") :to-equal nil)))
