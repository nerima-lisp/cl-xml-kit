(in-package #:cl-xml-kit/test)

(defun xml-test-character (code)
  "Return the implementation character for Unicode CODE."
  (or (code-char code)
      (error "Unicode code point #X~X is unavailable." code)))

(it "recognizes XML Char [2] boundaries and exclusions"
  (dolist (code '(#x9 #xA #xD #x20 #xD7FF #xE000 #xFDD0 #xFDEF
                  #xFFFD #x10000 #x10FFFF))
    (expect (cl-xml-kit::xml-char-p (xml-test-character code))
            :to-equal t))
  (dolist (code '(#x0 #x8 #xB #x1F #xD800 #xDFFF #xFFFE #xFFFF))
    (expect (cl-xml-kit::xml-char-p (xml-test-character code))
            :to-equal nil)))

(it "recognizes XML S [3] and its character boundaries"
  (expect (cl-xml-kit::xml-space-p
           (coerce '(#\Space #\Tab #\Return #\Newline) 'simple-string))
          :to-equal t)
  (expect (cl-xml-kit::xml-space-p "") :to-equal nil)
  (expect (cl-xml-kit::xml-space-p " x") :to-equal nil)
  (dolist (character '(#\Space #\Tab #\Return #\Newline))
    (expect (cl-xml-kit::xml-space-char-p character) :to-equal t))
  (expect (cl-xml-kit::xml-space-char-p #\A) :to-equal nil))

(it "recognizes XML NameStartChar [4] boundaries"
  (dolist (code '(#x3A #x41 #x5A #x5F #x61 #x7A #xC0 #xD6 #xD8 #xF6
                  #xF8 #x2FF #x370 #x37D #x37F #x1FFF #x200C #x200D
                  #x2070 #x218F #x2C00 #x2FEF #x3001 #xD7FF #xF900
                  #xFDCF #xFDF0 #xFFFD #x10000 #xEFFFF))
    (expect (cl-xml-kit::xml-name-start-char-p (xml-test-character code))
            :to-equal t))
  (dolist (code '(#x2D #x2E #x30 #xB7 #x37E #x200B #x2FFF #x3000
                  #xFDD0 #xFDEF #xF0000))
    (expect (cl-xml-kit::xml-name-start-char-p (xml-test-character code))
            :to-equal nil)))

(it "recognizes XML NameChar [4a] additions"
  (dolist (character '(#\- #\. #\0))
    (expect (cl-xml-kit::xml-name-char-p character) :to-equal t))
  (dolist (code '(#xB7 #x300 #x36F #x203F #x2040))
    (expect (cl-xml-kit::xml-name-char-p (xml-test-character code))
            :to-equal t))
  (expect (cl-xml-kit::xml-name-char-p #\:) :to-equal t)
  (expect (cl-xml-kit::xml-name-char-p (xml-test-character #x2FFF))
          :to-equal nil))

(it "recognizes XML PubidChar [13] exactly"
  (loop for character across
        (coerce '(#\Space #\Return #\Newline
                  #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
                  #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
                  #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
                  #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
                  #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                  #\- #\' #\( #\) #\+ #\, #\. #\/ #\: #\= #\? #\; #\! #\*
                  #\# #\@ #\$ #\_ #\%)
                'simple-string)
        do (expect (cl-xml-kit::xml-pubid-char-p character) :to-equal t))
  (dolist (character '(#\" #\< #\> #\[ #\] #\^ #\` #\{ #\| #\}))
    (expect (cl-xml-kit::xml-pubid-char-p character) :to-equal nil))
  (expect (cl-xml-kit::xml-pubid-char-p (xml-test-character #xA0))
          :to-equal nil))

(it "recognizes XML Namespaces NCName strings"
  (dolist (name '("name" "_name" "a-._9" "a:b"))
    (expect (cl-xml-kit::xml-ncname-p name)
            :to-equal (if (string= name "a:b") nil t)))
  (expect (cl-xml-kit::xml-ncname-p "") :to-equal nil)
  (expect (cl-xml-kit::xml-ncname-p "1name") :to-equal nil)
  (expect (cl-xml-kit::xml-ncname-p "-name") :to-equal nil)
  (expect (cl-xml-kit::xml-ncname-p "name.") :to-equal t)
  (expect (cl-xml-kit::xml-ncname-p
           (concatenate 'string "a" (string (xml-test-character #x300))))
          :to-equal t)
  (expect (cl-xml-kit::xml-ncname-p
           (concatenate 'string "a" (string (xml-test-character #xFDD0))))
          :to-equal nil))

(it "executes every character classifier through its function binding"
  (labels ((check (function positives negatives)
             (dolist (value positives)
               (expect (funcall function
                                (if (integerp value)
                                    (xml-test-character value)
                                    value))
                       :to-equal t))
             (dolist (value negatives)
               (expect (funcall function
                                (if (integerp value)
                                    (xml-test-character value)
                                    value))
                       :to-equal nil))))
    (check #'cl-xml-kit::xml-char-p
           '(#x9 #x20 #xD7FF #xE000 #x10FFFF)
           '(#x0 #x8 #xD800 #xFFFE))
    (check #'cl-xml-kit::xml-space-char-p
           '(#x20 #x9 #xD #xA)
           '(#x41))
    (check #'cl-xml-kit::xml-space-p
           '(" " "  " "   ")
           '("" " x"))
    (check #'cl-xml-kit::xml-name-start-char-p
           '(#x3A #x5A #x7A #xC0 #xD8 #xF8 #x370 #x37F #x200C
             #x2070 #x2C00 #x3001 #xF900 #xFDF0 #x10000)
           '(#x2D #x30 #x37E #x200B #x2FFF #x3000 #xFDD0))
    (check #'cl-xml-kit::xml-name-char-p
           '(#x3A #x2D #x2E #x30 #xB7 #x300 #x203F)
           '(#x2F #x2FFF))
    (check #'cl-xml-kit::xml-pubid-char-p
           '(#x20 #xD #xA #x41 #x61 #x30 #x2D #x27 #x28 #x29 #x2B
             #x2C #x2E #x2F #x3A #x3D #x3F #x3B #x21 #x2A #x23 #x40
             #x24 #x5F #x25)
           '(#x22 #x3C #x3E))
    (check #'cl-xml-kit::xml-ncname-start-char-p
           '(#x41 #xC0)
           '(#x3A #x2D))
    (check #'cl-xml-kit::xml-ncname-char-p
           '(#x41 #x2D #x2E #x30 #xB7 #x300 #x203F)
           '(#x3A #x2F))
    (check #'cl-xml-kit::xml-ncname-p
           '("name" "a-._9")
           '("" "1name" "a:b"))))

