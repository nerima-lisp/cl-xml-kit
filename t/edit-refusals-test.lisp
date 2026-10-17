;;;; t/edit-refusals-test.lisp
;;;;
;;;; Edits that are refused, with the condition type, reason, and path the
;;;; refusal carries.

(in-package #:cl-xml-kit/test)

(describe "refused targets"
  (it-each ((:set "/root/nothing/@a" "v" xml-path-error :no-match)
            (:delete "/root/nothing/@a" nil xml-path-error :no-match)
            (:delete "/root/item/@missing" nil xml-path-error :no-match)
            (:set "/root/nothing/text()" "v" xml-path-error :no-match)
            (:set "/root/nothing" "<a/>" xml-path-error :no-match)
            (:delete "/root/nothing" nil xml-path-error :no-match)
            (:append "/root/nothing" "<n/>" xml-path-error :no-match)
            (:set "/root/*/@id" "v" xml-path-error :multiple-matches)
            (:delete "/root/*/@id" nil xml-path-error :multiple-matches)
            (:set "/root/*/text()" "v" xml-path-error :multiple-matches)
            (:set "/root/*" "<a/>" xml-path-error :multiple-matches)
            (:delete "/root/*" nil xml-path-error :multiple-matches)
            (:append "/root/*" "<n/>" xml-path-error :multiple-matches)
            (:delete "/root/item/text()" nil xml-path-error :wrong-target)
            (:append "/root/@a" "<n/>" xml-path-error :wrong-target)
            (:append "/root/bare/text()" "<n/>" xml-path-error :wrong-target)
            (:set "/root/bare/@z:id" "v" xml-path-error :undeclared-prefix)
            (:set "/root/@xmlns" "urn:x" xml-unsupported-edit :namespace-declaration)
            (:set "/root/@xmlns:p" "urn:x" xml-unsupported-edit :namespace-declaration)
            (:set "/root/@xmlns:new" "urn:x" xml-unsupported-edit :namespace-declaration)
            (:delete "/root/@xmlns" nil xml-unsupported-edit :namespace-declaration)
            (:delete "/root/@xmlns:p" nil xml-unsupported-edit :namespace-declaration)
            (:delete "/root/@xmlns:new" nil xml-unsupported-edit :namespace-declaration)
            (:set "/root/governed/@id" "v" xml-unsupported-edit :attlist-declaration)
            (:set "/root/governed/@other" "v" xml-unsupported-edit :attlist-declaration)
            (:delete "/root/governed/@id" nil xml-unsupported-edit :attlist-declaration)
            (:set "/root/holder/b/@a" "v" xml-unsupported-edit :entity-reference)
            (:delete "/root/holder/b" nil xml-unsupported-edit :entity-reference)
            (:append "/root/holder/b" "<n/>" xml-unsupported-edit :entity-reference)
            (:set "/root/nested/text()" "v" xml-unsupported-edit :mixed-content)
            (:set "/root/noted/text()" "v" xml-unsupported-edit :mixed-content)
            (:set "/root/holder/text()" "v" xml-unsupported-edit :mixed-content)
            (:set "/root/text()" "v" xml-unsupported-edit :mixed-content)
            (:delete "/root" nil xml-unsupported-edit :root-element)
            (:delete "/*" nil xml-unsupported-edit :root-element)
            (:set "/root/dup/@q:flag" "2" xml-unsupported-edit :verification-failed)
            (:set "/root" "<other/>" xml-unsupported-edit :verification-failed))
      "refuses ~S at ~S with the value ~S as ~S with reason ~S"
      (operation path value type reason)
    (expect (lambda () (edit-apply operation +edit-source+ path value))
            :to-signal-xml (list type (list 'xml-edit-error-reason reason)
                                 (list 'xml-edit-error-path path))))

  (it-each ((:set "/root//a/@b" "v" 6)
            (:delete "/root//a" nil 6)
            (:append "/root/" "<n/>" 6))
      "refuses ~S at the malformed path ~S with the value ~S at index ~D"
      (operation path value index)
    (expect (lambda () (edit-apply operation +edit-source+ path value))
            :to-signal-xml (list 'xml-path-error '(xml-edit-error-reason :syntax)
                                 (list 'xml-path-error-index index)
                                 (list 'xml-edit-error-path path)))))

(describe "refused values and fragments"
  (it-each ((:set "/root/@b" "a~Cb" 0 1) (:set "/root/@b" "a~Cb" 8 1)
            (:set "/root/@b" "a~Cb" #xB 1) (:set "/root/@b" "a~Cb" #xD800 1)
            (:set "/root/@b" "a~Cb" #xFFFE 1) (:set "/root/@b" "a~Cb" #xFFFF 1)
            (:set "/root/bare/text()" "a~Cb" 1 1) (:set "/root/bare" "<a>~C</a>" 0 3)
            (:append "/root/bare" "<a>~C</a>" 0 3))
      "refuses ~S at ~S of the value ~S holding the character code ~D at index ~D"
      (operation path template code index)
    (expect (lambda ()
              (edit-apply operation +edit-source+ path (format nil template (code-char code))))
            :to-signal-xml (list 'xml-edit-value-error
                                 '(xml-edit-error-reason :invalid-character)
                                 (list 'xml-edit-error-path path)
                                 (list 'xml-condition-message
                                       (format nil "VALUE has a character that XML does not allow, U+~4,'0X at index ~D."
                                               code index)))))

  (it-each ((:set "") (:set "  ") (:set "text") (:set "<a/><b/>") (:set "<a/>x") (:set "<!--c--><a/>")
            (:append "") (:append "  ") (:append "text") (:append "<a/><b/>") (:append "<a/>x")
            (:append "<!--c--><a/>"))
      "refuses ~S at /root/bare of the fragment ~S, which is not exactly one element"
      (operation fragment)
    (expect (lambda () (edit-apply operation +edit-source+ "/root/bare" fragment))
            :to-signal-xml '(xml-edit-value-error
                             (xml-edit-error-reason :not-single-element)
                             (xml-edit-error-path "/root/bare")
                             (edit-cause-type nil))))

  (it-each ((:set "<a>" :reject xml-well-formedness-error)
            (:set "<a></b>" :reject xml-well-formedness-error)
            (:set "<z:a/>" :reject xml-namespace-error)
            (:set "<a>&xxe;</a>" :reject xml-well-formedness-error)
            (:set "<a>&e;</a>" :reject xml-well-formedness-error)
            (:set "<!DOCTYPE a><a/>" :reject xml-well-formedness-error)
            (:set "<!DOCTYPE a><a/>" :skip xml-well-formedness-error)
            (:set "<?xml version=\"1.0\"?><a/>" :reject xml-well-formedness-error)
            (:set "<!DOCTYPE a [<!ENTITY x SYSTEM \"x.ent\">]><a>&x;</a>" :reject xml-well-formedness-error)
            (:append "<a>" :reject xml-well-formedness-error)
            (:append "<z:a/>" :reject xml-namespace-error)
            (:append "<a>&xxe;</a>" :reject xml-well-formedness-error)
            (:append "<a>&e;</a>" :reject xml-well-formedness-error)
            (:append "<!DOCTYPE a><a/>" :reject xml-well-formedness-error)
            (:append "<!DOCTYPE a [<!ENTITY x SYSTEM \"x.ent\">]><a>&x;</a>" :reject xml-well-formedness-error)
            (:append "<?xml version=\"1.0\"?><a/>" :reject xml-well-formedness-error)
            (:append "</w><a/>" :reject xml-well-formedness-error))
      "refuses ~S at /root/bare of the fragment ~S under the ~S policy, keeping the ~S as its cause"
      (operation fragment policy cause-type)
    (expect (lambda ()
              (edit-apply operation +edit-source+ "/root/bare" fragment
                          :external-entity-policy policy))
            :to-signal-xml (list 'xml-edit-value-error
                                 '(xml-edit-error-reason :malformed-fragment)
                                 '(xml-edit-error-path "/root/bare")
                                 (list 'edit-cause-type cause-type))))

  (it-each ((:set "<a x=\"1\"/>" "/a/@y" "2" (:max-attributes-per-element 1) :max-attributes-per-element)
            (:set "<r><a>x</a></r>" "/r/a/text()" "abcdef" (:max-text-node-length 5) :max-text-node-length)
            (:set "<r xmlns:p=\"urn:p\"><a/></r>" "/r/a" "<b x=\"1\" y=\"2\" z=\"3\" w=\"4\"/>"
             (:max-attributes-per-element 3) :max-attributes-per-element)
            (:set "<r><x><y/></x></r>" "/r/x/y" "<a><b><c/></b></a>" (:max-depth 3) :max-depth)
            (:set "<r><x><y/></x></r>" "/r/x/y" "<a><b/></a>" (:max-depth 3) :max-depth)
            (:append "<r><a/></r>" "/r/a" "<b x=\"1\" y=\"2\" z=\"3\" w=\"4\"/>"
             (:max-attributes-per-element 3) :max-attributes-per-element)
            (:append "<r><x><y/></x></r>" "/r/x/y" "<a><b/></a>" (:max-depth 3) :max-depth))
      "refuses ~S on ~S at ~S of ~S under the limit ~{~S ~D~}, keeping the ~S limit as the cause"
      (operation source path value limits kind)
    (expect (lambda ()
              (edit-apply operation source path value
                          :resource-limits (apply #'make-xml-resource-limits limits)))
            :to-signal-xml (list 'xml-edit-value-error
                                 '(xml-edit-error-reason :resource-limit)
                                 (list 'xml-edit-error-path path)
                                 (list 'edit-cause-limit-kind kind)))))
