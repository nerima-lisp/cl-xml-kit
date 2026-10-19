;;;; Entry point of the cl-xml-kit benchmark. See benchmark/README.md.

(require :asdf)

(load (merge-pathnames "scripts/script-support.lisp" *load-truename*))

(cl-xml-kit/script-support:load-project-definition)

(asdf:load-system "cl-weave")
(asdf:load-system "cl-xml-kit/benchmark")

(uiop:symbol-call "CL-XML-KIT/BENCHMARK" "MAIN")
