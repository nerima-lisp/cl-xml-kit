;;;; Entry point of the cl-xml-kit benchmark. See benchmark/README.md.

(require :asdf)

(asdf:load-asd (merge-pathnames "cl-xml-kit.asd" *load-truename*))

(asdf:load-system "cl-xml-kit/benchmark")

(uiop:symbol-call "CL-XML-KIT/BENCHMARK" "MAIN")
