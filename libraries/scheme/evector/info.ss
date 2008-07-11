(module info (lib "infotab.ss" "setup")
  (define name "Extensible Vectors")
  (define blurb
    (list "Extensible vectors are a low level, resizeable data structure resembling normal Scheme vectors."))
  (define primary-file "evector.scm")
  (define doc.txt "doc.txt")
  (define categories '(datastructures))
  (define version "1.1")
  (define compile-omit-files (list "test-evector.scm"))
  (define release-notes
    '(p "Added evector-map, evector-for-each, evector-copy, and evector=?. Thanks to Paulo Matos for code and documentation.")))
    

