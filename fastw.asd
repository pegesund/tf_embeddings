;;;; fastw.asd

(asdf:defsystem #:fastw
  :description "Describe fastw here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:lmdb #:numcl #:str #:parse-number #:cl-store #:cl-threadpool #:cl-progress-bar #:mgl-mat #:gzip-stream #:archive #:xmls #:cl-json :plump)
  :components ((:file "package")
               (:file "fastw")
	       (:file "tfidf")
	       ))
