(in-package #:fastw)

;; helper - so lacking in lisp..

(defun filter (f list)
   (cond
    ((not list)                   ;; the list is empty
     nil)
    ((funcall f (first list))     ;; the first element satisfies the test
     (cons (first list) (filter f (rest list))))
    (t                            ;; otherwise ...
     (filter f (rest list)))))


;; this package simply buids up the *word-freq*

(defclass tfidf ()
  ((word-freq
    :initarg :word-freq
    :accessor word-freq
    :initform  (make-hash-table :test 'equal :synchronized T)
    )
   (word-total
    :initarg :word-total
    :accessor word-total
    :initform 0
    )))


(defmethod add-string (h s)
  (let* ((words-raw (str:words (sb-unicode:lowercase s)))
	 (words (loop for w in words-raw when (every #'alpha-char-p w) collect w)))
    (loop for word in words do
      (incf (word-total h))
      (setf (gethash word (word-freq h)) (+ 1 (or (gethash word (word-freq h)) 0))))))
	


;; code below just builds the word counter above, can be replaced with any of you own code

(defun text-from-xml(node)
  (cond ((typep node 'xmls:node)
	 (str:join " " (mapcar (lambda(n) (text-from-xml n))  (xmls:node-children node))))
	((typep node 'string) node)))

(defun parse-tar-file(tf file)
  (print file) 
  (gzip-stream:with-open-gzip-file (stream file)
               (archive:with-open-archive
                   (archive stream :direction :input)
                 (archive:do-archive-entries (entry archive)
		   (print  (format nil "~A" entry))
		   (let* ((content (alexandria:read-stream-content-into-string (flexi-streams:make-flexi-stream
										(archive:entry-stream entry))))
			  (content-str (cond ((str:ends-with? "xml>" (format nil "~A" entry))
					      (text-from-xml (xmls:parse content)))
					     (t content))))
		     (add-string tf content-str))))))

(defun create-freq-tar (tf &optional (dir #P"/home/petter/Nedlastinger/corpus/2/**/*.gz" ))
  ; in this case the data files are kept as gz-files below this directory
  (let ((files (directory dir)))
    (loop for file in files do
      (print file)
      (parse-tar-file tf file))))

(defun create-freq-txt  (tf &optional (dir #P"/home/petter/Nedlastinger/corpus/1/**/*.txt"))
    (let ((files (directory dir)))
      (loop for file in files do
	(print file)
	(with-open-file (stream file)
	  (do ((line (read-line stream nil)
		     (read-line stream nil)))
	      ((null line))
	    (add-string tf line))))))

(defun create-freq()
  (let ((tf (make-instance 'tfidf)))
    (create-freq-tar tf)
    (create-freq-txt tf)
    tf
  ))