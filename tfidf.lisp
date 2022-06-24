(in-package #:fastw)

(defun printem (&rest args)
  (format t "~{~a~^ ~}" args)
  (print "")
  )


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
    ))
  (:documentation "Simple word count in a word - number structure, the word-total just keeps the total numbers")
  )


(defmethod add-string (h s)
  (let* ((words-raw (str:words (sb-unicode:lowercase s)))
	 (words (loop for w in words-raw when (every #'alpha-char-p w) collect w)))
    (loop for word in words do
      (incf (word-total h))
      (incf (gethash word (word-freq h) 0)))
    (when (>= (hash-table-count (word-freq h)) 400000) ; change for max words before throwing away the seldom ones
      (let* ((old-size  (hash-table-count (word-freq h)))
	     (vals (alexandria:hash-table-values (word-freq h)))
	     (sorted-vals (sort vals #'<))
	     (i (round (/ (length sorted-vals) 2)))
	     (median-value (nth i sorted-vals))
	     (all-words (alexandria:hash-table-keys (word-freq h)))
	     (to-small (loop for w in all-words when (<= (gethash w (word-freq h)) median-value) collect w)))
	(printem "Deleting words: " (length to-small))
	(loop for w in to-small do (remhash w (word-freq h)))
	(printem "New hash size: " (hash-table-count (word-freq h)) " Old hash size: " old-size)))))
	

(defmethod save-tf (h file-name)
  (cl-store:store h file-name))

(defmethod load-tf (file-name)
  (cl-store:restore file-name))


(defun occurrences (lst)
  (let ((table (make-hash-table :test 'equal)))       
    (loop for e in lst
          do (incf (gethash e table 0)))
    table))

(defmethod tf-idf (h sentence)
  "Not a canonical tf-idf as the tf is simply occurences of words and words total"
  (loop
    with words = (str:words sentence)
    with occurence-hash = (occurrences words)
    with doc-count = (length words)
    with freq-modifier = 0.2
    for word in words
    collect (let* ((tf (/ (expt (gethash word occurence-hash) freq-modifier) doc-count))
		   (idf (log (/ (word-total h) (gethash word (word-freq h) 1)))))
		   (* tf idf))))
		       

(defun combined-tf-idf (sentence large-h small-h &optional (weight-to-small 1))
  "Combine tf-idf from two idf-counters, it is possible to weight the small one higher by weight"
  (let* ((small (tf-idf small-h sentence))
	 (large (tf-idf large-h sentence))
	 (pairs (mapcar #'cons small large)))
    (mapcar (lambda (v)
	      (/ (+ (cdr v) (* (car v) weight-to-small)) (+ 1 weight-to-small))) pairs)))

	       

;; code below just builds the word counter above, can be replaced with any of you own code
;; it is based on open corpus


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
		   ; (print  (format nil "~A" entry))
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
    (create-freq-txt tf)
    (create-freq-tar tf)
    tf
  ))

;;
;; Here we read in the domain specific data
;; In this case we this is job ads, which is kept in a json file
;;

(defun read-domain-data (&optional (dir #P"/home/petter/dev/python/jobs.json"))
  (let  ((tf (make-instance 'tfidf)))
    (loop for job in  (cl-json:decode-json-from-source dir) do
      (let ((html (cdr (assoc :job-description job))))
	(when (stringp html)
	  (add-string tf (plump:text (plump:parse html))))))
	tf))

;;
;; tf-idf calculation
;;






	 
