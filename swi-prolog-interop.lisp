(defpackage swi-prolog-interop
  (:use :cl)
  (:export start-prolog
	   stop-prolog
	   *swipl-process*
	   *swipl-wait-time*
	   assertz
	   query
	   query-X
	   query-multiple
	   test-process-stream))
(in-package :swi-prolog-interop)
(require :cl-ppcre)

(defun split-by (string char)
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position char string :start i)
          collect (subseq string i j)
          while j))

(defparameter *swipl-process* nil)
(defparameter *swipl-wait-time* 0.5)

(defun start-prolog (&optional list_of_databases)
  (let ((process 
	 (sb-ext:run-program "/usr/bin/swipl" list_of_databases
			     :output :stream
			     :input :stream
			     :wait nil
			     :search t
			     :error *standard-output*)))
    (setf *swipl-process* process)
    process))

(defun stop-prolog ()
  (close (sb-ext:process-output *swipl-process*))
  (close (sb-ext:process-input *swipl-process*))
  (sb-ext:process-close *swipl-process*))

(defun read-until-newline (process)
  (let ((r ""))
    (loop for c = (read-char-no-hang (sb-ext:process-output process))
       do (progn
	    (if (or (not c) (char= c #\newline))
		(return-from read-until-newline r)
		(setf r (concatenate 'string r (format nil "~c" c))))))))

(defun print-all-output (process &key (discard nil))
  (sleep *swipl-wait-time*)
  (loop 
     do (progn
	  (if (listen (sb-ext:process-output process))
	      (if (not discard)
		  (print (read-until-newline process))
		  (read-until-newline process))
	      (return)))))

(defun read-all-lines (process)
  (sleep 1)
  (let ((r (list)))
    (loop do (progn
	       (if (listen (sb-ext:process-output process))
		   (push (read-until-newline process) r)
		   (return))))
    r))

(defun send-to-prolog (process str)
  (format (sb-ext:process-input process) str)
  (finish-output (sb-ext:process-input process)))

(defun query (str)
  (send-to-prolog *swipl-process* (format nil "~a.~%" str))
  (let ((result (nth 0 (remove-if (lambda (x) (string= x "")) (read-all-lines *swipl-process*)))))
    (cond
      ((string= result "true.") t)
      ((string= result "false.") nil)
      (t nil))
    ))

(defun query-multiple (str)
  (cl-ppcre:register-groups-bind (parens)
      ("\\w+\\((.+)\\)" str :sharedp t)
    (let ((resulting_string "")
	  (scanner (cl-ppcre:create-scanner "[A-Z]+"))
	  (split_result (split-by parens #\,)))
      (loop for var in split_result
	 for i = 0 then (+ 1 i)
	 do (progn
	      (when (cl-ppcre:scan scanner var)
		(if (= i (- (length split_result) 1))
		    (setf resulting_string (concatenate 'string resulting_string (format nil ", print(~a)" var)))
		    (setf resulting_string (concatenate 'string resulting_string (format nil ", print(~a), print(:)" var)))))))
      (let ((query_string (format nil "~a~a, nl, false.~%" str resulting_string)))
	(format t "Sending query string:~a" query_string)
	(send-to-prolog *swipl-process* query_string)
	(send-to-prolog *swipl-process* (format nil "~%"))
	(remove nil
		(map 'list (lambda (pair)
			     (if (or (string= pair "") (string= pair "false."))
				 nil
				 (remove-if (lambda (item) (string= item "")) (split-by pair #\:))))
		     (read-all-lines *swipl-process*)))))))

(defun assertz (str)
  (let ((query_string (format nil "assertz(~a).~%" str)))
    (send-to-prolog *swipl-process* query_string)
    (send-to-prolog *swipl-process* (format nil "~%"))
    (if (string= "true." (nth 0 (remove-if (lambda (x) (string= x "")) (read-all-lines *swipl-process*))))
	t
	nil)))

(defun query-X (str)
  (cl-ppcre:register-groups-bind (X)
      ("\\w+\\(([A-Z]+),\\w+\\)" str :sharedp t)
    (let ((query_string (format nil "~a, print(~a), nl, false.~%" str X)))
      (send-to-prolog *swipl-process* query_string)
      (send-to-prolog *swipl-process* (format nil "~%"))
      (read-all-lines *swipl-process*))))


(defun test-process-stream ()
  (start-prolog)
  (assertz "has(bird,wings)")
  (assertz "can(X,fly):- has(X,wings)")
  (print (query "can(bird,fly)"))
  (print (query "can(dog,fly)"))
  ;;(send-to-prolog process "can(people,walk).~%")
  ;;(print-all-output process)
  (stop-prolog))
