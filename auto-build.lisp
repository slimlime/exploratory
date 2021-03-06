#!/usr/bin/sbcl --script
;;;
;;; background builder
;;;
;;; uses inotifywait
;;;
;;; and git check-ignore
;;;
;;; builds by creating an SBCL executable with the shared libraries already pre-loaded
;;;

;;; TODO don't inject now and log_line into build environment

(defparameter *preload-file* "preload.lisp")
(defparameter *project-file* "project.lisp")
(defparameter *success-file* "build/success")
(defparameter *project-builder* "build/build")
(defparameter *project-builder-script* "build/build-script.lisp")
(defparameter *version* 1)
(defparameter *built-version* 0)
(defparameter *inotifywait-handle* nil)
(defparameter *build-thread* nil)
(defparameter *build-interval* 10)

(setf *debugger-hook* #'(lambda (c h)
			  (declare (ignore h))
			  (format t "Build script error ~a~%" c)
			  (exit :code -1)))

(defparameter *log-functions*
  `(progn
    (defun __now ()
      (multiple-value-bind
	    (second minute hour day month year weekday dst-p tz)
	  (get-decoded-time)
	(declare (ignore weekday dst-p tz))
	(format nil "~d/~2,'0d/~d ~2,'0d:~2,'0d:~2,'0d"
		day month year hour minute second)))
    (defun __log-line (line)
      (princ (__now))
    (princ " - ")
    (write-line line)
    (finish-output))))

(eval *log-functions*)

(__log-line (format nil "Background builder script starting ~a" *load-truename*))

(load "../quicklisp/setup.lisp")

(ensure-directories-exist "build")

(ql:quickload "external-program")

;; On start-up, delete the project builder so we can go afresh

(defun delete-project-builder ()
  (when (probe-file *project-builder*)
    (delete-file *project-builder*)))

(delete-project-builder)

(defun set-build-status (pass)
  ;;todo, probably should delete the success file before building?
  ;;only doing this because it is a way to easily get into i3 status
  (if pass
      (with-open-file (f *success-file*
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
	(write-line (__now) f))
      (ignore-errors (delete-file *success-file*))))

(defparameter *project-script*
  `(progn
     (setf *debugger-hook* #'(lambda (c h)
			       (declare (ignore h))
			       (format t "Uncaught error ~a~%" c)
			       (exit :code -2)))

     (,@*log-functions*)

     (__log-line (format nil "Creating project builder for ~a at ~a" ,*project-file* *default-pathname-defaults*))
     
     (defun run-project-builder ()
       (let ((err nil))
	 (handler-case (load ,*project-file*)
	   (error (e) (setf err e)))
	 (when err (__log-line (format nil "Build error: ~a~%" err)))
	 (if err
	     (exit :code -1)
	     (exit :code 0))))
     
     (load ,*preload-file*)
     (save-lisp-and-die ,*project-builder* :toplevel #'run-project-builder
			:executable t)))

;; Create the script file we will need to execute out of process

(__log-line (format nil "Creating builder script..."))

(with-open-file (f *project-builder-script*
		   :direction :output
		   :if-exists :supersede
		   :if-does-not-exist :create)
  (print *project-script* f))

(defun handle-special-file (filename)
  (when (find filename (list *preload-file*))
    (when (probe-file *project-builder*)
      (__log-line (format nil "~a changed, project builder invalidated... deleting it." filename))
      (delete-project-builder))))

(defun build ()
  (unless (probe-file *project-builder*)
    (__log-line "  but first, creating the project builder...")
    (multiple-value-bind (status code)
	(external-program:run "/usr/bin/sbcl" (list "--disable-debugger" "--load" *project-builder-script*)
			      :output t)
      (__log-line (format nil "sbcl exited status=~a code=~a" status code))
      (unless (= 0 code)
	(__log-line "Failed to create project builder. Fix it and I'll try again...")
	(return-from build))))
  (multiple-value-bind (status code)
      (external-program:run *project-builder* nil :output t :error :output)
    (declare (ignore status))
    code))

(defun ignore-file (file)
  (let ((handle (external-program:start "/usr/bin/git" (list "check-ignore" file) :output :stream)))
    (loop for line = (read-line (external-program:process-output-stream handle) nil nil)
       while line
       do
	 ;;(format t "git says ignore ~a ~%" file)
	 (return-from ignore-file t)))
  nil)

(__log-line "Watching directory...")

(setf *inotifywait-handle* (external-program:start "/usr/bin/inotifywait"
						   (list "-mrq" "--format" "%w%f" "--exclude"
							 "^.\/build\/" "-e" "close_write,move,delete" ".")
						   :output :stream))

(__log-line "Starting build thread...")

(setf *build-thread*
      (sb-thread:make-thread #'(lambda ()
				   (__log-line "Build thread started.")
				   (tagbody
				    :go
				      (let ((version *version*))
					(when (/= version *built-version*)
					  (__log-line (format nil "Building mod ~a..." version))
					  (let ((result (build)))
					    (__log-line (if (= 0 result)
							  (format nil "Build mod ~a succeeded." version)
							  (format nil "Build mod ~a failed, exit code ~a" version result)))
					    (set-build-status (= 0 result))))
					(setf *built-version* version))
				      (sleep *build-interval*)
				      (go :go)))))

(handler-case
    (loop for filename = (read-line (external-program:process-output-stream *inotifywait-handle*) nil nil)
       while filename
       do
	 ;;(__log-line (format nil "DEBUG ~a changed~%" filename))
	 (unless (handle-special-file filename)
	   (unless (ignore-file filename)
	     (incf *version*)
	     (__log-line (format nil "~a changed (mod ~a)." filename *version*)))))
  (sb-sys:interactive-interrupt (condition)
    (declare (ignore condition))
    (terpri)
    (when *inotifywait-handle*
      (external-program:signal-process *inotifywait-handle* 9)
      (setf *inotifywait-handle* nil))
    (when *build-thread*
      (sb-thread:terminate-thread *build-thread*)
      (ignore-errors (sb-thread:join-thread *build-thread* :timeout 5)))
    (exit :code 0 :abort t)))
