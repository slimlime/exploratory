;; C-c ~ to assume project directory

;; load shared libraries etc

(defun now ()
  (multiple-value-bind
	(second minute hour day month year weekday dst-p tz)
      (get-decoded-time)
    (declare (ignore weekday dst-p tz))
    (format nil "~d/~2,'0d/~d ~2,'0d:~2,'0d:~2,'0d"
	    day month year hour minute second)))

(defun log-line (line)
  (princ (now))
  (princ " - ")
  (write-line line)
  (finish-output))

(log-line "Starting preload...")

(load "preload.lisp")

(log-line "Preload done.")

(log-line "Loading project...")

;; the compiler...

(load "utils.lisp")
(load "compile.lisp")
(load "monitor.lisp")
(load "asmutils.lisp")
(load "analysis.lisp")

;; the game engine...

(load "base64.lisp")  
(load "state.lisp")
(load "huffman.lisp")
(load "string.lisp")
(load "chars.lisp")
(load "vicky-shared.lisp")
(load "justify.lisp")
(load "image.lisp")
(load "graphics.lisp")
(load "parse.lisp")
(load "objects.lisp")
(load "dispatcher.lisp")
(load "bits.lisp")
(load "vm.lisp")
(load "declarations.lisp")
(load "location.lisp")
(load "handlers.lisp")
(load "build.lisp")
(load "testing.lisp")

;; the games...

(load "simple-game.lisp")

(log-line "Loading project done.")

;; tests

(log-line "Starting tests...")

(load "simple-game-tests.lisp")

(log-line "Tests complete.")

(flet ((dump-test-results (table name)
	 (format t "~a ~a~%" name (hash-table-count table))
	 (when (> (hash-table-count table) 0)
	   (maphash #'(lambda (test msg)
			(format t  "~a -> ~a~%" test msg))
		    table))))
  (dump-test-results *failing-tests* "Failed :")
  (terpri)
  (dump-test-results *warning-tests* "Warning:"))

(when (> (hash-table-count *failing-tests*) 0)
  (error "Failing tests; build failed"))
