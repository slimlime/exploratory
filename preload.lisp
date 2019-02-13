;;; Load libraries etc which are required for the project
;;; This file will be executed before the project builder binary
;;; is produced so that we can execute a build without having
;;; to execute these commands each time.

(defvar *preloaded* nil)

(unless *preloaded*
  (load "~/quicklisp/setup.lisp")
  (ql:quickload 'cl-6502)
  (ql:quickload 'lispbuilder-sdl))

(setf *preloaded* t)
