;; todo, make a project

(defun bload (file)
  (load (concatenate 'string "exploratory/" file)))

(defun load-project ()
  (ql:quickload 'cl-6502)
  (bload "compile.lisp")
  (bload "monitor.lisp")
  (bload "asmutils.lisp")
  (bload "string.lisp")
  (bload "sharedmem.lisp")
  (bload "chars.lisp")
  (bload "justify.lisp")
  (bload "vicky-shared.lisp")
  (bload "image.lisp")
  (bload "compress.lisp")
  (bload "graphics.lisp"))

(defun load-vicky ()
  (ql:quickload 'lispbuilder-sdl)
  (bload "sharedmem.lisp")
  (bload "vicky-shared.lisp")
  (bload "vicky.lisp"))
