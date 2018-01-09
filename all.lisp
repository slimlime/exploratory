;; todo, make a project

(defun bload (file)
  (load (concatenate 'string "exploratory/" file)))

(defun load-project ()
  (ql:quickload 'cl-6502)
  (ql:quickload 'lispbuilder-sdl)

  ;; the compiler...
  
  (bload "utils.lisp")
  (bload "compile.lisp")
  (bload "monitor.lisp")
  (bload "asmutils.lisp")
  (bload "analysis.lisp")

  ;; the game engine...

  (bload "state.lisp")
  (bload "string.lisp")
  (bload "sharedmem.lisp")
  (bload "chars.lisp")
  (bload "vicky-shared.lisp")
  (bload "vicky.lisp")
  (bload "justify.lisp")
  (bload "image.lisp")
  (bload "graphics.lisp")
  (bload "compress.lisp")
  (bload "parse.lisp")
  (bload "dispatcher.lisp")
  (bload "objects.lisp")
  (bload "declarations.lisp")
  (bload "location.lisp")
  (bload "handlers.lisp")

  ;; the games...
  
  (bload "simple-game.lisp"))
