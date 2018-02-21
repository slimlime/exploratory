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

  (bload "base64.lisp")  
  (bload "state.lisp")
  (bload "huffman.lisp")
  (bload "string.lisp")
  (bload "sharedmem.lisp")
  (bload "chars.lisp")
  (bload "vicky-shared.lisp")
  (bload "justify.lisp")
  (bload "image.lisp")
  (bload "graphics.lisp")
  (bload "parse.lisp")
  (bload "objects.lisp")
  (bload "dispatcher.lisp")
  (bload "bits.lisp")
  (bload "vm.lisp")
  (bload "declarations.lisp")
  (bload "location.lisp")
  (bload "handlers.lisp")
  (bload "build.lisp")
  (bload "testing.lisp")

  ;; the games...
  
  (bload "simple-game.lisp")
  (bload "simple-game-tests.lisp"))

(defun load-vicky ()
  (ql:quickload 'lispbuilder-sdl)
  (bload "sharedmem.lisp")
  (bload "vicky-shared.lisp")
  (bload "vicky.lisp"))
