;; C-c ~ to assume project directory

;; load shared libraries etc

(load "preload.lisp")

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

;; tests

(load "simple-game-tests.lisp")
