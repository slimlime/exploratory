(defparameter origin #x600)
      
(defun build-game (initial-location game-fn dictionary)  
  (reset-compiler)
  (reset-strings)
  (reset-bits)
  (reset-parser)
  (reset-object-model)
  (reset-game-state-ranges)
  (reset-images)
  ;;this (atm) must be called so the widths are initialised
  ;;todo, make a font-init function which just does the widths.
  (font-data)
  (reset-compiler)
  (reset-vm)
  (flet ((pass ()

	   (reset-dispatcher)
	   	   
	   (zeropage)
	   (org origin)
	   (CLD)
	   (label :start)
	   (set-colours #x10)
	   (sta16.zp (cons :font :present) :font)

	   (JSR :vm-enter)
	   (vm-nav initial-location)
	   (vm-done)
	   
	   (BRK)

	   (label :test-input)
	   (JSR :test-render-input)
	   (JSR :parse)
	   (JSR :dispatch)

	   (BRK "<-- The correct end point after input")

	   ;;game state
	   (label :game-code-start)

	   (funcall game-fn)
	   	   
	   (bit-table)

	   (label :game-code-end)

	   ;;inline functions we will need

	   (deref-w)
	   (print-message)
	   (memcpy)
	   (memset)
	   (typeset)
	   (fleuron)
	   (navigator)
	   (vm)
	   
	   ;;testing functions

	   (test-render-input)

	   (measure-size "Object Table"
	     (object-table))
	   (parser)

	   (measure-size "Dispatcher"
	     (dispatcher))
	   (measure-size "String Table"
	     (string-table dictionary nil t))
	   (measure-size "Images"
	     (image-table))
	   (huffman-decoder)
	   (image-decompressor)
	   (label :end)
	   ;font data is pretty boring so stick it here
	   (measure-size "Fonts"
	     (font-data))))
    
    (pass)
    (setf *word-table-built* t)
    ;;penultimate pass to ensure everything got a go and the structure
    ;;hasn't changed

    ;;these two passes to optimize dead vm branches
    (pass)
    (pass)
    
    (pass)
    (let ((end *compiler-ptr*))
      (pass)
      (assert (= end *compiler-ptr*) nil "Build was not stable"))
    
      (setf *compiler-final-pass* t)
      (pass)

      (format t "Build size ~a~%" (- *compiler-ptr* origin))))

(defun run-game (initial-location game-fn dictionary &key (break-on 'BRK))
  (build-game initial-location game-fn dictionary)
  (monitor-reset #x600)
  (monitor-run :break-on break-on)
  (setmem-copy (monitor-buffer)))
