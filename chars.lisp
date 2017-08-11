;; this file contains code to write characters to screen
;; todo fix :present q (missing serif) O, clumsy, m a messy
;;                   - has too big a gap

;; for the justification algorithm to lookup the widths
(defparameter *font-widths* (make-hash-table :test 'equal))
(defparameter *font-height* 10)
(defparameter *line-height* (1+ *font-height*))

(defun byte-width (b)
  (when (= 1 (logand 1 b)) (return-from byte-width 8))
  (when (= 2 (logand 2 b)) (return-from byte-width 7))
  (when (= 4 (logand 4 b)) (return-from byte-width 6))
  (when (= 8 (logand 8 b)) (return-from  byte-width 5))
  (when (= 16 (logand 16 b)) (return-from byte-width 4))
  (when (= 32 (logand 32 b)) (return-from byte-width 3))
  (when (= 64 (logand 64 b)) (return-from byte-width 2))
  (when (= 128 (logand 128 b)) (return-from byte-width 1)))

(defun char-width (data)
  (byte-width (apply #'logior data)))

(defun reverse-word (word)
  (let ((bit #x8000)
	(tib #x0001)
	(value 0))
    (dotimes (i 16)
      (when (not (zerop (logand bit word)))
	(setf value (logior value tib)))
      (setf bit (ash bit -1))
      (setf tib (ash tib 1)))
    value))

;the width byte contains the width in the lo-nybble.
;kerning occurs if bit 6 is set AND bit 7 of the next
;character is set. e.g. for 'To' T would have bit 6 set
;and o would have bit 7 set

; bit-6 is set with :kern-r 
; bit-7 is set with :kern-l
; bit-6 and 7 are set with :kern-lr

(defun width-byte (data &optional kerning)
  (logior (char-width data)
	  (case kerning
	    (:kern-r #x40)
	    (:kern-l #x80)
	    (:kern-lr #xC0)
	    (otherwise 0))))

; The 3 font tables are going to take up about 2K!

;; todo the font-data has to be called before the justify functions
;;      can be used. Which is a bit rubbish if we want to put them
;;      later in memory. Need to remove this dependency.

(defun font-data ()

  (dc "Font tables. First byte of each character is the width")
  (dc "Characters are stored upside down")

  (let ((font-name nil))
    (flet ((defchar (c data &optional kerning)
	     ;;apply the define bytes function to the character data
	     (setf (gethash (cons font-name c) *font-widths*)
		   (char-width data))
	     (apply #'db (append (list (cons font-name c))
				 (list (width-byte data kerning))
				 (reverse data))))
	   (deffont (label)
	     (setf font-name label)
	     (label label :font)
	     (dc (format nil "~a font table" font-name))))

      ;; Now the actual font data
      ;; spaces are special cases, and, wasteful.
      ;; could be handled in the code

      ;;redefine deffont to use with-namespace


      ;;X
      ;; X
      ;;  X
      ;; X
      ;;X
      
      (db :prompt 3 0 0 128 64 32 64 128 0 0 0)

      (deffont :present)

      (db '(:present . #\ ) 3 0 0 0 0 0 0 0 0 0 0)
      (setf (gethash '(:present . #\ ) *font-widths*) 3)

      (defchar #\! '(128 128 128 128 128 128 0 0 128 0))
      (defchar #\' '(192 192 64 128 0 0 0 0 0 0))
      (defchar #\, '(0 0 0 0 0 0 192 192 64 128))
      (defchar #\- '(0 0 0 #xF0 0 0 0 0 0 0))
      (defchar #\. '(0 0 0 0 0 0 0 192 192 0))
      (defchar #\0 '(60 102 195 195 195 195 195 195 102 60))
      (defchar #\1 '(96 224 96 96 96 96 96 96 96 240))
      (defchar #\2 '(120 204 140 12 28 56 112 228 204 252))
      (defchar #\3 '(120 204 140 28 120 28 12 140 252 120))
      (defchar #\4 '(12 28 60 108 204 254 12 12 12 62))
      (defchar #\5 '(254 194 194 192 252 14 6 134 254 124))
      (defchar #\6 '(56 108 192 192 248 204 132 204 252 120))
      (defchar #\7 '(252 196 140 12 24 24 48 48 96 224))
      (defchar #\8 '(120 204 132 132 204 120 204 132 204 120))
      (defchar #\9 '(112 216 136 216 120 8 8 24 120 224))
      (defchar #\? '(56 124 140 8 48 112 56 0 24 24))
      (defchar #\A '(56 28 44 46 70 70 254 135 131 135))
      (defchar #\B '(254 99 99 99 126 99 99 99 99 254))
      (defchar #\C '(63 115 225 192 192 192 192 225 115 62))
      (defchar #\D '(252 102 99 97 97 97 97 99 102 252))
      (defchar #\E '(255 99 97 96 126 96 96 97 99 255))
      (defchar #\F '(255 99 97 96 124 96 96 96 96 240))
      (defchar #\G '(63 115 225 192 192 199 195 227 115 62))
      (defchar #\H '(247 99 99 99 127 99 99 99 99 247))
      (defchar #\I '(240 96 96 96 96 96 96 96 96 240))
      (defchar #\J '(60 24 24 24 24 24 152 152 152 112))
      (defchar #\K '(247 98 100 104 120 120 108 102 99 243))
      (defchar #\L '(240 96 96 96 96 96 96 98 102 254))
      (defchar #\M '(199 238 238 214 214 214 214 214 214 215))
      (defchar #\N '(207 230 230 214 214 206 206 198 198 231))
      (defchar #\O '(60 126 231 195 195 195 195 227 118 60))
      (defchar #\P '(254 99 99 99 99 126 96 96 96 240))
      (defchar #\Q '(60 126 231 195 195 195 195 235 126 63))
      (defchar #\R '(252 102 102 102 102 120 108 102 99 243))
      (defchar #\S '(124 198 194 192 124 14 134 134 206 124))
      (defchar #\T '(255 219 153 24 24 24 24 24 24 60))
      (defchar #\U '(247 99 99 99 99 99 99 99 34 28))
      (defchar #\V '(199 195 195 71 70 46 44 60 24 24))
      (defchar #\W '(247 219 219 219 219 219 90 110 108 40))
      (defchar #\X '(247 227 118 60 24 28 46 70 198 231))
      (defchar #\Y '(247 98 114 60 24 24 24 24 24 60))
      (defchar #\Z '(255 195 135 14 28 56 112 225 195 255))
      (defchar #\a '(0 120 204 204 12 124 204 205 118 0) :kern-r)
      (defchar #\b '(32 224 96 126 115 99 99 99 115 238))
      (defchar #\c '(0 62 98 192 192 192 192 98 60 0))
      (defchar #\d '(2 14 6 126 206 198 198 198 206 119) :kern-r)
      (defchar #\e '(0 60 98 254 192 192 192 98 60 0))
      (defchar #\f '(56 100 96 96 248 96 96 96 96 240) :kern-r)
      (defchar #\g '(0 118 206 198 198 206 118 6 70 60))
      (defchar #\h '(32 224 108 118 102 102 102 102 247 0))
      (defchar #\i '(48 48 0 16 112 48 48 48 48 120) :kern-lr)
      (defchar #\j '(96 96 0 32 224 96 96 96 96 192))
      (defchar #\k '(32 224 96 102 100 104 120 108 102 243))
      (defchar #\l '(32 224 96 96 96 96 96 96 96 240) :kern-r)
      (defchar #\m '(0 0 214 255 219 219 219 219 219 255))
      (defchar #\n '(0 238 115 99 99 99 99 99 247 0))
      (defchar #\o '(0 60 102 195 195 195 195 102 60 0))
      (defchar #\p '(238 115 99 99 99 115 110 96 96 240))
      (defchar #\q '(119 206 198 198 198 206 118 6 6 6))
      (defchar #\r '(0 238 118 102 96 96 96 96 240 0))
      (defchar #\s '(0 60 100 96 60 6 70 70 60 0) :kern-l)
      (defchar #\t '(32 96 96 248 96 96 96 96 104 48))
      (defchar #\u '(0 239 198 198 198 198 198 206 115 0))
      (defchar #\v '(0 247 98 98 52 52 52 24 24 0))
      (defchar #\w '(0 219 219 219 219 219 90 110 108 0))
      (defchar #\x '(0 247 98 52 24 12 22 35 247 0))
      (defchar #\y '(247 97 98 50 52 28 24 8 216 112))
      (defchar #\z '(0 254 134 12 24 48 96 194 254 0))

      (deffont :past)

      ;Kerning FeFiFo

      (db '(:past . #\ ) 3 0 0 0 0 0 0 0 0 0 0)    
      (setf (gethash '(:past . #\ ) *font-widths*) 3)

      (defchar #\! '(32 96 224 96 96 96 96 64 32 112))
      (defchar #\' '(0 0 64 192 192 128 0 0 0 0))
      (defchar #\, '(0 0 0 0 0 0 96 224 32 64))
      (defchar #\- '(0 0 0 #xF0 0 0 0 0 0 0))
      (defchar #\. '(0 0 0 0 0 0 0 64 224 64))
      (defchar #\0 '(60 78 198 206 214 230 198 198 228 120))
      (defchar #\1 '(16 48 112 176 48 48 48 48 124 248))
      (defchar #\2 '(60 78 134 6 4 8 16 113 254 28))
      (defchar #\3 '(60 120 144 32 120 28 12 204 232 112))
      (defchar #\4 '(4 12 28 44 76 140 254 252 12 8))
      (defchar #\5 '(62 124 64 248 252 142 6 198 228 120))
      (defchar #\6 '(28 46 70 192 220 238 198 198 228 120))
      (defchar #\7 '(62 124 136 8 16 124 48 48 56 48))
      (defchar #\8 '(60 78 198 230 124 206 198 198 228 120))
      (defchar #\9 '(60 110 198 198 238 118 4 72 240 96))
      (defchar #\? '(0 0 56 124 140 8 48 112 56 16))
      (defchar #\A '(63 79 143 155 27 63 51 99 99 198))
      (defchar #\B '(220 110 102 102 108 118 102 102 100 248))
      (defchar #\C '(29 94 216 216 216 208 208 97 126 60))
      (defchar #\D '(124 254 7 35 99 99 99 99 71 190))
      (defchar #\E '(47 94 208 208 222 220 208 225 127 62))
      (defchar #\F '(113 255 30 48 62 60 48 176 160 192))
      (defchar #\G '(29 94 208 210 215 211 195 227 114 60))
      (defchar #\H '(112 224 110 119 99 99 99 99 115 230))
      (defchar #\I '(241 62 44 44 44 44 44 44 127 128))
      (defchar #\J '(59 126 134 22 22 22 22 119 186 156))
      (defchar #\K '(112 224 110 119 102 124 102 103 115 66))
      (defchar #\L '(226 124 112 48 48 48 32 112 255 14))
      (defchar #\M '(155 254 214 214 214 214 214 214 215 130))
      (defchar #\N '(36 110 182 182 182 182 54 54 246 35))
      (defchar #\O '(46 87 211 211 211 211 211 227 114 60))
      (defchar #\P '(102 239 115 99 99 99 123 110 96 192))
      (defchar #\Q '(46 83 211 211 211 211 83 102 61 30))
      (defchar #\R '(6 111 243 98 124 108 102 102 107 243))
      (defchar #\S '(123 206 232 126 22 103 195 227 126 60))
      (defchar #\T '(113 254 60 40 104 104 104 113 62 28))
      (defchar #\U '(51 118 166 70 70 198 198 230 119 58))
      (defchar #\V '(17 115 179 179 51 51 50 52 56 112))
      (defchar #\W '(83 214 214 214 214 214 214 214 126 36))
      (defchar #\X '(199 230 108 56 126 252 56 60 238 199))
      (defchar #\Y '(194 99 99 119 63 27 3 62 124 128))
      (defchar #\Z '(113 255 30 8 126 252 64 240 255 14))
      (defchar #\a '(0 0 48 252 12 108 220 238 116 0))
      (defchar #\b '(96 208 192 220 238 198 198 198 100 56))
      (defchar #\c '(0 0 56 76 196 192 192 228 120 0))
      (defchar #\d '(64 96 48 56 92 204 204 204 232 112))
      (defchar #\e '(0 0 56 92 200 208 224 228 120 0) :kern-l)
      (defchar #\f '(28 44 96 96 248 96 96 112 96 64) :kern-r)
      (defchar #\g '(0 60 76 204 236 124 12 12 200 240))
      (defchar #\h '(64 192 192 220 238 198 198 198 196 200))
      (defchar #\i '(32 96 0 96 224 96 96 96 112 96) :kern-l)
      (defchar #\j '(32 96 0 96 224 96 96 96 96 192))
      (defchar #\k '(112 224 96 110 119 103 126 103 99 243))
      (defchar #\l '(96 224 96 96 96 96 96 96 112 96))
      (defchar #\m '(0 0 86 251 219 219 219 219 219 0))
      (defchar #\n '(0 0 44 118 230 102 102 119 102 0))
      (defchar #\o '(0 0 56 92 204 204 204 232 112 0) :kern-l)
      (defchar #\p '(0 28 46 102 102 230 120 224 96 64))
      (defchar #\q '(0 56 92 204 204 236 124 14 12 8))
      (defchar #\r '(0 0 46 118 228 96 96 112 96 0))
      (defchar #\s '(0 0 56 76 228 120 156 200 112 0))
      (defchar #\t '(32 96 96 240 96 96 96 112 96 64))
      (defchar #\u '(0 0 67 198 198 198 206 247 102 0))
      (defchar #\v '(0 0 38 107 227 98 100 120 48 0))
      (defchar #\w '(0 0 75 221 217 217 218 236 72 0))
      (defchar #\x '(0 0 67 230 120 56 61 206 132 0))
      (defchar #\y '(0 36 110 230 102 118 62 6 100 120))
      (defchar #\z '(0 0 49 126 140 16 49 126 140 0))

      (deffont :future)

      (db '(:future . #\ ) 3 0 0 0 0 0 0 0 0 0 0)
      (setf (gethash '(:future . #\ ) *font-widths*) 3)

      (defchar #\! '(192 192 192 192 224 224 224 224 0 224))
      (defchar #\' '(192 192 192 0 0 0 0 0 0 0))
      (defchar #\, '(0 0 0 0 0 0 0 0 0 96) :kern-l)
      (defchar #\- '(0 0 0 #xF0 0 0 0 0 0 0))
      (defchar #\. '(0 0 0 0 0 0 0 0 192 192) :kern-l)
      (defchar #\0 '(255 195 195 195 195 227 227 227 227 255))
      (defchar #\1 '(192 192 192 192 192 224 224 224 224 224))
      (defchar #\2 '(255 195 3 3 6 12 24 48 103 255))
      (defchar #\3 '(255 6 12 24 63 3 3 3 227 255))
      (defchar #\4 '(3 6 12 25 51 99 195 255 3 3))
      (defchar #\5 '(255 192 192 192 255 3 3 3 227 255))
      (defchar #\6 '(255 195 192 192 255 195 227 227 227 255))
      (defchar #\7 '(255 3 3 3 6 12 56 56 56 56))
      (defchar #\8 '(255 195 195 195 255 227 227 227 227 255))
      (defchar #\9 '(255 195 195 195 255 3 6 12 248 240))
      (defchar #\? '(255 195 195 3 63 56 56 0 56 56))
      (defchar #\A '(63 99 195 195 255 227 227 227 227 227))
      (defchar #\B '(252 196 196 196 254 230 230 230 230 254))
      (defchar #\C '(254 192 192 192 192 224 224 224 224 254))
      (defchar #\D '(254 198 198 198 198 230 230 230 230 254))
      (defchar #\E '(252 192 192 192 252 224 224 224 224 254))
      (defchar #\F '(252 192 192 192 248 224 224 224 224 224))
      (defchar #\G '(255 192 192 192 207 227 227 227 227 255))
      (defchar #\H '(195 195 195 195 255 227 227 227 227 227))
      (defchar #\I '(192 192 192 192 192 224 224 224 224 224))
      (defchar #\J '(6 6 6 6 6 6 6 230 230 254))
      (defchar #\K '(195 198 204 216 240 240 248 236 230 227))
      (defchar #\L '(192 192 192 192 192 224 224 224 224 254))
      (defchar #\M '(129 195 231 255 219 195 227 227 227 227))
      (defchar #\N '(131 195 227 243 219 207 231 227 227 227))
      (defchar #\O '(255 195 195 195 195 199 199 199 199 255))
      (defchar #\P '(255 195 195 195 255 192 224 224 224 224))
      (defchar #\Q '(255 195 195 195 195 227 227 239 231 255))
      (defchar #\R '(255 195 195 195 255 240 248 236 230 227))
      (defchar #\S '(255 192 96 48 24 236 230 227 227 255))
      (defchar #\T '(255 24 24 24 24 56 56 56 56 56) :kern-r)
      (defchar #\U '(195 195 195 195 195 227 227 227 227 255))
      (defchar #\V '(195 195 195 198 204 248 240 224 192 128) :kern-r)
      (defchar #\W '(195 195 195 195 195 219 255 231 195 129))
      (defchar #\X '(195 195 102 60 24 60 102 195 227 227))
      (defchar #\Y '(195 195 195 102 60 24 24 56 56 56))
      (defchar #\Z '(255 3 6 12 24 48 96 224 224 255))
      (defchar #\a '(0 0 63 3 255 195 195 195 255 0) :kern-l)
      (defchar #\b '(192 192 192 255 195 195 195 195 255 0))
      (defchar #\c '(0 0 0 255 192 192 192 192 255 0) :kern-l)
      (defchar #\d '(3 3 3 255 195 195 195 195 255 0))
      (defchar #\e '(0 0 0 255 195 195 255 192 255 0) :kern-l)
      (defchar #\f '(120 96 96 96 248 96 96 96 96 96))
      (defchar #\g '(0 0 0 255 195 195 255 3 195 255))
      (defchar #\h '(192 192 192 255 195 195 195 195 195 0))
      (defchar #\i '(192 192 0 192 192 192 192 192 192 0))
      (defchar #\j '(3 0 3 3 3 3 3 3 195 255))
      (defchar #\k '(0 0 198 204 216 240 216 204 198 0))
      (defchar #\l '(192 192 192 192 192 192 192 192 224 0))
      (defchar #\m '(0 0 0 195 231 255 219 195 195 0))
      (defchar #\n '(0 0 0 195 227 243 219 207 199 0))
      (defchar #\o '(0 0 0 255 195 195 195 195 255 0) :kern-l)
      (defchar #\p '(0 0 0 255 195 195 195 195 255 192))
      (defchar #\q '(0 0 0 255 195 195 195 195 255 3))
      (defchar #\r '(0 0 0 255 192 192 192 192 192 0))
      (defchar #\s '(0 0 0 255 192 255 3 3 255 0))
      (defchar #\t '(96 96 96 240 96 96 96 96 112 0))
      (defchar #\u '(0 0 0 195 195 195 195 195 127 0))
      (defchar #\v '(0 0 0 195 198 204 216 240 224 0))
      (defchar #\w '(0 0 0 195 195 219 255 231 195 0))
      (defchar #\x '(0 0 0 102 60 24 60 102 195 0))
      (defchar #\y '(0 0 0 195 195 195 255 3 63 0))
      (defchar #\z '(0 0 0 254 12 24 48 96 254 0)))))

