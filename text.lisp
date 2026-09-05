;;;; Z-machine Interpreter in Common Lisp
;;;; text.lisp - Text encoding/decoding (ZSCII)

(in-package :zmachine)

;;; ============================================================
;;; Z-Character Alphabets
;;; ============================================================

;;; Default alphabets (Version 1)
(defparameter *alphabet-v1*
  #("abcdefghijklmnopqrstuvwxyz"   ; A0
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"   ; A1
    " 0123456789.,!?_#'\"/\\<-:()" ; A2
    ))

;;; Default alphabets (Version 2+)
(defparameter *alphabet-v2*
  #("abcdefghijklmnopqrstuvwxyz"   ; A0
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"   ; A1
    " \n0123456789.,!?_#'\"/\\-:()" ; A2 (position 1 is newline)
    ))

;;; Current alphabet table
(defvar *alphabet* nil)

(defun init-alphabet ()
  "Initialize alphabet table based on version"
  (setf *alphabet* 
        (if (<= (zm-version *zm*) 1)
            *alphabet-v1*
            *alphabet-v2*)))

;;; ============================================================
;;; Z-Character Decoding
;;; ============================================================

(defstruct ztext-state
  "State for Z-text decoding"
  (alphabet 0 :type (integer 0 2))   ; Current alphabet (0, 1, or 2)
  (shift nil)                         ; Temporary shift
  (abbrev-code nil)                   ; Pending abbreviation code
  (multi-byte nil)                    ; Multi-byte ZSCII sequence
  (multi-first nil))                  ; First byte of multi-byte

(defun decode-zchar (zchar state output)
  "Decode a single Z-character, updating state and appending to output"
  (let ((version (zm-version *zm*)))
    (cond
      ;; Handle pending abbreviation
      ((ztext-state-abbrev-code state)
       (let* ((abbrev-num (+ (* 32 (1- (ztext-state-abbrev-code state))) zchar))
              (abbrev-addr (* 2 (zm-read-word 
                                 (+ (zm-abbrev-addr *zm*) (* 2 abbrev-num))))))
         (setf (ztext-state-abbrev-code state) nil)
         (decode-zstring-from abbrev-addr output)))
      
      ;; Handle multi-byte ZSCII (escape sequence)
      ((ztext-state-multi-byte state)
       (cond
         ((ztext-state-multi-first state)
          ;; Second byte of escape sequence
          (let ((zscii (logior (ash (ztext-state-multi-first state) 5) zchar)))
            (when (plusp zscii)
              (write-char (code-char zscii) output)))
          (setf (ztext-state-multi-byte state) nil)
          (setf (ztext-state-multi-first state) nil))
         (t
          ;; First byte of escape sequence
          (setf (ztext-state-multi-first state) zchar))))
      
      ;; Z-character 0: space
      ((zerop zchar)
       (write-char #\Space output)
       (setf (ztext-state-shift state) nil))
      
      ;; Z-character 1
      ;;   V1  : newline
      ;;   V2+ : abbreviation
      ((and (= zchar 1) (= version 1))
       (write-char #\Newline output)
       (setf (ztext-state-shift state) nil))

      ((= zchar 1)
       (setf (ztext-state-abbrev-code state) zchar)
       (setf (ztext-state-shift state) nil))

      ;; Z-characters 2 and 3
      ;;   V1-2 : temporary shift, 2 one alphabet up and 3 one down
      ;;   V3+  : abbreviation
      ;; Without the V1-2 case these fell through to the alphabet table and
      ;; indexed it with zchar - 6, which is negative.
      ((and (<= 2 zchar 3) (<= version 2))
       (setf (ztext-state-shift state)
             (mod (+ (ztext-state-alphabet state)
                     (if (= zchar 2) 1 2))
                  3)))

      ((<= 2 zchar 3)
       (setf (ztext-state-abbrev-code state) zchar)
       (setf (ztext-state-shift state) nil))

      ;; Z-characters 4-5: shift characters
      ;;   V1-2 : shift lock, 4 one alphabet up and 5 one down
      ;;   V3+  : temporary shift, 4 to A1 and 5 to A2
      ((= zchar 4)
       (if (<= version 2)
           (setf (ztext-state-alphabet state)
                 (mod (1+ (ztext-state-alphabet state)) 3))
           (setf (ztext-state-shift state) 1)))

      ((= zchar 5)
       (if (<= version 2)
           (setf (ztext-state-alphabet state)
                 (mod (+ 2 (ztext-state-alphabet state)) 3))
           (setf (ztext-state-shift state) 2)))

      ;; Z-characters 6-31: alphabet characters
      (t
       (let ((current-alphabet (or (ztext-state-shift state)
                                   (ztext-state-alphabet state))))
         (cond
           ;; Special case: A2 position 6 is escape sequence
           ((and (= current-alphabet 2) (= zchar 6))
            (setf (ztext-state-multi-byte state) t))
           
           ;; Special case: A2 position 7 is newline (V2+)
           ((and (= current-alphabet 2) (= zchar 7) (>= version 2))
            (write-char #\Newline output))
           
           ;; Normal alphabet character
           (t
            (let ((alphabet-str (aref *alphabet* current-alphabet))
                  (char-index (- zchar 6)))
              (when (< -1 char-index (length alphabet-str))
                (write-char (char alphabet-str char-index) output)))))
         
         ;; Clear temporary shift
         (setf (ztext-state-shift state) nil))))))

(defun decode-zstring-from (addr &optional (output nil))
  "Decode Z-encoded string starting at address"
  (let ((stream (or output (make-string-output-stream)))
        (state (make-ztext-state)))
    (loop
      (let* ((word (zm-read-word addr))
             (end-bit (logbitp 15 word))
             (zchar1 (ldb (byte 5 10) word))
             (zchar2 (ldb (byte 5 5) word))
             (zchar3 (ldb (byte 5 0) word)))
        (decode-zchar zchar1 state stream)
        (decode-zchar zchar2 state stream)
        (decode-zchar zchar3 state stream)
        (when end-bit
          (return))
        (incf addr 2)))
    (if output
        nil
        (get-output-stream-string stream))))

(defun decode-zstring (addr)
  "Decode Z-encoded string at address, returning the string"
  (decode-zstring-from addr))

;;; ============================================================
;;; Z-String Length Calculation
;;; ============================================================

(defun zstring-length-bytes (addr)
  "Calculate the length in bytes of a Z-encoded string"
  (loop for offset from 0 by 2
        for word = (zm-read-word (+ addr offset))
        until (logbitp 15 word)
        finally (return (+ offset 2))))

;;; ============================================================
;;; ZSCII Output
;;; ============================================================

(defun zscii-to-char (zscii)
  "Convert ZSCII code to character"
  (cond
    ((= zscii 0) nil)                    ; Null
    ((= zscii 13) #\Newline)             ; Newline
    ((<= 32 zscii 126) (code-char zscii)) ; Printable ASCII
    (t #\?)))                             ; Unknown

(defun char-to-zscii (char)
  "Convert character to ZSCII code"
  (let ((code (char-code char)))
    (cond
      ((char= char #\Newline) 13)
      ((<= 32 code 126) code)
      (t 63))))  ; '?' for unknown

;;; ============================================================
;;; Print Functions with Block Translation
;;; ============================================================

(defvar *bilingual-mode* nil "When T, show both English and Japanese")
(defvar *line-buffer* "" "Buffer for current line")
(defvar *block-buffer* nil "Buffer for collecting multiple lines")

;;; ============================================================
;;; ANSI Styling
;;;
;;; The original English is dimmed, the translation is shown at normal
;;; brightness, and the status line is drawn in reverse video, so the three
;;; kinds of output are told apart at a glance. Escape sequences go only to
;;; the terminal, never into the Z-machine output buffer.
;;; ============================================================

(defvar *ansi-enabled* t
  "T always writes colour, NIL never does, :AUTO turns it off when output
is not a terminal")

(defvar *ansi-source* "2"
  "SGR parameters for the original English (2 = dim)")

(defvar *ansi-translation* "97"
  "SGR parameters for the translation (97 = bright white)")

(defvar *ansi-status* "44;97"
  "SGR parameters for the status line (44;97 = bright white on blue)")

(defvar *ansi-current* nil
  "Style currently active on the terminal")

(defun output-tty-p ()
  "NIL only when standard output is known not to be a terminal"
  (handler-case
      (not (eql 0 (sb-unix:unix-isatty
                   (sb-sys:fd-stream-fd sb-sys:*stdout*))))
    (error () t)))

(defun ansi-available-p ()
  "Whether escape sequences should be written"
  (cond ((null *ansi-enabled*) nil)
        ((eq *ansi-enabled* :auto) (output-tty-p))
        (t t)))

(defun ansi-style (code)
  "Switch the terminal to SGR CODE, or back to the default when CODE is NIL.
Every style is preceded by a reset, because SGR parameters are additive:
without it, bold applied over the grey of the source text would simply give
bold grey rather than a bold default colour."
  (when (and (ansi-available-p) (not (equal code *ansi-current*)))
    (format *standard-output* "~C[0~@[;~A~]m" #\Escape code)
    (setf *ansi-current* code)))

;;; ============================================================
;;; Status Line (V1-3)
;;;
;;; There is no screen model here, so the status line is printed as an
;;; ordinary line just before the ">" prompt rather than pinned to the top
;;; of the screen. In V1-3 the interpreter is responsible for drawing it
;;; before each input; show_status draws it too, for the rare story that
;;; asks explicitly.
;;; ============================================================

(defvar *status-line-enabled* t
  "Show the status line before each prompt (V1-3)")

(defvar *status-line-width* 76
  "Column width the status line is padded to")

(defvar *status-line-shown* nil
  "Set once the status line has been drawn for the current turn")

(defun display-width (string)
  "Width of STRING in terminal columns, counting CJK characters as two"
  (let ((width 0))
    (loop for c across string
          for code = (char-code c)
          do (incf width
                   (if (or (<= #x1100 code #x115F)
                           (<= #x2E80 code #xA4CF)
                           (<= #xAC00 code #xD7A3)
                           (<= #xF900 code #xFAFF)
                           (<= #xFE30 code #xFE6F)
                           (<= #xFF00 code #xFF60)
                           (<= #xFFE0 code #xFFE6))
                       2 1)))
    width))

(defun status-line-location ()
  "Name of the current room (global 0), translated when bilingual mode is on"
  (let* ((name (object-name (read-variable 16)))
         (translation (when (and *bilingual-mode* (fboundp 'translate-text))
                        (funcall 'translate-text name))))
    (if (and translation (not (string= translation name)))
        (format nil "~A (~A)" translation name)
        name)))

(defun status-line-right ()
  "Score and moves, or the time of day when the story is a time game"
  (let ((a (to-signed (read-variable 17)))
        (b (to-signed (read-variable 18))))
    (if (logbitp 1 (header-flags1))
        (format nil "Time: ~2,'0D:~2,'0D" (mod a 24) (mod b 60))
        (format nil "Score: ~D  Moves: ~D" a b))))

(defun show-status-line ()
  "Print the status line: location on the left, score on the right"
  (when (and *status-line-enabled* *zm* (<= (zm-version *zm*) 3))
    (let* ((left (status-line-location))
           (right (status-line-right))
           (gap (- *status-line-width*
                   (display-width left)
                   (display-width right))))
      (let ((line (concatenate 'string
                               left
                               (make-string (max 2 gap) :initial-element #\Space)
                               right)))
        (if (ansi-available-p)
            (format *standard-output* "~&~C[0;~Am~A~C[0m~%~%"
                    #\Escape *ansi-status* line #\Escape)
            (format *standard-output* "~&~A~%~%" line))
        (setf *ansi-current* nil))
      (force-output *standard-output*)
      (setf *status-line-shown* t))))

(defun flush-pending-line ()
  "Translate a line that was printed without a trailing newline.
A story may end its prompt mid-line, as in \"Do you wish to leave the game?
(Y is affirmative): \". Such a line is still sitting in the line buffer, so it
has to be picked up explicitly before the translations are printed."
  (when *bilingual-mode*
    (buffer-line-for-translation)
    (setf *line-buffer* "")))

(defun before-prompt ()
  "Called just before the \">\" prompt reaches the screen"
  (flush-pending-line)
  (flush-translation-block)
  (unless *status-line-shown*
    (show-status-line))
  (setf *status-line-shown* nil))

(defun set-text-style (style)
  "VAR:17 set_text_style, mapped onto SGR parameters.
0 roman, 1 reverse video, 2 bold, 4 italic, 8 fixed pitch."
  (ansi-style (case style
                (0 nil)
                (1 "7")
                (2 "1")
                (4 "3")
                (8 nil)
                (otherwise nil))))

(defvar *output-buffer-limit* 65536
  "Characters kept in the Z-machine output buffer before it is discarded.
Nothing reads the buffer back today, so letting it grow with every character
the story ever printed is simply a leak - a story stuck in a printing loop
exhausted the heap.")

(defvar *output-buffer-count* 0
  "Characters written to the output buffer since it was last discarded")

(defun buffer-game-output (count)
  "Account for COUNT characters written to the Z-machine output buffer"
  (incf *output-buffer-count* count)
  (when (> *output-buffer-count* *output-buffer-limit*)
    (get-output-stream-string (zm-output-buffer *zm*))
    (setf *output-buffer-count* 0)))

(defvar *current-window* 0
  "Window the story is writing to: 0 = main text, 1 = upper window")

(defvar *upper-window-height* 0
  "Number of lines the story reserved for the upper window")

(defvar *upper-window-rows*
  (make-array 0 :adjustable t :fill-pointer 0)
  "Rows of the upper window, as adjustable strings")

(defvar *upper-window-row* 0 "Cursor row in the upper window")
(defvar *upper-window-col* 0 "Cursor column in the upper window")

(defvar *upper-window-max-rows* 32
  "Upper bound on captured rows. Version 6 addresses the cursor in pixels
rather than character cells, so a story can ask for a row number far beyond
anything a text screen has.")

(defun upper-window-row (n)
  "Row N of the upper window, created on demand"
  (loop while (<= (fill-pointer *upper-window-rows*) n)
        do (vector-push-extend (make-array 0 :element-type 'character
                                            :adjustable t :fill-pointer 0)
                               *upper-window-rows*))
  (aref *upper-window-rows* n))

(defun upper-window-put (char)
  "Write one character at the upper window cursor"
  (if (char= char #\Newline)
      (progn (setf *upper-window-row*
                   (min (1+ *upper-window-row*) (1- *upper-window-max-rows*)))
             (setf *upper-window-col* 0))
      (let ((row (upper-window-row *upper-window-row*)))
        (when (>= *upper-window-col* *status-line-width*)
          (return-from upper-window-put))
        (loop while (< (fill-pointer row) *upper-window-col*)
              do (vector-push-extend #\Space row))
        (if (< *upper-window-col* (fill-pointer row))
            (setf (aref row *upper-window-col*) char)
            (vector-push-extend char row))
        (incf *upper-window-col*))))

(defun upper-window-write (text)
  "Write a string at the upper window cursor"
  (loop for c across text do (upper-window-put c)))

(defun upper-window-clear ()
  "Forget the contents of the upper window"
  (setf (fill-pointer *upper-window-rows*) 0
        *upper-window-row* 0
        *upper-window-col* 0))

(defun upper-window-flush ()
  "Draw what the story wrote into the upper window as status lines.
There is no screen model here, so the rows are printed where the cursor
happens to be, in the status style, rather than pinned to the top."
  (let ((rows (remove-if (lambda (r) (zerop (length (string-right-trim " " r))))
                         (coerce *upper-window-rows* 'list))))
    (when rows
      (dolist (row rows)
        (let* ((text (string-right-trim " " row))
               (gap (- *status-line-width* (display-width text)))
               (line (concatenate 'string text
                                  (make-string (max 0 gap)
                                               :initial-element #\Space))))
          (if (ansi-available-p)
              (format *standard-output* "~&~C[0;~Am~A~C[0m~%"
                      #\Escape *ansi-status* line #\Escape)
              (format *standard-output* "~&~A~%" line))))
      (setf *ansi-current* nil)
      (setf *status-line-shown* t)
      (force-output *standard-output*)))
  (upper-window-clear))

(defun split-upper-window (lines)
  "VAR:10 split_window"
  (setf *upper-window-height* lines)
  (when (zerop lines)
    (upper-window-clear)))

(defun select-window (window)
  "VAR:11 set_window. Leaving the upper window draws what it holds."
  (when (and (= *current-window* 1) (/= window 1))
    (upper-window-flush))
  (setf *current-window* window)
  (when (= window 1)
    (setf *upper-window-row* 0 *upper-window-col* 0)))

(defun set-window-cursor (line column)
  "VAR:15 set_cursor, 1-based, only meaningful in the upper window.
Version 6 gives the position in pixels, so the values are clamped to
something a text screen can actually hold."
  (when (= *current-window* 1)
    (setf *upper-window-row*
          (min (max 0 (1- line)) (1- *upper-window-max-rows*)))
    (setf *upper-window-col*
          (min (max 0 (1- column)) (1- *status-line-width*)))))

(defun zm-print (text)
  "Print text to Z-machine output"
  ;; The upper window is captured, not streamed
  (when (= *current-window* 1)
    (upper-window-write text)
    (return-from zm-print))
  ;; Check for prompt character before printing
  (if (find #\> text)
      (progn (before-prompt) (ansi-style nil))
      (ansi-style *ansi-source*))
  (write-string text (zm-output-buffer *zm*))
  (buffer-game-output (length text))
  (write-string text *standard-output*)
  ;; Buffer for translation
  (when *bilingual-mode*
    (setf *line-buffer* (concatenate 'string *line-buffer* text))
    (when (find #\Newline text)
      (buffer-line-for-translation)
      (setf *line-buffer* ""))))

(defun zm-print-char (char)
  "Print a character to Z-machine output"
  ;; The upper window is captured, not streamed
  (when (= *current-window* 1)
    (upper-window-put char)
    (return-from zm-print-char))
  ;; Check for prompt character before printing
  (if (char= char #\>)
      (progn (before-prompt) (ansi-style nil))
      (ansi-style *ansi-source*))
  (write-char char (zm-output-buffer *zm*))
  (buffer-game-output 1)
  (write-char char *standard-output*)
  ;; Buffer for translation (don't buffer the prompt)
  (when (and *bilingual-mode* (not (char= char #\>)))
    (setf *line-buffer* (concatenate 'string *line-buffer* (string char)))
    (when (char= char #\Newline)
      (buffer-line-for-translation)
      (setf *line-buffer* ""))))

(defun buffer-line-for-translation ()
  "Buffer a line and its translation for block output"
  (let ((trimmed (string-trim '(#\Space #\Newline #\Return) *line-buffer*)))
    (when (> (length trimmed) 2)
      (let ((translation (if (fboundp 'translate-text)
                             (funcall 'translate-text trimmed)
                             nil)))
        (push (cons trimmed translation) *block-buffer*)))))

(defun flush-translation-block ()
  "Print all buffered translations after the English block"
  (when (and *bilingual-mode* *block-buffer*)
    (let ((translations nil))
      ;; Collect translations (in order)
      (dolist (pair (reverse *block-buffer*))
        (when (cdr pair)
          (push (cdr pair) translations)))
      
      ;; Print translations (one newline between English and translation).
      ;; ~& first: a story may print a prompt with no trailing newline, and
      ;; the translation must not be appended to that line.
      (when translations
        (ansi-style *ansi-translation*)
        (format *standard-output* "~&")
        (dolist (trans (reverse translations))
          (format *standard-output* "~A~%" trans))
        (force-output *standard-output*))))
  (setf *block-buffer* nil))

(defun before-input ()
  "Called just before the story reads a line from the player.
Handles stories that read without printing a \">\" prompt first, and returns
the terminal to normal so that what the player types is not styled."
  (flush-pending-line)
  (flush-translation-block)
  (ansi-style nil)
  (force-output *standard-output*))

(defun zm-newline ()
  "Print a newline"
  (zm-print-char #\Newline))

(defun zm-print-num (num)
  "Print a number"
  (zm-print (format nil "~D" (to-signed num))))

;;; ============================================================
;;; Signed Number Conversion
;;; ============================================================

(defun to-signed (n)
  "Convert unsigned 16-bit to signed"
  (if (logbitp 15 n)
      (- n #x10000)
      n))

(defun to-unsigned (n)
  "Convert signed to unsigned 16-bit"
  (logand n #xFFFF))
