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

(defvar *status-pending* nil
  "A status window was updated and its bar has not been drawn yet")

(defun draw-pending-status ()
  "Draw whichever kind of status line this story has, once per turn"
  (cond (*status-pending* (draw-status-bar))
        ((not *status-line-shown*) (show-status-line)))
  (setf *status-line-shown* nil))

(defun before-prompt ()
  "Called just before the \">\" prompt reaches the screen"
  (flush-pending-line)
  (flush-translation-block)
  (draw-pending-status))

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

(defvar *memory-streams* nil
  "Open output stream 3 redirections, innermost first, as (table . count).
A story measures how wide a string will be by printing it to a table in
memory rather than to the screen. With this unimplemented the measuring pass
was printed as well, which is why a centred room name appeared twice.")

(defvar *screen-output-enabled* t
  "Whether output stream 1, the screen, is selected")

(defun memory-stream-active-p ()
  (and *memory-streams* t))

(defun open-memory-stream (table)
  "Send output to TABLE instead of the screen"
  (push (cons table 0) *memory-streams*))

(defun close-memory-stream ()
  "Stop redirecting, and record how many bytes were written"
  (let ((entry (pop *memory-streams*)))
    (when entry
      (zm-write-word (car entry) (cdr entry)))))

(defun memory-stream-write (text)
  "Append TEXT to the innermost open memory stream"
  (let ((entry (first *memory-streams*)))
    (loop for c across text
          do (zm-write-byte (+ (car entry) 2 (cdr entry)) (char-to-zscii c))
             (incf (cdr entry)))))

(defvar *at-line-start* t
  "Whether the last thing printed ended a line. A story that asks for input
without printing a prompt of its own leaves the player looking at a screen
that gives no sign it is waiting.")

(defvar *keypress-hint* "[key then Enter]"
  "Shown when a story waits for a single keypress without prompting.
Input is read a line at a time, so the key has to be followed by Enter. The
story usually wants a particular key - a menu asking for M or Q, say - and
Enter on its own delivers only a newline, which such a menu ignores.
NIL shows nothing.")

(defvar *input-hint* "[type a command]"
  "Shown when a story waits for a line without prompting. NIL shows nothing.")

(defvar *hint-showing* nil
  "Length of the hint currently on screen, so it can be wiped again")

(defun show-input-hint (hint)
  "Say that input is expected, unless the story already said so"
  (when (and hint *at-line-start*)
    (if (ansi-available-p)
        (format *standard-output* "~C[0;~Am~A~C[0m " #\Escape *ansi-status*
                hint #\Escape)
        (format *standard-output* "~A " hint))
    (setf *ansi-current* nil)
    (setf *at-line-start* nil)
    (setf *hint-showing* (1+ (display-width hint)))
    (force-output *standard-output*)))

(defun input-tty-p ()
  "NIL only when standard input is known not to be a terminal"
  (handler-case
      (not (eql 0 (sb-unix:unix-isatty
                   (sb-sys:fd-stream-fd sb-sys:*stdin*))))
    (error () t)))

(defun erase-input-hint (&optional cursor-moved-down)
  "Wipe the waiting-for-input hint.
Input is line buffered, so a keypress is really a keypress followed by
Return, and the terminal echoes that Return before we ever see the key. The
cursor is then a line below the hint, and clearing the current line would
leave the hint standing. CURSOR-MOVED-DOWN says to step back up first."
  (when *hint-showing*
    (cond
      ((ansi-available-p)
       (when cursor-moved-down
         (format *standard-output* "~C[A" #\Escape))
       (format *standard-output* "~C~C[K" #\Return #\Escape))
      (t
       (when cursor-moved-down
         (format *standard-output* "~C[A" #\Escape))
       (format *standard-output* "~C~A~C" #\Return
               (make-string *hint-showing* :initial-element #\Space)
               #\Return)))
    (setf *hint-showing* nil)
    (setf *at-line-start* t)
    (force-output *standard-output*)))

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

(defun upper-window-erase-line ()
  "VAR:14 erase_line: clear from the cursor to the end of the current row.
Without this a shorter value written over a longer one leaves the tail of the
old text behind."
  (when (and (status-window-p *current-window*)
             (< *upper-window-row* (fill-pointer *upper-window-rows*)))
    (let ((row (aref *upper-window-rows* *upper-window-row*)))
      (when (< *upper-window-col* (fill-pointer row))
        (setf (fill-pointer row) *upper-window-col*)))))

(defun upper-window-clear ()
  "Forget the contents of the upper window"
  (setf (fill-pointer *upper-window-rows*) 0
        *upper-window-row* 0
        *upper-window-col* 0))

(defvar *status-line-min-content* 4
  "Shortest upper window content still worth drawing as a status bar.
The window keeps its contents between draws, so a story that rewrites a
single field still produces the whole bar; this only skips a window that has
next to nothing in it.")

(defun status-content-worth-drawing-p (rows)
  "Whether ROWS carry enough to be worth a bar of their own"
  (>= (length (string-trim " " (format nil "~{~A~}" rows)))
      *status-line-min-content*))

(defun upper-window-flush ()
  "Note that the status window changed.
Drawing it here would put the bar between a block of story text and its
translation, because the story updates its status before printing the
prompt. The bar is drawn at the prompt instead, so the order is always
story text, translation, status, prompt - the same as in Versions 1 to 3."
  (setf *status-pending* t))

(defvar *status-bar-max-rows* 2
  "How many rows the status window may have and still be drawn as a bar.
A story also uses the upper window for whole screens - Arthur puts its hint
menu there - and painting a menu in reverse video from edge to edge makes it
unreadable. Anything taller is printed as ordinary text instead.")

(defun draw-status-bar ()
  "Print the captured status window."
  (let ((rows (remove-if (lambda (r) (zerop (length (string-right-trim " " r))))
                         (coerce *upper-window-rows* 'list))))
    (when (and rows (status-content-worth-drawing-p rows))
      (if (<= (length rows) *status-bar-max-rows*)
          ;; A status line: one bar the width of the screen
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
          ;; A whole screen of its own: print it plainly
          (progn
            (ansi-style nil)
            (format *standard-output* "~&~%")
            (dolist (row rows)
              (format *standard-output* "~A~%" (string-right-trim " " row)))
            (terpri *standard-output*)))
      (setf *ansi-current* nil)
      (setf *status-line-shown* t)
      (force-output *standard-output*)))
  (setf *status-pending* nil))

(defun split-upper-window (lines)
  "VAR:10 split_window"
  (setf *upper-window-height* lines)
  (when (zerop lines)
    (upper-window-clear)))

(defun window-property (window property)
  "One entry of a Version 6 window property table.
Answering zero to everything, as this used to, is not harmless: a story that
asks a window for its size and then divides by it stops with a division by
zero. Arthur does exactly that."
  (let* ((upper (eql window 1))
         (font-w (header-font-width))
         (font-h (header-font-height)))
    (case property
      (0 1)                             ; Y coordinate
      (1 1)                             ; X coordinate
      (2 (if upper                      ; Y size
             (max font-h (* *upper-window-height* font-h))
             *screen-pixel-height*))
      (3 *screen-pixel-width*)          ; X size
      (4 (1+ (* *upper-window-row* font-h)))   ; Y cursor
      (5 (1+ (* *upper-window-col* font-w)))   ; X cursor
      (12 1)                            ; font number
      (13 (logior (ash font-h 8) font-w))      ; font size
      (15 *screen-rows*)                ; line count
      (otherwise 0))))

(defun status-window-p (window)
  "Whether WINDOW is one of the ones captured into the status bar.
Window 0 is the main text. Version 6 has eight windows and a story may use
any of the others for decoration - Zork Zero puts part of its status in
window 7 - so everything above zero is captured."
  (plusp window))

(defun select-window (window)
  "VAR:11 set_window. Leaving a status window draws what it holds."
  (when (and (status-window-p *current-window*)
             (not (status-window-p window)))
    (upper-window-flush))
  (setf *current-window* window)
  (when (status-window-p window)
    (setf *upper-window-row* 0 *upper-window-col* 0)))

(defun set-window-cursor (line column)
  "VAR:15 set_cursor, 1-based, only meaningful in the upper window.
Version 6 gives the position in pixels rather than character cells, so it is
divided by the font size the header reports. Taking pixels for cells put
everything in the same place and ran the line off the right edge."
  (when (status-window-p *current-window*)
    (let ((row (1- line))
          (col (1- column)))
      (when (= (zm-version *zm*) 6)
        (let ((font-width (max 1 (zm-read-byte #x26)))
              (font-height (max 1 (zm-read-byte #x27))))
          (setf row (floor row font-height))
          (setf col (floor col font-width))))
      (setf *upper-window-row*
            (min (max 0 row) (1- *upper-window-max-rows*)))
      (setf *upper-window-col*
            (min (max 0 col) (1- *status-line-width*))))))

(defun zm-print (text)
  "Print text to Z-machine output"
  ;; Stream 3 takes precedence over everything else, and nothing reaches the
  ;; screen while it is open
  (when (memory-stream-active-p)
    (memory-stream-write text)
    (return-from zm-print))
  (unless *screen-output-enabled*
    (return-from zm-print))
  (erase-input-hint)
  ;; The upper window is captured, not streamed
  (when (status-window-p *current-window*)
    (upper-window-write text)
    (return-from zm-print))
  ;; Check for prompt character before printing
  (if (find #\> text)
      (progn (before-prompt) (ansi-style nil))
      (ansi-style *ansi-source*))
  (write-string text (zm-output-buffer *zm*))
  (buffer-game-output (length text))
  (when (plusp (length text))
    (setf *at-line-start* (char= (char text (1- (length text))) #\Newline)))
  (write-string text *standard-output*)
  ;; Buffer for translation
  (when *bilingual-mode*
    (setf *line-buffer* (concatenate 'string *line-buffer* text))
    (when (find #\Newline text)
      (buffer-line-for-translation)
      (setf *line-buffer* ""))))

(defun zm-print-char (char)
  "Print a character to Z-machine output"
  (when (memory-stream-active-p)
    (memory-stream-write (string char))
    (return-from zm-print-char))
  (unless *screen-output-enabled*
    (return-from zm-print-char))
  (erase-input-hint)
  ;; The upper window is captured, not streamed
  (when (status-window-p *current-window*)
    (upper-window-put char)
    (return-from zm-print-char))
  ;; Check for prompt character before printing
  (if (char= char #\>)
      (progn (before-prompt) (ansi-style nil))
      (ansi-style *ansi-source*))
  (write-char char (zm-output-buffer *zm*))
  (buffer-game-output 1)
  (setf *at-line-start* (char= char #\Newline))
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
  (draw-pending-status)
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
