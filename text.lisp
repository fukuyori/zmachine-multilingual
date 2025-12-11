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
      
      ;; Z-characters 1-3: abbreviations (V2+) or newline (V1)
      ((and (<= 1 zchar 3) (>= version 2))
       (setf (ztext-state-abbrev-code state) zchar)
       (setf (ztext-state-shift state) nil))
      
      ((and (= zchar 1) (= version 1))
       (write-char #\Newline output)
       (setf (ztext-state-shift state) nil))
      
      ;; Z-characters 4-5: shift characters
      ((= zchar 4)
       (if (and (<= version 2) (null (ztext-state-shift state)))
           ;; V1-2: shift lock
           (setf (ztext-state-alphabet state) 
                 (mod (1+ (ztext-state-alphabet state)) 3))
           ;; V3+: temporary shift
           (setf (ztext-state-shift state) 
                 (mod (1+ (or (ztext-state-shift state)
                             (ztext-state-alphabet state))) 3))))
      
      ((= zchar 5)
       (if (and (<= version 2) (null (ztext-state-shift state)))
           ;; V1-2: shift lock
           (setf (ztext-state-alphabet state) 
                 (mod (+ 2 (ztext-state-alphabet state)) 3))
           ;; V3+: temporary shift
           (setf (ztext-state-shift state) 
                 (mod (+ 2 (or (ztext-state-shift state)
                             (ztext-state-alphabet state))) 3))))
      
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
              (when (< char-index (length alphabet-str))
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

(defun zm-print (text)
  "Print text to Z-machine output"
  ;; Check for prompt character before printing
  (when (and *bilingual-mode* (find #\> text))
    (flush-translation-block))
  (write-string text (zm-output-buffer *zm*))
  (write-string text *standard-output*)
  ;; Buffer for translation
  (when *bilingual-mode*
    (setf *line-buffer* (concatenate 'string *line-buffer* text))
    (when (find #\Newline text)
      (buffer-line-for-translation)
      (setf *line-buffer* ""))))

(defun zm-print-char (char)
  "Print a character to Z-machine output"
  ;; Check for prompt character before printing
  (when (and *bilingual-mode* (char= char #\>))
    (flush-translation-block))
  (write-char char (zm-output-buffer *zm*))
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
      
      ;; Print translations without separator
      (when translations
        (dolist (trans (reverse translations))
          (format *standard-output* "~A~%" trans)))))
  (setf *block-buffer* nil))

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
