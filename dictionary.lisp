;;;; Z-machine Interpreter in Common Lisp
;;;; dictionary.lisp - Dictionary and tokenization

(in-package :zmachine)

;;; ============================================================
;;; Dictionary Structure
;;;
;;; Header:
;;;   - n bytes: word separators
;;;   - 1 byte: entry length
;;;   - 2 bytes: number of entries
;;; Entries (sorted alphabetically):
;;;   - V1-3: 4 bytes Z-encoded word + entry-length-4 bytes data
;;;   - V4+:  6 bytes Z-encoded word + entry-length-6 bytes data
;;; ============================================================

(defun dictionary-separators ()
  "Return list of word separator characters"
  (let* ((dict-addr (zm-dictionary-addr *zm*))
         (sep-count (zm-read-byte dict-addr)))
    (loop for i from 1 to sep-count
          collect (code-char (zm-read-byte (+ dict-addr i))))))

(defun dictionary-entry-length ()
  "Return length of each dictionary entry"
  (let* ((dict-addr (zm-dictionary-addr *zm*))
         (sep-count (zm-read-byte dict-addr)))
    (zm-read-byte (+ dict-addr sep-count 1))))

(defun dictionary-entry-count ()
  "Return number of dictionary entries"
  (let* ((dict-addr (zm-dictionary-addr *zm*))
         (sep-count (zm-read-byte dict-addr)))
    (let ((count (zm-read-word (+ dict-addr sep-count 2))))
      ;; Can be negative for unsorted dictionary
      (abs (to-signed count)))))

(defun dictionary-entries-start ()
  "Return address of first dictionary entry"
  (let* ((dict-addr (zm-dictionary-addr *zm*))
         (sep-count (zm-read-byte dict-addr)))
    (+ dict-addr sep-count 4)))

(defun dictionary-word-length ()
  "Return length of encoded word in dictionary"
  (if (<= (zm-version *zm*) 3) 4 6))

;;; ============================================================
;;; Word Encoding for Dictionary Lookup
;;; ============================================================

(defun encode-zchar (char)
  "Encode a character as Z-character(s) for dictionary lookup"
  ;; For dictionary, we only use A0 alphabet mainly
  (let ((pos (position char "abcdefghijklmnopqrstuvwxyz")))
    (if pos
        (list (+ pos 6))
        ;; Check for digits and punctuation in A2
        (let ((a2-pos (position char " \n0123456789.,!?_#'\"/\\-:()")))
          (if a2-pos
              (list 5 (+ a2-pos 6))  ; Shift to A2
              ;; Unknown character - encode as space
              (list 0))))))

(defun encode-word-to-zchars (word)
  "Encode a word to a list of Z-characters"
  (let ((zchars nil))
    (loop for char across (string-downcase word)
          do (setf zchars (append zchars (encode-zchar char))))
    zchars))

(defun encode-word (word)
  "Encode a word for dictionary lookup, returning bytes"
  (let* ((version (zm-version *zm*))
         (max-zchars (if (<= version 3) 6 9))
         (zchars (encode-word-to-zchars word))
         ;; Truncate or pad to exact length
         (padded (loop for i from 0 below max-zchars
                       collect (if (< i (length zchars))
                                   (nth i zchars)
                                   5)))  ; Pad with shift character (5)
         (result nil)
         (num-words (if (<= version 3) 2 3)))
    ;; Pack into 2-byte words (3 Z-chars per word)
    (loop for i from 0 below num-words
          for base = (* i 3)
          for zc1 = (nth base padded)
          for zc2 = (nth (+ base 1) padded)
          for zc3 = (nth (+ base 2) padded)
          for word = (logior (ash zc1 10)
                            (ash zc2 5)
                            zc3)
          ;; Set end bit on last word
          do (when (= i (1- num-words))
               (setf word (logior word #x8000)))
             (push (ldb (byte 8 8) word) result)
             (push (ldb (byte 8 0) word) result))
    (nreverse result)))

;;; ============================================================
;;; Dictionary Lookup
;;; ============================================================

(defun compare-encoded-word (addr encoded)
  "Compare encoded word with dictionary entry"
  (loop for i from 0 below (length encoded)
        for dict-byte = (zm-read-byte (+ addr i))
        for enc-byte = (nth i encoded)
        do (cond
             ((< enc-byte dict-byte) (return -1))
             ((> enc-byte dict-byte) (return 1)))
        finally (return 0)))

(defun dictionary-lookup (word)
  "Look up word in dictionary, return entry address or 0"
  (let* ((encoded (encode-word word))
         (entry-len (dictionary-entry-length))
         (entry-count (dictionary-entry-count))
         (start (dictionary-entries-start)))
    ;; Binary search
    (loop with lo = 0
          with hi = (1- entry-count)
          while (<= lo hi)
          for mid = (floor (+ lo hi) 2)
          for addr = (+ start (* mid entry-len))
          for cmp = (compare-encoded-word addr encoded)
          do (cond
               ((zerop cmp) (return addr))
               ((plusp cmp) (setf lo (1+ mid)))    ; encoded > dict entry, go higher
               (t (setf hi (1- mid))))             ; encoded < dict entry, go lower
          finally (return 0))))

;;; ============================================================
;;; Debug Functions
;;; ============================================================

(defun show-dictionary-entry (addr)
  "Show a dictionary entry at address"
  (let* ((word-len (dictionary-word-length))
         (bytes (loop for i from 0 below word-len
                      collect (zm-read-byte (+ addr i)))))
    (format t "Entry at ~X: ~{~2,'0X ~}~%" addr bytes)))

(defun decode-dict-word (addr)
  "Decode Z-encoded word from dictionary"
  (decode-zstring addr))

(defun show-dictionary (&optional (count 20))
  "Show first N dictionary entries"
  (let* ((entry-len (dictionary-entry-length))
         (entry-count (dictionary-entry-count))
         (start (dictionary-entries-start)))
    (format t "Dictionary: ~D entries, ~D bytes each~%" entry-count entry-len)
    (loop for i from 0 below (min count entry-count)
          for addr = (+ start (* i entry-len))
          do (format t "~3D: ~X = ~S~%" i addr (decode-dict-word addr)))))

(defun test-encode (word)
  "Test encoding a word"
  (let ((encoded (encode-word word)))
    (format t "~S -> ~{~2,'0X ~}~%" word encoded)
    encoded))

(defun find-word-in-dict (word)
  "Debug: find a word in dictionary"
  (let* ((encoded (encode-word word))
         (entry-len (dictionary-entry-length))
         (entry-count (dictionary-entry-count))
         (start (dictionary-entries-start)))
    (format t "Looking for ~S encoded as ~{~2,'0X ~}~%" word encoded)
    (loop for i from 0 below entry-count
          for addr = (+ start (* i entry-len))
          for dict-bytes = (loop for j from 0 below (dictionary-word-length)
                                 collect (zm-read-byte (+ addr j)))
          when (equal encoded dict-bytes)
            do (format t "Found at entry ~D, addr ~X~%" i addr)
               (return addr)
          finally (format t "Not found~%")
                  (return 0))))

;;; ============================================================
;;; Tokenization
;;; ============================================================

(defun tokenize-input (input parse-buffer &optional (dictionary nil))
  "Tokenize input string into parse buffer"
  (declare (ignore dictionary))  ; Use default dictionary for now
  (let* ((separators (dictionary-separators))
         (max-tokens (zm-read-byte parse-buffer))
         (tokens nil)
         (pos 0))
    
    ;; Parse into tokens
    (loop while (< pos (length input))
          for char = (char input pos)
          do (cond
               ;; Skip spaces
               ((char= char #\Space)
                (incf pos))
               ;; Separator character (is its own token)
               ((member char separators :test #'char=)
                (push (list (string char) pos 1) tokens)
                (incf pos))
               ;; Word
               (t
                (let ((start pos))
                  (loop while (and (< pos (length input))
                                  (not (char= (char input pos) #\Space))
                                  (not (member (char input pos) separators :test #'char=)))
                        do (incf pos))
                  (push (list (subseq input start pos) start (- pos start)) tokens)))))
    
    (setf tokens (nreverse tokens))
    
    ;; Limit to max tokens
    (when (> (length tokens) max-tokens)
      (setf tokens (subseq tokens 0 max-tokens)))
    
    ;; Write to parse buffer
    (let ((text-offset (if (<= (zm-version *zm*) 4) 1 2)))
      ;; Write token count
      (zm-write-byte (+ parse-buffer 1) (length tokens))
      
      ;; Write each token
      (loop for i from 0 below (length tokens)
            for (word start len) in tokens
            for addr = (+ parse-buffer 2 (* i 4))
            for dict-entry = (dictionary-lookup word)
            do (progn
                 ;; Dictionary address (2 bytes)
                 (zm-write-word addr dict-entry)
                 ;; Length (1 byte)
                 (zm-write-byte (+ addr 2) len)
                 ;; Position in text buffer (1 byte)
                 (zm-write-byte (+ addr 3) (+ text-offset start)))))))
