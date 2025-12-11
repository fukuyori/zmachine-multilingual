;;;; Z-machine Interpreter in Common Lisp
;;;; opcodes-var.lisp - VAR opcode implementations

(in-package :zmachine)

;;; ============================================================
;;; VAR Opcodes (0x00 - 0x1F with variable form)
;;; ============================================================

;;; VAR:0 - call routine ...args -> (result) [V1-3] / call_vs [V4+]
(defop *opcodes-var* 0 call (operands)
  (do-call (first operands) (rest operands) (fetch-store)))

;;; VAR:1 - storew array word-index value
(defop *opcodes-var* 1 storew (operands)
  (zm-write-word (+ (first operands) (* 2 (second operands)))
                 (third operands)))

;;; VAR:2 - storeb array byte-index value
(defop *opcodes-var* 2 storeb (operands)
  (zm-write-byte (+ (first operands) (second operands))
                 (third operands)))

;;; VAR:3 - put_prop obj prop value
(defop *opcodes-var* 3 put_prop (operands)
  (object-put-prop (first operands) (second operands) (third operands)))

;;; VAR:4 - sread/read text parse [V1-3] / read text parse time routine [V4] / aread [V5+]
(defop *opcodes-var* 4 read (operands)
  ;; Flush translation block before input prompt
  (when (fboundp 'flush-translation-block)
    (funcall 'flush-translation-block))
  (let* ((text-buffer (first operands))
         (parse-buffer (second operands))
         ;; Get input from user
         (input (progn
                  (force-output)
                  (read-line))))
    ;; Store input in text buffer
    (let ((max-len (if (<= (zm-version *zm*) 4)
                       (1- (zm-read-byte text-buffer))
                       (zm-read-byte text-buffer))))
      (setf input (subseq input 0 (min (length input) max-len)))
      (setf input (string-downcase input))
      
      ;; Write to text buffer
      (if (<= (zm-version *zm*) 4)
          ;; V1-4: text starts at byte 1, terminated by 0
          (progn
            (loop for i from 0 below (length input)
                  do (zm-write-byte (+ text-buffer 1 i) 
                                    (char-code (char input i))))
            (zm-write-byte (+ text-buffer 1 (length input)) 0))
          ;; V5+: byte 1 is length, text starts at byte 2
          (progn
            (zm-write-byte (+ text-buffer 1) (length input))
            (loop for i from 0 below (length input)
                  do (zm-write-byte (+ text-buffer 2 i)
                                    (char-code (char input i))))))
      
      ;; Tokenize if parse buffer given
      (when (and parse-buffer (not (zerop parse-buffer)))
        (tokenize-input input parse-buffer))
      
      ;; V5+: return terminating character
      (when (>= (zm-version *zm*) 5)
        (store-result (fetch-store) 13)))))  ; Return newline

;;; VAR:5 - print_char zscii-char
(defop *opcodes-var* 5 print_char (operands)
  (let ((char (zscii-to-char (first operands))))
    (when char
      (zm-print-char char))))

;;; VAR:6 - print_num value
(defop *opcodes-var* 6 print_num (operands)
  (zm-print-num (first operands)))

;;; VAR:7 - random range -> (result)
(defop *opcodes-var* 7 random (operands)
  (let ((range (to-signed (first operands))))
    (store-result (fetch-store)
                  (cond
                    ((plusp range)
                     (1+ (random range)))
                    ((minusp range)
                     ;; Seed random number generator
                     (setf *random-state* (make-random-state t))
                     0)
                    (t 0)))))

;;; VAR:8 - push value
(defop *opcodes-var* 8 push (operands)
  (write-variable 0 (first operands)))

;;; VAR:9 - pull (variable) [V1-5] / pull stack -> (result) [V6]
(defop *opcodes-var* 9 pull (operands)
  (if (<= (zm-version *zm*) 5)
      (write-variable (first operands) (read-variable 0))
      (store-result (fetch-store) (read-variable 0))))

;;; VAR:10 - split_window lines
(defop *opcodes-var* 10 split_window (operands)
  (declare (ignore operands))
  ;; Window splitting not implemented
  )

;;; VAR:11 - set_window window
(defop *opcodes-var* 11 set_window (operands)
  (declare (ignore operands))
  ;; Window management not implemented
  )

;;; VAR:12 - call_vs2 routine ...args -> (result) [V4+]
(defop *opcodes-var* 12 call_vs2 (operands)
  (do-call (first operands) (rest operands) (fetch-store)))

;;; VAR:13 - erase_window window [V4+]
(defop *opcodes-var* 13 erase_window (operands)
  (declare (ignore operands))
  ;; Clear screen (simplified)
  (format t "~%~%"))

;;; VAR:14 - erase_line value [V4+]
(defop *opcodes-var* 14 erase_line (operands)
  (declare (ignore operands)))

;;; VAR:15 - set_cursor line column [V4+]
(defop *opcodes-var* 15 set_cursor (operands)
  (declare (ignore operands)))

;;; VAR:16 - get_cursor array [V4+]
(defop *opcodes-var* 16 get_cursor (operands)
  (zm-write-word (first operands) 1)   ; row
  (zm-write-word (+ (first operands) 2) 1)) ; column

;;; VAR:17 - set_text_style style [V4+]
(defop *opcodes-var* 17 set_text_style (operands)
  (declare (ignore operands)))

;;; VAR:18 - buffer_mode flag [V4+]
(defop *opcodes-var* 18 buffer_mode (operands)
  (declare (ignore operands)))

;;; VAR:19 - output_stream stream table [V3+]
(defop *opcodes-var* 19 output_stream (operands)
  (declare (ignore operands)))

;;; VAR:20 - input_stream stream [V3+]
(defop *opcodes-var* 20 input_stream (operands)
  (declare (ignore operands)))

;;; VAR:21 - sound_effect number effect volume routine [V3+]
(defop *opcodes-var* 21 sound_effect (operands)
  (declare (ignore operands)))

;;; VAR:22 - read_char 1 time routine -> (result) [V4+]
(defop *opcodes-var* 22 read_char (operands)
  (declare (ignore operands))
  (force-output)
  (let ((char (read-char)))
    (store-result (fetch-store) (char-to-zscii char))))

;;; VAR:23 - scan_table x table len form -> (result) [V4+]
(defop *opcodes-var* 23 scan_table (operands)
  (let* ((x (first operands))
         (table (second operands))
         (len (third operands))
         (form (or (fourth operands) #x82))  ; Default: word entries
         (entry-size (logand form #x7F))
         (is-word (logbitp 7 form)))
    (store-result (fetch-store)
                  (loop for i from 0 below len
                        for addr = (+ table (* i entry-size))
                        for value = (if is-word
                                        (zm-read-word addr)
                                        (zm-read-byte addr))
                        when (= value x)
                          do (progn
                               (do-branch t)
                               (return addr))
                        finally (progn
                                  (do-branch nil)
                                  (return 0))))))

;;; VAR:24 - not value -> (result) [V5+]
(defop *opcodes-var* 24 not-v5 (operands)
  (store-result (fetch-store) (logxor (first operands) #xFFFF)))

;;; VAR:25 - call_vn routine ...args [V5+]
(defop *opcodes-var* 25 call_vn (operands)
  (do-call (first operands) (rest operands) nil))

;;; VAR:26 - call_vn2 routine ...args [V5+]
(defop *opcodes-var* 26 call_vn2 (operands)
  (do-call (first operands) (rest operands) nil))

;;; VAR:27 - tokenise text parse dictionary flag [V5+]
(defop *opcodes-var* 27 tokenise (operands)
  (let* ((text-buffer (first operands))
         (parse-buffer (second operands))
         (text-start (if (<= (zm-version *zm*) 4) 1 2))
         (text-len (if (<= (zm-version *zm*) 4)
                       ;; Find null terminator
                       (loop for i from text-start
                             until (zerop (zm-read-byte (+ text-buffer i)))
                             finally (return (- i text-start)))
                       (zm-read-byte (+ text-buffer 1))))
         (input (make-string text-len)))
    ;; Read text from buffer
    (loop for i from 0 below text-len
          do (setf (char input i)
                   (code-char (zm-read-byte (+ text-buffer text-start i)))))
    (tokenize-input input parse-buffer)))

;;; VAR:28 - encode_text zscii-text length from coded-text [V5+]
(defop *opcodes-var* 28 encode_text (operands)
  (declare (ignore operands))
  ;; Not implemented
  )

;;; VAR:29 - copy_table first second size [V5+]
(defop *opcodes-var* 29 copy_table (operands)
  (let ((first (first operands))
        (second (second operands))
        (size (to-signed (third operands))))
    (cond
      ((zerop second)
       ;; Zero the table
       (loop for i from 0 below (abs size)
             do (zm-write-byte (+ first i) 0)))
      ((plusp size)
       ;; Copy forward
       (loop for i from 0 below size
             do (zm-write-byte (+ second i) (zm-read-byte (+ first i)))))
      (t
       ;; Copy backward (for overlapping regions)
       (loop for i from (1- (abs size)) downto 0
             do (zm-write-byte (+ second i) (zm-read-byte (+ first i))))))))

;;; VAR:30 - print_table zscii-text width height skip [V5+]
(defop *opcodes-var* 30 print_table (operands)
  (let* ((addr (first operands))
         (width (second operands))
         (height (or (third operands) 1))
         (skip (or (fourth operands) 0)))
    (loop for row from 0 below height
          do (progn
               (loop for col from 0 below width
                     for char = (zm-read-byte (+ addr (* row (+ width skip)) col))
                     do (zm-print-char (code-char char)))
               (when (< row (1- height))
                 (zm-newline))))))

;;; VAR:31 - check_arg_count arg-num ?(label) [V5+]
(defop *opcodes-var* 31 check_arg_count (operands)
  (let* ((arg-num (first operands))
         (frame (car (zm-call-stack *zm*)))
         (supplied (if frame
                      ;; Count non-zero locals as arguments
                      ;; This is simplified; real implementation tracks actual args
                      (call-frame-local-count frame)
                      0)))
    (do-branch (<= arg-num supplied))))
