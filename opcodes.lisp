;;;; Z-machine Interpreter in Common Lisp
;;;; opcodes.lisp - Opcode implementations

(in-package :zmachine)

;;; ============================================================
;;; Opcode Tables
;;; ============================================================

(defvar *opcodes-0op* (make-hash-table))
(defvar *opcodes-1op* (make-hash-table))
(defvar *opcodes-2op* (make-hash-table))
(defvar *opcodes-var* (make-hash-table))
(defvar *opcodes-ext* (make-hash-table))

(defmacro defop (table num name lambda-list &body body)
  "Define an opcode"
  `(setf (gethash ,num ,table)
         (list ',name (lambda ,lambda-list ,@body))))

;;; ============================================================
;;; 2OP Opcodes (0x00 - 0x1F)
;;; ============================================================

;;; 2OP:1 - je a b ?(label)
(defop *opcodes-2op* 1 je (operands)
  (let ((a (first operands)))
    (do-branch (some (lambda (b) (= a b)) (rest operands)))))

;;; 2OP:2 - jl a b ?(label)
(defop *opcodes-2op* 2 jl (operands)
  (do-branch (< (to-signed (first operands)) 
                (to-signed (second operands)))))

;;; 2OP:3 - jg a b ?(label)
(defop *opcodes-2op* 3 jg (operands)
  (do-branch (> (to-signed (first operands)) 
                (to-signed (second operands)))))

;;; 2OP:4 - dec_chk (variable) value ?(label)
(defop *opcodes-2op* 4 dec_chk (operands)
  (let* ((var (first operands))
         (value (to-signed (second operands)))
         (current (to-signed (peek-variable var)))
         (new-val (1- current)))
    (write-variable var (to-unsigned new-val))
    (do-branch (< new-val value))))

;;; 2OP:5 - inc_chk (variable) value ?(label)
(defop *opcodes-2op* 5 inc_chk (operands)
  (let* ((var (first operands))
         (value (to-signed (second operands)))
         (current (to-signed (peek-variable var)))
         (new-val (1+ current)))
    (write-variable var (to-unsigned new-val))
    (do-branch (> new-val value))))

;;; 2OP:6 - jin obj1 obj2 ?(label)
(defop *opcodes-2op* 6 jin (operands)
  (do-branch (= (object-parent (first operands)) (second operands))))

;;; 2OP:7 - test bitmap flags ?(label)
(defop *opcodes-2op* 7 test (operands)
  (do-branch (= (logand (first operands) (second operands))
                (second operands))))

;;; 2OP:8 - or a b -> (result)
(defop *opcodes-2op* 8 or (operands)
  (store-result (fetch-store) 
                (logior (first operands) (second operands))))

;;; 2OP:9 - and a b -> (result)
(defop *opcodes-2op* 9 and (operands)
  (store-result (fetch-store) 
                (logand (first operands) (second operands))))

;;; 2OP:10 - test_attr obj attr ?(label)
(defop *opcodes-2op* 10 test_attr (operands)
  (do-branch (object-has-attr (first operands) (second operands))))

;;; 2OP:11 - set_attr obj attr
(defop *opcodes-2op* 11 set_attr (operands)
  (object-set-attr (first operands) (second operands)))

;;; 2OP:12 - clear_attr obj attr
(defop *opcodes-2op* 12 clear_attr (operands)
  (object-clear-attr (first operands) (second operands)))

;;; 2OP:13 - store (variable) value
(defop *opcodes-2op* 13 store (operands)
  (write-variable (first operands) (second operands)))

;;; 2OP:14 - insert_obj obj dest
(defop *opcodes-2op* 14 insert_obj (operands)
  (object-insert (first operands) (second operands)))

;;; 2OP:15 - loadw array word-index -> (result)
(defop *opcodes-2op* 15 loadw (operands)
  (store-result (fetch-store)
                (zm-read-word (+ (first operands) (* 2 (second operands))))))

;;; 2OP:16 - loadb array byte-index -> (result)
(defop *opcodes-2op* 16 loadb (operands)
  (store-result (fetch-store)
                (zm-read-byte (+ (first operands) (second operands)))))

;;; 2OP:17 - get_prop obj prop -> (result)
(defop *opcodes-2op* 17 get_prop (operands)
  (store-result (fetch-store)
                (object-get-prop (first operands) (second operands))))

;;; 2OP:18 - get_prop_addr obj prop -> (result)
(defop *opcodes-2op* 18 get_prop_addr (operands)
  (store-result (fetch-store)
                (object-get-prop-addr (first operands) (second operands))))

;;; 2OP:19 - get_next_prop obj prop -> (result)
(defop *opcodes-2op* 19 get_next_prop (operands)
  (store-result (fetch-store)
                (object-get-next-prop (first operands) (second operands))))

;;; 2OP:20 - add a b -> (result)
(defop *opcodes-2op* 20 add (operands)
  (store-result (fetch-store)
                (+ (to-signed (first operands)) 
                   (to-signed (second operands)))))

;;; 2OP:21 - sub a b -> (result)
(defop *opcodes-2op* 21 sub (operands)
  (store-result (fetch-store)
                (- (to-signed (first operands)) 
                   (to-signed (second operands)))))

;;; 2OP:22 - mul a b -> (result)
(defop *opcodes-2op* 22 mul (operands)
  (store-result (fetch-store)
                (* (to-signed (first operands)) 
                   (to-signed (second operands)))))

;;; 2OP:23 - div a b -> (result)
(defop *opcodes-2op* 23 div (operands)
  (let ((b (to-signed (second operands))))
    (when (zerop b)
      (error "Division by zero"))
    (store-result (fetch-store)
                  (truncate (to-signed (first operands)) b))))

;;; 2OP:24 - mod a b -> (result)
(defop *opcodes-2op* 24 mod (operands)
  (let ((b (to-signed (second operands))))
    (when (zerop b)
      (error "Division by zero"))
    (store-result (fetch-store)
                  (rem (to-signed (first operands)) b))))

;;; 2OP:25 - call_2s routine arg1 -> (result) [V4+]
(defop *opcodes-2op* 25 call_2s (operands)
  (do-call (first operands) (list (second operands)) (fetch-store)))

;;; 2OP:26 - call_2n routine arg1 [V5+]
(defop *opcodes-2op* 26 call_2n (operands)
  (do-call (first operands) (list (second operands)) nil))

;;; 2OP:27 - set_colour fg bg [V5+]
(defop *opcodes-2op* 27 set_colour (operands)
  ;; For now, ignore color settings
  (declare (ignore operands)))

;;; 2OP:28 - throw value stack-frame [V5+]
(defop *opcodes-2op* 28 throw (operands)
  (declare (ignore operands))
  (error "throw not implemented"))

;;; ============================================================
;;; 1OP Opcodes (0x00 - 0x0F in short form)
;;; ============================================================

;;; 1OP:0 - jz a ?(label)
(defop *opcodes-1op* 0 jz (operands)
  (do-branch (zerop (first operands))))

;;; 1OP:1 - get_sibling obj -> (result) ?(label)
(defop *opcodes-1op* 1 get_sibling (operands)
  (let ((sibling (object-sibling (first operands))))
    (store-result (fetch-store) sibling)
    (do-branch (not (zerop sibling)))))

;;; 1OP:2 - get_child obj -> (result) ?(label)
(defop *opcodes-1op* 2 get_child (operands)
  (let ((child (object-child (first operands))))
    (store-result (fetch-store) child)
    (do-branch (not (zerop child)))))

;;; 1OP:3 - get_parent obj -> (result)
(defop *opcodes-1op* 3 get_parent (operands)
  (store-result (fetch-store) (object-parent (first operands))))

;;; 1OP:4 - get_prop_len prop-addr -> (result)
(defop *opcodes-1op* 4 get_prop_len (operands)
  (store-result (fetch-store) 
                (if (zerop (first operands))
                    0
                    (property-length-at (first operands)))))

;;; 1OP:5 - inc (variable)
(defop *opcodes-1op* 5 inc (operands)
  (let* ((var (first operands))
         (value (to-signed (peek-variable var))))
    (write-variable var (to-unsigned (1+ value)))))

;;; 1OP:6 - dec (variable)
(defop *opcodes-1op* 6 dec (operands)
  (let* ((var (first operands))
         (value (to-signed (peek-variable var))))
    (write-variable var (to-unsigned (1- value)))))

;;; 1OP:7 - print_addr byte-addr
(defop *opcodes-1op* 7 print_addr (operands)
  (zm-print (decode-zstring (first operands))))

;;; 1OP:8 - call_1s routine -> (result) [V4+]
(defop *opcodes-1op* 8 call_1s (operands)
  (do-call (first operands) nil (fetch-store)))

;;; 1OP:9 - remove_obj obj
(defop *opcodes-1op* 9 remove_obj (operands)
  (object-remove (first operands)))

;;; 1OP:10 - print_obj obj
(defop *opcodes-1op* 10 print_obj (operands)
  (zm-print (object-name (first operands))))

;;; 1OP:11 - ret value
(defop *opcodes-1op* 11 ret (operands)
  (do-return (first operands)))

;;; 1OP:12 - jump offset
(defop *opcodes-1op* 12 jump (operands)
  (incf (zm-pc *zm*) (- (to-signed (first operands)) 2)))

;;; 1OP:13 - print_paddr packed-addr
(defop *opcodes-1op* 13 print_paddr (operands)
  (zm-print (decode-zstring (unpack-string-addr (first operands)))))

;;; 1OP:14 - load (variable) -> (result)
(defop *opcodes-1op* 14 load (operands)
  (store-result (fetch-store) (peek-variable (first operands))))

;;; 1OP:15 - not value -> (result) [V1-4] / call_1n routine [V5+]
(defop *opcodes-1op* 15 not/call_1n (operands)
  (if (<= (zm-version *zm*) 4)
      (store-result (fetch-store) (logxor (first operands) #xFFFF))
      (do-call (first operands) nil nil)))

;;; ============================================================
;;; 0OP Opcodes (0x00 - 0x0F in short form with type=11)
;;; ============================================================

;;; 0OP:0 - rtrue
(defop *opcodes-0op* 0 rtrue (operands)
  (declare (ignore operands))
  (do-return 1))

;;; 0OP:1 - rfalse
(defop *opcodes-0op* 1 rfalse (operands)
  (declare (ignore operands))
  (do-return 0))

;;; 0OP:2 - print (literal-string)
(defop *opcodes-0op* 2 print (operands)
  (declare (ignore operands))
  (zm-print (decode-zstring (zm-pc *zm*)))
  ;; Skip past the string
  (incf (zm-pc *zm*) (zstring-length-bytes (zm-pc *zm*))))

;;; 0OP:3 - print_ret (literal-string)
(defop *opcodes-0op* 3 print_ret (operands)
  (declare (ignore operands))
  (zm-print (decode-zstring (zm-pc *zm*)))
  (zm-newline)
  (do-return 1))

;;; 0OP:4 - nop
(defop *opcodes-0op* 4 nop (operands)
  (declare (ignore operands)))

;;; 0OP:5 - save ?(label) [V1-3] / illegal [V4] / save -> result [V5+]
(defop *opcodes-0op* 5 save (operands)
  (declare (ignore operands))
  (let ((filename (get-save-filename)))
    (if filename
        (let ((success (save-game filename)))
          (cond
            ((<= (zm-version *zm*) 3)
             (do-branch success))
            ((>= (zm-version *zm*) 5)
             (store-result (fetch-store) (if success 1 0))))
          (when success
            (format t "~&Game saved.~%")))
        (cond
          ((<= (zm-version *zm*) 3)
           (do-branch nil))
          ((>= (zm-version *zm*) 5)
           (store-result (fetch-store) 0))))))

;;; 0OP:6 - restore ?(label) [V1-3] / illegal [V4] / restore -> result [V5+]
(defop *opcodes-0op* 6 restore (operands)
  (declare (ignore operands))
  (let ((filename (get-save-filename)))
    (if filename
        (let ((success (restore-game filename)))
          (cond
            ((<= (zm-version *zm*) 3)
             (when success
               (format t "~&Game restored.~%"))
             (do-branch success))
            ((>= (zm-version *zm*) 5)
             ;; V5+: store result 2 (meaning "restored")
             (store-result (fetch-store) (if success 2 0))
             (when success
               (format t "~&Game restored.~%")))))
        (cond
          ((<= (zm-version *zm*) 3)
           (do-branch nil))
          ((>= (zm-version *zm*) 5)
           (store-result (fetch-store) 0))))))

;;; 0OP:7 - restart
(defop *opcodes-0op* 7 restart (operands)
  (declare (ignore operands))
  (reset-story)
  (setf (zm-running *zm*) t))

;;; 0OP:8 - ret_popped
(defop *opcodes-0op* 8 ret_popped (operands)
  (declare (ignore operands))
  (do-return (read-variable 0)))  ; Pop from stack

;;; 0OP:9 - pop [V1-4] / catch -> result [V5+]
(defop *opcodes-0op* 9 pop/catch (operands)
  (declare (ignore operands))
  (if (<= (zm-version *zm*) 4)
      (read-variable 0)  ; Just pop and discard
      (store-result (fetch-store) (length (zm-call-stack *zm*)))))

;;; 0OP:10 - quit
(defop *opcodes-0op* 10 quit (operands)
  (declare (ignore operands))
  (setf (zm-running *zm*) nil))

;;; 0OP:11 - new_line
(defop *opcodes-0op* 11 new_line (operands)
  (declare (ignore operands))
  (zm-newline))

;;; 0OP:12 - show_status [V3]
(defop *opcodes-0op* 12 show_status (operands)
  (declare (ignore operands))
  ;; Status line is not implemented in this basic version
  )

;;; 0OP:13 - verify ?(label)
(defop *opcodes-0op* 13 verify (operands)
  (declare (ignore operands))
  (do-branch t))  ; Always succeed for now

;;; 0OP:14 - [extended opcode escape]
;;; This is handled specially in decode

;;; 0OP:15 - piracy ?(label) [V5+]
(defop *opcodes-0op* 15 piracy (operands)
  (declare (ignore operands))
  (do-branch t))  ; Always "genuine"
