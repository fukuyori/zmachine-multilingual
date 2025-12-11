;;;; Z-machine Interpreter in Common Lisp
;;;; decode.lisp - Instruction decoding

(in-package :zmachine)

;;; ============================================================
;;; Instruction Forms and Operand Types
;;; ============================================================

;;; Operand types (2-bit encoding)
(defconstant +op-large+  #b00)  ; Large constant (2 bytes)
(defconstant +op-small+  #b01)  ; Small constant (1 byte)
(defconstant +op-var+    #b10)  ; Variable
(defconstant +op-omit+   #b11)  ; Omitted

;;; Instruction forms
(deftype instruction-form ()
  '(member :long :short :extended :variable))

;;; ============================================================
;;; Instruction Decoding
;;; ============================================================

(defun fetch-byte ()
  "Fetch next byte from PC and increment PC"
  (prog1 (zm-read-byte (zm-pc *zm*))
    (incf (zm-pc *zm*))))

(defun fetch-word ()
  "Fetch next word from PC and increment PC"
  (prog1 (zm-read-word (zm-pc *zm*))
    (incf (zm-pc *zm*) 2)))

(defun decode-operand-types (type-byte count)
  "Decode operand types from a type byte"
  (loop for i from 0 below count
        for shift = (- 6 (* i 2))
        for type = (ldb (byte 2 shift) type-byte)
        until (= type +op-omit+)
        collect type))

(defun fetch-operand (type)
  "Fetch an operand of the given type"
  (ecase type
    (#.+op-large+ (fetch-word))
    (#.+op-small+ (fetch-byte))
    (#.+op-var+ (read-variable (fetch-byte)))))

(defun fetch-operands (types)
  "Fetch operands according to type list"
  (mapcar #'fetch-operand types))

(defun decode-instruction ()
  "Decode the next instruction, returning (opcode operand-count operands store branch)"
  (let* ((opcode-byte (fetch-byte))
         (form nil)
         (op-count nil)
         (opcode nil)
         (operand-types nil)
         (operands nil))
    
    (cond
      ;; Extended form (0xBE prefix)
      ((= opcode-byte #xBE)
       (setf form :extended)
       (setf opcode (fetch-byte))
       (setf op-count :var)
       (let ((types-byte (fetch-byte)))
         (setf operand-types (decode-operand-types types-byte 4))))
      
      ;; Variable form (top 2 bits = 11)
      ((= (ldb (byte 2 6) opcode-byte) #b11)
       (setf form :variable)
       (setf opcode (ldb (byte 5 0) opcode-byte))
       (if (logbitp 5 opcode-byte)
           (setf op-count :var)
           (setf op-count :2op))
       (let ((types-byte (fetch-byte)))
         (setf operand-types (decode-operand-types types-byte 4))
         ;; VAR:12 and VAR:26 (call_vs2, call_vn2) have 8 operands
         (when (and (eq op-count :var) (member opcode '(12 26)))
           (let ((types-byte2 (fetch-byte)))
             (setf operand-types 
                   (append operand-types 
                           (decode-operand-types types-byte2 4)))))))
      
      ;; Short form (top 2 bits = 10)
      ((= (ldb (byte 2 6) opcode-byte) #b10)
       (setf form :short)
       (setf opcode (ldb (byte 4 0) opcode-byte))
       (let ((op-type (ldb (byte 2 4) opcode-byte)))
         (if (= op-type +op-omit+)
             (setf op-count :0op)
             (progn
               (setf op-count :1op)
               (setf operand-types (list op-type))))))
      
      ;; Long form (top bit = 0)
      (t
       (setf form :long)
       (setf op-count :2op)
       (setf opcode (ldb (byte 5 0) opcode-byte))
       ;; In long form, bit 6 = first operand type, bit 5 = second
       ;; 0 = small constant, 1 = variable
       (setf operand-types
             (list (if (logbitp 6 opcode-byte) +op-var+ +op-small+)
                   (if (logbitp 5 opcode-byte) +op-var+ +op-small+)))))
    
    ;; Fetch operands
    (setf operands (fetch-operands operand-types))
    
    ;; Return decoded instruction
    (values form op-count opcode operands)))

;;; ============================================================
;;; Store and Branch Handling
;;; ============================================================

(defun fetch-store ()
  "Fetch store variable number"
  (fetch-byte))

(defun fetch-branch ()
  "Fetch branch offset, returning (condition offset)"
  (let* ((byte1 (fetch-byte))
         (condition (logbitp 7 byte1))
         (offset (if (logbitp 6 byte1)
                     ;; Short form (6-bit offset)
                     (ldb (byte 6 0) byte1)
                     ;; Long form (14-bit signed offset)
                     (let* ((byte2 (fetch-byte))
                            (offset14 (logior (ash (ldb (byte 6 0) byte1) 8) byte2)))
                       (if (logbitp 13 offset14)
                           (- offset14 #x4000)
                           offset14)))))
    (values condition offset)))

(defun do-branch (condition)
  "Execute a branch based on condition result"
  (multiple-value-bind (branch-on offset) (fetch-branch)
    (when (eq (not (null condition)) branch-on)
      (cond
        ((= offset 0) (do-return 0))
        ((= offset 1) (do-return 1))
        (t (incf (zm-pc *zm*) (- offset 2)))))))

(defun store-result (var-num value)
  "Store result in variable"
  (write-variable var-num (to-unsigned value)))
