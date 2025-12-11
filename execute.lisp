;;;; Z-machine Interpreter in Common Lisp
;;;; execute.lisp - Execution engine and routine calls

(in-package :zmachine)

;;; ============================================================
;;; Routine Calls
;;; ============================================================

(defun do-call (packed-addr args store-var)
  "Call a routine"
  ;; Handle call to address 0 (return false/0)
  (when (zerop packed-addr)
    (when store-var
      (store-result store-var 0))
    (return-from do-call))
  
  (let* ((routine-addr (unpack-routine-addr packed-addr))
         (local-count (zm-read-byte routine-addr))
         (frame (make-call-frame
                 :return-pc (zm-pc *zm*)
                 :return-store store-var
                 :local-count local-count
                 :stack-pointer (fill-pointer (zm-stack *zm*))
                 :discard-result (null store-var))))
    
    ;; Initialize local variables
    (if (<= (zm-version *zm*) 4)
        ;; V1-4: Local values follow header
        (loop for i from 0 below local-count
              for addr = (+ routine-addr 1 (* i 2))
              do (setf (aref (call-frame-locals frame) i)
                       (zm-read-word addr)))
        ;; V5+: Locals are all initialized to 0
        (loop for i from 0 below local-count
              do (setf (aref (call-frame-locals frame) i) 0)))
    
    ;; Copy arguments to locals
    (loop for i from 0 below (min (length args) local-count)
          do (setf (aref (call-frame-locals frame) i) (nth i args)))
    
    ;; Push frame
    (push frame (zm-call-stack *zm*))
    
    ;; Set PC to first instruction
    (setf (zm-pc *zm*)
          (+ routine-addr 1
             (if (<= (zm-version *zm*) 4)
                 (* local-count 2)
                 0)))))

(defun do-return (value)
  "Return from current routine"
  (let ((frame (pop (zm-call-stack *zm*))))
    (unless frame
      (error "Return from non-existent routine"))
    
    ;; Restore stack pointer
    (setf (fill-pointer (zm-stack *zm*)) (call-frame-stack-pointer frame))
    
    ;; Restore PC
    (setf (zm-pc *zm*) (call-frame-return-pc frame))
    
    ;; Store result if needed
    (when (and (call-frame-return-store frame)
               (not (call-frame-discard-result frame)))
      (store-result (call-frame-return-store frame) value))))

;;; ============================================================
;;; Main Execution Loop
;;; ============================================================

(defun execute-instruction ()
  "Execute a single instruction"
  (multiple-value-bind (form op-count opcode operands)
      (decode-instruction)
    (let* ((table (ecase op-count
                    (:0op *opcodes-0op*)
                    (:1op *opcodes-1op*)
                    (:2op *opcodes-2op*)
                    (:var *opcodes-var*)
                    (:ext *opcodes-ext*)))
           (handler (gethash opcode table)))
      (unless handler
        (error "Unknown opcode: ~A:~D at PC ~X" op-count opcode (zm-pc *zm*)))
      
      ;; Execute the opcode handler
      (funcall (second handler) operands))))

(defun run (&optional (max-instructions nil))
  "Run the Z-machine"
  (unless *zm*
    (error "No story loaded"))
  
  (init-alphabet)
  (setf (zm-running *zm*) t)
  
  (handler-case
      (loop while (zm-running *zm*)
            for count from 0
            when (and max-instructions (>= count max-instructions))
              do (return)
            do (execute-instruction))
    (error (c)
      (format *error-output* "~&Error: ~A~%" c)
      (setf (zm-running *zm*) nil))))

(defun step-instruction ()
  "Execute a single instruction (for debugging)"
  (unless *zm*
    (error "No story loaded"))
  (init-alphabet)
  (let ((pc-before (zm-pc *zm*)))
    (execute-instruction)
    (format t "~&PC: ~X -> ~X~%" pc-before (zm-pc *zm*))))

;;; ============================================================
;;; Debug Utilities
;;; ============================================================

(defun disassemble-at (addr &optional (count 1))
  "Disassemble instructions at address"
  (let ((saved-pc (zm-pc *zm*)))
    (setf (zm-pc *zm*) addr)
    (unwind-protect
        (dotimes (i count)
          (let ((pc-before (zm-pc *zm*)))
            (multiple-value-bind (form op-count opcode operands)
                (decode-instruction)
              (let* ((table (ecase op-count
                              (:0op *opcodes-0op*)
                              (:1op *opcodes-1op*)
                              (:2op *opcodes-2op*)
                              (:var *opcodes-var*)
                              (:ext *opcodes-ext*)))
                     (handler (gethash opcode table))
                     (name (if handler (first handler) "???")))
                (format t "~&~6X: ~A:~2D ~A ~{~X ~}~%"
                        pc-before op-count opcode name operands)))))
      (setf (zm-pc *zm*) saved-pc))))

(defun show-stack ()
  "Show current stack contents"
  (format t "~&Stack (~D items):~%" (fill-pointer (zm-stack *zm*)))
  (loop for i from (1- (fill-pointer (zm-stack *zm*))) downto 0
        do (format t "  ~D: ~X~%" i (aref (zm-stack *zm*) i))))

(defun show-locals ()
  "Show local variables of current routine"
  (if (zm-call-stack *zm*)
      (let ((frame (car (zm-call-stack *zm*))))
        (format t "~&Local variables (~D):~%" (call-frame-local-count frame))
        (loop for i from 0 below (call-frame-local-count frame)
              do (format t "  L~2,'0D: ~X (~D)~%" 
                        (1+ i) 
                        (aref (call-frame-locals frame) i)
                        (to-signed (aref (call-frame-locals frame) i)))))
      (format t "~&No active routine~%")))

(defun show-globals (&optional (start 0) (count 16))
  "Show global variables"
  (format t "~&Global variables:~%")
  (loop for i from start below (min (+ start count) 240)
        for addr = (+ (zm-globals-addr *zm*) (* 2 i))
        do (format t "  G~2,'0X: ~4,'0X (~D)~%" 
                  (+ i 16) 
                  (zm-read-word addr)
                  (to-signed (zm-read-word addr)))))

(defun show-object (obj-num)
  "Show object details"
  (format t "~&Object ~D: ~S~%" obj-num (object-name obj-num))
  (format t "  Parent: ~D, Sibling: ~D, Child: ~D~%"
          (object-parent obj-num)
          (object-sibling obj-num)
          (object-child obj-num))
  (format t "  Attributes: ")
  (loop for i from 0 below (if (<= (zm-version *zm*) 3) 32 48)
        when (object-has-attr obj-num i)
          do (format t "~D " i))
  (format t "~%")
  ;; Show properties
  (format t "  Properties: ")
  (loop for prop = (object-get-next-prop obj-num 0) then (object-get-next-prop obj-num prop)
        until (zerop prop)
        do (format t "~D " prop))
  (format t "~%"))

(defun show-header ()
  "Show story file header"
  (format t "~&Z-machine Version: ~D~%" (header-version))
  (format t "Flags 1: ~8,'0B~%" (header-flags1))
  (format t "High memory: ~X~%" (header-high-mem))
  (format t "Initial PC: ~X~%" (header-initial-pc))
  (format t "Dictionary: ~X~%" (header-dictionary))
  (format t "Objects: ~X~%" (header-objects))
  (format t "Globals: ~X~%" (header-globals))
  (format t "Static memory: ~X~%" (header-static-mem))
  (format t "Abbreviations: ~X~%" (header-abbrev)))
