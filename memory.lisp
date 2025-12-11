;;;; Z-machine Interpreter in Common Lisp
;;;; memory.lisp - Memory system and machine state

(in-package :zmachine)

;;; ============================================================
;;; Z-machine State Structure
;;; ============================================================

(defstruct zm
  "Z-machine state"
  ;; Memory
  (memory nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (original-dynamic nil :type (or null (simple-array (unsigned-byte 8) (*))))
  
  ;; Memory boundaries
  (dynamic-end 0 :type fixnum)      ; End of dynamic memory
  (static-end 0 :type fixnum)       ; End of static memory
  (high-start 0 :type fixnum)       ; Start of high memory
  
  ;; Registers
  (pc 0 :type fixnum)               ; Program Counter
  (version 0 :type (integer 0 8))   ; Z-machine version (1-8)
  
  ;; Stack
  (stack (make-array 1024 :element-type 'fixnum :fill-pointer 0) 
         :type (vector fixnum))
  
  ;; Call stack (for routine calls)
  (call-stack nil :type list)       ; List of call frames
  
  ;; Global variables base address
  (globals-addr 0 :type fixnum)
  
  ;; Object table address
  (objects-addr 0 :type fixnum)
  
  ;; Dictionary address
  (dictionary-addr 0 :type fixnum)
  
  ;; Abbreviations table address
  (abbrev-addr 0 :type fixnum)
  
  ;; Running state
  (running nil :type boolean)
  
  ;; Output buffer
  (output-buffer (make-string-output-stream) :type stream))

;;; Call frame structure
(defstruct call-frame
  "Call stack frame for routine calls"
  (return-pc 0 :type fixnum)        ; Return address
  (return-store nil)                 ; Variable to store result (or nil)
  (local-count 0 :type fixnum)      ; Number of local variables
  (locals (make-array 15 :element-type 'fixnum :initial-element 0)
          :type (simple-array fixnum (15)))
  (stack-pointer 0 :type fixnum)    ; Stack pointer at call time
  (discard-result nil :type boolean)) ; Whether to discard result

;;; Global Z-machine instance
(defvar *zm* nil "Current Z-machine instance")

;;; ============================================================
;;; Memory Access Functions
;;; ============================================================

(declaim (inline zm-read-byte zm-read-word zm-write-byte zm-write-word))

(defun zm-read-byte (addr)
  "Read a byte from memory"
  (declare (type fixnum addr))
  (aref (zm-memory *zm*) addr))

(defun zm-read-word (addr)
  "Read a 16-bit word from memory (big-endian)"
  (declare (type fixnum addr))
  (let ((mem (zm-memory *zm*)))
    (logior (ash (aref mem addr) 8)
            (aref mem (1+ addr)))))

(defun zm-write-byte (addr value)
  "Write a byte to dynamic memory"
  (declare (type fixnum addr value))
  (when (>= addr (zm-dynamic-end *zm*))
    (error "Attempt to write to static/high memory at address ~X" addr))
  (setf (aref (zm-memory *zm*) addr) (logand value #xFF)))

(defun zm-write-word (addr value)
  "Write a 16-bit word to dynamic memory (big-endian)"
  (declare (type fixnum addr value))
  (when (>= addr (zm-dynamic-end *zm*))
    (error "Attempt to write to static/high memory at address ~X" addr))
  (let ((mem (zm-memory *zm*)))
    (setf (aref mem addr) (logand (ash value -8) #xFF))
    (setf (aref mem (1+ addr)) (logand value #xFF))))

;;; ============================================================
;;; Packed Address Handling
;;; ============================================================

(defun unpack-routine-addr (packed-addr)
  "Convert packed routine address to byte address"
  (let ((version (zm-version *zm*)))
    (cond
      ((<= version 3) (* packed-addr 2))
      ((<= version 5) (* packed-addr 4))
      ((<= version 7) (* packed-addr 4))  ; + routine offset
      (t (* packed-addr 8)))))

(defun unpack-string-addr (packed-addr)
  "Convert packed string address to byte address"
  (let ((version (zm-version *zm*)))
    (cond
      ((<= version 3) (* packed-addr 2))
      ((<= version 5) (* packed-addr 4))
      ((<= version 7) (* packed-addr 4))  ; + string offset
      (t (* packed-addr 8)))))

;;; ============================================================
;;; Variable Access
;;; ============================================================

(defun read-variable (var-num)
  "Read a variable (0=stack, 1-15=locals, 16-255=globals)"
  (declare (type (integer 0 255) var-num))
  (cond
    ;; Stack (pop)
    ((zerop var-num)
     (if (> (fill-pointer (zm-stack *zm*)) 
            (if (zm-call-stack *zm*)
                (call-frame-stack-pointer (car (zm-call-stack *zm*)))
                0))
         (vector-pop (zm-stack *zm*))
         (error "Stack underflow")))
    ;; Local variable
    ((<= var-num 15)
     (if (zm-call-stack *zm*)
         (aref (call-frame-locals (car (zm-call-stack *zm*))) (1- var-num))
         (error "No local variables outside routine")))
    ;; Global variable
    (t
     (zm-read-word (+ (zm-globals-addr *zm*) (* 2 (- var-num 16)))))))

(defun write-variable (var-num value)
  "Write a variable (0=stack, 1-15=locals, 16-255=globals)"
  (declare (type (integer 0 255) var-num)
           (type fixnum value))
  (let ((value (logand value #xFFFF))) ; Ensure 16-bit
    (cond
      ;; Stack (push)
      ((zerop var-num)
       (vector-push-extend value (zm-stack *zm*)))
      ;; Local variable
      ((<= var-num 15)
       (if (zm-call-stack *zm*)
           (setf (aref (call-frame-locals (car (zm-call-stack *zm*))) (1- var-num))
                 value)
           (error "No local variables outside routine")))
      ;; Global variable
      (t
       (zm-write-word (+ (zm-globals-addr *zm*) (* 2 (- var-num 16))) value)))))

(defun peek-variable (var-num)
  "Read a variable without popping stack"
  (declare (type (integer 0 255) var-num))
  (if (zerop var-num)
      (let ((sp (fill-pointer (zm-stack *zm*))))
        (if (> sp 0)
            (aref (zm-stack *zm*) (1- sp))
            (error "Stack underflow")))
      (read-variable var-num)))

;;; ============================================================
;;; Header Access Functions
;;; ============================================================

(defun header-version () (zm-read-byte 0))
(defun header-flags1 () (zm-read-byte 1))
(defun header-high-mem () (zm-read-word #x04))
(defun header-initial-pc () (zm-read-word #x06))
(defun header-dictionary () (zm-read-word #x08))
(defun header-objects () (zm-read-word #x0A))
(defun header-globals () (zm-read-word #x0C))
(defun header-static-mem () (zm-read-word #x0E))
(defun header-flags2 () (zm-read-word #x10))
(defun header-abbrev () (zm-read-word #x18))
(defun header-file-length () (zm-read-word #x1A))
(defun header-checksum () (zm-read-word #x1C))

;;; ============================================================
;;; Story File Loading
;;; ============================================================

(defun load-story (filename)
  "Load a Z-machine story file"
  (with-open-file (stream filename 
                          :direction :input 
                          :element-type '(unsigned-byte 8))
    (let* ((file-length (file-length stream))
           (memory (make-array file-length 
                              :element-type '(unsigned-byte 8))))
      ;; Read entire file into memory
      (read-sequence memory stream)
      
      ;; Create Z-machine instance
      (let ((zm (make-zm :memory memory)))
        ;; Set version
        (setf (zm-version zm) (aref memory 0))
        
        ;; Validate version
        (unless (<= 1 (zm-version zm) 8)
          (error "Unsupported Z-machine version: ~D" (zm-version zm)))
        
        ;; Set memory boundaries
        (setf (zm-dynamic-end zm) 
              (logior (ash (aref memory #x0E) 8) (aref memory #x0F)))
        (setf (zm-high-start zm)
              (logior (ash (aref memory #x04) 8) (aref memory #x05)))
        (setf (zm-static-end zm) (min file-length #xFFFF))
        
        ;; Set initial PC
        (setf (zm-pc zm)
              (logior (ash (aref memory #x06) 8) (aref memory #x07)))
        
        ;; Set table addresses
        (setf (zm-globals-addr zm)
              (logior (ash (aref memory #x0C) 8) (aref memory #x0D)))
        (setf (zm-objects-addr zm)
              (logior (ash (aref memory #x0A) 8) (aref memory #x0B)))
        (setf (zm-dictionary-addr zm)
              (logior (ash (aref memory #x08) 8) (aref memory #x09)))
        (setf (zm-abbrev-addr zm)
              (logior (ash (aref memory #x18) 8) (aref memory #x19)))
        
        ;; Save original dynamic memory for restart
        (setf (zm-original-dynamic zm)
              (copy-seq (subseq memory 0 (zm-dynamic-end zm))))
        
        ;; Set as current instance
        (setf *zm* zm)
        
        ;; Print loading info
        (format t "~&Loaded Z-machine version ~D story file~%" (zm-version zm))
        (format t "  Dynamic memory: 0 - ~X~%" (zm-dynamic-end zm))
        (format t "  High memory: ~X - ~X~%" (zm-high-start zm) file-length)
        (format t "  Initial PC: ~X~%" (zm-pc zm))
        
        zm))))

(defun reset-story ()
  "Reset the story to initial state"
  (when *zm*
    ;; Restore dynamic memory
    (replace (zm-memory *zm*) (zm-original-dynamic *zm*))
    
    ;; Reset PC
    (setf (zm-pc *zm*) (header-initial-pc))
    
    ;; Clear stack and call stack
    (setf (fill-pointer (zm-stack *zm*)) 0)
    (setf (zm-call-stack *zm*) nil)
    
    ;; Clear output buffer
    (get-output-stream-string (zm-output-buffer *zm*))
    
    (setf (zm-running *zm*) nil)))

;;; ============================================================
;;; Save/Restore Functions
;;; ============================================================

(defvar *save-directory* nil "Directory for save files")

(defun get-save-filename ()
  "Prompt user for save filename"
  (format t "~&Save filename: ")
  (force-output)
  (let ((name (string-trim '(#\Space #\Newline #\Return) (read-line))))
    (if (string= name "")
        nil
        (if (find #\. name)
            name
            (concatenate 'string name ".sav")))))

(defun save-game (filename)
  "Save current game state to file"
  (handler-case
      (with-open-file (out filename :direction :output 
                                    :element-type '(unsigned-byte 8)
                                    :if-exists :supersede)
        ;; Write header marker "ZSAV"
        (write-byte (char-code #\Z) out)
        (write-byte (char-code #\S) out)
        (write-byte (char-code #\A) out)
        (write-byte (char-code #\V) out)
        
        ;; Write version byte
        (write-byte (zm-version *zm*) out)
        
        ;; Write dynamic memory size (2 bytes, big-endian)
        (let ((dyn-size (zm-dynamic-end *zm*)))
          (write-byte (ash dyn-size -8) out)
          (write-byte (logand dyn-size #xFF) out))
        
        ;; Write PC (3 bytes, big-endian)
        (let ((pc (zm-pc *zm*)))
          (write-byte (ash pc -16) out)
          (write-byte (logand (ash pc -8) #xFF) out)
          (write-byte (logand pc #xFF) out))
        
        ;; Write stack pointer
        (let ((sp (fill-pointer (zm-stack *zm*))))
          (write-byte (ash sp -8) out)
          (write-byte (logand sp #xFF) out))
        
        ;; Write call stack depth
        (let ((depth (length (zm-call-stack *zm*))))
          (write-byte depth out))
        
        ;; Write call frames
        (dolist (frame (reverse (zm-call-stack *zm*)))
          ;; Return PC (3 bytes)
          (let ((ret-pc (call-frame-return-pc frame)))
            (write-byte (ash ret-pc -16) out)
            (write-byte (logand (ash ret-pc -8) #xFF) out)
            (write-byte (logand ret-pc #xFF) out))
          ;; Return store (1 byte, or #xFF if nil)
          (let ((ret-store (call-frame-return-store frame)))
            (write-byte (if ret-store ret-store #xFF) out))
          ;; Local count (actual number of locals used)
          (let ((local-count (call-frame-local-count frame))
                (locals (call-frame-locals frame)))
            (write-byte local-count out)
            ;; Local values (2 bytes each, only used ones)
            (dotimes (i local-count)
              (let ((val (aref locals i)))
                (write-byte (ash val -8) out)
                (write-byte (logand val #xFF) out))))
          ;; Stack pointer at frame start
          (let ((sp (call-frame-stack-pointer frame)))
            (write-byte (ash sp -8) out)
            (write-byte (logand sp #xFF) out))
          ;; Discard result flag
          (write-byte (if (call-frame-discard-result frame) 1 0) out))
        
        ;; Write evaluation stack
        (let ((stack (zm-stack *zm*)))
          (dotimes (i (fill-pointer stack))
            (let ((val (aref stack i)))
              (write-byte (ash val -8) out)
              (write-byte (logand val #xFF) out))))
        
        ;; Write dynamic memory (compressed with XOR against original)
        (let ((mem (zm-memory *zm*))
              (orig (zm-original-dynamic *zm*))
              (dyn-end (zm-dynamic-end *zm*)))
          (dotimes (i dyn-end)
            (write-byte (logxor (aref mem i) (aref orig i)) out)))
        
        t)
    (error (e)
      (format t "~&Save failed: ~A~%" e)
      nil)))

(defun restore-game (filename)
  "Restore game state from file"
  (handler-case
      (with-open-file (in filename :direction :input
                                   :element-type '(unsigned-byte 8))
        ;; Check header marker
        (unless (and (= (read-byte in) (char-code #\Z))
                     (= (read-byte in) (char-code #\S))
                     (= (read-byte in) (char-code #\A))
                     (= (read-byte in) (char-code #\V)))
          (error "Not a valid save file"))
        
        ;; Check version
        (let ((version (read-byte in)))
          (unless (= version (zm-version *zm*))
            (error "Save file version mismatch: ~D vs ~D" 
                   version (zm-version *zm*))))
        
        ;; Read dynamic memory size
        (let ((dyn-size (logior (ash (read-byte in) 8) (read-byte in))))
          (unless (= dyn-size (zm-dynamic-end *zm*))
            (error "Dynamic memory size mismatch")))
        
        ;; Read PC
        (setf (zm-pc *zm*)
              (logior (ash (read-byte in) 16)
                      (ash (read-byte in) 8)
                      (read-byte in)))
        
        ;; Read stack pointer
        (let ((sp (logior (ash (read-byte in) 8) (read-byte in))))
          (setf (fill-pointer (zm-stack *zm*)) sp))
        
        ;; Read call stack depth
        (let ((depth (read-byte in)))
          (setf (zm-call-stack *zm*) nil)
          (dotimes (i depth)
            ;; Read return PC
            (let ((ret-pc (logior (ash (read-byte in) 16)
                                  (ash (read-byte in) 8)
                                  (read-byte in)))
                  ;; Read return store
                  (ret-store-byte (read-byte in))
                  ;; Read locals count
                  (num-locals (read-byte in)))
              ;; Create locals array with correct type (15 elements, fixnum)
              (let ((locals (make-array 15 :element-type 'fixnum :initial-element 0))
                    (ret-store (if (= ret-store-byte #xFF) nil ret-store-byte)))
                ;; Read local values
                (dotimes (j num-locals)
                  (setf (aref locals j)
                        (logior (ash (read-byte in) 8) (read-byte in))))
                ;; Read stack pointer
                (let* ((frame-sp (logior (ash (read-byte in) 8) (read-byte in)))
                       ;; Read discard-result flag
                       (discard (= (read-byte in) 1)))
                  (push (make-call-frame :return-pc ret-pc
                                        :return-store ret-store
                                        :local-count num-locals
                                        :locals locals
                                        :stack-pointer frame-sp
                                        :discard-result discard)
                        (zm-call-stack *zm*)))))))
        
        ;; Read evaluation stack
        (let ((stack (zm-stack *zm*)))
          (dotimes (i (fill-pointer stack))
            (setf (aref stack i)
                  (logior (ash (read-byte in) 8) (read-byte in)))))
        
        ;; Read and restore dynamic memory
        (let ((mem (zm-memory *zm*))
              (orig (zm-original-dynamic *zm*))
              (dyn-end (zm-dynamic-end *zm*)))
          (dotimes (i dyn-end)
            (setf (aref mem i) (logxor (read-byte in) (aref orig i)))))
        
        t)
    (error (e)
      (format t "~&Restore failed: ~A~%" e)
      nil)))
