;;;; Z-machine Interpreter in Common Lisp
;;;; objects.lisp - Object system

(in-package :zmachine)

;;; ============================================================
;;; Object Table Structure
;;; 
;;; V1-3: 
;;;   - Property defaults: 31 words (62 bytes) at table start
;;;   - Each object entry: 9 bytes
;;;     - 4 bytes: 32 attributes
;;;     - 3 bytes: parent, sibling, child (1 byte each)
;;;     - 2 bytes: property table address
;;;
;;; V4+:
;;;   - Property defaults: 63 words (126 bytes) at table start
;;;   - Each object entry: 14 bytes
;;;     - 6 bytes: 48 attributes
;;;     - 6 bytes: parent, sibling, child (2 bytes each)
;;;     - 2 bytes: property table address
;;; ============================================================

(defun object-table-start ()
  "Return the start of object entries (after property defaults)"
  (+ (zm-objects-addr *zm*)
     (if (<= (zm-version *zm*) 3) 62 126)))

(defun object-entry-size ()
  "Return the size of an object entry"
  (if (<= (zm-version *zm*) 3) 9 14))

(defun object-addr (obj-num)
  "Return the address of object entry"
  (when (zerop obj-num)
    (return-from object-addr 0))
  (+ (object-table-start)
     (* (1- obj-num) (object-entry-size))))

;;; ============================================================
;;; Attribute Operations
;;; ============================================================

(defun object-has-attr (obj-num attr)
  "Test if object has attribute"
  (when (zerop obj-num) (return-from object-has-attr nil))
  (let* ((addr (object-addr obj-num))
         (byte-num (floor attr 8))
         (bit-num (- 7 (mod attr 8)))
         (byte-val (zm-read-byte (+ addr byte-num))))
    (logbitp bit-num byte-val)))

(defun object-set-attr (obj-num attr)
  "Set an attribute on object"
  (when (zerop obj-num) (return-from object-set-attr nil))
  (let* ((addr (object-addr obj-num))
         (byte-num (floor attr 8))
         (bit-num (- 7 (mod attr 8)))
         (byte-addr (+ addr byte-num))
         (byte-val (zm-read-byte byte-addr)))
    (zm-write-byte byte-addr (logior byte-val (ash 1 bit-num)))))

(defun object-clear-attr (obj-num attr)
  "Clear an attribute on object"
  (when (zerop obj-num) (return-from object-clear-attr nil))
  (let* ((addr (object-addr obj-num))
         (byte-num (floor attr 8))
         (bit-num (- 7 (mod attr 8)))
         (byte-addr (+ addr byte-num))
         (byte-val (zm-read-byte byte-addr)))
    (zm-write-byte byte-addr (logand byte-val (lognot (ash 1 bit-num))))))

;;; ============================================================
;;; Object Tree Operations
;;; ============================================================

(defun object-parent (obj-num)
  "Get parent of object"
  (when (zerop obj-num) (return-from object-parent 0))
  (let ((addr (object-addr obj-num)))
    (if (<= (zm-version *zm*) 3)
        (zm-read-byte (+ addr 4))
        (zm-read-word (+ addr 6)))))

(defun object-sibling (obj-num)
  "Get sibling of object"
  (when (zerop obj-num) (return-from object-sibling 0))
  (let ((addr (object-addr obj-num)))
    (if (<= (zm-version *zm*) 3)
        (zm-read-byte (+ addr 5))
        (zm-read-word (+ addr 8)))))

(defun object-child (obj-num)
  "Get first child of object"
  (when (zerop obj-num) (return-from object-child 0))
  (let ((addr (object-addr obj-num)))
    (if (<= (zm-version *zm*) 3)
        (zm-read-byte (+ addr 6))
        (zm-read-word (+ addr 10)))))

(defun set-object-parent (obj-num parent)
  "Set parent of object"
  (let ((addr (object-addr obj-num)))
    (if (<= (zm-version *zm*) 3)
        (zm-write-byte (+ addr 4) parent)
        (zm-write-word (+ addr 6) parent))))

(defun set-object-sibling (obj-num sibling)
  "Set sibling of object"
  (let ((addr (object-addr obj-num)))
    (if (<= (zm-version *zm*) 3)
        (zm-write-byte (+ addr 5) sibling)
        (zm-write-word (+ addr 8) sibling))))

(defun set-object-child (obj-num child)
  "Set first child of object"
  (let ((addr (object-addr obj-num)))
    (if (<= (zm-version *zm*) 3)
        (zm-write-byte (+ addr 6) child)
        (zm-write-word (+ addr 10) child))))

;;; ============================================================
;;; Object Manipulation
;;; ============================================================

(defun object-remove (obj-num)
  "Remove object from its parent"
  (when (zerop obj-num) (return-from object-remove nil))
  (let ((parent (object-parent obj-num)))
    (when (zerop parent) (return-from object-remove nil))
    
    ;; Find and unlink from sibling chain
    (let ((child (object-child parent)))
      (if (= child obj-num)
          ;; Object is first child
          (set-object-child parent (object-sibling obj-num))
          ;; Find in sibling chain
          (loop for prev = child then (object-sibling prev)
                for curr = (object-sibling prev)
                until (zerop curr)
                when (= curr obj-num)
                  do (progn
                       (set-object-sibling prev (object-sibling obj-num))
                       (return)))))
    
    ;; Clear parent and sibling
    (set-object-parent obj-num 0)
    (set-object-sibling obj-num 0)))

(defun object-insert (obj-num dest-num)
  "Insert object as first child of destination"
  (when (or (zerop obj-num) (zerop dest-num))
    (return-from object-insert nil))
  
  ;; First remove from current location
  (object-remove obj-num)
  
  ;; Insert as first child of destination
  (set-object-sibling obj-num (object-child dest-num))
  (set-object-child dest-num obj-num)
  (set-object-parent obj-num dest-num))

;;; ============================================================
;;; Property Operations
;;; ============================================================

(defun object-prop-table-addr (obj-num)
  "Get address of object's property table"
  (when (zerop obj-num) (return-from object-prop-table-addr 0))
  (let ((addr (object-addr obj-num)))
    (if (<= (zm-version *zm*) 3)
        (zm-read-word (+ addr 7))
        (zm-read-word (+ addr 12)))))

(defun object-name (obj-num)
  "Get object's short name"
  (when (zerop obj-num) (return-from object-name ""))
  (let* ((prop-addr (object-prop-table-addr obj-num))
         (name-len (zm-read-byte prop-addr)))
    (if (zerop name-len)
        ""
        (decode-zstring (1+ prop-addr)))))

(defun property-default (prop-num)
  "Get default value for a property"
  (zm-read-word (+ (zm-objects-addr *zm*) (* 2 (1- prop-num)))))

(defun find-property (obj-num prop-num)
  "Find property in object, returns (addr size) or nil"
  (when (zerop obj-num) (return-from find-property nil))
  (let* ((prop-addr (object-prop-table-addr obj-num))
         (name-len (zm-read-byte prop-addr))
         (addr (+ prop-addr 1 (* 2 name-len))))  ; Skip name
    (loop
      (let* ((size-byte (zm-read-byte addr))
             (prop-size nil)
             (this-prop nil))
        (when (zerop size-byte)
          (return nil))  ; End of properties
        
        (if (<= (zm-version *zm*) 3)
            ;; V1-3: size/prop in one byte
            (progn
              (setf prop-size (1+ (ldb (byte 3 5) size-byte)))
              (setf this-prop (ldb (byte 5 0) size-byte))
              (incf addr))
            ;; V4+: more complex encoding
            (progn
              (setf this-prop (ldb (byte 6 0) size-byte))
              (if (logbitp 7 size-byte)
                  ;; Two-byte header
                  (let ((size-byte2 (zm-read-byte (1+ addr))))
                    (setf prop-size (ldb (byte 6 0) size-byte2))
                    (when (zerop prop-size) (setf prop-size 64))
                    (incf addr 2))
                  ;; One-byte header
                  (progn
                    (setf prop-size (if (logbitp 6 size-byte) 2 1))
                    (incf addr)))))
        
        (when (= this-prop prop-num)
          (return (values addr prop-size)))
        
        (when (< this-prop prop-num)
          (return nil))  ; Properties are in descending order
        
        (incf addr prop-size)))))

(defun property-length-at (prop-data-addr)
  "Get length of property data at given address"
  (let ((size-byte (zm-read-byte (1- prop-data-addr))))
    (if (<= (zm-version *zm*) 3)
        (1+ (ldb (byte 3 5) size-byte))
        (if (logbitp 7 size-byte)
            ;; This was a two-byte header, size-byte is second byte
            (let ((size (ldb (byte 6 0) size-byte)))
              (if (zerop size) 64 size))
            ;; One-byte header
            (if (logbitp 6 size-byte) 2 1)))))

(defun object-get-prop (obj-num prop-num)
  "Get property value"
  (multiple-value-bind (addr size) (find-property obj-num prop-num)
    (if addr
        (cond
          ((= size 1) (zm-read-byte addr))
          ((= size 2) (zm-read-word addr))
          (t (zm-read-word addr)))  ; Return first word for larger props
        (property-default prop-num))))

(defun object-put-prop (obj-num prop-num value)
  "Set property value"
  (multiple-value-bind (addr size) (find-property obj-num prop-num)
    (unless addr
      (error "Property ~D not found on object ~D" prop-num obj-num))
    (cond
      ((= size 1) (zm-write-byte addr (logand value #xFF)))
      (t (zm-write-word addr value)))))

(defun object-get-prop-addr (obj-num prop-num)
  "Get address of property data"
  (multiple-value-bind (addr size) (find-property obj-num prop-num)
    (declare (ignore size))
    (or addr 0)))

(defun object-get-next-prop (obj-num prop-num)
  "Get next property number after given one (0 for first)"
  (when (zerop obj-num) (return-from object-get-next-prop 0))
  (let* ((prop-addr (object-prop-table-addr obj-num))
         (name-len (zm-read-byte prop-addr))
         (addr (+ prop-addr 1 (* 2 name-len))))
    (if (zerop prop-num)
        ;; Return first property
        (let ((size-byte (zm-read-byte addr)))
          (if (zerop size-byte)
              0
              (if (<= (zm-version *zm*) 3)
                  (ldb (byte 5 0) size-byte)
                  (ldb (byte 6 0) size-byte))))
        ;; Find next after given property
        (progn
          (multiple-value-bind (found-addr size) (find-property obj-num prop-num)
            (unless found-addr
              (error "Property ~D not found on object ~D" prop-num obj-num))
            (let* ((next-addr (+ found-addr size))
                   (size-byte (zm-read-byte next-addr)))
              (if (zerop size-byte)
                  0
                  (if (<= (zm-version *zm*) 3)
                      (ldb (byte 5 0) size-byte)
                      (ldb (byte 6 0) size-byte)))))))))
