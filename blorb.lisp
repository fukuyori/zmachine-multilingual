;;;; blorb.lisp - Blorb resource files
;;;;
;;;; Version 6 stories do not carry their pictures: those live in a separate
;;;; resource file. Modern packagings use Blorb, an IFF file whose FORM type
;;;; is IFRS. Its resource index lists each picture by number, and each one is
;;;; a chunk of its own - usually PNG, sometimes a bare rectangle used only
;;;; for layout.
;;;;
;;;;   (load-resources "games/ZorkZero.blb")
;;;;   (show-resources)
;;;;
;;;; A resource file next to the story with the same base name, or one whose
;;;; name matches the story, is picked up automatically by load-story.

(in-package :zmachine)

;;; ============================================================
;;; Binary File Reading
;;; ============================================================

(defun read-binary-file (path)
  "Read PATH into a byte vector, NIL if missing"
  (when (probe-file path)
    (with-open-file (in path :direction :input
                             :element-type '(unsigned-byte 8))
      (let* ((len (file-length in))
             (buffer (make-array len :element-type '(unsigned-byte 8)))
             (count (read-sequence buffer in)))
        (if (= count len) buffer (subseq buffer 0 count))))))

(defun write-binary-file (path bytes)
  "Write BYTES to PATH"
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
    (write-sequence bytes out))
  path)

(defun be16 (bytes offset)
  "Big-endian 16-bit value at OFFSET"
  (logior (ash (aref bytes offset) 8) (aref bytes (1+ offset))))

(defun be32 (bytes offset)
  "Big-endian 32-bit value at OFFSET"
  (logior (ash (aref bytes offset) 24)
          (ash (aref bytes (+ offset 1)) 16)
          (ash (aref bytes (+ offset 2)) 8)
          (aref bytes (+ offset 3))))

(defun chunk-id (bytes offset)
  "Four-character chunk identifier at OFFSET"
  (map 'string #'code-char (subseq bytes offset (+ offset 4))))

;;; ============================================================
;;; State
;;; ============================================================

(defvar *blorb-data* nil
  "Contents of the loaded resource file")

(defvar *blorb-path* nil
  "Where the loaded resource file came from")

(defvar *blorb-pictures* (make-hash-table)
  "Picture number -> (:kind :png|:rect :offset n :length n :width n :height n)")

(defun resources-loaded-p ()
  (and *blorb-data* (plusp (hash-table-count *blorb-pictures*))))

;;; ============================================================
;;; Parsing
;;; ============================================================

(defun blorb-picture-dimensions (data offset kind)
  "Width and height of the picture whose chunk starts at OFFSET"
  (case kind
    ;; PNG: the IHDR chunk follows the 8 byte signature, and its first two
    ;; fields are the width and the height
    (:png (let ((png (+ offset 8)))
            (values (be32 data (+ png 16)) (be32 data (+ png 20)))))
    ;; Rect: the whole chunk is a width and a height
    (:rect (values (be32 data (+ offset 8)) (be32 data (+ offset 12))))
    (t (values 0 0))))

(defun parse-blorb (data)
  "Fill *blorb-pictures* from the resource index of DATA. Returns the count."
  (clrhash *blorb-pictures*)
  (unless (and (> (length data) 12)
               (string= (chunk-id data 0) "FORM")
               (string= (chunk-id data 8) "IFRS"))
    (return-from parse-blorb 0))
  ;; Find the resource index
  (let ((end (min (length data) (+ 8 (be32 data 4))))
        (pos 12)
        (ridx nil))
    (loop while (< pos (- end 8))
          do (let ((id (chunk-id data pos))
                   (len (be32 data (+ pos 4))))
               (when (string= id "RIdx")
                 (setf ridx pos)
                 (return))
               (incf pos (+ 8 len (logand len 1)))))
    (unless ridx
      (return-from parse-blorb 0))
    (let ((count (be32 data (+ ridx 8))))
      (dotimes (i count)
        (let* ((entry (+ ridx 12 (* i 12)))
               (usage (chunk-id data entry))
               (number (be32 data (+ entry 4)))
               (start (be32 data (+ entry 8))))
          (when (and (string= usage "Pict") (< (+ start 8) (length data)))
            (let* ((id (chunk-id data start))
                   (len (be32 data (+ start 4)))
                   (kind (cond ((string= id "PNG ") :png)
                               ((string= id "JPEG") :jpeg)
                               ((string= id "Rect") :rect)
                               (t :other))))
              (multiple-value-bind (w h) (blorb-picture-dimensions data start kind)
                (setf (gethash number *blorb-pictures*)
                      (list :kind kind :offset (+ start 8) :length len
                            :width w :height h)))))))
      (hash-table-count *blorb-pictures*))))

;;; ============================================================
;;; Matching a resource file to the story
;;; ============================================================

(defun blorb-story-identity (data)
  "Release, serial and checksum from the IFhd chunk, or NIL"
  (let ((end (min (length data) (+ 8 (be32 data 4))))
        (pos 12))
    (loop while (< pos (- end 8))
          do (let ((id (chunk-id data pos))
                   (len (be32 data (+ pos 4))))
               (when (string= id "IFhd")
                 (let ((d (+ pos 8)))
                   (return-from blorb-story-identity
                     (list (be16 data d)
                           (map 'string #'code-char (subseq data (+ d 2) (+ d 8)))
                           (be16 data (+ d 8))))))
               (incf pos (+ 8 len (logand len 1)))))
    nil))

(defun blorb-release-number ()
  "Release number of the picture file, from its RelN chunk"
  (let ((data *blorb-data*))
    (when data
      (let ((end (min (length data) (+ 8 (be32 data 4))))
            (pos 12))
        (loop while (< pos (- end 8))
              do (let ((id (chunk-id data pos))
                       (len (be32 data (+ pos 4))))
                   (when (string= id "RelN")
                     (return-from blorb-release-number (be16 data (+ pos 8))))
                   (incf pos (+ 8 len (logand len 1)))))
        0))))

(defun story-identity ()
  "Release, serial and checksum of the loaded story"
  (when *zm*
    (list (zm-read-word 2)
          (map 'string #'code-char
               (loop for i from #x12 below #x18 collect (zm-read-byte i)))
          (zm-read-word #x1C))))

(defun resources-match-story-p (data)
  "Whether DATA names the story that is loaded. T when it names none."
  (let ((theirs (blorb-story-identity data))
        (ours (story-identity)))
    (or (null theirs) (null ours) (equal theirs ours))))

;;; ============================================================
;;; Loading
;;; ============================================================

(defun load-resources (path &optional quiet)
  "Load a Blorb resource file. Returns the number of pictures found."
  (let ((data (read-binary-file path)))
    (cond
      ((null data)
       (unless quiet (format t "Resource file not found: ~A~%" path))
       0)
      (t
       (let ((count (parse-blorb data)))
         (cond
           ((zerop count)
            (unless quiet
              (format t "No pictures in ~A (not a Blorb resource file?)~%" path))
            0)
           (t
            (setf *blorb-data* data
                  *blorb-path* path)
            (unless (resources-match-story-p data)
              (format t "[warning] ~A belongs to a different story~%" path))
            (unless quiet
              (format t "Resources loaded: ~D pictures from ~A~%"
                      count (file-namestring path)))
            count)))))))

(defun clear-resources ()
  "Forget the loaded resource file"
  (setf *blorb-data* nil *blorb-path* nil)
  (clrhash *blorb-pictures*))

(defun find-resource-file (story-path)
  "A Blorb file next to STORY-PATH that belongs to it, or NIL"
  (let* ((story (pathname story-path))
         (dir (make-pathname :directory (pathname-directory story)
                             :device (pathname-device story)))
         (candidates (append
                      (list (merge-pathnames
                             (make-pathname :name (pathname-name story)
                                            :type "blb")
                             dir))
                      (directory (merge-pathnames "*.blb" dir)))))
    (dolist (path candidates)
      (when (probe-file path)
        (let ((data (read-binary-file path)))
          (when (and data
                     (> (length data) 12)
                     (string= (chunk-id data 0) "FORM")
                     (equal (blorb-story-identity data) (story-identity)))
            (return-from find-resource-file path)))))
    nil))

;;; ============================================================
;;; Access
;;; ============================================================

(defun picture-exists-p (number)
  (and (resources-loaded-p) (gethash number *blorb-pictures*) t))

(defun picture-size (number)
  "Width and height of picture NUMBER, or (values 0 0)"
  (let ((p (gethash number *blorb-pictures*)))
    (if p
        (values (getf p :width) (getf p :height))
        (values 0 0))))

(defun picture-kind (number)
  (getf (gethash number *blorb-pictures*) :kind))

(defun picture-count ()
  (hash-table-count *blorb-pictures*))

(defun picture-bytes (number)
  "Raw image data of picture NUMBER, or NIL"
  (let ((p (gethash number *blorb-pictures*)))
    (when (and p *blorb-data* (member (getf p :kind) '(:png :jpeg)))
      (subseq *blorb-data* (getf p :offset)
              (+ (getf p :offset) (getf p :length))))))

(defun show-resources ()
  "Summary of the loaded resource file"
  (if (not (resources-loaded-p))
      (format t "No resource file loaded.~%")
      (let ((kinds (make-hash-table))
            (drawable 0))
        (loop for n being the hash-keys of *blorb-pictures*
              do (incf (gethash (picture-kind n) kinds 0))
                 (when (member (picture-kind n) '(:png :jpeg))
                   (incf drawable)))
        (format t "~%=== Resources ===~%")
        (format t "File     : ~A~%" *blorb-path*)
        (format t "Pictures : ~D  (~{~(~A~)=~D~^, ~})~%"
                (picture-count)
                (loop for k being the hash-keys of kinds using (hash-value v)
                      append (list k v)))
        (format t "Drawable : ~D~%" drawable)
        (picture-count))))
