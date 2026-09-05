;;;; glossary.lisp - Glossary (terminology) support for Z-machine translation
;;;;
;;;; Purpose:
;;;; - Keep proper nouns and game terms consistent across the whole playthrough
;;;; - Glossary entries are injected into the LLM prompt (Ollama / Claude)
;;;; - Translations can be audited and re-translated when they break the glossary
;;;;
;;;; Glossary files live in glossaries/glossary-<code>.lisp and contain
;;;; (add-glossary "white house" "白い家") forms.

(in-package :zmachine)

;;; ============================================================
;;; Forward Declarations (also defined in translate.lisp / languages.lisp)
;;; ============================================================

(defvar *current-language* :en "Current target language")
(defvar *translation-table* (make-hash-table :test 'equal) "Translation cache")
(defvar *use-api-translation* nil "Enable API translation")

;;; ============================================================
;;; Configuration
;;; ============================================================

(defvar *glossary* (make-hash-table :test 'equal)
  "Glossary: English term -> fixed translation for the current language")

(defvar *glossary-enabled* t
  "Inject glossary terms into API translation prompts")

(defvar *glossary-enforce* t
  "Retry once when the API translation drops a glossary term")

(defvar *glossary-max-terms* 40
  "Maximum number of glossary entries injected into a single prompt")

(defvar *glossary-modified* nil
  "Modification flag for the glossary")

;;; ============================================================
;;; Entry Management
;;; ============================================================

(defun add-glossary (en term)
  "Register a fixed translation for a term (used inside glossary files)"
  (setf (gethash (string-trim " " en) *glossary*) term)
  (setf *glossary-modified* t)
  term)

(defun remove-glossary (en)
  "Remove a glossary entry"
  (if (remhash en *glossary*)
      (progn (setf *glossary-modified* t)
             (format t "Removed: ~A~%" en) t)
      (progn (format t "Not found: ~A~%" en) nil)))

(defun glossary-lookup (en)
  "Look up a glossary entry (case-insensitive)"
  (or (gethash en *glossary*)
      (let ((found nil))
        (maphash (lambda (k v)
                   (when (and (null found) (string-equal k en))
                     (setf found v)))
                 *glossary*)
        found)))

(defun show-glossary (&optional n)
  "Show glossary entries"
  (let ((entries nil))
    (maphash (lambda (k v) (push (cons k v) entries)) *glossary*)
    (setf entries (sort entries #'string-lessp :key #'car))
    (format t "~%=== Glossary (~A): ~D entries ===~%"
            *current-language* (length entries))
    (loop for (en . term) in entries
          for i from 1
          while (or (null n) (<= i n))
          do (format t "~3D: ~A~30T=> ~A~%" i en term))
    (when (and n (> (length entries) n))
      (format t "... ~D more~%" (- (length entries) n)))
    (length entries)))

;;; ============================================================
;;; Term Matching
;;; ============================================================

(defun ascii-string-p (str)
  "True if STR contains only ASCII characters"
  (every (lambda (c) (< (char-code c) 128)) str))

(defun term-present-p (term text)
  "True if TERM occurs in TEXT. ASCII terms honor word boundaries;
non-ASCII terms (CJK etc.) use plain substring search."
  (when (and term text (> (length term) 0))
    (let ((tl (string-downcase term))
          (xl (string-downcase text)))
      (if (not (ascii-string-p term))
          (and (search tl xl) t)
          (loop with start = 0
                for pos = (search tl xl :start2 start)
                while pos
                do (let ((before (when (> pos 0) (char xl (1- pos))))
                         (after (let ((e (+ pos (length tl))))
                                  (when (< e (length xl)) (char xl e)))))
                     (when (and (or (null before)
                                    (not (or (alphanumericp before) (char= before #\'))))
                                (or (null after)
                                    (not (or (alphanumericp after) (char= after #\')))))
                       (return t))
                     (setf start (1+ pos)))
                finally (return nil))))))

(defun glossary-matches (text)
  "Glossary entries whose English term appears in TEXT, longest term first"
  (let ((matches nil))
    (maphash (lambda (en term)
               (when (term-present-p en text)
                 (push (cons en term) matches)))
             *glossary*)
    (sort matches #'> :key (lambda (x) (length (car x))))))

(defun glossary-missing-terms (source translation)
  "Glossary entries present in SOURCE but whose fixed translation is
missing from TRANSLATION"
  (remove-if (lambda (pair) (term-present-p (cdr pair) translation))
             (glossary-matches source)))

;;; ============================================================
;;; Prompt Construction
;;; ============================================================

(defun glossary-prompt-section (text &optional emphasize)
  "Glossary lines for the terms occurring in TEXT, or NIL.
EMPHASIZE is an optional list of (en . term) pairs listed first."
  (when (and *glossary-enabled* (> (hash-table-count *glossary*) 0))
    (let* ((matches (glossary-matches text))
           (ordered (append emphasize
                            (remove-if (lambda (p)
                                         (member (car p) emphasize
                                                 :key #'car :test #'string=))
                                       matches))))
      (when ordered
        (with-output-to-string (s)
          (loop for (en . term) in ordered
                for i from 1 to *glossary-max-terms*
                do (format s "- ~A => ~A~%" en term)))))))

;;; ============================================================
;;; File Management
;;; ============================================================

(defun load-glossary (&optional (code *current-language*))
  "Load the glossary for a language.
One file: the name comes from the configuration, or from the language code.
It is also the file the glossary is saved back to."
  (clrhash *glossary*)
  (setf *glossary-modified* nil)
  (when (eq code :en)
    (return-from load-glossary 0))
  (let ((file (user-glossary-file code)))
    (if (probe-file file)
        (load file :external-format :utf-8)
        (format t "Glossary not found, starting empty: ~A~%" file)))
  (setf *glossary-modified* nil)
  (let ((n (hash-table-count *glossary*)))
    (when (> n 0)
      (format t "Glossary loaded: ~D terms~%" n))
    n))

(defun save-glossary (&optional (code *current-language*))
  "Save the glossary.
The file name comes from the configuration, or from the language code."
  (when (eq code :en)
    (return-from save-glossary nil))
  (let ((filename (user-glossary-file code))
        (entries nil))
    (maphash (lambda (k v) (push (cons k v) entries)) *glossary*)
    (setf entries (sort entries #'string-lessp :key #'car))
    (ensure-directories-exist filename)
    (with-open-file (out filename :direction :output
                                  :if-exists :supersede
                                  :external-format :utf-8)
      (format out ";;;; Glossary for Z-machine translation~%")
      (format out ";;;; Language: ~A~%~%" code)
      (format out "(in-package :zmachine)~%~%")
      (loop for (en . term) in entries
            do (format out "(add-glossary ~S ~S)~%" en term)))
    (setf *glossary-modified* nil)
    (format t "Saved: ~A (~D terms)~%" filename (length entries))
    filename))

;;; ============================================================
;;; Auditing
;;; ============================================================

(defun glossary-violations ()
  "Cached translations that drop a glossary term.
Returns a list of (english translation missing-pairs)"
  (let ((result nil))
    (maphash (lambda (en trans)
               (let ((missing (glossary-missing-terms en trans)))
                 (when missing
                   (push (list en trans missing) result))))
             *translation-table*)
    (nreverse result)))

(defun glossary-check (&optional (n 20))
  "Report cached translations that violate the glossary"
  (let ((violations (glossary-violations)))
    (format t "~%=== Glossary Check (~A) ===~%" *current-language*)
    (format t "Glossary terms : ~D~%" (hash-table-count *glossary*))
    (format t "Translations   : ~D~%" (hash-table-count *translation-table*))
    (format t "Violations     : ~D~%~%" (length violations))
    (loop for (en trans missing) in violations
          for i from 1 to n
          do (format t "~3D: ~A~%     -> ~A~%     missing: ~{~A~^, ~}~%"
                     i
                     (subseq en 0 (min 60 (length en)))
                     (subseq trans 0 (min 60 (length trans)))
                     (mapcar (lambda (p) (format nil "~A => ~A" (car p) (cdr p)))
                             missing)))
    (when (> (length violations) n)
      (format t "... ~D more~%" (- (length violations) n)))
    violations))

(defun glossary-fix (&optional (delay 0))
  "Re-translate every cached translation that violates the glossary"
  (unless *use-api-translation*
    (format t "API not configured.~%")
    (return-from glossary-fix nil))
  (let ((violations (glossary-violations))
        (fixed 0) (failed 0))
    (format t "Re-translating ~D entries...~%" (length violations))
    (loop for (en nil missing) in violations
          do (format t "  ~A..." (subseq en 0 (min 30 (length en))))
             (let ((new (api-translate-once en missing)))
               (cond ((and new (null (glossary-missing-terms en new)))
                      (add-trans en new)
                      (format t " OK -> ~A~%" (subseq new 0 (min 40 (length new))))
                      (incf fixed))
                     (new
                      (add-trans en new)
                      (format t " partial~%")
                      (incf fixed))
                     (t (format t " FAILED~%") (incf failed))))
             (when (> delay 0) (sleep delay)))
    (format t "Done: ~D fixed / ~D failed~%" fixed failed)
    (when (> fixed 0) (save-language-translations))
    fixed))
