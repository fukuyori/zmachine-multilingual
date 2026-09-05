;;;; settings.lisp - Data file settings
;;;;
;;;; The translation cache lives in translations/ and is named after the
;;;; language: translations/translations-<language>.lisp. A different name can
;;;; be set instead, so that separate games (or separate playthroughs) can keep
;;;; separate caches. A bare file name goes inside translations/ too.
;;;;
;;;; Settings go in the launch script, before (set-language ...):
;;;;
;;;;   (set-translation-file "zork2-ja.lisp")   ; -> translations/zork2-ja.lisp
;;;;   (set-glossary-file    "zork2-ja-glossary.lisp")
;;;;   (set-language :ja)
;;;;
;;;; The glossary works the same way, in glossaries/.
;;;;
;;;; There is exactly one file of each kind: the file named here is the only one
;;;; read, and the only one written.

(in-package :zmachine)

;;; ============================================================
;;; Forward Declarations (also defined in translate.lisp / languages.lisp)
;;; ============================================================

(defvar *current-language* :en "Current target language")

;;; ============================================================
;;; Configuration
;;; ============================================================

(defvar *translation-file* nil
  "User translation cache file name.
NIL means translations-<language>.lisp, as before.")

(defvar *glossary-file* nil
  "Glossary file name.
NIL means glossary-<language>.lisp, as before.")

;;; ============================================================
;;; File Name Resolution
;;; ============================================================

(defvar *translations-dir* "translations/"
  "Directory the translation cache lives in")

(defvar *glossaries-dir* "glossaries/"
  "Directory the glossary lives in")

(defun in-data-dir (dir name)
  "Put a bare file name inside DIR.
A name that already contains a directory is used as given."
  (if (or (find #\/ name) (find #\\ name))
      name
      (concatenate 'string dir name)))

(defun default-translation-file (code)
  "Language-derived name of the translation cache"
  (format nil "translations-~A.lisp" (string-downcase (symbol-name code))))

(defun default-glossary-file (code)
  "Language-derived name of the glossary"
  (format nil "glossary-~A.lisp" (string-downcase (symbol-name code))))

(defun user-translation-file (&optional (code *current-language*))
  "The one translation cache to read and write, inside translations/"
  (in-data-dir *translations-dir*
               (or *translation-file* (default-translation-file code))))

(defun user-glossary-file (&optional (code *current-language*))
  "The one glossary to read and write, inside glossaries/"
  (in-data-dir *glossaries-dir*
               (or *glossary-file* (default-glossary-file code))))

;;; ============================================================
;;; Settings
;;; ============================================================

(defun set-translation-file (name)
  "Use NAME as the translation cache instead of translations-<language>.lisp.
A bare file name goes inside translations/. Call this before (set-language ...).
NIL restores the language-derived name."
  (setf *translation-file* name)
  (format t "Translation cache: ~A~A~%"
          (user-translation-file)
          (if name "" " (by language)"))
  name)

(defun set-glossary-file (name)
  "Use NAME as the glossary instead of glossary-<language>.lisp.
A bare file name goes inside glossaries/. Call this before (set-language ...).
NIL restores the language-derived name."
  (setf *glossary-file* name)
  (format t "Glossary file: ~A~A~%"
          (user-glossary-file)
          (if name "" " (by language)"))
  name)

(defun show-config ()
  "Show which files the current settings resolve to"
  (format t "~%=== Data Files ===~%")
  (format t "Language         : ~A~%" *current-language*)
  (format t "Translation cache: ~A~A~A~%"
          (user-translation-file)
          (if *translation-file* " (set)" " (by language)")
          (if (probe-file (user-translation-file)) "" " - not created yet"))
  (format t "Glossary         : ~A~A~A~%"
          (user-glossary-file)
          (if *glossary-file* " (set)" " (by language)")
          (if (probe-file (user-glossary-file)) "" " - not created yet"))
  (values))
