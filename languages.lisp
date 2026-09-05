;;;; languages.lisp - Multi-language support for Z-machine
;;;;
;;;; Supported languages:
;;;; - English (no translation)
;;;; - Japanese, Korean
;;;; - Simplified Chinese, Traditional Chinese
;;;; - French, German, Spanish, Portuguese
;;;; - Russian

(in-package :zmachine)

;;; ============================================================
;;; Language Definitions
;;; ============================================================

(defstruct language
  "Language definition"
  (code nil :type keyword)        ; Internal code (:ja, :zh-hans, etc.)
  (name "" :type string)          ; English name
  (native-name "" :type string)   ; Native name
  (deepl-code "" :type string))   ; DeepL API target language code

(defvar *languages* (make-hash-table :test 'eq)
  "Available languages")

(defvar *current-language* :en
  "Current target language")

(defvar *source-language* "EN"
  "Source language for translation (English)")

(defun define-language (code name native-name deepl-code)
  "Register a language"
  (setf (gethash code *languages*)
        (make-language :code code
                       :name name
                       :native-name native-name
                       :deepl-code deepl-code)))

(defun init-languages ()
  "Initialize all supported languages"
  (clrhash *languages*)
  
  ;; East Asian
  (define-language :en "English" "English" "")           ; No translation
  (define-language :ja "Japanese" "日本語" "JA")
  (define-language :ko "Korean" "한국어" "KO")
  (define-language :zh-hans "Simplified Chinese" "简体中文" "ZH")
  (define-language :zh-hant "Traditional Chinese" "繁體中文" "ZH")
  
  ;; European
  (define-language :fr "French" "Français" "FR")
  (define-language :de "German" "Deutsch" "DE")
  (define-language :es "Spanish" "Español" "ES")
  (define-language :pt "Portuguese" "Português" "PT")
  (define-language :ru "Russian" "Русский" "RU"))

;;; ============================================================
;;; Language Selection
;;; ============================================================

(defun get-language (code)
  "Get language by code"
  (gethash code *languages*))

(defun list-languages ()
  "List all available languages"
  (format t "~%=== Available Languages ===~%")
  (format t "~%Code~12TName~30TNative~%")
  (format t "----------------------------------------~%")
  (maphash (lambda (code lang)
             (format t "~A~12T~A~30T~A~A~%"
                     code
                     (language-name lang)
                     (language-native-name lang)
                     (if (eq code *current-language*) " *" "")))
           *languages*)
  (format t "~%Current: ~A~%" *current-language*))

(defun set-language (code)
  "Set target language"
  (let ((lang (get-language code)))
    (unless lang
      (format t "Unknown language: ~A~%" code)
      (format t "Use (list-languages) to see available options.~%")
      (return-from set-language nil))
    
    (setf *current-language* code)
    
    (if (eq code :en)
        (progn
          (setf *bilingual-mode* nil)
          (setf *auto-save-translations* nil)
          (format t "Language: ~A (~A)~%"
                  (language-name lang) (language-native-name lang))
          (format t "Bilingual mode: disabled (English only)~%"))
        (progn
          (setf *bilingual-mode* t)
          (load-language-translations code)
          (setf *auto-save-translations* t)  ; Enable auto-save
          (setf *translations-modified* nil)
          (format t "Language: ~A (~A)~%"
                  (language-name lang) (language-native-name lang))
          (format t "Bilingual mode: enabled~%")
          (format t "Translations loaded: ~D~%" (hash-table-count *translation-table*))
          (format t "Auto-save: enabled~%")))
    code))

;;; ============================================================
;;; Translation File Management
;;; ============================================================

(defun load-language-translations (code)
  "Load translations for specified language"
  (clrhash *translation-table*)
  (setf *untranslated-log* nil)
  
  ;; One cache file: the name comes from the configuration, or from the
  ;; language code. It is also the file that translations are saved back to.
  (let ((file (user-translation-file code)))
    (if (probe-file file)
        (let ((*auto-save-translations* nil))
          (load file :external-format :utf-8)
          (format t "Loaded: ~A~%" file))
        (format t "Cache not found, starting empty: ~A~%" file)))

  ;; Load the glossary for terminology consistency
  (load-glossary code))

(defun save-language-translations (&optional (code *current-language*))
  "Save translations for current language"
  (when (eq code :en)
    (return-from save-language-translations nil))
  
  (let* ((lang (get-language code))
         (filename (user-translation-file code))
         (entries nil))
    (maphash (lambda (en trans) (push (cons en trans) entries)) *translation-table*)
    ;; Sorted, so that saving twice produces the same file
    (setf entries (sort entries #'string-lessp :key #'car))
    (ensure-directories-exist filename)
    (with-open-file (out filename :direction :output 
                                  :if-exists :supersede
                                  :external-format :utf-8)
      (format out ";;;; ~A translations for Z-machine~%" (language-name lang))
      (format out ";;;; Language: ~A (~A)~%"
              (language-name lang) (language-native-name lang))
      (format out ";;;; Generated: ~A~%~%" (get-universal-time))
      (format out "(in-package :zmachine)~%~%")
      (loop for (en . trans) in entries
            do (format out "(add-trans ~S ~S)~%" en trans)))
    filename))

;;; ============================================================
;;; API Integration
;;; ============================================================

(defun get-deepl-target-code ()
  "Get DeepL target language code for current language"
  (let ((lang (get-language *current-language*)))
    (when lang
      (language-deepl-code lang))))

(defun language-supported-p (code)
  "Check if language is supported"
  (not (null (get-language code))))

;; Initialize on load
(init-languages)
