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
  (deepl-code "" :type string)    ; DeepL API target language code
  (file-name "" :type string))    ; Translation file name

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
                       :deepl-code deepl-code
                       :file-name (format nil "translations-~A.lisp"
                                         (string-downcase (symbol-name code))))))

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
          (format t "Language: ~A (~A)~%"
                  (language-name lang) (language-native-name lang))
          (format t "Bilingual mode: disabled (English only)~%"))
        (progn
          (setf *bilingual-mode* t)
          (load-language-translations code)
          (format t "Language: ~A (~A)~%"
                  (language-name lang) (language-native-name lang))
          (format t "Bilingual mode: enabled~%")
          (format t "Translations loaded: ~D~%" (hash-table-count *translation-table*))))
    code))

;;; ============================================================
;;; Translation File Management
;;; ============================================================

(defun translation-file-path (code)
  "Get path to translation file for language"
  (let ((lang (get-language code)))
    (when lang
      (merge-pathnames (language-file-name lang)
                       (merge-pathnames "translations/" 
                                       *default-pathname-defaults*)))))

(defun load-language-translations (code)
  "Load translations for specified language"
  (clrhash *translation-table*)
  (setf *untranslated-log* nil)
  
  ;; Load base translations (built-in)
  (load-base-translations)
  
  ;; Load language-specific file if exists
  (let ((file (translation-file-path code)))
    (when (and file (probe-file file))
      (let ((*auto-save-translations* nil))
        (load file :external-format :utf-8)
        (format t "Loaded: ~A~%" (file-namestring file)))))
  
  ;; Also load user translations file in current directory
  (let ((user-file (format nil "translations-~A.lisp"
                          (string-downcase (symbol-name code)))))
    (when (probe-file user-file)
      (let ((*auto-save-translations* nil))
        (load user-file :external-format :utf-8)
        (format t "Loaded user file: ~A~%" user-file)))))

(defun save-language-translations (&optional (code *current-language*))
  "Save translations for current language"
  (when (eq code :en)
    (format t "English does not need translations.~%")
    (return-from save-language-translations nil))
  
  (let* ((lang (get-language code))
         (filename (format nil "translations-~A.lisp"
                          (string-downcase (symbol-name code)))))
    (with-open-file (out filename :direction :output 
                                  :if-exists :supersede
                                  :external-format :utf-8)
      (format out ";;;; ~A translations for Z-machine~%" (language-name lang))
      (format out ";;;; Language: ~A (~A)~%"
              (language-name lang) (language-native-name lang))
      (format out ";;;; Generated: ~A~%~%" (get-universal-time))
      (format out "(in-package :zmachine)~%~%")
      (maphash (lambda (en trans)
                 (format out "(add-trans ~S ~S)~%" en trans))
               *translation-table*))
    (format t "Saved ~D translations to ~A~%" 
            (hash-table-count *translation-table*) filename)))

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
