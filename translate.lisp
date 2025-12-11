;;;; translate.lisp - Multi-language Translation System for Z-machine
;;;;
;;;; Features:
;;;; - 10 language support
;;;; - Auto-translation via DeepL/Claude API
;;;; - Translation caching and persistence

(in-package :zmachine)

;;; ============================================================
;;; Forward Declarations (defined in languages.lisp)
;;; ============================================================

(defvar *current-language* :en "Current target language")

;;; ============================================================
;;; Configuration
;;; ============================================================

(defvar *bilingual-mode* nil "Enable bilingual output")
(defvar *translation-table* (make-hash-table :test 'equal) "Translation cache")
(defvar *untranslated-log* nil "List of untranslated texts")
(defvar *use-api-translation* nil "Enable API translation")
(defvar *auto-save-translations* nil "Auto-save on translation add")
(defvar *translations-modified* nil "Modification flag")
(defvar *min-text-length-for-translation* 3 "Minimum text length")

;; API keys
(defvar *deepl-api-key* nil)
(defvar *anthropic-api-key* nil)
(defvar *curl-available* :unknown)

;;; ============================================================
;;; Base Translation Data (English source texts)
;;; ============================================================

(defvar *base-texts* nil "List of translatable texts from Zork I")

(defun load-base-translations ()
  "Load base English texts (no translations yet)"
  ;; This just initializes the table - actual translations come from language files
  nil)

;;; ============================================================
;;; Core Translation Functions
;;; ============================================================

(defun add-trans (en translation)
  "Add translation to table"
  (setf (gethash en *translation-table*) translation)
  (when *auto-save-translations*
    (setf *translations-modified* t)
    (auto-save-check)))

(defun auto-save-check ()
  "Save if modified and auto-save is enabled"
  (when (and *auto-save-translations* *translations-modified*)
    (save-language-translations)
    (setf *translations-modified* nil)))

(defun translate-text (text)
  "Translate text using multiple strategies"
  (unless *bilingual-mode*
    (return-from translate-text nil))
  
  (let ((trimmed (string-trim '(#\Space #\Newline #\Return #\Tab) text)))
    (when (< (length trimmed) *min-text-length-for-translation*)
      (return-from translate-text nil))
    
    ;; Strategy 1: Exact match
    (let ((exact (gethash trimmed *translation-table*)))
      (when exact
        (return-from translate-text exact)))
    
    ;; Strategy 2: Case-insensitive
    (maphash (lambda (en trans)
               (when (string-equal en trimmed)
                 (return-from translate-text trans)))
             *translation-table*)
    
    ;; Strategy 3: Partial matching
    (let ((partial (find-partial-matches trimmed)))
      (when partial
        (return-from translate-text partial)))
    
    ;; Strategy 4: API translation
    (when *use-api-translation*
      (let ((api-result (api-translate trimmed)))
        (when api-result
          (setf (gethash trimmed *translation-table*) api-result)
          (setf *translations-modified* t)
          (auto-save-check)  ; Save immediately after API translation
          (return-from translate-text api-result))))
    
    ;; Log untranslated
    (log-untranslated trimmed)
    nil))

(defun find-partial-matches (text)
  "Find partial translations"
  (let ((matches nil)
        (text-lower (string-downcase text)))
    (maphash (lambda (en trans)
               (when (and (> (length en) 5)
                         (search (string-downcase en) text-lower))
                 (push (cons en trans) matches)))
             *translation-table*)
    (when matches
      (setf matches (sort matches #'> :key (lambda (x) (length (car x)))))
      (let ((best (first matches)))
        (when (> (length (car best)) (/ (length text) 3))
          (cdr best))))))

(defun log-untranslated (text)
  "Log untranslated text"
  (unless (member text *untranslated-log* :test #'string=)
    (push text *untranslated-log*)))

;;; ============================================================
;;; Bilingual Mode Control
;;; ============================================================

(defun enable-bilingual (&optional (lang :ja))
  "Enable bilingual mode with specified language"
  (set-language lang)
  (setf *auto-save-translations* t)
  (setf *translations-modified* nil))

(defun disable-bilingual ()
  "Disable bilingual mode"
  (when *translations-modified*
    (save-language-translations))
  (setf *bilingual-mode* nil)
  (setf *auto-save-translations* nil)
  (when *untranslated-log*
    (format t "Untranslated: ~D items~%" (length *untranslated-log*)))
  (format t "Bilingual mode disabled~%"))

;;; ============================================================
;;; API Translation
;;; ============================================================

(defun setup-deepl (api-key)
  "Setup DeepL API"
  (setf *deepl-api-key* api-key)
  (setf *use-api-translation* t)
  (format t "DeepL API configured.~%"))

(defun setup-claude-api (api-key)
  "Setup Claude API"
  (setf *anthropic-api-key* api-key)
  (setf *use-api-translation* t)
  (format t "Claude API configured.~%"))

(defun api-translate (text)
  "Translate using configured API"
  (cond
    (*deepl-api-key* (deepl-translate text))
    (*anthropic-api-key* (claude-translate text))
    (t nil)))

(defun deepl-translate (text)
  "Translate via DeepL API"
  (let ((target-code (get-deepl-target-code)))
    (unless target-code
      (return-from deepl-translate nil))
    (handler-case
        (let ((result (deepl-request text target-code)))
          (when result
            (extract-json-text result)))
      (error (e)
        (format t "DeepL error: ~A~%" e)
        nil))))

(defun deepl-request (text target-lang)
  "Make DeepL API request"
  (if (curl-available-p)
      (deepl-request-curl text target-lang)
      (when (windows-p)
        (deepl-request-powershell text target-lang))))

(defun deepl-request-curl (text target-lang)
  "DeepL request via curl (supports long text)"
  (let* ((temp-dir (or (sb-ext:posix-getenv "TEMP")
                       (sb-ext:posix-getenv "TMP")
                       "/tmp"))
         (input-file (format nil "~A/deepl-input.txt" temp-dir))
         (output-file (format nil "~A/deepl-output.txt" temp-dir)))
    (handler-case
        (progn
          ;; Write POST data to input file
          (with-open-file (out input-file :direction :output 
                                          :if-exists :supersede
                                          :external-format :latin-1)
            (format out "auth_key=~A&text=~A&source_lang=EN&target_lang=~A"
                    *deepl-api-key* (url-encode text) target-lang))
          ;; Run curl with output to file
          (sb-ext:run-program "curl"
                              (list "-s" "-X" "POST"
                                    "https://api-free.deepl.com/v2/translate"
                                    "-d" (format nil "@~A" input-file)
                                    "-o" output-file)
                              :search t
                              :wait t)
          ;; Read result from output file
          (let ((result nil))
            (when (probe-file output-file)
              (with-open-file (in output-file :direction :input
                                              :external-format :utf-8
                                              :if-does-not-exist nil)
                (when in
                  (setf result (make-string (file-length in)))
                  (read-sequence result in))))
            ;; Clean up
            (when (probe-file input-file) (delete-file input-file))
            (when (probe-file output-file) (delete-file output-file))
            result))
      (error (e) 
        (format t "~&[DeepL error: ~A]~%" e)
        nil))))

(defun deepl-request-powershell (text target-lang)
  "DeepL request via PowerShell (supports long text)"
  (let* ((temp-file (format nil "~A\\deepl-temp.txt" 
                           (or (sb-ext:posix-getenv "TEMP") ".")))
         (script (format nil 
"$text = Get-Content -Path '~A' -Raw -Encoding UTF8
$body = @{auth_key='~A'; text=$text; source_lang='EN'; target_lang='~A'}
Invoke-RestMethod -Uri 'https://api-free.deepl.com/v2/translate' -Method Post -Body $body | ConvertTo-Json"
                         temp-file *deepl-api-key* target-lang)))
    ;; Write text to temp file
    (with-open-file (out temp-file :direction :output 
                                   :if-exists :supersede
                                   :external-format :utf-8)
      (write-string text out))
    (let ((result (run-powershell script)))
      ;; Clean up
      (when (probe-file temp-file)
        (delete-file temp-file))
      result)))

(defun claude-translate (text)
  "Translate via Claude API"
  (let* ((lang (get-language *current-language*))
         (lang-name (if lang (language-name lang) "Japanese")))
    (handler-case
        (let* ((prompt (format nil "Translate to ~A. Output ONLY the translation: ~A" 
                              lang-name text))
               (json-body (format nil "{\"model\":\"claude-3-haiku-20240307\",\"max_tokens\":1024,\"messages\":[{\"role\":\"user\",\"content\":\"~A\"}]}"
                                 (json-escape prompt)))
               (output (make-string-output-stream)))
          (sb-ext:run-program "curl"
                              (list "-s" "-X" "POST"
                                    "https://api.anthropic.com/v1/messages"
                                    "-H" "Content-Type: application/json"
                                    "-H" (format nil "x-api-key: ~A" *anthropic-api-key*)
                                    "-H" "anthropic-version: 2023-06-01"
                                    "-d" json-body)
                              :output output :error nil :search t)
          (let ((result (get-output-stream-string output)))
            (when (> (length result) 0)
              (extract-json-text result))))
      (error (e)
        (format t "Claude error: ~A~%" e)
        nil))))

(defun extract-json-text (json-str)
  "Extract text from JSON response"
  (let ((start (search "\"text\":\"" json-str)))
    (when start
      (let* ((begin (+ start 8))
             (end (position #\" json-str :start begin)))
        (when end
          (unescape-json (subseq json-str begin end)))))))

;;; ============================================================
;;; Utilities
;;; ============================================================

(defun windows-p ()
  "Check if Windows"
  (or (member :windows *features*)
      (member :win32 *features*)
      (search "Windows" (software-type))
      (search "windows" (machine-instance))))

(defun curl-available-p ()
  "Check if curl available"
  (when (eq *curl-available* :unknown)
    (setf *curl-available*
          (handler-case
              (let ((out (make-string-output-stream)))
                (sb-ext:run-program "curl" '("--version")
                                    :output out :error nil :search t)
                (search "curl" (get-output-stream-string out)))
            (error () nil))))
  *curl-available*)

(defun run-powershell (script)
  "Run PowerShell command"
  (let ((output (make-string-output-stream)))
    (handler-case
        (progn
          (sb-ext:run-program "powershell"
                              (list "-NoProfile" "-Command" script)
                              :output output :error nil :search t)
          (get-output-stream-string output))
      (error () nil))))

(defun url-encode (str)
  "URL encode string (UTF-8 safe)"
  (with-output-to-string (out)
    (loop for c across str
          for code = (char-code c)
          do (cond
               ((or (alphanumericp c) (find c "-_.~")) (write-char c out))
               ((char= c #\Space) (write-string "%20" out))
               ((< code 128) (format out "%~2,'0X" code))
               ;; UTF-8 encode for non-ASCII
               ((< code #x800)
                (format out "%~2,'0X%~2,'0X"
                        (logior #xC0 (ash code -6))
                        (logior #x80 (logand code #x3F))))
               ((< code #x10000)
                (format out "%~2,'0X%~2,'0X%~2,'0X"
                        (logior #xE0 (ash code -12))
                        (logior #x80 (logand (ash code -6) #x3F))
                        (logior #x80 (logand code #x3F))))
               (t
                (format out "%~2,'0X%~2,'0X%~2,'0X%~2,'0X"
                        (logior #xF0 (ash code -18))
                        (logior #x80 (logand (ash code -12) #x3F))
                        (logior #x80 (logand (ash code -6) #x3F))
                        (logior #x80 (logand code #x3F))))))))

(defun ps-escape (str)
  "Escape for PowerShell"
  (with-output-to-string (out)
    (loop for c across str do
      (case c
        (#\' (write-string "''" out))
        (otherwise (write-char c out))))))

(defun json-escape (str)
  "Escape for JSON"
  (with-output-to-string (out)
    (loop for c across str do
      (case c
        (#\" (write-string "\\\"" out))
        (#\\ (write-string "\\\\" out))
        (#\Newline (write-string "\\n" out))
        (otherwise (write-char c out))))))

(defun unescape-json (str)
  "Unescape JSON string"
  (let ((result (make-array (length str) :element-type 'character 
                            :fill-pointer 0 :adjustable t)))
    (loop with i = 0
          while (< i (length str))
          do (let ((c (char str i)))
               (if (and (char= c #\\) (< (1+ i) (length str)))
                   (let ((next (char str (1+ i))))
                     (case next
                       (#\n (vector-push-extend #\Newline result))
                       (#\t (vector-push-extend #\Tab result))
                       (#\" (vector-push-extend #\" result))
                       (#\\ (vector-push-extend #\\ result))
                       (#\u (when (< (+ i 5) (length str))
                              (handler-case
                                  (vector-push-extend 
                                   (code-char (parse-integer 
                                               (subseq str (+ i 2) (+ i 6)) 
                                               :radix 16))
                                   result)
                                (error () (vector-push-extend #\? result)))
                              (incf i 4)))
                       (otherwise (vector-push-extend next result)))
                     (incf i 2))
                   (progn
                     (vector-push-extend c result)
                     (incf i)))))
    (coerce result 'string)))

;;; ============================================================
;;; User Interface
;;; ============================================================

(defun add-translation (english translation)
  "Add translation (user function)"
  (setf (gethash english *translation-table*) translation)
  (setf *translations-modified* t)
  (format t "Added: ~A → ~A~%" english translation))

(defun show-untranslated (&optional (n 20))
  "Show untranslated texts"
  (let ((texts (reverse *untranslated-log*)))
    (format t "~%=== Untranslated Texts (~D items) ===~%" (length texts))
    (loop for text in texts
          for i from 1 to n
          do (format t "~3D: ~S~%" i text))))

(defun quick-translate (n translation)
  "Quick add translation by number"
  (let ((texts (reverse *untranslated-log*)))
    (when (<= 1 n (length texts))
      (let ((english (nth (1- n) texts)))
        (add-trans english translation)
        (setf *untranslated-log* (remove english *untranslated-log* :test #'string=))
        (format t "Added: ~A~%  → ~A~%" english translation)))))

(defun translation-stats ()
  "Show statistics"
  (format t "~%=== Translation Statistics ===~%")
  (format t "Language: ~A~%" *current-language*)
  (format t "Translations: ~D~%" (hash-table-count *translation-table*))
  (format t "Untranslated: ~D~%" (length *untranslated-log*))
  (format t "API: ~A~%" (if *use-api-translation* "enabled" "disabled")))

(defun auto-translate-all (&optional (delay 1.0))
  "Translate all untranslated via API"
  (unless *use-api-translation*
    (format t "API not configured.~%")
    (return-from auto-translate-all nil))
  (let ((texts (reverse *untranslated-log*))
        (success 0) (failed 0))
    (format t "Translating ~D items...~%" (length texts))
    (dolist (text texts)
      (format t "  ~A..." (subseq text 0 (min 30 (length text))))
      (let ((translation (api-translate text)))
        (if translation
            (progn
              (add-trans text translation)
              (format t " → ~A~%" translation)
              (incf success))
            (progn
              (format t " FAILED~%")
              (incf failed))))
      (sleep delay))
    (setf *untranslated-log* 
          (remove-if (lambda (txt) (gethash txt *translation-table*))
                     *untranslated-log*))
    (format t "Done: ~D success / ~D failed~%" success failed)
    (when (> success 0)
      (save-language-translations))))

;;; ============================================================
;;; Testing
;;; ============================================================

(defun test-deepl-api ()
  "Test DeepL API"
  (unless *deepl-api-key*
    (format t "DeepL API key not set.~%")
    (return-from test-deepl-api nil))
  (format t "Testing DeepL API...~%")
  (format t "Method: ~A~%" (if (curl-available-p) "curl" "PowerShell"))
  (format t "Target: ~A~%" (get-deepl-target-code))
  (let ((result (deepl-request "Hello" (get-deepl-target-code))))
    (format t "Response: ~A~%" result)
    (if (and result (search "text" result))
        (progn
          (format t "Translation: ~A~%" (extract-json-text result))
          (format t "✓ API OK~%") t)
        (progn (format t "✗ API Error~%") nil))))

(defun test-curl ()
  "Test curl"
  (setf *curl-available* :unknown)
  (if (curl-available-p)
      (progn (format t "✓ curl available~%") t)
      (progn (format t "✗ curl not available~%") nil)))

(defun check-environment ()
  "Check environment"
  (format t "~%=== Environment Check ===~%")
  (format t "OS: ~A~%" (if (windows-p) "Windows" "Linux/Mac"))
  (format t "curl: ~A~%" (if (curl-available-p) "available" "not available"))
  (format t "Language: ~A~%" *current-language*)
  (when (and (windows-p) (not (curl-available-p)))
    (format t "PowerShell: fallback~%")))
