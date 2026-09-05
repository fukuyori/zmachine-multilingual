;;;; translate.lisp - Multi-language Translation System for Z-machine
;;;;
;;;; Features:
;;;; - 10 language support
;;;; - Auto-translation via Ollama (local LLM) / DeepL / Claude API
;;;; - Glossary-driven terminology consistency (see glossary.lisp)
;;;; - Translation caching and persistence

(in-package :zmachine)

;;; ============================================================
;;; Forward Declarations (defined in languages.lisp)
;;; ============================================================

(defvar *current-language* :en "Current target language")
(defvar *source-language*)

;; Defined with their default values in ollama.lisp
(defvar *ollama-url*)
(defvar *ollama-model*)

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

;; API backend
(defvar *translation-backend* nil
  "Active translation backend: :ollama, :deepl, :claude or NIL (auto)")
(defvar *deepl-api-key* nil)
(defvar *anthropic-api-key* nil)
(defvar *curl-available* :unknown)
(defvar *http-timeout* 180 "HTTP request timeout in seconds")

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
    
    ;; Strategy 3: API translation
    (when *use-api-translation*
      (let ((api-result (api-translate trimmed)))
        (when api-result
          (setf (gethash trimmed *translation-table*) api-result)
          (setf *translations-modified* t)
          (auto-save-check)  ; Save immediately after API translation
          (return-from translate-text api-result))))
    
    ;; Strategy 4: Partial matching, last resort only.
    ;; It returns the translation of a fragment, so the result is incomplete
    ;; by construction. It must come after the API, or a text that happens to
    ;; contain a cached phrase would never be translated properly at all.
    ;; The result is deliberately not cached.
    (let ((partial (find-partial-matches trimmed)))
      (when partial
        (return-from translate-text partial)))
    
    ;; Log untranslated
    (log-untranslated trimmed)
    nil))

(defvar *use-partial-matches* nil
  "Fall back to the translation of a cached fragment when nothing else worked.
Off by default: the answer is a fragment, so it is incomplete by construction,
and with a translation backend configured it is never needed.")

(defvar *partial-match-threshold* 0.7
  "How much of the text the cached fragment has to cover, 0.0 to 1.0.
The bundled translations contain sentence fragments such as
\"This gives you the rank of\", because the story completes them at run time.
A low threshold makes those usable but also lets a short entry stand in for a
long sentence - \"Forest\" would answer for \"Forest Path\".")

(defun find-partial-matches (text)
  "Translation of the longest cached English that is a substring of TEXT.
Only a last resort: the answer is a fragment, so it is necessarily incomplete."
  (when *use-partial-matches*
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
          (when (>= (length (car best))
                    (* *partial-match-threshold* (length text)))
            (cdr best)))))))

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
  (setf *translation-backend* :deepl)
  (setf *use-api-translation* t)
  (format t "DeepL API configured.~%"))

(defun setup-claude-api (api-key)
  "Setup Claude API"
  (setf *anthropic-api-key* api-key)
  (setf *translation-backend* :claude)
  (setf *use-api-translation* t)
  (format t "Claude API configured.~%"))

(defun active-backend ()
  "Backend actually used for API translation"
  (or *translation-backend*
      (cond (*deepl-api-key* :deepl)
            (*anthropic-api-key* :claude))))

(defun llm-backend-p (&optional (backend (active-backend)))
  "True for prompt-driven backends that understand a glossary"
  (member backend '(:ollama :claude)))

(defun api-translate-once (text &optional emphasize)
  "Single API call. EMPHASIZE is a list of (english . term) glossary pairs
that the prompt should stress (LLM backends only)."
  (case (active-backend)
    (:ollama (ollama-translate text emphasize))
    (:claude (claude-translate text emphasize))
    (:deepl (deepl-translate text))
    (t nil)))

(defun api-translate (text)
  "Translate using configured API, keeping glossary terms consistent"
  (let ((result (api-translate-once text)))
    (when (and result *glossary-enabled* *glossary-enforce* (llm-backend-p))
      (let ((missing (glossary-missing-terms text result)))
        (when missing
          (let ((retry (api-translate-once text missing)))
            (when (and retry
                       (< (length (glossary-missing-terms text retry))
                          (length missing)))
              (setf result retry))))))
    result))

;;; ------------------------------------------------------------
;;; Prompt construction (shared by Ollama and Claude)
;;; ------------------------------------------------------------

(defun build-translation-prompt (text &optional emphasize)
  "Build a translation prompt including the glossary terms found in TEXT"
  (let* ((lang (get-language *current-language*))
         (lang-name (if lang (language-name lang) "Japanese"))
         (glossary (glossary-prompt-section text emphasize)))
    (with-output-to-string (s)
      (format s "You are a translator for the interactive fiction game Zork.~%")
      (format s "Translate the English text below into ~A.~%" lang-name)
      (when glossary
        (format s "~%Glossary - when one of these English terms appears in the text, ")
        (format s "translate it exactly like this:~%~A" glossary))
      (format s "~%Rules:~%")
      (format s "- Translate the complete text, every sentence, from beginning to end.~%")
      (format s "- Output only the ~A translation: no explanations, no romanization, no quotation marks.~%"
              lang-name)
      (format s "- Keep the original line breaks and punctuation.~%")
      (when emphasize
        (format s "- The previous attempt ignored the glossary. These translations are mandatory: ~{~A~^, ~}~%"
                (mapcar #'cdr emphasize)))
      (format s "~%English text:~%~A~%" text)
      (format s "~%~A translation of the complete text:" lang-name))))

(defun clean-llm-output (text)
  "Strip decorations LLMs tend to add around a translation"
  (let ((result text))
    ;; Remove <think>...</think> blocks
    (let ((start (search "<think>" result)))
      (when start
        (let ((end (search "</think>" result)))
          (setf result (if end
                           (concatenate 'string (subseq result 0 start)
                                        (subseq result (+ end 8)))
                           (subseq result 0 start))))))
    (setf result (string-trim '(#\Space #\Newline #\Return #\Tab) result))
    ;; Remove a leading "Translation:" style label
    (let ((colon (position #\: result)))
      (when (and colon (< colon 20)
                 (let ((head (subseq result 0 colon)))
                   (and (ascii-string-p head)
                        (search "translat" (string-downcase head)))))
        (setf result (string-trim '(#\Space #\Newline #\Return #\Tab)
                                  (subseq result (1+ colon))))))
    ;; Remove wrapping quotes
    (when (and (>= (length result) 2)
               (char= (char result 0) #\")
               (char= (char result (1- (length result))) #\")
               (not (find #\" result :start 1 :end (1- (length result)))))
      (setf result (subseq result 1 (1- (length result)))))
    (if (> (length result) 0) result nil)))

(defvar *deepl-url* "https://api-free.deepl.com/v2/translate"
  "DeepL endpoint. Use https://api.deepl.com/v2/translate for a Pro key.")

(defun deepl-auth-header ()
  "DeepL requires the key in an Authorization header.
The auth_key request parameter it used to accept is gone, and requests that
still send it are rejected with 403."
  (format nil "Authorization: DeepL-Auth-Key ~A" *deepl-api-key*))

(defun deepl-translate (text)
  "Translate via DeepL API"
  (let ((target-code (get-deepl-target-code)))
    (unless target-code
      (return-from deepl-translate nil))
    (handler-case
        (let ((response (deepl-request text target-code)))
          (when response
            (let ((message (json-string-field response "message")))
              (when message
                (format t "~&[DeepL error: ~A]~%" message)
                (return-from deepl-translate nil)))
            (let ((result (json-string-field response "text")))
              (cond (result result)
                    (t (format t "~&[DeepL: unexpected response: ~A]~%"
                               (if (> (length response) 200)
                                   (concatenate 'string (subseq response 0 200) "...")
                                   response))
                       nil)))))
      (error (e)
        (format t "DeepL error: ~A~%" e)
        nil))))

(defun deepl-request (text target-lang)
  "POST one text to DeepL and return the raw response body"
  (http-post-json *deepl-url*
                  (format nil "{\"text\":[\"~A\"],\"source_lang\":\"~A\",\"target_lang\":\"~A\"}"
                          (json-escape text)
                          (json-escape *source-language*)
                          (json-escape target-lang))
                  (list (deepl-auth-header))))

(defvar *claude-model* "claude-3-5-haiku-latest" "Model used by the Claude backend")

(defun claude-translate (text &optional emphasize)
  "Translate via Claude API"
  (handler-case
      (let* ((prompt (build-translation-prompt text emphasize))
             (json-body (format nil "{\"model\":\"~A\",\"max_tokens\":1024,\"messages\":[{\"role\":\"user\",\"content\":\"~A\"}]}"
                                (json-escape *claude-model*)
                                (json-escape prompt)))
             (response (http-post-json "https://api.anthropic.com/v1/messages"
                                       json-body
                                       (list (format nil "x-api-key: ~A" *anthropic-api-key*)
                                             "anthropic-version: 2023-06-01"))))
        (when response
          (let ((result (extract-json-text response)))
            (when result (clean-llm-output result)))))
    (error (e)
      (format t "Claude error: ~A~%" e)
      nil)))

(defun extract-json-text (json-str)
  "Extract the first \"text\" field from a JSON response"
  (json-string-field json-str "text"))

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

(defvar *http-temp-counter* 0)

(defun temp-directory ()
  "Directory for temporary files"
  (or (sb-ext:posix-getenv "TEMP")
      (sb-ext:posix-getenv "TMP")
      "/tmp"))

(defun temp-file-path (name)
  "Unique temporary file path"
  (format nil "~A/zm-~A-~D" (temp-directory) name (incf *http-temp-counter*)))

(defun read-text-file (path)
  "Read a UTF-8 text file, NIL if missing"
  (when (probe-file path)
    (with-open-file (in path :direction :input
                             :external-format :utf-8
                             :if-does-not-exist nil)
      (when in
        (let* ((buffer (make-string (file-length in)))
               (count (read-sequence buffer in)))
          (subseq buffer 0 count))))))

(defun write-text-file (path text)
  "Write TEXT to PATH as UTF-8"
  (with-open-file (out path :direction :output
                            :if-exists :supersede
                            :external-format :utf-8)
    (write-string text out))
  path)

(defun delete-file-if-exists (path)
  (ignore-errors
   (when (probe-file path) (delete-file path))))

(defun http-post-json (url json-body &optional headers)
  "POST JSON-BODY to URL, return the response body as a string.
HEADERS is a list of \"Name: value\" strings. UTF-8 safe on Windows."
  (if (curl-available-p)
      (http-post-json-curl url json-body headers)
      (when (windows-p)
        (http-post-json-powershell url json-body headers))))

(defun http-post-json-curl (url json-body headers)
  "POST JSON via curl using temp files (long text and UTF-8 safe)"
  (let ((in-file (temp-file-path "req.json"))
        (out-file (temp-file-path "res.json")))
    (unwind-protect
         (handler-case
             (progn
               (write-text-file in-file json-body)
               (sb-ext:run-program
                "curl"
                (append (list "-s" "-S" "-X" "POST" url
                              "-H" "Content-Type: application/json")
                        (loop for h in headers append (list "-H" h))
                        (list "--max-time" (format nil "~D" *http-timeout*)
                              "--data-binary" (format nil "@~A" in-file)
                              "-o" out-file))
                :search t :wait t :error nil)
               (read-text-file out-file))
           (error (e)
             (format t "~&[HTTP error: ~A]~%" e)
             nil))
      (delete-file-if-exists in-file)
      (delete-file-if-exists out-file))))

(defun ps-header-hashtable (headers)
  "PowerShell hashtable literal for HTTP headers"
  (if (null headers)
      "@{}"
      (with-output-to-string (s)
        (format s "@{")
        (loop for h in headers
              for sep = "" then "; "
              do (let ((colon (position #\: h)))
                   (when colon
                     (format s "~A'~A'='~A'" sep
                             (ps-escape (string-trim " " (subseq h 0 colon)))
                             (ps-escape (string-trim " " (subseq h (1+ colon))))))))
        (format s "}"))))

(defun http-post-json-powershell (url json-body headers)
  "POST JSON via PowerShell (fallback when curl is unavailable)"
  (let ((in-file (temp-file-path "req.json"))
        (out-file (temp-file-path "res.json")))
    (unwind-protect
         (handler-case
             (progn
               (write-text-file in-file json-body)
               (run-powershell
                (format nil
                        "$ErrorActionPreference='Stop'
$body = [System.IO.File]::ReadAllBytes('~A')
$r = Invoke-WebRequest -Uri '~A' -Method Post -Body $body -ContentType 'application/json' -Headers ~A -UseBasicParsing -TimeoutSec ~D
[System.IO.File]::WriteAllText('~A', $r.Content, [System.Text.Encoding]::UTF8)"
                        in-file (ps-escape url) (ps-header-hashtable headers)
                        *http-timeout* out-file))
               (read-text-file out-file))
           (error (e)
             (format t "~&[HTTP error: ~A]~%" e)
             nil))
      (delete-file-if-exists in-file)
      (delete-file-if-exists out-file))))

(defun http-get (url)
  "GET URL, return the response body as a string"
  (let ((out-file (temp-file-path "get.json")))
    (unwind-protect
         (handler-case
             (progn
               (if (curl-available-p)
                   (sb-ext:run-program "curl"
                                       (list "-s" "-S" "--max-time"
                                             (format nil "~D" *http-timeout*)
                                             url "-o" out-file)
                                       :search t :wait t :error nil)
                   (when (windows-p)
                     (run-powershell
                      (format nil
                              "$ErrorActionPreference='Stop'
$r = Invoke-WebRequest -Uri '~A' -UseBasicParsing -TimeoutSec ~D
[System.IO.File]::WriteAllText('~A', $r.Content, [System.Text.Encoding]::UTF8)"
                              (ps-escape url) *http-timeout* out-file))))
               (read-text-file out-file))
           (error () nil))
      (delete-file-if-exists out-file))))

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
        (#\Return (write-string "\\r" out))
        (#\Tab (write-string "\\t" out))
        (otherwise
         (if (< (char-code c) 32)
             (format out "\\u~4,'0X" (char-code c))
             (write-char c out)))))))

(defun json-read-string-at (json start)
  "Read the JSON string starting at the opening quote at START.
Returns (values unescaped-string index-after-closing-quote)"
  (let ((out (make-string-output-stream))
        (n (length json))
        (i (1+ start)))
    (loop
      (when (>= i n) (return))
      (let ((c (char json i)))
        (cond ((and (char= c #\\) (< (1+ i) n))
               ;; keep the escape sequence, unescape-json handles it later
               (write-char c out)
               (write-char (char json (1+ i)) out)
               (incf i 2))
              ((char= c #\") (incf i) (return))
              (t (write-char c out) (incf i)))))
    (values (unescape-json (get-output-stream-string out)) i)))

(defun json-field-value-start (json key &optional (from 0))
  "Index of the opening quote of KEY's string value, or NIL"
  (let* ((pattern (format nil "\"~A\":" key))
         (pos (search pattern json :start2 from)))
    (when pos
      (let ((i (+ pos (length pattern)))
            (n (length json)))
        (loop while (and (< i n)
                         (member (char json i) '(#\Space #\Tab #\Newline #\Return)))
              do (incf i))
        (when (and (< i n) (char= (char json i) #\"))
          i)))))

(defun json-string-field (json key)
  "Value of the first string field named KEY, or NIL"
  (let ((start (json-field-value-start json key)))
    (when start
      (values (json-read-string-at json start)))))

(defun json-string-fields (json key)
  "Values of every string field named KEY, in order"
  (let ((results nil)
        (from 0))
    (loop
      (let ((start (json-field-value-start json key from)))
        (unless start (return))
        (multiple-value-bind (value next) (json-read-string-at json start)
          (push value results)
          (setf from next))))
    (nreverse results)))

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
  (format t "API: ~A~%" (if *use-api-translation* "enabled" "disabled"))
  (format t "Backend: ~A~%" (or (active-backend) "none"))
  (when (eq (active-backend) :ollama)
    (format t "Ollama model: ~A (~A)~%" *ollama-model* *ollama-url*))
  (format t "Glossary: ~D terms (~A~A)~%"
          (hash-table-count *glossary*)
          (if *glossary-enabled* "enabled" "disabled")
          (if (and *glossary-enabled* *glossary-enforce*) ", enforced" "")))

(defun auto-translate-all (&optional (delay (if (eq (active-backend) :ollama) 0 1.0)))
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
  (format t "URL   : ~A~%" *deepl-url*)
  (format t "Target: ~A~%" (get-deepl-target-code))
  (let ((response (deepl-request "Hello" (get-deepl-target-code))))
    (format t "Response: ~A~%" response)
    (let ((message (and response (json-string-field response "message")))
          (result (and response (json-string-field response "text"))))
      (cond ((null response)
             (format t "✗ No response (is curl available?)~%") nil)
            (message
             (format t "✗ DeepL rejected the request: ~A~%" message) nil)
            (result
             (format t "Translation: ~A~%" result)
             (format t "✓ API OK~%") t)
            (t (format t "✗ Unexpected response~%") nil)))))

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
  (format t "Backend: ~A~%" (or (active-backend) "none"))
  (format t "Ollama: ~A~%"
          (let ((models (ollama-model-names)))
            (if models
                (format nil "~A (~D models, current: ~A)"
                        *ollama-url* (length models) *ollama-model*)
                (format nil "not reachable (~A)" *ollama-url*))))
  (format t "Glossary: ~D terms~%" (hash-table-count *glossary*))
  (when (and (windows-p) (not (curl-available-p)))
    (format t "PowerShell: fallback~%")))
