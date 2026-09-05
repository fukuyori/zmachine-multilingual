;;;; ollama.lisp - Local LLM translation backend (Ollama)
;;;;
;;;; Any model installed in Ollama can be selected:
;;;;   (setup-ollama "qwen3.5:9b")
;;;;   (setup-ollama "translategemma:12b" "http://192.168.1.10:11434")
;;;;   (list-ollama-models)   ; show installed models
;;;;   (set-ollama-model "gemma3:12b")

(in-package :zmachine)

;;; ============================================================
;;; Configuration
;;; ============================================================

(defvar *ollama-url* "http://localhost:11434"
  "Base URL of the Ollama server")

(defvar *ollama-model* "gemma3:4b"
  "Model name used for translation")

(defvar *ollama-temperature* 0.2
  "Sampling temperature (low = consistent terminology)")

(defvar *ollama-num-predict* 1024
  "Maximum number of tokens to generate")

(defvar *ollama-think* nil
  "Allow thinking output on reasoning models (slower)")

(defvar *ollama-keep-alive* "10m"
  "How long Ollama keeps the model loaded between requests")

;;; ============================================================
;;; Setup
;;; ============================================================

(defun setup-ollama (&optional model url)
  "Use Ollama as translation backend.
MODEL defaults to *ollama-model*, URL to *ollama-url*."
  (when url (setf *ollama-url* url))
  (when model (setf *ollama-model* model))
  (setf *translation-backend* :ollama)
  (setf *use-api-translation* t)
  (format t "Ollama configured.~%")
  (format t "  URL   : ~A~%" *ollama-url*)
  (format t "  Model : ~A~%" *ollama-model*)
  (let ((models (ollama-model-names)))
    (cond ((null models)
           (format t "  [warning] Ollama server not reachable.~%"))
          ((not (member *ollama-model* models :test #'string=))
           (format t "  [warning] Model ~S is not installed.~%" *ollama-model*)
           (format t "            Run: ollama pull ~A~%" *ollama-model*)
           (format t "            Installed: ~{~A~^, ~}~%" models))))
  *ollama-model*)

(defun set-ollama-model (model)
  "Change the Ollama model"
  (setf *ollama-model* model)
  (format t "Ollama model: ~A~%" model)
  model)

(defun ollama-model-names ()
  "List of model names installed on the Ollama server (NIL if unreachable)"
  (let ((response (http-get (format nil "~A/api/tags" *ollama-url*))))
    (when response
      (json-string-fields response "name"))))

(defun list-ollama-models ()
  "Show models installed on the Ollama server"
  (let ((models (ollama-model-names)))
    (if (null models)
        (format t "Ollama server not reachable at ~A~%" *ollama-url*)
        (progn
          (format t "~%=== Ollama Models (~A) ===~%" *ollama-url*)
          (dolist (m models)
            (format t "  ~A~A~%" m
                    (if (string= m *ollama-model*) " *" "")))
          (format t "~%Current: ~A~%" *ollama-model*)))
    models))

;;; ============================================================
;;; Generation
;;; ============================================================

(defun ollama-generate (prompt &key (model *ollama-model*))
  "Send PROMPT to Ollama and return the generated text"
  (let* ((body (format nil
                       "{\"model\":\"~A\",\"stream\":false,\"think\":~A,\"keep_alive\":\"~A\",\"options\":{\"temperature\":~,2F,\"num_predict\":~D},\"prompt\":\"~A\"}"
                       (json-escape model)
                       (if *ollama-think* "true" "false")
                       (json-escape *ollama-keep-alive*)
                       *ollama-temperature*
                       *ollama-num-predict*
                       (json-escape prompt)))
         (response (http-post-json (format nil "~A/api/generate" *ollama-url*) body)))
    (when response
      (let ((err (json-string-field response "error")))
        (when err
          (format t "~&[Ollama error: ~A]~%" err)
          (return-from ollama-generate nil)))
      (json-string-field response "response"))))

(defun ollama-translate (text &optional emphasize)
  "Translate TEXT with the configured Ollama model.
EMPHASIZE is an optional list of (english . term) glossary pairs to stress."
  (handler-case
      (let ((result (ollama-generate (build-translation-prompt text emphasize))))
        (when result
          (clean-llm-output result)))
    (error (e)
      (format t "~&[Ollama error: ~A]~%" e)
      nil)))

;;; ============================================================
;;; Testing
;;; ============================================================

(defun test-ollama (&optional (text "You are standing in an open field west of a white house."))
  "Test the Ollama backend"
  (format t "~%=== Ollama Test ===~%")
  (format t "URL      : ~A~%" *ollama-url*)
  (format t "Model    : ~A~%" *ollama-model*)
  (format t "Language : ~A~%" *current-language*)
  (format t "Glossary : ~D terms~%" (hash-table-count *glossary*))
  (let ((models (ollama-model-names)))
    (unless models
      (format t "x Server not reachable~%")
      (return-from test-ollama nil))
    (unless (member *ollama-model* models :test #'string=)
      (format t "x Model not installed (ollama pull ~A)~%" *ollama-model*)
      (return-from test-ollama nil)))
  (let ((result (ollama-translate text)))
    (if result
        (progn (format t "Source: ~A~%" text)
               (format t "Result: ~A~%" result)
               (format t "o Ollama OK~%")
               result)
        (progn (format t "x Translation failed~%") nil))))
