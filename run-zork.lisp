;;;; run-zork.lisp - Launch script for Z-machine
;;;;
;;;; Usage: sbcl --load run-zork.lisp
;;;;
;;;; Available languages:
;;;;   :en - English (no translation)
;;;;   :ja - Japanese (日本語)
;;;;   :ko - Korean (한국어)
;;;;   :zh-hans - Simplified Chinese (简体中文)
;;;;   :zh-hant - Traditional Chinese (繁體中文)
;;;;   :fr - French (Français)
;;;;   :de - German (Deutsch)
;;;;   :es - Spanish (Español)
;;;;   :pt - Portuguese (Português)
;;;;   :ru - Russian (Русский)

;; Setup ASDF path (change to your zmachine folder path)
(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)

;; Load system
(asdf:load-system :zmachine)
(in-package :zmachine)

;; Data file names (must come before set-language)
;; One translation cache in translations/ and one glossary in glossaries/, each
;; read at startup and written back as you work. A bare file name is enough -
;; use a different one to keep separate files per game, e.g. "zork2-ja.lisp".
(set-translation-file "translations-ja.lisp")
(set-glossary-file "glossary-ja.lisp")

;; Version 6 pictures (optional)
;; A Blorb resource file next to the story is found automatically, and its
;; illustrations are drawn with sixel graphics when the terminal supports it.
;; (setf *graphics-enabled* t)      ; draw even if the terminal is not detected
;; (setf *picture-min-area* 2000)   ; show smaller pictures too
;; (setf *declare-pictures* nil)    ; keep the story in its text layout

;; Waiting for input (optional)
;; (setf *keypress-hint* "[キーを入力して Enter]")
;; (setf *input-hint* "[コマンドを入力してください]")

;; Select language (change as needed)
;; This also loads glossaries/glossary-<lang>.lisp for terminology consistency
(set-language :ja)  ; Japanese

;; Translation backend (optional - pick one)

;; Ollama (local LLM - no API key needed, model name is free to choose)
;; (list-ollama-models)              ; show installed models
;; (setup-ollama "qwen3.5:9b")       ; select the model to translate with
;; (setup-ollama "gemma3:12b" "http://192.168.1.10:11434")  ; remote server

;; DeepL (get a free API key at https://www.deepl.com/pro-api)
;; (setup-deepl "your-api-key")

;; Claude API
;; (setup-claude-api "your-anthropic-api-key")

;; Load story file (change path as needed)
(load-story "zork1.z3")

;; Run
(run)

;;; After playing, you can:
;;; (show-untranslated)      - Show untranslated texts
;;; (auto-translate-all)     - Auto-translate via API
;;; (list-languages)         - Show available languages
;;; (set-language :fr)       - Change language
;;;
;;; Glossary (terminology consistency):
;;; (show-glossary)          - Show glossary terms
;;; (add-glossary "grue" "グルー")  - Add a term
;;; (save-glossary)          - Save to glossary-ja.lisp
;;; (glossary-check)         - Find translations that break the glossary
;;; (glossary-fix)           - Re-translate them via API
;;;
;;; Appearance:
;;; (setf *ansi-enabled* nil)   - Turn off colour
;;; (setf *status-line-enabled* nil)  - Hide the status line
;;;
;;; Cache files:
;;; (show-config)            - Which files the current settings resolve to
;;; (set-translation-file "zork1-ja.lisp")
;;; (set-glossary-file "zork1-ja-glossary.lisp")
;;;
;;; Ollama:
;;; (list-ollama-models)     - Show installed models
;;; (set-ollama-model "gemma3:12b")  - Switch model
;;; (test-ollama)            - Test the backend
