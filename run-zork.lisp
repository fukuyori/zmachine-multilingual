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

;; Select language (change as needed)
(set-language :ja)  ; Japanese

;; Setup DeepL API (optional - get free API key at https://www.deepl.com/pro-api)
;; (setup-deepl "your-api-key")

;; Or Claude API
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
