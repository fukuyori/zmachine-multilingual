;;;; packages.lisp - Package definitions for Z-machine

(defpackage :zmachine
  (:use :cl)
  (:export
   ;; Core API
   #:load-story
   #:run
   #:reset-story
   
   ;; State
   #:*zm*
   #:zm-version
   #:zm-pc
   
   ;; Save/Restore
   #:save-game
   #:restore-game
   
   ;; Language Selection
   #:set-language
   #:list-languages
   #:*current-language*
   
   ;; Bilingual Mode
   #:enable-bilingual
   #:disable-bilingual
   #:*bilingual-mode*
   
   ;; Translation Management
   #:add-translation
   #:show-untranslated
   #:quick-translate
   #:translation-stats
   #:save-language-translations
   #:load-language-translations
   #:auto-translate-all
   
   ;; API Setup
   #:setup-deepl
   #:setup-claude-api
   #:setup-ollama
   #:set-ollama-model
   #:list-ollama-models
   #:*translation-backend*
   #:*ollama-model*
   #:*ollama-url*
   #:*ollama-temperature*
   #:*ollama-num-predict*
   #:*ollama-think*
   #:*claude-model*
   #:*deepl-url*

   ;; Glossary (terminology consistency)
   #:add-glossary
   #:remove-glossary
   #:show-glossary
   #:load-glossary
   #:save-glossary
   #:glossary-check
   #:glossary-fix
   #:*glossary*
   #:*glossary-enabled*
   #:*glossary-enforce*

   ;; Partial matching
   #:*use-partial-matches*
   #:*partial-match-threshold*

   ;; Version 6 graphics
   #:load-resources
   #:clear-resources
   #:show-resources
   #:show-picture
   #:list-pictures
   #:graphics-status
   #:*graphics-enabled*
   #:*declare-pictures*
   #:*picture-min-area*
   #:*picture-width*
   #:*sixel-levels*
   #:*sixel-dither*

   ;; Interpreter capabilities
   #:*screen-columns*
   #:*screen-rows*
   #:*screen-pixel-width*
   #:*screen-pixel-height*
   #:*interpreter-number*
   #:*strict-opcodes*
   #:*output-buffer-limit*

   ;; Terminal appearance
   #:*ansi-enabled*
   #:*ansi-source*
   #:*ansi-translation*
   #:*ansi-status*

   ;; Status line
   #:show-status-line
   #:*status-line-enabled*
   #:*status-line-width*
   #:*status-line-min-content*
   #:*status-bar-max-rows*
   #:*input-hint*
   #:*keypress-hint*

   ;; Cache file settings
   #:set-translation-file
   #:set-glossary-file
   #:show-config
   #:*translation-file*
   #:*translations-dir*
   #:*glossary-file*

   ;; Testing
   #:test-deepl-api
   #:test-ollama
   #:test-curl
   #:check-environment))

(in-package :zmachine)
