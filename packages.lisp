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
   
   ;; Testing
   #:test-deepl-api
   #:test-curl
   #:check-environment))

(in-package :zmachine)
