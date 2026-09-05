;;;; translations-template.lisp - Empty translation cache
;;;;
;;;; A cache file with no translations in it. Copy it to start a language, or a
;;;; game, from scratch - every line the game prints then has to go through the
;;;; translation API, which makes it obvious whether the API is working at all.
;;;;
;;;; Usage
;;;;   Copy this file to the name you want and point the launch script at it:
;;;;
;;;;     cp translations/translations-template.lisp translations/zork2-ja.lisp
;;;;
;;;;     ;; run-zork.lisp, before (set-language ...)
;;;;     (set-translation-file "zork2-ja.lisp")
;;;;
;;;;   You do not have to create the file at all - naming a cache that does not
;;;;   exist starts empty and creates it on the first save. Copy this template
;;;;   when you want the file to exist up front, with a header of your own.
;;;;
;;;;   This file itself is never loaded automatically - "template" is not a
;;;;   language code.
;;;;
;;;; Format
;;;;   One (add-trans "<English source>" "<translation>") form per entry.
;;;;   The English string must match the game text exactly, after leading and
;;;;   trailing whitespace has been trimmed. Lookup is by exact string, so
;;;;   punctuation and capitalisation matter.
;;;;
;;;;   (add-trans "West of House" "家の西側")
;;;;   (add-trans "There is a small mailbox here."
;;;;              "ここに小さな郵便受けがあります。")
;;;;
;;;;   The interpreter rewrites this file as you play, sorted by source text.
;;;;   Comments and grouping are not preserved across a save.

(in-package :zmachine)
