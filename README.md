# Z-machine Interpreter

A Z-machine interpreter written in Common Lisp. Play classic text adventures like Zork in 10 languages.

## Supported Languages

| Code | Language | Native |
|------|----------|--------|
| :en | English | English |
| :ja | Japanese | 日本語 |
| :ko | Korean | 한국어 |
| :zh-hans | Simplified Chinese | 简体中文 |
| :zh-hant | Traditional Chinese | 繁體中文 |
| :fr | French | Français |
| :de | German | Deutsch |
| :es | Spanish | Español |
| :pt | Portuguese | Português |
| :ru | Russian | Русский |

## Features

- Z-machine version 1-5 support
- Bilingual display (English + translation)
- Auto-translation via DeepL/Claude API
- Translation caching and persistence
- Save/Restore game state

## Requirements

- SBCL (Steel Bank Common Lisp)
- curl (for auto-translation)

## Installation

```bash
unzip zmachine.zip
cd zmachine
```

## Usage

### Quick Start

```bash
sbcl --load run-zork.lisp
```

### Manual Setup

```lisp
(require :asdf)
(push #p"/path/to/zmachine/" asdf:*central-registry*)
(asdf:load-system :zmachine)
(in-package :zmachine)

;; Select language
(list-languages)      ; Show available languages
(set-language :ja)    ; Select Japanese

;; Start game
(load-story "zork1.z3")
(run)
```

### Change Language

```lisp
;; Can change during gameplay
(set-language :fr)    ; Switch to French
(set-language :en)    ; English only (no translation)
```

### Auto-Translation Setup

```lisp
;; DeepL API (free tier available, recommended)
(setup-deepl "your-api-key")

;; Or Claude API
(setup-claude-api "your-api-key")
```

Get a free DeepL API key at https://www.deepl.com/pro-api

### Translation Management

```lisp
;; Show untranslated texts
(show-untranslated)

;; Add translation manually
(quick-translate 1 "translated text")

;; Auto-translate all untranslated
(auto-translate-all)

;; Show statistics
(translation-stats)

;; Save translations
(save-language-translations)
```

### Save/Restore

In-game:
```
>save
Save filename: mygame
Game saved.

>restore  
Save filename: mygame
Game restored.
```

## File Structure

```
zmachine/
├── packages.lisp           # Package definitions
├── memory.lisp             # Memory management, save/restore
├── text.lisp               # Text output
├── objects.lisp            # Object tree
├── dictionary.lisp         # Dictionary processing
├── decode.lisp             # Instruction decoding
├── opcodes.lisp            # 0OP/1OP/2OP instructions
├── opcodes-var.lisp        # VAR instructions
├── execute.lisp            # Execution loop
├── translate.lisp          # Translation system
├── languages.lisp          # Language definitions
├── run-zork.lisp           # Launch script
├── zmachine.asd            # ASDF system definition
└── translations/           # Translation database
    ├── translations-ja.lisp
    ├── translations-ko.lisp
    ├── translations-zh-hans.lisp
    ├── translations-zh-hant.lisp
    ├── translations-fr.lisp
    ├── translations-de.lisp
    ├── translations-es.lisp
    ├── translations-pt.lisp
    └── translations-ru.lisp
```

## Translation Data

- Built-in translations are in `translations/translations-XX.lisp`
- User translations are saved to `translations-XX.lisp` in working directory
- Automatically loaded on next startup

## Contributing

Edit translation files and submit pull requests.

## License

MIT License
