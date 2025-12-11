# Z-machine Interpreter

A Z-machine interpreter implemented in **Common Lisp**, providing multilingual support for classic interactive fiction titles such as *Zork*.
This project enables gameplay in **ten languages**, with optional automatic translation powered by DeepL or Claude.

---

## 📚 Supported Languages

| Code     | Language            | Native    |
| -------- | ------------------- | --------- |
| :en      | English             | English   |
| :ja      | Japanese            | 日本語       |
| :ko      | Korean              | 한국어       |
| :zh-hans | Simplified Chinese  | 简体中文      |
| :zh-hant | Traditional Chinese | 繁體中文      |
| :fr      | French              | Français  |
| :de      | German              | Deutsch   |
| :es      | Spanish             | Español   |
| :pt      | Portuguese          | Português |
| :ru      | Russian             | Русский   |

---

## ✨ Features

* Full support for **Z-machine versions 1–5**
* **Bilingual output** with dynamic language switching
* **Automatic machine translation** via DeepL or Claude APIs
* Persistent caching of translated strings
* Built-in **save/restore** functionality
* Extensible translation system with user-modifiable dictionaries

---

## 🔧 Requirements

* **SBCL** (Steel Bank Common Lisp)
* **curl** (used for translation API requests)

---

## 📦 Installation

```bash
unzip zmachine.zip
cd zmachine
```

---

## 🚀 Usage

### Basic Launch

```bash
sbcl --load run-zork.lisp
```

### Manual Setup

```lisp
(require :asdf)
(push #p"/path/to/zmachine/" asdf:*central-registry*)
(asdf:load-system :zmachine)
(in-package :zmachine)

;; List available languages
(list-languages)

;; Set active language
(set-language :ja)    ; Japanese

;; Load and run a story file
(load-story "zork1.z3")
(run)
```

---

## 🌐 Switching Languages

Language can be changed at any time, including during gameplay:

```lisp
(set-language :fr)    ; Switch to French
(set-language :en)    ; English only, no translation
```

---

## 🤖 Automatic Translation Setup

```lisp
;; DeepL API (recommended; free tier available)
(setup-deepl "your-api-key")

;; Or use the Claude API
(setup-claude-api "your-api-key")
```

A free DeepL API key is available at:
[https://www.deepl.com/pro-api](https://www.deepl.com/pro-api)

---

## 📝 Translation Management

```lisp
(show-untranslated)          ; Display untranslated strings
(quick-translate 1 "text")   ; Add a translation manually
(auto-translate-all)         ; Translate all remaining entries
(translation-stats)          ; Show statistics
(save-language-translations) ; Persist translations to disk
```

---

## 💾 Save / Restore

Within the game:

```
>save
Save filename: mygame
Game saved.

>restore
Save filename: mygame
Game restored.
```

---

## 📁 Project Structure

```
zmachine/
├── packages.lisp           ; Package definitions
├── memory.lisp             ; Memory management, save/restore logic
├── text.lisp               ; Text rendering
├── objects.lisp            ; Object tree and hierarchy
├── dictionary.lisp         ; Dictionary and lexical handling
├── decode.lisp             ; Instruction decoder
├── opcodes.lisp            ; 0OP/1OP/2OP opcodes
├── opcodes-var.lisp        ; VAR opcodes
├── execute.lisp            ; Core execution loop
├── translate.lisp          ; Translation subsystem
├── languages.lisp          ; Language registry
├── run-zork.lisp           ; Startup script
├── zmachine.asd            ; ASDF system definition
└── translations/           ; Translation files
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

---

## 🗂 Translation Files

* Each language’s translations are stored in `translations/translations-XX.lisp`
* User-added translations are written to `translations-XX.lisp` in the working directory
* These are automatically loaded on startup

---

## 🤝 Contributing

Contributions are welcome.
To help improve translations, edit the corresponding file under `translations/` and submit a pull request.

---

## 📄 License

This project is licensed under the **MIT License**.
