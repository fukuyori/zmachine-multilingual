# Z-machine Interpreter

A Z-machine interpreter written in Common Lisp. Play classic text adventures like Zork in 10 languages.

## Supported Languages

| Code | Language | Native |
| --- | --- | --- |
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

* Z-machine version 1-5 support
* Bilingual display (English + translation)
* Auto-translation via DeepL/Claude API
* Translation caching and persistence
* Save/Restore game state

## Requirements

* SBCL (Steel Bank Common Lisp)
* curl (for auto-translation)

## Obtaining Story Files (.z3, .z5)

To play games on the Z-machine, you need story files. Here's how to obtain them:

### ZORK I, II, III (Open Source)

In November 2025, Microsoft open-sourced ZORK I, II, and III under the MIT License. Story files (.z3 extension) can be obtained from the COMPILED directory at the following repositories:

* [Zork 1 - historicalsource](https://github.com/historicalsource/zork1)
* [Zork 2 - historicalsource](https://github.com/historicalsource/zork2)
* [Zork 3 - historicalsource](https://github.com/historicalsource/zork3)

### Interactive Fiction Archive

The [IF Archive](https://ifarchive.org/) hosts many Z-machine compatible games.

### File Naming Conventions

* `.z3` - Z-machine version 3 (Infocom classics including ZORK)
* `.z5` - Z-machine version 5 (later Infocom games)
* `.z8` - Z-machine version 8 (extended format)

Currently, testing has been done with `.z3` files.

Place story files in the same directory as the interpreter, or specify the full path when loading.

## Installation

```bash
git clone https://github.com/fukuyori/zmachine-multilingual.git
cd zmachine-multilingual
```

## Usage

### Quick Start

Edit the following sections in run-zork.lisp and run with SBCL.

- Set up story file

```lisp
;; Specify the path to the story file you want to play
(load-story "zork1.z3")           ; If in the same directory
(load-story "/path/to/zork1.z3")  ; Specify with full path
```

- Change language

```lisp
;; Can be changed during gameplay
(set-language :fr)    ; Switch to French
(set-language :en)    ; English only (no translation)
```

- Set up auto-translation

```lisp
;; DeepL API (free tier available, recommended)
(setup-deepl "your-api-key")

;; Or Claude API
(setup-claude-api "your-api-key")
```

Get a free DeepL API key at https://www.deepl.com/pro-api

- Run from command line

```bash
sbcl --script run-zork.lisp
```

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

---

## Technical Background

### Why Implement in Lisp?

Implementing a Z-machine interpreter in Common Lisp has special significance.

ZORK itself was written in MDL (a Lisp dialect). ZIL was also heavily influenced by Lisp. Writing a Z-machine interpreter in Lisp is, in a sense, "returning to the roots."

Technically, Lisp is well-suited for Z-machine implementation.

## Multilingual Z-machine Interpreter

### Bringing Classics to the World

ZORK could only be played in English. Due to technical constraints of the 1980s and Infocom's North American market focus, no translations were made.

However, after more than 40 years, it's natural to want people worldwide to enjoy these text adventure classics. Experience "being eaten by a grue" in Japanese. Explore the underground empire in French.

The `zmachine-multilingual` project was created to realize this wish. It integrates a translation system into the Z-machine interpreter, enabling gameplay in 10 languages.

### Translation System Architecture

The translation system intercepts Z-machine text output. For each text output:

1. Check cache
2. If cached, use that translation
3. If not, call translation API
4. Save result to cache
5. Display both original and translation (bilingual mode)

### Importance of Translation Cache

In text adventures, the same text appears repeatedly: room descriptions, error messages, system messages...

Calling the translation API every time is inefficient and costly. The translation cache allows once-translated text to be reused.

The cache persists to file. Once you play through a game, all text is translated, and subsequent playthroughs require no API calls.

### Bilingual Display

```
West of House
You are standing in an open field west of a white house, 
with a boarded front door.
There is a small mailbox here.

家の西側
あなたは白い家の西側の開けた野原に立っています。
玄関は板で塞がれています。
ここに小さな郵便受けがあります。
>
```

Displaying both original English text and translation simultaneously allows checking nuances in the original while playing. It also makes it easy to verify translation quality and make corrections as needed.

---

## Project Structure

```
zmachine-multilingual/
├── packages.lisp           # Package definitions
├── memory.lisp             # Memory management, save/load
├── text.lisp               # Text processing (ZSCII decoding)
├── objects.lisp            # Object tree operations
├── dictionary.lisp         # Dictionary processing (input parsing)
├── decode.lisp             # Instruction decoding
├── opcodes.lisp            # Basic opcodes (0OP/1OP/2OP)
├── opcodes-var.lisp        # Variable opcodes (VAR)
├── execute.lisp            # Execution loop
├── translate.lisp          # Translation system
├── languages.lisp          # Language definitions
├── run-zork.lisp           # Launch script
├── zmachine.asd            # ASDF system definition
└── translations/           # Translation data
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

### Module Descriptions

* **packages.lisp**: Defines Common Lisp packages. Declares functions exported externally (`load-story`, `run`, `set-language`, etc.).

* **memory.lisp**: The heart of the Z-machine. Loads story files as byte arrays and provides memory access functions. Also manages global variables, local variables, and stack. Save/restore functionality is implemented here.

* **text.lisp**: Handles ZSCII encoding. Implements 5-bit character decoding, alphabet shifting, and abbreviation expansion. Also serves as the connection point with the translation system.

* **objects.lisp**: Implements the Z-machine object system. Objects have a tree structure, representing containment through parent-child relationships. Provides access to attributes (32 flags) and properties (variable-length data).

* **dictionary.lisp**: Handles player input parsing. Splits input strings into tokens and looks up each token in the dictionary. Found addresses are stored in the parse buffer and passed to game logic.

* **decode.lisp**: Bytecode decoder. Determines instruction format (0OP/1OP/2OP/VAR), reads operands, and generates instruction structures.

* **opcodes.lisp** and **opcodes-var.lisp**: Implementation of approximately 100 opcodes. All Z-machine functionality is contained here, including arithmetic operations, comparisons, branching, object manipulation, and I/O.

* **execute.lisp**: Main execution loop. Decodes instructions, calls corresponding opcode functions, and repeats. Also manages routine calls and returns.

* **translate.lisp**: Core of multilingual support. Implements translation cache management, DeepL/Claude API integration, and translation data save/load.

* **languages.lisp**: Supported language definitions. Contains a table of language codes, English names, and native names.

## Translation Data

* Built-in translations are stored in `translations/translations-XX.lisp`
* User translations are saved to `translations-XX.lisp` in the working directory
* Automatically loaded on next startup

## References

### Official Documentation & Specifications

* [The Z-Machine Standards Document](https://inform-fiction.org/zmachine/standards/)
* [Interactive Fiction Archive](https://ifarchive.org/)

### History & Background

* [The Digital Antiquarian - ZIL and the Z-Machine](https://www.filfre.net/2012/01/zil-and-the-z-machine/)
* [MIT Technology Review - The Enduring Legacy of Zork](https://www.technologyreview.com/2017/08/22/149560/the-enduring-legacy-of-zork/)

### Source Code

* [Infocom Source Code (GitHub)](https://github.com/historicalsource)
* [zmachine-multilingual](https://github.com/fukuyori/zmachine-multilingual)

### Tools

* [Frotz - Z-Machine Interpreter](https://davidgriffith.gitlab.io/frotz/)
* [Inform 7](http://inform7.com/)

## Contributing

Edit translation files and submit pull requests.

## License

MIT License
