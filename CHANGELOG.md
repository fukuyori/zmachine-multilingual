# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.6.1] - 2026-09-05

Versions 6 and 8 now run. All 34 story files in the test set play.

### Fixed

* **Version 8 stories could not run.** `pull` stores its result in Version 6
  only, but the version test was `<= 5`, so Versions 7 and 8 took the Version 6
  path. That consumed a store byte which was not there, losing instruction
  alignment, and popped the stack once too often. `advent.z8` died with "Stack
  underflow".
* **Version 6 stories started at the wrong address.** In Version 6 the word at
  $06 is the packed address of the `main` routine and has to be *called*; the
  loader jumped to it as a byte address instead. `zork0.z6` broke on its very
  first instruction.
* **The routine and string offsets were ignored for Versions 6 and 7.** Packed
  addresses now add the offsets from $28 and $2A.
* **`throw` was not implemented** and raised an error. It now unwinds the call
  stack to the depth `catch` reported and returns from that routine.
* **The Z-machine output buffer grew without limit.** Every character the story
  printed was appended to a buffer that nothing ever read and only `reset-story`
  discarded, so a story that printed enough exhausted the heap. It is now capped
  at `*output-buffer-limit*` characters (64K).
* **`read_char` raised an error at end of input.** It now reports a newline.
* Version 6 addresses the cursor in pixels rather than character cells, so a
  story could ask for a row number far past the end of a text screen. Captured
  upper window rows are bounded.

### Added

* **The Version 6 extended opcodes as stubs**: `draw_picture`, `picture_data`,
  `erase_picture`, `set_margins`, `set_true_colour`, `move_window`,
  `window_size`, `window_style`, `get_wind_prop`, `scroll_window`, `pop_stack`,
  `read_mouse`, `mouse_window`, `push_stack`, `put_wind_prop`, `print_form`,
  `make_menu`, `picture_table` and `buffer_screen`. There are no graphics and no
  real windows here, so what matters is that each one consumes exactly the store
  or branch byte the standard gives it - skipping those is what loses
  instruction alignment. The store and branch columns were taken from the opcode
  table in the Z-Machine Standards Document 1.1, section 14.

### Verified

| Version | Stories | Result |
|:--|--:|:--|
| 3 | 17 | All play |
| 4 | 3 | All play |
| 5 | 8 | All play |
| 6 | 2 | Play as text |
| 8 | 1 | Plays |

### Known limitations

* Version 6 graphics and pixel-positioned windows are not reproduced. Stories
  run and stay readable, but pictures are absent and the upper window is drawn
  as a status bar rather than laid out in pixels.
* Version 4 timed input (the extra operands of `read`) is still ignored.

## [0.5.0] - 2026-09-05

Version 5 stories now run. Verified against 34 story files: every Version 3
story and every Version 5 story in the test set plays.

### Fixed

* **Extended opcodes were dispatched to the wrong table, so no Version 5 story
  could run.** The `0xBE` prefix was decoded correctly but the instruction was
  then looked up among the VAR opcodes, so `EXT:9` (`save_undo`) executed as
  `VAR:9` (`pull`) and emptied the stack. `Advent.z5` died with "Stack
  underflow" on the first command. The extended table was never reached at all.

### Added

* **The extended opcodes for Version 5**: `save_table`, `restore_table`,
  `log_shift`, `art_shift`, `set_font`, `save_undo`, `restore_undo`,
  `print_unicode` and `check_unicode`. The Version 6 picture opcodes are
  deliberately left out.
* **The upper window** - `split_window`, `set_window`, `set_cursor`,
  `get_cursor` and `erase_window` were empty stubs, so a story's status line
  landed in the middle of the transcript. What the story writes to the upper
  window is now captured and drawn as a status bar when it switches back to the
  main window.
* **Interpreter capabilities are declared in the header.** The loader used to
  read the story without telling it anything about the interpreter, so stories
  could not lay out their status line or know which text styles were safe. Flags
  1 and 2, the screen size, the interpreter number and the standard revision are
  now written before execution starts, and again on restart.
  * `*screen-columns*` (80) and `*screen-rows*` (24) set the size reported
  * `*interpreter-number*` (6, IBM PC) sets the interpreter identity
* `set_text_style` now maps onto ANSI: reverse video, bold and italic.
* **An unimplemented opcode no longer ends the story.** It is reported once and
  skipped. Set `*strict-opcodes*` to `T` for the old behaviour, which is useful
  when tracking down a decoding problem.

### Known limitations

* Version 6 needs graphics and multiple windows; `advent.z6` and `zork0.z6` do
  not run.
* Version 8 does not run yet - `advent.z8` loses instruction alignment inside a
  routine. The cause has not been identified.
* Version 4 timed input (the extra operands of `read`) is ignored.

## [0.4.1] - 2026-09-05

### Added

* **The three kinds of output are now styled differently.** The original English
  is dimmed, the translation is bright white, and the status
  line is bright white on blue, so they can be told apart at a glance.
  * `*ansi-enabled*` defaults to `T`; set it to `:auto` to drop the escape
    sequences when output is not a terminal, or to `NIL` to turn colour off
  * `*ansi-source*`, `*ansi-translation*` and `*ansi-status*` take raw SGR
    parameters, so the scheme can be changed freely
  * Escape sequences go only to the terminal, never into the Z-machine output
    buffer, and the terminal is returned to normal at the prompt so that typed
    input is not styled
* **Status line** (V1-3). `show_status` used to be an empty stub, so the player
  could not see where they were or what their score was. The location, score and
  turn count are now printed on one line just before each `>` prompt.
  * The location is translated when bilingual mode is on, shown as
    `家の西側 (West of House)`
  * A story flagged as a time game shows `Time: hh:mm` instead of score and moves
  * There is no screen model in this interpreter, so the line scrolls with the
    text rather than being pinned to the top of the screen
  * `*status-line-enabled*` turns it off, `*status-line-width*` sets the column
    the right-hand side is aligned to (CJK characters count as two columns)

### Changed

* **Partial matching is now a last resort, and is off by default.** When no
  exact translation was cached, the interpreter used to answer with the
  translation of any cached English phrase that made up more than a third of the
  line - so `Forest` could answer for `Forest Path`. Worse, it ran *before* the
  translation API and did not cache its answer, so an affected line was never
  translated properly, no matter how many times it appeared.
  * The order is now: exact match, case-insensitive match, API, partial match
  * `*use-partial-matches*` defaults to `NIL`; set it to `T` to re-enable the
    fallback for playing without a translation backend
  * `*partial-match-threshold*` (default `0.7`) is the fraction of the line the
    cached phrase has to cover, replacing the hardcoded one third
  * Measured against the bundled Japanese cache, the old rule picked a wrong
    fragment for 12 of 179 entries; the new threshold reduces that to 4, and
    with the reordering none of them reach the screen when a backend is set up

### Removed

* Two bundled Japanese entries whose translation was a grammatical fragment,
  `"This gives you the rank of"` and `"Your score is"`. Both ended mid-sentence
  and could only ever produce truncated output. The full sentences are
  translated properly by the API and cached. Object and room names that are
  printed on their own, such as `brass lantern` and `West of House`, are of
  course kept.

## [0.4.0] - 2026-09-05

Local LLM translation through Ollama, and a glossary that keeps terminology
consistent across a whole playthrough.

### Added

* **Ollama backend** (`ollama.lisp`) - translate with any locally installed
  model, no API key required.
  * `(setup-ollama "qwen3.5:9b")` selects the model, optionally with a server
    URL: `(setup-ollama "gemma3:12b" "http://192.168.1.10:11434")`
  * `(list-ollama-models)` lists installed models, `(set-ollama-model ...)`
    switches, `(test-ollama)` checks the connection
  * `setup-ollama` warns when the server is unreachable or the model is not
    installed, and prints the `ollama pull` command needed
  * Tuning variables: `*ollama-url*`, `*ollama-model*`, `*ollama-temperature*`,
    `*ollama-num-predict*`, `*ollama-think*`
* **Glossary** (`glossary.lisp`) - fixed translations for proper nouns and game
  terms, so the same English term always produces the same wording.
  * Only the terms actually present in the source text are injected into the
    prompt, so prompt size stays bounded
  * When a translation drops a glossary term it is retried once with that term
    emphasized (`*glossary-enforce*`)
  * `(show-glossary)`, `(add-glossary ...)`, `(remove-glossary ...)`,
    `(save-glossary)`
  * `(glossary-check)` audits cached translations, `(glossary-fix)`
    re-translates the ones that break the glossary
  * Matching uses word boundaries for ASCII terms and plain substring matching
    for CJK
* **`glossaries/glossary-ja.lisp`** - 59 Zork I terms (locations, objects,
  characters), loaded automatically with the language.
* **Data file names** (`settings.lisp`) - the translation cache and the
  glossary can be named explicitly in the launch script instead of being derived
  from the language code, so separate games or playthroughs can keep separate
  caches.
  * `(set-translation-file "zork2-ja.lisp")` and `(set-glossary-file ...)`,
    called before `(set-language ...)`
  * The cache lives in `translations/` and the glossary in `glossaries/`, so a
    bare file name is enough; a name that already contains a directory is used
    as given
  * `NIL` restores the language-derived name
  * `(show-config)` reports what the settings resolve to
  * Directories in the name are created when the cache is first written
* **Explicit backend selection** - `*translation-backend*` is set to `:ollama`,
  `:deepl` or `:claude` by the corresponding setup function, and is reported by
  `(translation-stats)` and `(check-environment)`.
* **Shared HTTP layer** - `http-post-json` and `http-get`, using curl with a
  PowerShell fallback. Request and response bodies pass through UTF-8 temporary
  files, which removes both the command-line length limit and the encoding
  problems of passing JSON as an argument on Windows.
* **`*claude-model*`** - the Claude model is now configurable instead of being
  hardcoded.

### Changed

* All LLM backends now share one prompt builder (`build-translation-prompt`)
  carrying explicit translation rules and the glossary section, instead of the
  one-line prompt used before.
* `clean-llm-output` strips `<think>` blocks, leading labels such as
  `Translation:` and wrapping quotation marks from model replies.
* Claude backend: default model moved from `claude-3-haiku-20240307` to
  `claude-3-5-haiku-latest`, and the request body is written to a file rather
  than passed as a `-d` command-line argument.
* **There is now exactly one translation cache, and one glossary.** Both used to
  be read from two files - a bundled one under `translations/` or `glossaries/`,
  plus one in the working directory - while only the second was ever written.
  The first stage is gone for both. The configured file, by default
  `translations/translations-<language>.lisp` and
  `glossaries/glossary-<language>.lisp`, is the only one read and the only one
  written. A missing file is now reported at startup instead of silently
  loading nothing.
* The translation cache is written sorted by source text, so saving the same
  content twice produces the same file. The glossary already did this.
* DeepL now goes through the shared HTTP layer and sends a JSON body, instead
  of its own curl and PowerShell code paths with a form-encoded body. Newlines
  and quotation marks in the source text survive the round trip.
* `*deepl-url*` selects the endpoint - keep the default for a free key, set it
  to `https://api.deepl.com/v2/translate` for a Pro key.
* `(auto-translate-all)` no longer waits between requests on the Ollama
  backend, since there is no rate limit to respect.
* `(set-language ...)` also loads the glossary for that language.
* `(translation-stats)` and `(check-environment)` report the active backend,
  the Ollama model and URL, and the glossary size.

### Fixed

* **DeepL translation never worked.** The key was sent as an `auth_key` request
  parameter, which DeepL no longer accepts: every request came back
  `403 Missing Authorization header, expected 'Authorization: DeepL-Auth-Key
  <API key>'`. Rejected requests are not billed, which is why the DeepL usage
  page kept showing zero characters and the translation cache was never written.
  The key now goes in the `Authorization` header.
  ([#1](https://github.com/fukuyori/zmachine-multilingual/issues/1))
* DeepL failures were completely silent. A rejected request is not a Lisp error,
  so it was swallowed and the line simply stayed untranslated. The `message`
  field of an error response is now printed, and `(test-deepl-api)` reports why
  a request failed.
* Claude responses were truncated at the first quotation mark. The JSON text
  field was extracted with a naive search for the next `"`, which stopped at the
  `\"` of any quoted phrase inside the translation. JSON strings are now parsed
  with escape handling.
* `json-escape` did not escape carriage returns, tabs or other control
  characters, so text containing them produced invalid JSON and the request
  failed. All control characters are now escaped.

## [0.3.3] - 2025-12-13

Earlier releases predate this changelog; see the commit history for details.

## [0.3.2] - 2025-12-13
## [0.3.1] - 2025-12-11
## [0.3] - 2025-12-11
## [0.2.3] - 2025-12-11
## [0.2.2] - 2025-12-11
## [0.2.1] - 2025-12-11
## [0.2] - 2025-12-11
## [0.1] - 2025-12-11
