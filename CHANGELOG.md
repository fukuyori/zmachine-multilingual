# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
