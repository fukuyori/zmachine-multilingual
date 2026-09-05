# バージョン更新チェックリスト

バージョンを上げるときに更新するファイルの一覧。

| # | ファイル | 箇所 | 備考 |
|:--|:--|:--|:--|
| 1 | `zmachine.asd` | `:version "X.Y.Z"` | 機械可読なバージョン。ここが正 |
| 2 | `CHANGELOG.md` | 先頭に `## [X.Y.Z] - YYYY-MM-DD` を追加 | Added / Changed / Fixed / Removed |
| 3 | `CHANGELOG-jp.md` | 同上（追加 / 変更 / 修正 / 削除） | 英語版と内容を一致させる |
| 4 | `README.md` | 「Changelog」節の `Current version:` | |
| 5 | `README-jp.md` | 「変更履歴」節の「現在のバージョンは」 | |
| 6 | コミットメッセージ | `X.Y.Z` | 既存の慣習。過去のリリースはすべてこの形式 |

## 手順

1. 上の 1〜5 を更新する
2. クリーンビルドで警告が増えていないことを確認する

   ```
   sbcl --non-interactive \
        --eval '(require :asdf)' \
        --eval '(push *default-pathname-defaults* asdf:*central-registry*)' \
        --eval '(asdf:load-system :zmachine)'
   ```

   既知の style-warning は 3 件（`execute.lisp` の `FORM` 未使用 × 2、
   `languages.lisp` の `LANGUAGE-NAME` インライン化不可 × 1）。full WARNING は 0 件。
3. 動作確認

   ```lisp
   (set-language :ja)      ; 用語集と翻訳が読み込まれること
   (check-environment)     ; バックエンドの状態
   (translation-stats)     ; 件数とバックエンド
   ```
4. コミットする（バージョン番号のみをメッセージにする）

## 注意

* バージョン番号を持つファイルは `zmachine.asd` だけ。Lisp のソース中に
  ハードコードされたバージョン文字列はない（増やさないこと）。
* タグは打っていない。リリースはコミットメッセージのバージョン番号で識別している。
* `run.lisp` は個人用の起動スクリプトで、API キーが直書きされている。
  `.gitignore` で除外済み。追跡対象に加えないこと。
