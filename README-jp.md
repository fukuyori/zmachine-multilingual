# Z-machine Interpreter

Common Lispで実装されたZ-machineインタプリタです。Zorkなどのテキストアドベンチャーゲームを10言語で楽しめます。

## 対応言語

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

## 機能

- Z-machine version 1-5 対応
- 10言語バイリンガル表示
- DeepL/Claude APIによる自動翻訳
- 翻訳の自動キャッシュ・永続化
- ゲームのセーブ/リストア

## 必要環境

- SBCL (Steel Bank Common Lisp)
- curl (自動翻訳に使用)

## インストール

```bash
unzip zmachine.zip
cd zmachine
```

## 使い方

### 基本

```bash
sbcl --load run-zork.lisp
```

または手動で:

```lisp
(require :asdf)
(push #p"/path/to/zmachine/" asdf:*central-registry*)
(asdf:load-system :zmachine)
(in-package :zmachine)

;; 言語を選択
(list-languages)      ; 利用可能な言語を表示
(set-language :ja)    ; 日本語を選択

;; ゲーム開始
(load-story "zork1.z3")
(run)
```

### 言語の変更

```lisp
;; ゲーム中でも変更可能
(set-language :fr)    ; フランス語に変更
(set-language :en)    ; 英語のみ（翻訳なし）
```

### 自動翻訳の設定

```lisp
;; DeepL API (無料枠あり、推奨)
(setup-deepl "your-api-key")

;; または Claude API
(setup-claude-api "your-api-key")
```

DeepL APIキーは https://www.deepl.com/pro-api で無料取得できます。

### 翻訳管理

```lisp
;; 未翻訳テキストを表示
(show-untranslated)

;; 手動で翻訳を追加
(quick-translate 1 "翻訳テキスト")

;; 未翻訳をすべて自動翻訳
(auto-translate-all)

;; 統計を表示
(translation-stats)

;; 翻訳を保存
(save-language-translations)
```

### セーブ/リストア

ゲーム内で:
```
>save
Save filename: mygame
Game saved.

>restore  
Save filename: mygame
Game restored.
```

## ファイル構成

```
zmachine/
├── packages.lisp           # パッケージ定義
├── memory.lisp             # メモリ管理、セーブ/リストア
├── text.lisp               # テキスト出力
├── objects.lisp            # オブジェクトツリー
├── dictionary.lisp         # 辞書処理
├── decode.lisp             # 命令デコード
├── opcodes.lisp            # 0OP/1OP/2OP命令
├── opcodes-var.lisp        # VAR命令
├── execute.lisp            # 実行ループ
├── translate.lisp          # 翻訳システム
├── languages.lisp          # 言語定義
├── run-zork.lisp           # 起動スクリプト
├── zmachine.asd            # ASDFシステム定義
└── translations/           # 翻訳データ
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

## 翻訳データ

- 各言語の翻訳は `translations/translations-XX.lisp` に保存
- ユーザーの追加翻訳は作業ディレクトリの `translations-XX.lisp` に保存
- 次回起動時に自動的に読み込まれます

## 翻訳への貢献

翻訳ファイルを編集して、プルリクエストをお送りください。

## ライセンス

MIT License
