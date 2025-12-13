# Z-machine インタプリタ

Common Lisp で書かれた Z-machine インタプリタです。Zork などの古典テキストアドベンチャーを10言語で楽しめます。

## 対応言語

| コード | 言語 | 表記 |
| --- | --- | --- |
| :en | 英語 | English |
| :ja | 日本語 | 日本語 |
| :ko | 韓国語 | 한국어 |
| :zh-hans | 簡体中国語 | 简体中文 |
| :zh-hant | 繁体中国語 | 繁體中文 |
| :fr | フランス語 | Français |
| :de | ドイツ語 | Deutsch |
| :es | スペイン語 | Español |
| :pt | ポルトガル語 | Português |
| :ru | ロシア語 | Русский |

## 機能

* Z-machine バージョン 1-5 対応
* バイリンガル表示（英語 + 翻訳）
* DeepL/Claude API による自動翻訳
* 翻訳のキャッシュと永続化
* ゲーム状態のセーブ/リストア

## 動作要件

* SBCL (Steel Bank Common Lisp)
* curl（自動翻訳用）

## ストーリーファイル（.z3, .z5）の入手方法

Z-machine でゲームをプレイするには、ストーリーファイルが必要です。以下の方法で入手できます。

### ZORK I, II, III（オープンソース）

2025年11月、Microsoft が ZORK I, II, III を MIT ライセンスでオープンソース化しました。ストーリーファイル（拡張子 .z3）は以下のサイトの COMPILED ディレクトリから入手できます。

* [Zork 1 - historicalsource](https://github.com/historicalsource/zork1)
* [Zork 2 - historicalsource](https://github.com/historicalsource/zork2)
* [Zork 3 - historicalsource](https://github.com/historicalsource/zork3)

### Interactive Fiction Archive

[IF Archive](https://ifarchive.org/) には多くの Z-machine 対応ゲームが公開されています：

### ファイルの命名規則

* `.z3` - Z-machine バージョン 3（ZORK を含む Infocom の代表作）
* `.z5` - Z-machine バージョン 5（後期の Infocom ゲーム）
* `.z8` - Z-machine バージョン 8（拡張フォーマット）

現在、`.z3` での動作確認をしています。

ストーリーファイルはインタプリタと同じディレクトリに配置するか、読み込み時にフルパスを指定してください。

## インストール

```bash
git clone https://github.com/fukuyori/zmachine-multilingual.git
cd zmachine-multilingual
```

## 使い方

### クイックスタート

run-zork.lisp の以下の箇所を修正し、SBCL で実行します。

- ストーリーファイルの設定

```lisp
;; プレイするストーリーファイルのパスを指定
(load-story "zork1.z3")           ; 同じディレクトリにある場合
(load-story "/path/to/zork1.z3")  ; フルパスで指定する場合
```

- 言語の変更

```lisp
;; ゲームプレイ中でも変更可能
(set-language :fr)    ; フランス語に切り替え
(set-language :en)    ; 英語のみ（翻訳なし）
```

- 自動翻訳の設定

```lisp
;; DeepL API（無料枠あり、推奨）
(setup-deepl "your-api-key")

;; または Claude API
(setup-claude-api "your-api-key")
```

無料の DeepL API キーは https://www.deepl.com/pro-api で取得できます。

- コマンドラインから起動

```bash
sbcl --script run-zork.lisp
```

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

ゲーム内で：

```
>save
Save filename: mygame
Game saved.

>restore  
Save filename: mygame
Game restored.
```

---

## 技術的背景

### なぜ Lisp で実装するのか

Z-machine インタプリタを Common Lisp で実装することには、特別な意味があります。

ZORK 自体が MDL（LISP の方言）で書かれていました。ZIL も LISP の影響を強く受けています。Lisp で Z-machine インタプリタを書くことは、ある意味で「原点回帰」です。

技術的にも、Lisp は Z-machine の実装に適しています：

## 多言語対応 Z-machine インタプリタ

### 古典を世界へ

ZORK は英語でしかプレイできませんでした。1980年代の技術的制約、そして Infocom の市場が北米中心だったことから、他言語への翻訳は行われませんでした。

しかし40年以上経った今、テキストアドベンチャーの古典を世界中の人々に楽しんでもらいたいと考えるのは自然なことです。日本語で「グルーに食べられる」経験をしてみたい。フランス語で地下帝国を探検したい。

`zmachine-multilingual` プロジェクトは、この願いを実現するために作られました。Z-machine インタプリタに翻訳システムを統合し、10言語でゲームをプレイできるようにします。

### 翻訳システムのアーキテクチャ

翻訳システムは、Z-machine のテキスト出力をインターセプトします。テキストが出力されるたびに：

1. キャッシュを確認
2. キャッシュにあれば、その翻訳を使用
3. なければ、翻訳 API を呼び出し
4. 結果をキャッシュに保存
5. オリジナルと翻訳を両方表示（バイリンガルモード）

### 翻訳キャッシュの重要性

テキストアドベンチャーでは、同じテキストが何度も表示されます。部屋の説明、エラーメッセージ、システムメッセージ...

毎回翻訳 API を呼び出すのは非効率的ですし、コストもかかります。翻訳キャッシュにより、一度翻訳したテキストは再利用されます。

キャッシュはファイルに永続化されます。一度ゲームを最後までプレイすれば、すべてのテキストが翻訳され、次回からは API 呼び出しなしでプレイできます。

### バイリンガル表示

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

オリジナルの英語テキストと翻訳を同時に表示することで、原文のニュアンスを確認しながらプレイできます。また、翻訳の品質を確認し、必要に応じて修正することも容易になります。

---

## プロジェクト構成

```
zmachine-multilingual/
├── packages.lisp           # パッケージ定義
├── memory.lisp             # メモリ管理、セーブ/ロード
├── text.lisp               # テキスト処理（ZSCII デコード）
├── objects.lisp            # オブジェクトツリー操作
├── dictionary.lisp         # 辞書処理（入力解析）
├── decode.lisp             # 命令デコード
├── opcodes.lisp            # 基本オペコード (0OP/1OP/2OP)
├── opcodes-var.lisp        # 可変オペコード (VAR)
├── execute.lisp            # 実行ループ
├── translate.lisp          # 翻訳システム
├── languages.lisp          # 言語定義
├── run-zork.lisp           # 起動スクリプト
├── zmachine.asd            # ASDF システム定義
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

### 各モジュールの役割

* **packages.lisp**：Common Lisp のパッケージを定義する。外部に公開する関数（`load-story`, `run`, `set-language` など）を宣言している。

* **memory.lisp**：Z-machine の心臓部。ストーリーファイルをバイト配列として読み込み、メモリアクセス関数を提供する。グローバル変数、ローカル変数、スタックの管理もここで行う。セーブ/リストア機能も実装されている。

* **text.lisp**：ZSCII エンコーディングを扱う。5ビット文字のデコード、アルファベットシフト、略語展開を実装。翻訳システムとの接続点でもある。

* **objects.lisp**：Z-machine のオブジェクトシステムを実装。オブジェクトはツリー構造を持ち、親子関係で包含関係を表現する。属性（32個のフラグ）とプロパティ（可変長データ）へのアクセスを提供。

* **dictionary.lisp**：プレイヤー入力の解析を担当。入力文字列をトークンに分割し、各トークンを辞書で検索する。見つかったアドレスをパースバッファに格納し、ゲームロジックに渡す。

* **decode.lisp**：バイトコードのデコーダ。オペコードの形式（0OP/1OP/2OP/VAR）を判定し、オペランドを読み取り、命令構造体を生成する。

* **opcodes.lisp** と **opcodes-var.lisp**：約100個のオペコードの実装。算術演算、比較、分岐、オブジェクト操作、入出力など、Z-machine のすべての機能がここに含まれる。

* **execute.lisp**：メイン実行ループ。命令をデコードし、対応するオペコード関数を呼び出し、これを繰り返す。ルーチン呼び出しとリターンの管理も行う。

* **translate.lisp**：多言語対応の中核。翻訳キャッシュの管理、DeepL/Claude API との連携、翻訳データの保存/読み込みを実装。

* **languages.lisp**：対応言語の定義。言語コード、英語名、ネイティブ名のテーブルを持つ。

## 翻訳データについて

* 組み込み翻訳は `translations/translations-XX.lisp` に格納
* ユーザー翻訳は作業ディレクトリの `translations-XX.lisp` に保存される
* 次回起動時に自動的に読み込まれる

## 参考資料

### 公式資料・仕様書

* [The Z-Machine Standards Document](https://inform-fiction.org/zmachine/standards/)
* [Interactive Fiction Archive](https://ifarchive.org/)

### 歴史・背景

* [The Digital Antiquarian - ZIL and the Z-Machine](https://www.filfre.net/2012/01/zil-and-the-z-machine/)
* [MIT Technology Review - The Enduring Legacy of Zork](https://www.technologyreview.com/2017/08/22/149560/the-enduring-legacy-of-zork/)

### ソースコード

* [Infocom Source Code (GitHub)](https://github.com/historicalsource)
* [zmachine-multilingual](https://github.com/fukuyori/zmachine-multilingual)

### ツール

* [Frotz - Z-Machine Interpreter](https://davidgriffith.gitlab.io/frotz/)
* [Inform 7](http://inform7.com/)

## コントリビューション

翻訳ファイルを編集してプルリクエストを送ってください。

## ライセンス

MIT License
