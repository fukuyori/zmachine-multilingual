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

* Z-machine バージョン 1-8 対応（34 本のストーリーファイルで検証済み）
* バイリンガル表示（英語 + 翻訳）
* 現在地・スコア・手数を表示するステータス行（V1-3）
* 原文・訳文・ステータス行を色分けして区別
* Ollama（ローカル LLM・モデル指定可）/ DeepL / Claude API による自動翻訳
* 用語集（glossary）による訳語の統一
* 翻訳のキャッシュと永続化
* ゲーム状態のセーブ/リストア

## 動作要件

* SBCL (Steel Bank Common Lisp)
* curl（自動翻訳用）
* Ollama（ローカル LLM で翻訳する場合。API キー不要）

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
* `.z4` - Z-machine バージョン 4（大型化した Infocom ゲーム）
* `.z5` - Z-machine バージョン 5（後期の Infocom ゲーム）
* `.z6` - Z-machine バージョン 6（グラフィック作品。本実装ではテキストのみ）
* `.z8` - Z-machine バージョン 8（拡張フォーマット）

34 本のストーリーファイルで検証しています。

| バージョン | 本数 | 状況 |
|:--|--:|:--|
| 3（`.z3`） | 17 | 全て動作 |
| 4（`.z4`） | 3 | 全て動作。タイマー付き入力は無視されます |
| 5（`.z5`） | 8 | 全て動作 |
| 6（`.z6`） | 2 | テキストとして動作。グラフィックは非対応で、上部ウィンドウはピクセル配置ではなくステータスバーとして描画されます |
| 8（`.z8`） | 1 | 動作 |

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
;; Ollama（ローカル LLM・API キー不要）
(list-ollama-models)          ; インストール済みモデルを一覧表示
(setup-ollama "qwen3.5:9b")   ; 翻訳に使うモデルを指定

;; DeepL API（無料枠あり）
(setup-deepl "your-api-key")

;; または Claude API
(setup-claude-api "your-api-key")
```

無料の DeepL API キーは https://www.deepl.com/pro-api で取得できます。

`*deepl-url*` でエンドポイントを選べます。無料キーは既定のまま、Pro キーの場合は
`https://api.deepl.com/v2/translate` を指定してください。`(test-deepl-api)` は生の
レスポンスと、拒否された場合は DeepL が返す理由を表示します。

- コマンドラインから起動

```bash
sbcl --script run-zork.lisp
```

### キャッシュファイル名の指定

翻訳キャッシュは**1 つだけ**で、`translations/` の中に置かれます。起動時に読み込み、
プレイ中に同じファイルへ書き戻します。既定はリポジトリに同梱されている
`translations/translations-<言語>.lisp` です。`run-zork.lisp` で別名を指定すれば、
ゲームごと・プレイスルーごとにキャッシュを分けられます。ファイル名だけを書けば
`translations/` の中に置かれます。

```lisp
;; run-zork.lisp - (set-language ...) より前に書くこと
(set-translation-file "zork2-ja.lisp")   ; -> translations/zork2-ja.lisp
(set-glossary-file "zork2-ja-glossary.lisp")

(set-language :ja)
```

```lisp
(show-config)                        ; 設定がどのファイルに解決されるか
(set-translation-file nil)           ; 言語別の名前に戻す
```

| 変数 | 既定値 | 説明 |
|:--|:--|:--|
| `*translation-file*` | `NIL` | キャッシュのファイル名。`NIL` なら `translations-<言語>.lisp` |
| `*translations-dir*` | `"translations/"` | キャッシュを置くディレクトリ |
| `*glossary-file*` | `NIL` | 用語集のファイル名。`NIL` なら `glossary-<言語>.lisp` |
| `*glossaries-dir*` | `"glossaries/"` | 用語集を置くディレクトリ |

用語集もまったく同じ扱いです。`glossaries/` の中の 1 ファイルを起動時に読み込み、
`(save-glossary)` が同じファイルへ書き戻します。

`"saves/mine-ja.lisp"` のようにディレクトリを含む名前を指定した場合は、
`translations/` や `glossaries/` の下には置かず、その指定をそのまま使います。

ファイルが存在しない場合は起動時にその旨を表示し、最初の保存時に作成します。
名前にディレクトリが含まれていれば、そのディレクトリも作成します。

### Ollama によるローカル翻訳

Ollama が動いていれば、API キーなしで任意のモデルを翻訳に使えます。

```lisp
(list-ollama-models)                 ; 利用可能なモデル一覧
(setup-ollama "gemma3:12b")          ; モデルを指定して有効化
(setup-ollama "qwen3.5:9b" "http://192.168.1.10:11434")  ; リモートの Ollama
(set-ollama-model "translategemma:12b")  ; モデルだけ切り替え
(test-ollama)                        ; 接続と翻訳のテスト
```

調整用の変数：

| 変数 | 既定値 | 説明 |
|:--|:--|:--|
| `*ollama-url*` | `"http://localhost:11434"` | Ollama サーバの URL |
| `*ollama-model*` | `"gemma3:4b"` | 使用モデル |
| `*ollama-temperature*` | `0.2` | 低いほど訳語がぶれない |
| `*ollama-num-predict*` | `1024` | 生成する最大トークン数 |
| `*ollama-think*` | `nil` | 推論モデルの thinking を許可（遅くなる） |

小さいモデル（4B 程度）は語順や助詞が崩れることがあります。9B〜12B 以上を推奨します。

### 用語集（glossary）による訳語の統一

`glossaries/glossary-<言語コード>.lisp` に登録した用語は、原文に現れたときだけ
プロンプトに差し込まれ、LLM に同じ訳語を使わせます（Ollama / Claude）。
言語を選ぶと自動で読み込まれます。

```lisp
;; glossaries/glossary-ja.lisp
(add-glossary "brass lantern" "真鍮のランタン")
(add-glossary "grue" "グルー")
(add-glossary "trap door" "仕掛け扉")
```

```lisp
(show-glossary)                    ; 登録済みの用語を表示
(add-glossary "thief" "泥棒")      ; 実行中に追加
(remove-glossary "thief")          ; 削除
(save-glossary)                    ; カレントディレクトリの glossary-ja.lisp に保存

(glossary-check)                   ; 用語集に反する既存訳を検出
(glossary-fix)                     ; 検出された訳を API で訳し直す
```

翻訳結果から用語集の訳語が抜け落ちた場合は、その語を強調して 1 回だけ再翻訳します
（`*glossary-enforce*` が `t` のとき。無効にするには `nil` を設定）。

`score` や `moves` のような一般的な語は、助詞や助数詞を伴って訳されるため
誤検出の原因になります。固有名詞やアイテム名を中心に登録してください。

### 原文・訳文・ステータスの区別

3 種類の出力を一目で見分けられるよう、表示を変えています。

| | 表示 |
|:--|:--|
| 原文（英語） | 減光。訳文が主、原文が従になります |
| 訳文 | 明るい白 |
| ステータス行 | 青地に明るい白。ステータスバーのような見た目になります |

```lisp
(setf *ansi-enabled* nil)      ; エスケープシーケンスを一切出さない
(setf *ansi-source* "90")      ; 減光ではなく原文をグレーにする
(setf *ansi-translation* "0")  ; 訳文を端末の既定のままにする
(setf *ansi-status* "7")       ; ステータス行を反転表示にする
```

| 変数 | 既定値 | 説明 |
|:--|:--|:--|
| `*ansi-enabled*` | `T` | `T` で常に色付け、`NIL` で無効、`:auto` は端末以外への出力時に自動で無効化 |
| `*ansi-source*` | `"2"` | 原文の SGR パラメータ |
| `*ansi-translation*` | `"97"` | 訳文の SGR パラメータ |
| `*ansi-status*` | `"44;97"` | ステータス行の SGR パラメータ |

エスケープシーケンスは端末にのみ書き出し、Z-machine の出力バッファには入れません。
リダイレクトした出力にも色を入れたくない場合は `*ansi-enabled*` を `:auto` に、
色付けを完全にやめる場合は `NIL` にしてください。

訳文に太字（`"1"`）は避けてください。端末は太字を太字ウェイトのフォントで描画しますが、
日本語フォントの多くは bold を持たないため、英字だけが太字になります。明るさや色の
指定であれば、どの文字種にも均等に効きます。

### ステータス行

バージョン 1-3 では、ステータス行の描画はインタプリタの責任です。`>` プロンプトの
直前に 1 行で出力し、左に現在地、右にスコアを表示します。

```
West of House                                             Score: 0  Moves: 0
>
```

バイリンガルモードでは現在地を翻訳します。

```
家の西側 (West of House)                                  Score: 0  Moves: 1
>
```

time game のストーリーでは、代わりに `Time: hh:mm` を表示します。

| 変数 | 既定値 | 説明 |
|:--|:--|:--|
| `*status-line-enabled*` | `T` | `NIL` にするとステータス行を出さない |
| `*status-line-width*` | `76` | 右側を揃える桁位置 |

本インタプリタには画面モデルが無いため、画面最上部に固定するのではなく、本文と
一緒にスクロールする 1 行として出力します。桁揃えの計算では CJK 文字を 2 桁として
数えます。

### インタプリタの設定

バージョン 4 以降の作品は、インタプリタの能力と画面サイズをヘッダから読み取り、
それに合わせてステータス行のレイアウトを決めます。

| 変数 | 既定値 | 説明 |
|:--|:--|:--|
| `*screen-columns*` | `80` | 作品に伝える画面幅 |
| `*screen-rows*` | `24` | 作品に伝える画面高 |
| `*interpreter-number*` | `6` | インタプリタの種別（6 = IBM PC）。これを見て文字セットを選ぶ作品があります |
| `*strict-opcodes*` | `NIL` | `T` にすると、未実装命令を読み飛ばさずに停止します |
| `*output-buffer-limit*` | `65536` | Z-machine の出力バッファを破棄するまでの文字数 |

未実装の命令は一度だけ報告して読み飛ばします。読み飛ばしは推測を含む処理で、
オペランドは消費済みでも store バイトや分岐バイトが残っている可能性があります。
作品の不具合を追う際は `*strict-opcodes*` を `T` にしてください。

バージョン 6 はグラフィックとピクセル単位のウィンドウ配置を前提としますが、本実装には
どちらもありません。作品は動作し可読ですが、画像は表示されず、上部ウィンドウは
ステータスバーになります。

バージョン 4・5 の作品はステータス行を上部ウィンドウに描きます。本インタプリタには
画面モデルが無いため、上部ウィンドウへの書き込みを捕捉し、メインウィンドウに戻った
時点でステータスバーとして出力します（画面最上部への固定表示ではありません）。

### 1 行が翻訳されるまで

ストーリーが出力する各行は、次の順で解決されます。

1. **完全一致** — 翻訳キャッシュ
2. **大文字小文字を無視した一致**
3. **翻訳 API** — 結果はその場でキャッシュに追加・保存されます
4. **部分一致** — 既定では無効（下記）

```lisp
(setf *use-partial-matches* t)       ; 代替動作を有効にする
(setf *partial-match-threshold* 0.7) ; 行全体の何割を占める必要があるか
```

部分一致は、行の中に含まれるキャッシュ済みの英語表現の訳を返します。返るのは断片なので
訳は必ず不完全になり、キャッシュにも保存されません。翻訳バックエンドを使わずに遊ぶ
場合にのみ有効化する価値があります。

### 翻訳管理

```lisp
;; 未翻訳テキストを表示
(show-untranslated)

;; 手動で翻訳を追加
(quick-translate 1 "翻訳テキスト")

;; 未翻訳をすべて自動翻訳
(auto-translate-all)

;; 統計を表示（バックエンド・モデル・用語集の状態も表示）
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
├── settings.lisp           # 設定ファイル対応
├── glossary.lisp           # 用語集（訳語の統一）
├── translate.lisp          # 翻訳システム
├── ollama.lisp             # Ollama バックエンド
├── languages.lisp          # 言語定義
├── run-zork.lisp           # 起動スクリプト
├── zmachine.asd            # ASDF システム定義
├── glossaries/             # 用語集データ
│   └── glossary-ja.lisp
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

* **memory.lisp**：Z-machine の心臓部。ストーリーファイルをバイト配列として読み込み、インタプリタの能力をヘッダに宣言し、メモリアクセス関数を提供する。グローバル変数、ローカル変数、スタックの管理もここで行う。セーブ/リストア機能も実装されている。

* **text.lisp**：ZSCII エンコーディング、ステータス行、上部ウィンドウを扱う。5ビット文字のデコード、アルファベットシフト、略語展開を実装。翻訳システムとの接続点でもある。

* **objects.lisp**：Z-machine のオブジェクトシステムを実装。オブジェクトはツリー構造を持ち、親子関係で包含関係を表現する。属性（32個のフラグ）とプロパティ（可変長データ）へのアクセスを提供。

* **dictionary.lisp**：プレイヤー入力の解析を担当。入力文字列をトークンに分割し、各トークンを辞書で検索する。見つかったアドレスをパースバッファに格納し、ゲームロジックに渡す。

* **decode.lisp**：バイトコードのデコーダ。オペコードの形式（0OP/1OP/2OP/VAR/EXT）を判定し、オペランドを読み取り、命令構造体を生成する。

* **opcodes.lisp** と **opcodes-var.lisp**：約100個のオペコードの実装。算術演算、比較、分岐、オブジェクト操作、入出力など、Z-machine のすべての機能がここに含まれる。

* **execute.lisp**：メイン実行ループ。命令をデコードし、対応するオペコード関数を呼び出し、これを繰り返す。ルーチン呼び出しとリターンの管理も行う。

* **settings.lisp**：データファイルの設定。どの翻訳キャッシュと用語集を読み書きするかを解決する。

* **glossary.lisp**：用語集の管理。原文に含まれる用語の抽出、プロンプトへの差し込み、訳語が守られているかの検査と再翻訳を実装。

* **translate.lisp**：多言語対応の中核。翻訳キャッシュの管理、翻訳プロンプトの生成、Ollama/DeepL/Claude API との連携、翻訳データの保存/読み込みを実装。

* **ollama.lisp**：ローカル LLM（Ollama）バックエンド。モデルの選択、一覧表示、生成リクエストを担当。

* **languages.lisp**：対応言語の定義。言語コード、英語名、ネイティブ名のテーブルを持つ。

## 翻訳データについて

* 翻訳キャッシュは `translations/translations-XX.lisp`、または `set-translation-file` で指定した名前。起動時に読み込むファイルと、プレイ中に書き戻すファイルは同じ
* 保存時は原文でソートして書き出すので、同じ内容なら毎回同じファイルになる
* 用語集は `glossaries/glossary-XX.lisp`、または `set-glossary-file` で指定した名前。起動時に読み込み、`(save-glossary)` が同じファイルへ書き戻す
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

## 変更履歴

現在のバージョンは **0.5.1** です。リリース履歴は [CHANGELOG-jp.md](CHANGELOG-jp.md) を参照してください。

## コントリビューション

翻訳ファイルを編集してプルリクエストを送ってください。

## ライセンス

MIT License
