# ttt

ttt は Emacs 上で動作するモードレス日本語入力です。

入力モードの切り替えなしに、日本語と半角英数字の混在したテキストを入力できます。

日本語の入力には、漢字直接入力の TT-code (T-code の拡張) を利用しています。

## 動作環境

* Emacs 28.2 でテストしています。

## インストール

ttt.el を `load-path` の通ったところに置いて init.el に次のように書きます。

```emacs-lisp
(require 'ttt)
(define-key global-map (kbd "M-j") 'ttt-do-ttt)
(define-key isearch-mode-map (kbd "M-j") 'ttt-isearch-do-ttt)
(define-key isearch-mode-map (kbd "M-t") 'ttt-isearch-toggle-ttt)
```

## 使用法

### 入力

TT-code をタイプして <kbd>M-j</kbd> (`ttt-do-ttt`) を入力すると、日本語に変換されます。

* 入力: `kryglp/tlj` <kbd>M-j</kbd>
* 結果: `日本語入力`

半角英数字と日本語の間には、スペースを入れてください。デフォルトでは、スペースは変換後も残ります。

* 入力: `Emacs 0rwj` <kbd>M-j</kbd>
* 結果: `Emacs 拡張`

スペースを入れたくないところは、コロン `:` で区切ってください。区切りは変換後に削除されます。

* 入力: `,g` <kbd>M-j</kbd> `ttt:jv` <kbd>M-j</kbd>
* 結果: `「ttt」`

あるいは、`(setq ttt-remove-space t)` と設定すると、変換後にスペースを残さないスタイルになります。

* 入力: `default jg` <kbd>M-j</kbd> `nil hg,fhf` <kbd>M-j</kbd>
* 結果: `defaultはnilです。`

## 補助変換

補助変換を利用するには、辞書が必要です。
[kanchoku/tc](https://github.com/kanchoku/tc) の tcode ディレクトリから以下のファイルを入手して、 `user-emacs-directory` (init.el のあるディレクトリ) に置いてください。

* 部首合成変換に必要: bushu.rev, symbol.rev
* 交ぜ書き変換に必要: pd_kihon.yom, jukujiku.maz, greece.maz
* 異体字変換に必要: itaiji.maz

mazegaki.dic は必要ありません。

### 部首合成変換 (前置型)

`jfjf` に続く 2 文字が合成されます。

* 例: `jfjf` `pw` `ha` <kbd>M-j</kbd> → `森`

再帰的な変換も可能です。

* 例: `jfjf` `pg` `jfjf` `pw` `pw` <kbd>M-j</kbd> → `淋`

変換できない組み合わせの場合は、 `jfjf` は `◆` となって残ります。

* 例: `jfjf` `l4` `z/` <kbd>M-j</kbd> → `◆漢字`

### 交ぜ書き変換 (前置型)

`fjfj` に続く読み (漢字を含んでいてもよい) が変換されます。

* 例: `fjfj` `ml1fhr` <kbd>M-j</kbd> → `完璧`

変換候補が複数ある場合は、ミニバッファに候補が表示されます。
<kbd>C-s</kbd> / <kbd>C-r</kbd> で選択し <kbd>RET</kbd> で確定すると入力できます。

* 例: `fjfj` `jendz/` <kbd>M-j</kbd> → `{換字 | 漢字}` <kbd>C-s</kbd> <kbd>RET</kbd> → `漢字`

変換は単語変換です。
活用する語は、活用語尾を除いた読み (語幹) で変換してください。
変換できない読みの場合は、 `fjfj` は `◇` となって残ります。

* 例: `fjfj` `kd.uhdks` <kbd>M-j</kbd> → `◇のぞいた`
* 例: `fjfj` `kd.u` <kbd>M-j</kbd> → `{除 | 覗 | 望 | 臨}`

異体字変換を含んでいます。

* 例: `fjfj` `eg` <kbd>M-j</kbd> → `廣`

## Tips

### `ttt-do-ttt` の連続実行

<kbd>M-j</kbd> は、続けて実行することができます。

* 入力: `ojiy(feiy) # yr,x,dle` <kbd>M-j</kbd> <kbd>M-j</kbd> <kbd>M-j</kbd>
* 結果: `関数(引数) # コメント`

### Isearch

<kbd>M-j</kbd> は isearch でも利用できます。

たとえば、`Emacs 拡張` を検索するには、isearch に入って `Emacs 0rwj` とタイプします。このとき、おそらく `0rwj` が見つからないとエラーになりますが、かまわず <kbd>M-j</kbd> と入力してください。すると isearch 内で変換が行われ、`Emacs 拡張` が検索されます。

### Migemo 風の isearch

Emacs に [migemo.el](https://github.com/emacs-jp/migemo) がインストール・設定されていれば、<kbd>M-j</kbd> を入力することなく、migemo 風の isearch を行えます。

Isearch モードで <kbd>M-t</kbd> (`ttt-isearch-toggle-ttt`) により、通常の migemo isearch と migemo ttt isearch をトグルできます。

検索文字列を入力する際は、日本語と半角英数字の間にスペースを入れてください。たとえば `emacs 0rwj` と入力すれば、`Emacs拡張` や `Emacs 拡張` がヒットします。

### 1 文字検索

init.el に、

``` emacs-lisp
(define-key global-map (kbd "C-.") 'ttt-jump-to-char-forward)
(define-key global-map (kbd "C-,") 'ttt-jump-to-char-backward)
```

と書いておくと、Vim の <kbd>f</kbd> / <kbd>F</kbd> コマンドに似た、カーソルの移動が使えるようになります。

たとえば、<kbd>C-.</kbd> <kbd>RET</kbd> `z/` で、カーソルが前方 (右方向) の `字` にジャンプします (<kbd>RET</kbd> は <kbd>C-m</kbd> でもかまいません)。<kbd>C-,</kbd> は同様に後方 (左方向) にジャンプします。

<kbd>RET</kbd> の代わりに ASCII 文字をタイプすると、その文字にジャンプします。<kbd>C-.</kbd> / <kbd>C-,</kbd> を繰り返すと、直前のジャンプを繰り返します。

### tc.el との併用

tc.el と併用する場合は、init.el に次の設定が必要かもしれません。

``` emacs-lisp
(setq tcode-isearch-enable-wrapped-search nil)
```

### kkc を利用したかな漢字変換

ttt の補助入力として kkc (Kana Kanji converter) を利用することができます。
kkc は Emacs に付属するかな漢字変換です。
init.el に次のように書いて <kbd>M-j</kbd> を `ttt-do-ttt-with-kkc` に割り当てます。

``` emacs-lisp
(define-key global-map (kbd "M-j") 'ttt-do-ttt-with-kkc)
```

TT-code でひらがなをタイプして <kbd>C-u M-j</kbd> を入力すると kkc によりかな漢字変換が行われ、エコーエリアに変換した文字のコードヘルプが表示されます。

* 入力: `jendux` <kbd>C-u M-j</kbd>
* 結果: `漢字`
* エコーエリア: `漢<l4>字<z/>`

交ぜ書き変換 (読みに漢字を含む変換) はできません。

kkc の変換中は、次のようなキー操作が可能です。

| key            | binding                                  |
|----------------|------------------------------------------|
| `C-n`, `SPC`   | `kkc-next`                               |
| `C-p`          | `kkc-prev`                               |
| `l`            | `kkc-show-conversion-list-or-next-group` |
| `L`            | `kkc-show-conversion-list-or-prev-group` |
| `0` .. `9`     | `kkc-select-from-list`                   |
| `H`            | `kkc-hiragana`                           |
| `K`            | `kkc-katakana`                           |
| `C-o`          | `kkc-longer`                             |
| `C-i`, `TAB`   | `kkc-shorter`                            |
| `C-f`          | `kkc-next-phrase`                        |
| `C-c`, `DEL`   | `kkc-cancel`                             |
| `C-m`, `RET`   | `kkc-terminate`                          |
| `C-@`, `C-SPC` | `kkc-first-char-only`                    |
| `C-h`          | `kkc-help`                               |
| `O`            | `kkc-longer-phrase`                      |
| `I`            | `kkc-shorter-conversion`                 |

ひらがなを対象に文字数を指定して変換することもできます。

* 入力: `漢字にへんかん` <kbd>C-u 4 M-j</kbd>
* 結果: `漢字に変換`

### kkc を利用した交ぜ書き変換 (実験的機能)

kkc-maze.el を `load-path` の通ったところに置いて init.el に次のように書いておくと kkc で交ぜ書き変換が利用できるようになるかも知れません。

初回の読み込みには時間がかかります。

``` emacs-lisp
(require 'kkc-maze)
(define-key global-map (kbd "M-j") 'ttt-do-ttt-with-kkc)
```

TT-code でかな漢字交じり語をタイプして <kbd>C-u M-j</kbd> を入力すると交ぜ書き変換が行われます。

* 入力: `jendz/` <kbd>C-u M-j</kbd>
* 結果: `漢字`

## 詳細とカスタマイズ

### Dvorak キーボード

Dvorak キーボードで ttt.el を使うには、init.el に次のように書きます。

``` emacs-lisp
(setq ttt-keys "1234567890',.pyfgcrlaoeuidhtns;qjkxbmwvz")
```

### 区切り

区切りは、変換の際にコードをどこまでさかのぼるかを示す文字列で、デフォルトでは `:` です。

区切りを、たとえば `-` に変更するには、init.el に次のように記述します。

``` emacs-lisp
(setq ttt-delimiter "-")
```

区切りは変換後に削除されます。区切りを残したいときは、`::` のように 1 つ余分にタイプしてください (ただし、`:` が区切りと解釈されないところでは、その必要はありません)。

例:

| 入力 | 結果|
|----|----|
| `http:mwnsleyrkw`  | `httpプロトコル`   |
| `http::mwnsleyrkw` | `http:プロトコル`  |
| `http: mwnsleyrkw` | `http: プロトコル` |

### 和英間のスペース

`ttt-remove-space` は、日本語と半角英数字の間のスペースを制御する設定項目です。

設定できる値は `nil` (変換後にスペースを残す) と `t` (変換後にスペースを削除する) で、デフォルト値は `nil` です。

`ttt-remove-space` が `t` の場合でも、コンマ (`,`)・ピリオド (`.`)・セミコロン (`;`) または非コード文字の記号の後のスペースは残します。

例:

| 入力 | `nil` | `t` | 備考 |
|----|----|----|----|
| `ascii ;lz/`  | `ascii 文字` | `ascii文字`  | 英小文字はコード文字 |
| `ascii:;lz/`  | `ascii文字`  | `ascii文字`  | `:` で明示的に詰める |
| `ascii :;lz/` | `ascii 文字` | `ascii 文字` | `:` で明示的に空ける |
| `Tyrhsjz`     | `Tコード`    | `Tコード`    | 大文字は非コード文字 |
| `T yrhsjz`    | `T コード`   | `Tコード`    | 大文字だが `t` では詰める |
| `1 in`        | `1 個`       | `1個`        | 数字はコード文字 |
| `/ ;nxxlnjk`  | `/ 正規表現` | `/正規表現`  | `/` はコード文字 |
| `# /euejs`    | `# 見出し`   | `# 見出し`   | `#` は非コード文字 |
| `#ie/x`       | `#タグ`      | `#タグ`      | `#` は非コード文字 |
| `1. pduale`   | `1. リスト`  | `1. リスト`  | `,` `.` `;` はコード文字だが空ける |

### 変換テーブルのカスタマイズ (実験的機能)

変換テーブルに新しく定義を追加するには `M-x ttt-userdef-define` を実行し、定義したい文字とコードを指定します。

たとえば、『﨑』を `jfa5` に割り当てるには `M-x ttt-userdef-define RET 﨑 RET jfa5 RET` と入力します。

コードに指定できるパターンは `??` (本表), `jf??` (右表, Dvorak では `hu??`), `fj??` (左表, Dvorak では `uh??`), `43??` (白表), `78??` (黒表) の 5 通りです。

追加した定義を削除するには `M-x ttt-userdef-undefine` を実行するか、 `M-x ttt-userdef-define` で空文字列を指定し、削除するコードを指定します。
