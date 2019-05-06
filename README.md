# ttt

ttt は Emacs 上で動作するモードレス日本語入力です。

入力モードの切り替えなしに、日本語と半角英数字の混在したテキストを入力できます。

日本語の入力には、漢字直接入力の TT-code (T-code の拡張) を利用しています。

## 動作環境

* Emacs 24.3 以降

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

半角英数字と日本語の間には、スペースを入れてください。デフォルトでは、スペースは変換後も残ります (フレンチスペーシング)。

* 入力: `Emacs 0rwj` <kbd>M-j</kbd>
* 結果: `Emacs 拡張`

スペースを残したくないところは、コロン `:` で区切ってください。区切りは変換後に削除されます。

* 入力: `,g` <kbd>M-j</kbd> `ttt:jv` <kbd>M-j</kbd>
* 結果: `「ttt」`

`(setq ttt-spacing 'japanese)` と設定すると、変換後にスペースを残さないスタイルになります (日本語スペーシング)。

* 入力: `default jg` <kbd>M-j</kbd> `french spacing hg,fhf` <kbd>M-j</kbd>
* 結果: `defaultはfrench spacingです。`

## Tips

### `ttt-do-ttt` の連続実行

<kbd>M-j</kbd> は、続けて実行することができます。

* 入力: `ojiy(feiy) # yr,x,dle` <kbd>M-j</kbd> <kbd>M-j</kbd> <kbd>M-j</kbd>
* 結果: `関数(引数) # コメント`

### Isearch

<kbd>M-j</kbd> は isearch でも利用できます。

たとえば、`Emacs 拡張` を検索するには、isearch に入って `Emacs 0rwj` とタイプします。このとき、おそらく `0rwj` が見つからないとエラーになりますが、かまわず <kbd>M-j</kbd> と入力してください。すると isearch 内で変換が行われ、`Emacs 拡張` が検索されます。

#### Isearch a la Migemo

Emacs に [migemo](https://github.com/emacs-jp/migemo) がインストール・設定されていれば、<kbd>M-j</kbd> を入力することなく、migemo 風の isearch を行うことができます。

Isearch 時に <kbd>M-t</kbd> で、通常の migemo isearch と migemo ttt isearch をトグルできます。

### 1 文字検索

init.el に、

``` emacs-lisp
(define-key global-map (kbd "C-.") 'ttt-jump-to-char-forward)
(define-key global-map (kbd "C-,") 'ttt-jump-to-char-backward)
```

と書いておくと、Vim の <kbd>f</kbd> /  <kbd>F</kbd> コマンドに似た、カーソルの移動が使えるようになります。

たとえば、<kbd>C-.</kbd> <kbd>RET</kbd> `z/` で、カーソルが前方 (右方向) の `字` にジャンプします (<kbd>RET</kbd> は <kbd>C-m</kbd> でもかまいません)。<kbd>C-,</kbd> は同様に後方 (左方向) にジャンプします。

<kbd>RET</kbd> の代わりに ASCII 文字をタイプすると、その文字にジャンプします。<kbd>C-.</kbd> / <kbd>C-,</kbd> を繰り返すと、直前のジャンプを繰り返します。

### tc.el との併用

tc.el と併用する場合は、init.el に次の設定が必要かもしれません。

``` emacs-lisp
(setq tcode-isearch-enable-wrapped-search nil)
```

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

* 入力: `http:mwnsleyrkw` <kbd>M-j</kbd> `http::mwnsleyrkw` <kbd>M-j</kbd> `http: mwnsleyrkw` <kbd>M-j</kbd>
* 結果: `httpプロトコル` `http:プロトコル` `http: プロトコル`

### スペーシング

`ttt-spacing` は、変換の際に和英間のスペースを制御する設定項目です。

設定できる値は `'french` (フレンチスペーシング、変換後にスペースを残す) と `'japanese` (日本語スペーシング、変換後にスペースを削除する) で、デフォルト値は `'french` です。

日本語スペーシングでも、コンマ (`,`)・ピリオド (`.`)・セミコロン (`;`) または非コード文字の記号の後のスペースは残します。

例:

| 入力 | 'french | 'japanese | 備考 |
|----|----|----|----|
| `ascii ;lz/`  | `ascii 文字` | `ascii文字`  | 英小文字はコード文字 |
| `ascii:;lz/`  | `ascii文字`  | `ascii文字`  | `:` で明示的に詰める |
| `ascii :;lz/` | `ascii 文字` | `ascii 文字` | `:` で明示的に空ける |
| `Tyrhsjz`     | `Tコード`    | `Tコード`    | 大文字は非コード文字 |
| `T yrhsjz`    | `T コード`   | `Tコード`    | 大文字だが `'japanese` では詰める |
| `1 in`        | `1 個`       | `1個`        | 数字はコード文字 |
| `/ ;nxxlnjk`  | `/ 正規表現` | `/正規表現`  | `/` はコード文字 |
| `# /euejs`    | `# 見出し`   | `# 見出し`   | `#` は非コード文字 |
| `#ie/x`       | `#タグ`      | `#タグ`      | `#` は非コード文字 |
| `1. pduale`   | `1. リスト`  | `1. リスト`  | `,` `.` `;` はコード文字だが空ける |

### 変換テーブル

コードから日本語への変換テーブルをカスタマイズする簡単な方法は、(現在のところ) 用意されていません。
