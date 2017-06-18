# ttt

ttt (Tiny TT-Code Translation program) is a kind of Japanese input for Emacs.

## Features

### Pros

* **Modeless Japanese input**.
  No need to switch modes when writing English-Japanese mixed text
* **Dierct Japanese input**. No need of kana to kanji conversion;
  ttt comes with TT-Code,
  which is an extension of [T-code](http://openlab.jp/tcode/ "Home of T-Code")
  and covers most of characters commonly used in Japanese
* **Single file implementation**. Easy to install and setup

### Cons

* No helper functions such as bushu conversion or mazegaki conversion
* Stroke help or virtual keyboard is not available
* Does not support other input methods such as TUT-Code, Try-Code etc.

## Requirements

* Emacs 24 or higher

## Installation

Put ttt.el into somewhere in your `load-path`.
Then add following to .emacs or init.el:

```emacs-lisp
(autoload 'ttt-do-ttt "ttt" nil t)
(autoload 'ttt-isearch-do-ttt "ttt" nil t)
(define-key global-map (kbd "M-j") 'ttt-do-ttt)
(define-key isearch-mode-map (kbd "M-j") 'ttt-isearch-do-ttt)

;; You may need next line if you are using tc.el
;; (setq tcode-isearch-enable-wrapped-search nil)

;; Optional setting
(autoload 'ttt-jump-to-char-forward "ttt" nil t)
(autoload 'ttt-jump-to-char-backward "ttt" nil t)
(define-key global-map (kbd "C-,") 'ttt-jump-to-char-backward)
(define-key global-map (kbd "C-.") 'ttt-jump-to-char-forward)
```

## Usage

### Writing Japanese text

Just type T-Code as well as writing English text,
and hit <kbd>M-j</kbd> (`ttt-do-ttt`),
which scans the T-Code string before cursor on the current line
and decodes it to Japanese text.

Here are some examples:

* Input: `ysksjsks/ajgjdjfkdt8p3;gpzjshdjtighdiakslghdhgia` <kbd>M-j</kbd>  
  Output: `わたしたちは、氷砂糖をほしいくらいもたないでも`  

* Input: `yfkd` <kbd>M-j</kbd> ` Iha-Tovo kd,fhrjaoajrksqr` <kbd>M-j</kbd>  
  Output: `あの Iha-Tovo のすきとおった風`  
  (Note:  T-Code string scan goes backward
  until white space or non T-Code character found)

* Input: `うつくしい森で飾られた Morio:/v` <kbd>M-j</kbd>  
  Output: `うつくしい森で飾られた Morio市`  
  (Note: Use colon (`:`) as *delimiter* where white space is not desired;
  colon is removed after decode)

* Input: `(またAladdin　lyfjlk[usubmw]jajc)` <kbd>M-j</kbd> <kbd>M-j</kbd> <kbd>M-j</kbd>  
  Output: `(またAladdin　洋燈[ラムプ]とり)`  
  (Note: Multiple <kbd>M-j</kbd> at a time is OK;
  it skips already decoded text as well as non T-Code characters)

### Using ttt in isearch

Although ttt is available also in isearch,
searching Japanese text works *not incrementally*;
the limitation is unavoidable because ttt is actually a postfix conversion.

For example, suppose Emacs buffer contains text below,
with cursor at beginning of buffer:

> あのイーハトーヴォのすきとおった風、  
> 夏でも底に冷たさをもつ青いそら、  
> うつくしい森で飾られたモーリオ市、  
> 郊外のぎらぎらひかる草の波。

Searching character "草" (T-Code spells it as "sl") goes like following:

1. Hit <kbd>C-s</kbd> to start isearch
2. Type `sl`
3. Emacs immediately searches `sl` which is not found,
   resulting in `Failing I-search: sl` error with beep
4. Ignore beep and simply hit <kbd>M-j</kbd>
5. Then Emacs echoes `I-search: 草` and finds "草" properly

Probably mechanism like [migemo](http://0xcc.net/migemo/) or
[migemo.el](https://github.com/emacs-jp/migemo) should be considered,
but it is not yet implemented for ttt.

### Moving around in buffer

ttt.el provides `ttt-jump-to-char-forward` and `ttt-jump-to-char-backward`,
which are similar to Vim's normal commands <kbd>f</kbd> and <kbd>F</kbd>,
with T-Code extension.

Call `ttt-jump-to-char-forward`, hit <kbd>Return</kbd> or <kbd>C-m</kbd>
and type T-Code string for a Japanese character,
then cursor jumps forward to that character,
while `ttt-jump-to-char-backward` jumps backward.

Hitting a normal ASCII character instead of <kbd>Return</kbd> or <kbd>C-m</kbd>
simply moves cursor to the character itself.

Repeat use of the commands repeats last jump.

Both commands set mark before moving cursor;
so `ttt-jump-to-char-forward` followed by <kbd>C-w</kbd>
acts as `zap-up-to-char` with TT-Code enhancement.

## Customization

### Using ttt with Dvorak keyboard

Add following, or something like, to .emacs or init.el:

``` emacs-lisp
(setq ttt-keys "1234567890',.pyfgcrlaoeuidhtns;qjkxbmwvz")
```

### Changing delimiter character

To set delimiter to `-`, for example, add following to .emacs or init.el:

``` emacs-lisp
(setq ttt-delimiter ?-)
```

### Customizing mapping of TT-Code to Japanese character

Mapping of TT-Code to Japanese character is defined by variable `ttt-table`.

For now ttt.el does not provide convenient way of customizing this variable,
so set it to desired value directly if needed.

The value of `ttt-table` must be an array of 40 elements,
where each element is either `nil`, string or another array of 40 elements.

The magic number 40 here agrees with the length of `ttt-keys`,
with *n*th element of `ttt-table` corrensponding to *n*th key of `ttt-keys`.
`ttt-keys` defaults to `"1234567890qwertyuiopasdfghjkl;zxcvbnm,./"`.

For example, T-Code `kd` maps to character `の`,
because `k` is 27th key of `ttt-keys`, while `d` is 22nd,
and `(aref (aref ttt-table 27) 22)` is `"の"`.
Another example: `jfkd` maps to `氷`,
where `j` is 26th, `f` is 23rd and
`(aref (aref (aref (aref ttt-table 26) 23) 27) 22)` is `"氷"`.

See ttt.el for more detail.
