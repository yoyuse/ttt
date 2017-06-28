# ttt

ttt is another modeless Japanese input for Emacs, or a simple and
minimal implementation of TT-code, one of kanji direct input
methods.

ttt provides an easy way of inputting English-Japanese mixed text
without switching modes. Inputting Japanese characters is done by
means of simple decode of TT-code, rather than complex process such
as in kana-kanji conversion.

## Requirements

* Emacs 24.3 or higher

## Installation

Put ttt.el into `load-path` and add following to .emacs or init.el:

```emacs-lisp
(require 'ttt)
(define-key global-map (kbd "M-j") 'ttt-do-ttt)
(define-key isearch-mode-map (kbd "M-j") 'ttt-isearch-do-ttt)

;; You may need next line if you are using tc.el
;; (setq tcode-isearch-enable-wrapped-search nil)

;; Optional setting
(define-key global-map (kbd "C-.") 'ttt-jump-to-char-forward)
(define-key global-map (kbd "C-,") 'ttt-jump-to-char-backward)
```

## Usage

### Inputting

Type TT-code and hit <kbd>M-j</kbd> (`ttt-do-ttt`),
which scans the TT-code string before the cursor on the current line
and decodes it to Japanese text.

* Input:
  `ysksjsks/ajgjdjfkdt8p3;gpzjshdjtighdiakslghdhgiajd` <kbd>M-j</kbd>
* Result:
  `わたしたちは、氷砂糖をほしいくらいもたないでも、`

TT-code string scanning goes backward from the cursor
to the beginning of the line,
or until a white space or any non-TT-code character found.

* Input:
  `yfkd` <kbd>M-j</kbd> ` Iha-Tovo kd,fhrjaoajrksqrjd` <kbd>M-j</kbd>
* Result:
  `あの Iha-Tovo のすきとおった風、`

Use a colon (`:`) as the *delimiter* where a white space is not desired;
the colon is removed after decode.

* Input:
  `うつくしい森で飾られた Morio:/vjd` <kbd>M-j</kbd>
* Result:
  `うつくしい森で飾られた Morio市、`

Multiple decode at a time is allowable.

* Input:
  `(またAladdin  lyfjlk[usubmw]jajc)` <kbd>M-j</kbd> <kbd>M-j</kbd> <kbd>M-j</kbd>
* Result:
  `(またAladdin  洋燈[ラムプ]とり)`

### Isearching

Though ttt is available also in isearch, it works *not incrementally*.
For example, searching character "草", which is encoded to "sl" in TT-code,
would be done as below:

1. Hit <kbd>C-s</kbd> to start isearch, followed by typing `sl`
1. Emacs attempts to search `sl`, which is not found,
   resulting in `Failing I-search: sl` error
1. Ignore the error and proceed to hit <kbd>M-j</kbd>
1. Then Emacs echoes `I-search: 草` and the search hits "草" finally

This behavior is not the best;
it is a trade-off for keeping ttt simple and consistent.

### Single character searching

As alternative searching, ttt.el provides
<kbd>C-.</kbd> (`ttt-jump-to-char-forward`)
and <kbd>C-,</kbd> (`ttt-jump-to-char-backward`),
which are similar to Vim's normal mode commands <kbd>f</kbd> and <kbd>F</kbd>,
with TT-code enhancement.

Hit <kbd>C-.</kbd> followd by  <kbd>RET</kbd> or <kbd>C-m</kbd>
then type TT-code string for a single Japanese character,
and cursor jumps forward to the character,
while <kbd>C-,</kbd> jumps backward.

Typing a normal ASCII character
instead of hitting <kbd>RET</kbd> or <kbd>C-m</kbd>
just moves the cursor to the character itself.

Repeat use of the commands repeats the last jump.

## Customization

### Using ttt with Dvorak keyboard

Add following to .emacs or init.el:

``` emacs-lisp
(setq ttt-keys "1234567890',.pyfgcrlaoeuidhtns;qjkxbmwvz")
```

### Changing delimiter

To set delimiter to `-`, add following to .emacs or init.el:

``` emacs-lisp
(setq ttt-delimiter ?-)
```

Or, to set to Space, add following:

``` emacs-lisp
(setq ttt-delimiter ?\ )
```

### Rewriting decode rules

Currently ttt.el does not provide a convenient way of customizing decode rules.
Here is given only a summary how the rules are implemented.

ttt.el defines the decode rules with variable `ttt-table`.
The value of `ttt-table` is an array of 40 elements,
where each element is either `nil`, string or another array of 40 elements.

The number 40 agrees with the length of `ttt-keys`
which defaults to `"1234567890qwertyuiopasdfghjkl;zxcvbnm,./"`.
`ttt-table` is constructed so that
its *n*th element should correnspond to *n*th key of `ttt-keys`.

For example, decode of `kd` is defined as character `の`,
because `k` is 27th key of `ttt-keys`, `d` is 22nd,
and `(aref (aref ttt-table 27) 22)` is `"の"`.
Another example: `jfkd` is defined as `氷`,
because `j` is 26th, `f` is 23rd and
`(aref (aref (aref (aref ttt-table 26) 23) 27) 22)` is `"氷"`.
