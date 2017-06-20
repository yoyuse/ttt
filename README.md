# ttt

ttt (Tiny TT-Code Translation program) is a kind of Japanese input for Emacs.

ttt provides modeless Japanese input.
That is, with ttt there is no need to switch modes
when inputting English-Japanese mixed text.
Moreover, input with ttt is done by simple decode of TT-Code,
rather than complex process such as kana-kanji conversion.

On the other hand, ttt does not provide any helper functions
such as bushu conversion, mazegaki conversion, code help or
virtual keyboard, which are familiar to [T-code](http://openlab.jp/tcode/ "Home of T-Code") or
[kanchoku](https://github.com/kanchoku) users.

## Requirements

* Emacs 24.3 or higher

## Installation

Put ttt.el into somewhere in `load-path`.
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

### Inputting Japanese text

Type TT-Code and hit <kbd>M-j</kbd> (`ttt-do-ttt`),
which scans the T-Code string before cursor on the current line
and decodes it to Japanese text.

Here are some examples:

* Input:
  `ysksjsks/ajgjdjfkdt8p3;gpzjshdjtighdiakslghdhgia` <kbd>M-j</kbd>  
  Result:
  `わたしたちは、氷砂糖をほしいくらいもたないでも`  

* Input:
  `yfkd` <kbd>M-j</kbd> ` Iha-Tovo kd,fhrjaoajrksqr` <kbd>M-j</kbd>  
  Result:
  `あの Iha-Tovo のすきとおった風`  
  (Note:  T-Code string scan goes backward
  until white space or non-T-Code character found)

* Input:
  `うつくしい森で飾られた Morio:/v` <kbd>M-j</kbd>  
  Result:
  `うつくしい森で飾られた Morio市`  
  (Note: Use colon (`:`) as *delimiter* where white space is not desired;
  colon is removed after decode)

* Input:
  `(またAladdin　lyfjlk[usubmw]jajc)` <kbd>M-j</kbd> <kbd>M-j</kbd> <kbd>M-j</kbd>  
  Result:
  `(またAladdin　洋燈[ラムプ]とり)`  
  (Note: Multiple <kbd>M-j</kbd> are OK;
  each <kbd>M-j</kbd> skips already decoded text
  as well as non-T-Code characters)

### Isearch with ttt

Though ttt is available also in isearch, it works *not incrementally*.
For example, searching character "草", which is encoded to "sl" in T-Code,
would be done as below:

1. Hit <kbd>C-s</kbd> to start isearch, followed by typing `sl`
1. Emacs attempts to search `sl` which is probably not found,
   yielding `Failing I-search: sl` error
1. Ignore error and proceed to hit <kbd>M-j</kbd>
1. Then Emacs echoes `I-search: 草` and finds "草" finally

This behavior,
caused from that ttt is actually a kind of postfix conversion,
is not the best, but it is a trade-off for keeping ttt simple.

### Alternative searching

ttt.el provides `ttt-jump-to-char-forward` and `ttt-jump-to-char-backward`,
which are similar to Vim's normal mode commands <kbd>f</kbd> and <kbd>F</kbd>,
with T-Code enhancement.

Call `ttt-jump-to-char-forward`, hit <kbd>RET</kbd> or <kbd>C-m</kbd>
and type T-Code string for a Japanese character,
then cursor jumps forward to that character,
while `ttt-jump-to-char-backward` jumps backward.

Typing a normal ASCII character 
instead of hitting <kbd>RET</kbd> or <kbd>C-m</kbd>
just moves the cursor to the character itself.

Repeat use of the commands repeats last jump.

## Customization

### Using ttt with Dvorak keyboard

Add following, or something like, to .emacs or init.el:

``` emacs-lisp
(setq ttt-keys "1234567890',.pyfgcrlaoeuidhtns;qjkxbmwvz")
```

### Changing delimiter

To set delimiter to `-`, for example, add following to .emacs or init.el:

``` emacs-lisp
(setq ttt-delimiter ?-)
```

### Defining TT-Code

Decode of TT-Code is defined with variable `ttt-table`.
The value of `ttt-table` must be an array of 40 elements,
where each element is either `nil`, string or another array of 40 elements.

Number 40 here agrees with length of `ttt-keys`,
which defaults to `"1234567890qwertyuiopasdfghjkl;zxcvbnm,./"`.
`ttt-table` is constructed so that
its *n*th element correnspond to *n*th key of `ttt-keys`.

For example, decode of `kd` is defined as character `の`,
because `k` is 27th key of `ttt-keys`, `d` is 22nd,
and `(aref (aref ttt-table 27) 22)` is `"の"`.
Another example: `jfkd` is defined as `氷`,
because `j` is 26th, `f` is 23rd and
`(aref (aref (aref (aref ttt-table 26) 23) 27) 22)` is `"氷"`.
