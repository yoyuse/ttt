;;; kkc-maze.el --- kkc with mazegaki conversion     -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2022  YUSE Yosihiro

;; Author: YUSE Yosihiro <yoyuse@gmail.com>
;; Keywords: input method, japanese

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 2022-11-26 kkc-maze.el from ttt-maze-kkc.el
;; 2019-05-12
;; 2019-05-11 ttt-maze-kkc.el

;;; Code:

(require 'kkc)

;;; product

(defun kkc-maze-flatten (a)
  "リスト A を平坦化したリストを返す."
  (if (nlistp a)
      (list a)
    (apply 'append (mapcar 'kkc-maze-flatten a))))

(defun kkc-maze-product (s1 s2)
  "2 つの集合 S1 と S2 の直積集合を返す."
  (let (result)
    (dolist (a s1)
      (dolist (b s2)
        (push (kkc-maze-flatten (list a b)) result)))
    (nreverse result)))

(defun kkc-maze-product-list (&rest list)
  "LIST (n 個の集合) の直積集合を返す."
  (if (null list)
      list
    (let ((a (car list)))
      (dolist (b (cdr list))
        (setq a (kkc-maze-product a b)))
      a)))

;;; cache

;; (defvar ttt-lookup-delimiter "[ :]"
;;   "*絞り込み検索で用いる読みの区切り文字列 (正規表現).")

(defvar kkc-maze-cache-file-name (locate-user-emacs-file "kkc-maze-init.el")
  "*交ぜ書き変換で使用するキャッシュファイル名.
Nil のときはキャッシュファイルを使用しない.")

(defun kkc-maze-save-cache-file ()
  "変数 `kkc-maze-cache-file-name' で指定されたキャッシュファイルに書き込む."
  (let ((coding-system-for-write 'utf-8)
        (print-length nil))
    (when kkc-maze-cache-file-name
      (write-region (format "(setq kkc-maze-tankanji-table '%S)
(setq kkc-maze-tankanji-obarray %S)"
                            kkc-maze-tankanji-table kkc-maze-tankanji-obarray)
                    nil
                    kkc-maze-cache-file-name))))

;;; tankanji

(defvar kkc-maze-tankanji-table nil "単漢字の読みを引くためのテーブル.")
(defvar kkc-maze-tankanji-obarray (make-vector 4999 nil) "オブジェクト配列.")

(defun kkc-maze-tankanji-to-kana (n)
  "かなを表す数値 N を対応するかな文字列にする."
  (cond ((= n 0) "ー")
        ((< n 0) "")
        (t (char-to-string
            (or (decode-char 'japanese-jisx0208 (+ (* #x100 #x24) (+ n #x20)))
                (aref "ーぁあぃいぅうぇえぉおかがきぎくぐけげこごさざしじすずせぜそぞただちぢっつづてでとどなにぬねのはばぱひびぴふぶぷへべぺほぼぽまみむめもゃやゅゆょよらりるれろゎわゐゑをん" n))))))

(defun kkc-maze-tankanji-make (dic pre)
  "辞書 DIC と読みのプレフィックス PRE から単漢字の読みを引くためのテーブルを作成する."
  (let* ((l dic))
    (while l
      (let* ((l (car l))
             (car (car l))
             (cdr (cdr l))
             (yomi (concat pre (kkc-maze-tankanji-to-kana car))))
        (if (consp (car cdr))
            (dolist (kanji (car cdr))
              (let* ((ls (plist-get kkc-maze-tankanji-table
                                    (intern-soft kanji
                                                 kkc-maze-tankanji-obarray))))
                (when (and (eq 1 (length kanji))
                           (not (member yomi ls)))
                  (setq kkc-maze-tankanji-table
                        (plist-put kkc-maze-tankanji-table
                                   (intern kanji kkc-maze-tankanji-obarray)
                                   (cons yomi ls)))))))
        (kkc-maze-tankanji-make (cdr cdr) yomi))
      (setq l (cdr l)))))

(defun kkc-maze-cache-make ()
  "交ぜ書き変換のためのキャッシュデータを作成する."
  ;;
  (require 'ja-dic-utl)
  (load-library "ja-dic/ja-dic")
  ;;
  (message "Making tankanji table...")
  ;; (setq kkc-maze-tankanji-table nil)
  ;; (setq kkc-maze-tankanji-obarray (make-vector 4999 nil))
  (kkc-maze-tankanji-make (cdr skkdic-prefix) "")
  (kkc-maze-tankanji-make (cdr skkdic-postfix) "")
  (kkc-maze-tankanji-make (cdr skkdic-okuri-ari) "")
  (kkc-maze-tankanji-make (cdr skkdic-okuri-nasi) "")
  (message "Making tankanji table... done"))

;; 単漢字テーブルの作成

(if (and kkc-maze-cache-file-name
         (file-readable-p kkc-maze-cache-file-name))
    (condition-case nil
        (load-file kkc-maze-cache-file-name)
      (kkc-maze-error "Invalid data in %s" kkc-maze-cache-file-name))
  (kkc-maze-cache-make)
  (kkc-maze-save-cache-file))

(if (fboundp 'define-error)
    (define-error 'kkc-maze-error nil)
  (put 'kkc-maze-error 'error-conditions '(kkc-maze-error error)))

(defun kkc-maze-error (&rest args)
  "Signal error `kkc-maze-error' with message ARGS."
  (signal 'kkc-maze-error (apply #'format-message args)))

(defun kkc-maze-tankanji-lookup (kanji)
  "単漢字 KANJI の読みのリストを返す."
  (plist-get kkc-maze-tankanji-table
             (intern-soft kanji kkc-maze-tankanji-obarray)))

(defun kkc-maze-lookup-yomi (maze)
  "漢字かな交じり語 MAZE の読みのリストを返す."
  (let* ((maze-list (split-string maze "" t))
         (maze-list (mapcar (function (lambda (ch)
                                        (or (kkc-maze-tankanji-lookup ch)
                                            (list ch))))
                            maze-list))
         (maze-list (apply 'kkc-maze-product-list maze-list))
         (maze-list (mapcar (function (lambda (ls)
                                        (if (listp ls)
                                            (apply 'concat ls)
                                          ls)))
                            maze-list)))
    maze-list))

(defun kkc-maze-regexp (maze)
  "漢字かな交じり語 MAZE から正規表現を生成して返す.
例: \"かん字\" -> \".+字\"."
  (let* ((re (regexp-quote maze))
         (re (replace-regexp-in-string "[ーぁ-ん]+" ".+" maze))
         (re (concat "^" re "$")))
    re))

;;; filter

;; (defun ttt-lookup-entry (yomi)
;;   "`skkdic-lookup-key' を用いて読み YOMI を検索して候補のリストを返す."
;;   (let* ((vec (string-to-vector yomi))
;;          (len (length vec)))
;;     (skkdic-lookup-key vec len)))

;; (defun ttt-lookup-make-regexp (entry)
;;   "候補のリスト ENTRY から正規表現を生成して返す."
;;   (let* ((list (string-to-list
;;                 (mapconcat #'identity entry "")))
;;          (list (seq-uniq list)) ; XXX: YYY: exclude KANA
;;          (regexp (mapconcat (lambda (c) (regexp-quote (char-to-string c)))
;;                             list "")))
;;     (if (string= "" regexp)
;;         regexp
;;       (concat "[" regexp "]"))))

(defun kkc-maze-lookup-filter (list regexp)
  "正規表現 REGEXP で候補のリスト LIST を絞りこむ."
  (seq-filter (lambda (elm) (string-match regexp elm))
              list))

;; (defun ttt-lookup-with-yomi (yomi &rest yomis)
;;   "読み YOMI の候補を読み YOMIS の候補で絞りこむ."
;;   (let* ((entry (ttt-lookup-entry yomi))
;;          (entries (mapcar #'ttt-lookup-entry yomis))
;;          (regexps (mapcar #'ttt-lookup-make-regexp entries)))
;;     (dolist (regexp regexps)
;;       (setq entry (kkc-maze-lookup-filter entry regexp)))
;;     entry))

;; ;;;###autoload
;; (defun ttt-lookup-yomi (yomis)
;;   "読み YOMIS で索字する.
;; YOMIS には `ttt-lookup-delimiter' で区切って複数の読みを指定できる."
;;   (interactive "syomis? ")
;;   (let* ((yomis (ttt--decode-string yomis)) ; XXX
;;          (yomis (split-string yomis ttt-lookup-delimiter t))
;;          (list (apply #'ttt-lookup-with-yomi yomis))
;;          (certain nil)
;;          (certain-p #'(lambda (ch) (member ch certain))))
;;     (message (mapconcat #'ttt-code-help-str list " "))))

;; ;;;###autoload
;; (defun ttt-lookup-maze (maze)
;;   "漢字かな交じり語 MAZE で索字する.
;; 例: \"かん字\" -> \"漢<l4>字 換<7t>字\"."
;;   (interactive "smaze? ")
;;   (let* ((maze (ttt--decode-string maze)) ; XXX
;;          (yomis (ttt-maze-lookup-yomi maze))
;;          (re (ttt-maze-regexp maze))
;;          (ls (ttt-maze-flatten (mapcar 'ttt-lookup-entry yomis)))
;;          (certain (split-string maze "" t))
;;          (certain-p #'(lambda (ch) (member ch certain)))
;;          (ls (seq-uniq ls))
;;          (ls (seq-filter (lambda (elm) (string-match re elm))
;;                          ls)))
;;     (message (mapconcat #'ttt-code-help-str ls " "))))

;;; kkc-maze

(defun kkc-maze-lookup-yomi-with-delta (maze)
  "漢字かな交じり語 MAZE の読みと読みの長さの増加分の cons のリストを返す."
  (let ((maze-list (kkc-maze-lookup-yomi maze))
        (len (length maze)))
    (mapcar (function (lambda (yomi)
                        (let ((new-len (length yomi)))
                          (cons yomi (- new-len len)))))
            maze-list)))

(defun kkc-maze-lookup-key (maze len &optional postfix prefer-noun)
  "列 MAZE の長さ LEN の変換候補のリストを返す.

MAZE はかなまたは漢字の配列.
LEN が MAZE の長さより短いときは, MAZE の最初の LEN 文字だけ見る.

オプションの POSTFIX が非 nil なら, 接尾辞も考慮する.

オプションの PREFER-NOUN が非 nil なら, 送りなしの変換候補が
リストの先頭に来る (XXX: この部分は未実装)."
  (let* ((maze (concat maze))
         (ls (mapcar (lambda (cons)
                       (let ((seq (car cons))
                             (delta (cdr cons)))
                         (skkdic-lookup-key seq (+ len delta)
                                            postfix prefer-noun)))
                     (kkc-maze-lookup-yomi-with-delta (substring maze 0 len))))
         (ls (kkc-maze-flatten ls))
         (regexp (kkc-maze-regexp (substring maze 0 len)))
         (ls (kkc-maze-lookup-filter ls regexp))
         (ls (seq-uniq ls)))
    ls))

;;; kkc-lookup-key from kkc.el.gz

;; kkc-lookup-key の skkdic-lookup-key を kkc-maze-lookup-key に書き換え

;; Lookup Japanese dictionary to set list of conversions in
;; kkc-current-conversions for key sequence kkc-current-key of length
;; LEN.  If no conversion is found in the dictionary, don't change
;; kkc-current-conversions and return nil.
;; Postfixes are handled only if POSTFIX is non-nil.
(defun kkc-maze-kkc-lookup-key (len &optional postfix prefer-noun)
  ;; At first, prepare cache data if any.
  (unless kkc-init-file-flag
    (setq kkc-init-file-flag t
	  kkc-lookup-cache nil)
    (add-hook 'kill-emacs-hook 'kkc-save-init-file)
    (if (file-readable-p kkc-init-file-name)
	(condition-case nil
	    (load-file kkc-init-file-name)
	  (kkc-error "Invalid data in %s" kkc-init-file-name))))
  (or (and (nested-alist-p kkc-lookup-cache)
	   (eq (car kkc-lookup-cache) kkc-lookup-cache-tag))
      (setq kkc-lookup-cache (list kkc-lookup-cache-tag)
	    kkc-init-file-flag 'kkc-lookup-cache))
  (let ((entry (lookup-nested-alist kkc-current-key kkc-lookup-cache len 0 t)))
    (if (consp (car entry))
	(setq kkc-length-converted len
	      kkc-current-conversions-width nil
	      kkc-current-conversions (car entry))
      ;; <MODIFIED>
      ;; (setq entry (skkdic-lookup-key kkc-current-key len postfix prefer-noun))
      (setq entry (kkc-maze-lookup-key kkc-current-key len postfix prefer-noun))
      ;; </MODIFIED>
      (if entry
	  (progn
	    (setq kkc-length-converted len
		  kkc-current-conversions-width nil
		  kkc-current-conversions (cons 1 entry))
	    (if postfix
		;; Store this conversions in the cache.
		(progn
		  (set-nested-alist kkc-current-key kkc-current-conversions
				    kkc-lookup-cache kkc-length-converted)
		  (setq kkc-init-file-flag 'kkc-lookup-cache)))
	    t)
	(if (= len 1)
	    (setq kkc-length-converted 1
		  kkc-current-conversions-width nil
		  kkc-current-conversions (cons 0 nil)))))))

;;; kkc-next-phrase from kkc.el.gz

;; kkc-next-phrase の (looking-at "\\CH") を無効化

(defun kkc-maze-kkc-next-phrase ()
  "Fix the currently converted string and try to convert the remaining string."
  (interactive)
  (if (>= kkc-length-head (length kkc-current-key))
      (kkc-terminate)
    (setq kkc-length-head (- (length kkc-current-key) kkc-length-head))
    (goto-char (overlay-end kkc-overlay-head))
    (while (and (< (point) (overlay-end kkc-overlay-tail))
		;; <MODIFIED>
		;; 正規表現 \CH は ひらがな (H) 以外の文字 (C) か
		;; (looking-at "\\CH"))
		nil)
      ;; </MODIFIED>
      (goto-char (match-end 0))
      (setq kkc-length-head (1- kkc-length-head)))
    (if (= kkc-length-head 0)
	(kkc-terminate)
      (let ((newkey (make-vector kkc-length-head 0))
	    (idx (- (length kkc-current-key) kkc-length-head))
	    (len kkc-length-head)
	    (i 0))
	;; For the moment, (setq kkc-original-kana (concat newkey))
	;; doesn't work.
	(setq kkc-original-kana "")
	(while (< i kkc-length-head)
	  (aset newkey i (aref kkc-current-key (+ idx i)))
	  (setq kkc-original-kana
		(concat kkc-original-kana (char-to-string (aref newkey i))))
	  (setq i (1+ i)))
	(setq kkc-current-key newkey)
	(setq kkc-length-converted 0)
	(while (and (not (kkc-lookup-key kkc-length-head nil
					 (< kkc-length-head len)))
		    (> kkc-length-head 1))
	  (setq kkc-length-head (1- kkc-length-head)))
	(let ((pos (point))
	      (tail (overlay-end kkc-overlay-tail)))
	  (move-overlay kkc-overlay-head pos tail)
	  (move-overlay kkc-overlay-tail tail tail))
	(kkc-update-conversion 'all)))))

;;; advice

(defcustom kkc-maze-enable-maze-p t
  "*非 nil のとき KKC で交ぜ書き変換を行う."
  :group 'ttt
  :type 'boolean)

(defadvice kkc-lookup-key (around kkc-maze-kkc-lookup-key-ad activate)
  "Adviced by ttt."
  (if kkc-maze-enable-maze-p
      (setq ad-return-value (kkc-maze-kkc-lookup-key (ad-get-arg 0)
                                                     (ad-get-arg 1)
                                                     (ad-get-arg 2)))
    ad-do-it))

(defadvice kkc-next-phrase (around kkc-maze-kkc-next-phrase-ad activate)
  "Adviced by ttt."
  (if kkc-maze-enable-maze-p
      (setq ad-return-value (kkc-maze-kkc-next-phrase))
    ad-do-it))

;;; provide

(provide 'kkc-maze)
;;; kkc-maze.el ends here
