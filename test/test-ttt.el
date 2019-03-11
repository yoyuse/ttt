;;; test-ttt.el --- test for ttt   -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019  YUSE Yosihiro

;; Author: YUSE Yosihiro <yoyuse@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert)
(require 'ttt)

(message "Running tests on Emacs %s" emacs-version)

;;
;; ttt--decode-substring
;;

(defun ttt--decode-substring (str)
  (let* ((ls (ttt--backward str))
         (dst (nth 0 ls))
         (src-len (nth 1 ls))
         (tail-len (nth 2 ls)))
    (while (string-prefix-p " " dst)
      (setq dst (substring dst 1)
            src-len (1- src-len)))
    (list dst src-len tail-len)))

;;
;; ttt--decode-substring
;;

(ert-deftest test-decode-substring ()
  (let ((tests
         '(("kdjd;sjdkd;s" ("の、が、のが" 12 0))
           ("ABCDyfhdydEFG" ("あいう" 6 3))
           ("abcd yfhdyd EFG" ("あいう" 6 4))
           ("abcd:yfhdydEFG" ("あいう" 7 3))
           (";d;fha" ("123" 6 0))
           ("ysksjsks/ajgjdjfkdt8p3;gpzjshdjtighdiakslghdhgiajd" ("わたしたちは、氷砂糖をほしいくらいもたないでも、" 50 0))
           ("あの Iha-Tovo kd,fhrjaoajrks風、" ("のすきとおった" 14 2))
           ("うつくしい森で飾られた Morio:/vjd" ("市、" 5 0))
           ("(またAladdin  lyfjlk[ラムプ]とり)" ("洋燈" 6 8)))))
    (dolist (test tests)
      (should (equal (cadr test)
                     (ttt--decode-substring (car test)))))))

;;
;; ttt--decode-substring
;;

(defun ttt--decode-substring-to-string (str)
  (let* ((decode (ttt--decode-substring str))
         (decoded-body (car decode))
         (body-len (nth 1 decode))
         (tail-len (nth 2 decode))
         (head-len (- (length str)
                      body-len
                      tail-len)))
    (concat (substring str 0 head-len)
            decoded-body
            (substring str (+ head-len body-len)))))

(defun ttt--decode-at-marker (str marker)
  (let (i src decode)
    (while (setq i (ttt--index str marker))
      (setq src (substring str 0 i))
      (setq dec (ttt--decode-substring-to-string src))
      (setq str (concat dec
                        (substring str (+ i (length marker))))))
    str))

(ert-deftest test-multiple-decode-substring ()
  (let ((tests
         '(("kdjd;sjdkd;s%" "の、が、のが")
           ("ABCDyfhdydEFG%" "ABCDあいうEFG")
           ("abcd yfhdyd% efg" "abcd あいう efg")
           ("abcd:yfhdyd%efg" "abcdあいうefg")
           (";d%;f%ha" "岳ha")
           ("ysksjsks/ajgjdjfkdt8p3;gpzjshdjtighdiakslghdhgiajd%" "わたしたちは、氷砂糖をほしいくらいもたないでも、")
           ("yfkd% Iha-Tovo kd,fhrjaoajrks風、%" "あの Iha-Tovo のすきとおった風、")
           ("うつくしい森で飾られた Morio:/vjd%" "うつくしい森で飾られた Morio市、")
           ("(またAladdin  lyfjlk[usubmw]jajc)%%%" "(またAladdin  洋燈[ラムプ]とり)"))))
    (dolist (test tests)
      (let* ((src (car test))
             (expected (cadr test))
             (actual (ttt--decode-at-marker src "%")))
        (should (equal expected actual))))))

;;
;; ttt-do-ttt
;;

(defun simulate-ttt-with-temp-buffer (inputs)
  (with-temp-buffer
    (let (input)
      (while inputs
        (setq input (car inputs)
              inputs (cdr inputs))
        (cond ((eq input 'M-j) (ttt-do-ttt))
              ((eq input 'RET) (insert "\n"))
              (t (insert input)))))
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest test-simulate-ttt ()
  (let ((simulate-ttt-inputs
         '("yfkd" M-j " Iha-Tovo kd,fhrjaoajrksqrjd" M-j RET
           "xqhgiaf6kgp.ksug;gia.dvfhd;bigjd" M-j RET
           "yd.djtjshdjfoxhgw7ig;eks" M-j " Morio:/vjd" M-j RET
           "jfcwm;kdqfigqfigm.je;aslkdbnhf" M-j RET
           RET
           "(mdks" M-j "Aladdin  lyfjlk[usubmw]jajc)" M-j M-j M-j RET))
        (simulate-ttt-expected
         (concat "あの Iha-Tovo のすきとおった風、" "\n"
                 "夏でも底に冷たさをもつ青いそら、" "\n"
                 "うつくしい森で飾られた Morio市、" "\n"
                 "郊外のぎらぎらひかる草の波。" "\n"
                 "\n"
                 "(またAladdin  洋燈[ラムプ]とり)" "\n")))
    (should (equal simulate-ttt-expected
                   (simulate-ttt-with-temp-buffer simulate-ttt-inputs)))))

;;
;; spacing
;;

(ert-deftest test-ttt-spacing ()
  (let ((ttt-spacing-inputs '("http:mwnsleyrkw" M-j RET
                              "http::mwnsleyrkw" M-j RET
                              "http: mwnsleyrkw" M-j RET
                              "ascii ;lz/" M-j RET
                              "ascii:;lz/" M-j RET
                              "ascii :;lz/" M-j RET
                              "Tyrhsjz" M-j RET
                              "T yrhsjz" M-j RET
                              "1 in" M-j RET
                              "/ ;nxxlnjk" M-j RET
                              "# /euejs" M-j RET
                              "#ie/x" M-j RET
                              "1. pduale" M-j RET))
        (ttt-spacing-expected-french
         (concat "httpプロトコル" "\n"
                 "http:プロトコル" "\n"
                 "http: プロトコル" "\n"
                 "ascii 文字" "\n"
                 "ascii文字" "\n"
                 "ascii 文字" "\n"
                 "Tコード" "\n"
                 "T コード" "\n"
                 "1 個" "\n"
                 "/ 正規表現" "\n"
                 "# 見出し" "\n"
                 "#タグ" "\n"
                 "1. リスト" "\n"))
        (ttt-spacing-expected-japanese
         (concat "httpプロトコル" "\n"
                 "http:プロトコル" "\n"
                 "http: プロトコル" "\n"
                 "ascii文字" "\n"
                 "ascii文字" "\n"
                 "ascii 文字" "\n"
                 "Tコード" "\n"
                 "Tコード" "\n"
                 "1個" "\n"
                 "/正規表現" "\n"
                 "# 見出し" "\n"
                 "#タグ" "\n"
                 "1. リスト" "\n"))
        )
    (let ((ttt-spacing 'french))
      (should (equal ttt-spacing-expected-french
                     (simulate-ttt-with-temp-buffer ttt-spacing-inputs))))
    (let ((ttt-spacing 'japanese))
      (should (equal ttt-spacing-expected-japanese
                     (simulate-ttt-with-temp-buffer ttt-spacing-inputs))))
    ))

;;
;; do tests
;;

(if noninteractive
    (ert-run-tests-batch-and-exit)
  (ert t))

;;; test-ttt.el ends here
