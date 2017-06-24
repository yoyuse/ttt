;;; test-ttt.el --- test for ttt   -*- lexical-binding: t; -*-

;; Copyright (C) 2017  YUSE Yosihiro

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

(ert-deftest test-decode-substring ()
  (let ((tests
         '(("kdjd;sjdkd;s" ("の、が、のが" 12 0))
           ("ABCDyfhdydEFG" ("あいう" 6 3))
           ("abcd yfhdyd EFG" ("あいう" 6 4))
           ("abcd:yfhdydEFG" ("あいう" 7 3))
           (";d;fha" ("123" 6 0))
           ("ysksjsks/ajgjfkdt8p3;gpzjshdjtighdiakslghdhgia" ("わたしたちは氷砂糖をほしいくらいもたないでも" 46 0))
           ("あの Iha-Tovo kd,fhrjaoajrks風" ("のすきとおった" 14 1))
           ("うつくしい森で飾られた Morio:/v" ("市" 3 0))
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
           ("ysksjsks/ajgjfkdt8p3;gpzjshdjtighdiakslghdhgia%" "わたしたちは氷砂糖をほしいくらいもたないでも")
           ("yfkd% Iha-Tovo kd,fhrjaoajrks風%" "あの Iha-Tovo のすきとおった風")
           ("うつくしい森で飾られた Morio:/v%" "うつくしい森で飾られた Morio市")
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
        (if (eq input 'xfer)
            (ttt-do-ttt)
          (insert input))))
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest test-simulate-ttt ()
  (let ((simulate-ttt-inputs
         '("yfkd" xfer " Iha-Tovo kd,fhrjaoajrksqrjd" xfer "\n"
           "xqhgiaf6kgp.ksug;gia.dvfhd;bigjd" xfer "\n"
           "yd.djtjshdjfoxhgw7ig;eks" xfer " Morio:/vjd" xfer "\n"
           "jfcwm;kdqfigqfigm.je;aslkdbnhf" xfer "\n"
           "\n"
           "(mdks" xfer "Aladdin  lyfjlk[usubmw]jajc)" xfer xfer xfer
           "\n"
           ))
        (simulate-ttt-expected
         (concat "あの Iha-Tovo のすきとおった風、\n"
                 "夏でも底に冷たさをもつ青いそら、\n"
                 "うつくしい森で飾られた Morio市、\n"
                 "郊外のぎらぎらひかる草の波。\n"
                 "\n"
                 "(またAladdin  洋燈[ラムプ]とり)"
                 "\n")))
    (should (equal simulate-ttt-expected
                   (simulate-ttt-with-temp-buffer simulate-ttt-inputs)))))


;;
;; do tests
;;

(if noninteractive
    (ert-run-tests-batch-and-exit)
  (ert t))

;;; test-ttt.el ends here
