;;; ttt.el --- Tiny TT-code Translation   -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2017  YUSE Yosihiro

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ttt is yet another modeless Japanese input, or a simple and minimal
;; implementation of TT-code, one of kanji direct input methods.

;; ttt provides an easy way of inputting English-Japanese mixed text
;; without switching modes. Inputting Japanese characters is done by
;; means of simple decode of TT-code, rather than complex process such
;; as in kana-kanji conversion.

;; Setup:

;; (require 'ttt)
;; (define-key global-map (kbd "M-j") 'ttt-do-ttt)
;; (define-key isearch-mode-map (kbd "M-j") 'ttt-isearch-do-ttt)
;;
;; ;; You may need next line if you are using tc.el
;; ;; (setq tcode-isearch-enable-wrapped-search nil)
;;
;; ;; Optional setting
;; (define-key global-map (kbd "C-.") 'ttt-jump-to-char-forward)
;; (define-key global-map (kbd "C-,") 'ttt-jump-to-char-backward)

;; Usage:

;; Type TT-code and hit M-j (`ttt-do-ttt'), which scans the TT-code
;; string before the cursor on the currrent line and decodes it to
;; Japanse text.

;; TT-code string scanning goes backward from the cursor to the
;; beginning of the line, or until a white space, any non-TT-code
;; character or `ttt-delimiter' found. `ttt-delimiter' is removed
;; after decode.

;;; Code:

;;
;; Customization variables
;;

(defgroup ttt nil
  "Tiny TT-code Translation."
  :group 'mule)

(defcustom ttt-keys "1234567890qwertyuiopasdfghjkl;zxcvbnm,./"
  "TT-code keys."
  :type 'string
  :group 'ttt)

(defcustom ttt-delimiter ?:
  "Delimiter between TT-code and non-TT-code text."
  :type 'character
  :group 'ttt)

(defvar ttt-table
  [
   ;; 1
   ["" "" "" "" "" "" "" "" "" ""
    "ヲ" "ゥ" "ヴ" "ヂ" "ヅ" "簡" "承" "快" "包" "唱"
    "ぱ" "ぴ" "ぷ" "ぺ" "ぽ" "朱" "陣" "眼" "執" "岳"
    "ぁ" "ぃ" "ぅ" "ぇ" "ぉ" "欲" "迫" "留" "替" "還"]
   ;; 2
   ["" "" "" "" "" "" "" "" "" ""
    "哀" "逢" "宛" "囲" "庵" "徴" "章" "否" "納" "暮"
    "慰" "為" "陰" "隠" "胃" "遅" "鶴" "繁" "紹" "刑"
    "" "" "" "" "" "巣" "災" "列" "沼" "更"]
   ;; 3
   ["" "" "" "" "" "" "" "" "" ""
    "暇" "牙" "壊" "較" "寒" "触" "候" "歯" "頼" "憲"
    "我" "掛" "敢" "甘" "患" "甲" "鹿" "誌" "夢" "弱"
    "瓦" "" "" "" "" "茂" "恋" "刻" "?" "占"]
   ;; 4
   ["" ""
    ;; 43
    [
     ;; 431
     ["" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""
      "" "鄙" "" "" "" "蛛" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""]
     ;; 432
     ["" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" "甦"]
     ;; 433
     ["" "" "" "" "" "" "" "" "" ""
      "瑕" "鴉" "" "" "賽" "" "" "" "" ""
      "" "" "瞰" "嵌" "" "匣" "" "" "儚" ""
      "礫" "" "" "" "" "藪" "" "哭" "？" ""]
     ;; 434
     ["" "" "" "" "" "" "" "" "" ""
      "" "" "" "遽" "" "" "" "" "" "寃"
      "" "" "" "" "" "" "" "聘" "" "靄"
      "" "" "" "" "" "" "" "" "" "筐"]
     ;; 435
     ["" "" "" "" "" "" "" "" "" ""
      "" "" "辟" "痺" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "悸" "" ""
      "" "" "" "" "" "" "" "" "" ""]
     ;; 436
     ["" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""
      "櫻" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""]
     ;; 437
     ["" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""
      "漱" "" "" "" "" "" "皺" "" "" ""
      "" "" "" "訝" "疇" "" "" "" "" ""]
     ;; 438
     ["" "" "" "" "" "" "" "" "" ""
      "痰" "" "" "" "焉" "輻" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""
      "" "" "熾" "" "" "" "" "" "" ""]
     ;; 439
     ["" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" "蟲"
      "" "" "佛" "" "" "眈" "" "" "" ""
      "" "" "" "" "翳" "" "" "" "" ""]
     ;; 430
     ["" "" "" "" "" "" "" "" "" ""
      "" "舊" "" "絋" "" "饅" "" "" "痒" ""
      "" "" "" "" "" "杞" "" "祀" "爬" ""
      "" "" "" "" "靡" "" "" "" "" ""]
     ;; 43q
     ["" "" "隗" "" "" "" "" "" "" ""
      "" "" "" "颯" "" "" "" "" "癪" ""
      "" "" "" "" "" "" "" "峙" "佇" ""
      "" "" "" "讀" "" "孵" "檻" "" "揄" ""]
     ;; 43w
     ["" "" "墟" "" "" "" "" "躊" "" ""
      "" "" "懺" "偕" "" "凭" "" "" "逞" ""
      "" "" "" "" "" "疼" "" "躓" "" ""
      "" "" "" "" "" "" "" "" "" ""]
     ;; 43e
     ["" "" "鋏" "" "" "躇" "" "" "" ""
      "" "" "魎" "" "莉" "" "" "" "" ""
      "" "" "" "" "廣" "" "媚" "" "" ""
      "" "" "" "" "" "" "" "" "" ""]
     ;; 43r
     ["" "渕" "" "" "" "" "" "澹" "" ""
      "" "釉" "" "" "" "" "" "" "" ""
      "" "彿" "" "" "" "" "膀" "" "" ""
      "橙" "" "" "" "" "" "燵" "" "" ""]
     ;; 43t
     ["" "" "" "" "" "" "" "炒" "" ""
      "" "" "！" "" "揶" "" "" "" "" ""
      "" "" "" "螢" "" "僭" "" "條" "" ""
      "褪" "" "" "" "" "" "" "" "" ""]
     ;; 43y
     ["" "" "" "" "" "" "" "" "" "爛"
      "" "" "" "珈" "" "" "" "" "檬" ""
      "鍮" "" "謳" "嗚" "" "" "" "" "" "麒"
      "" "" "薔" "" "" "" "" "" "" ""]
     ;; 43u
     ["竟" "" "" "" "" "" "彗" "" "孕" "朧"
      "徊" "" "" "" "" "" "" "" "" ""
      "" "薇" "４" "琥" "" "" "" "" "罠" ""
      "" "靱" "" "癇" "" "翅" "" "" "" ""]
     ;; 43i
     ["" "" "" "軋" "" "" "晰" "" "茹" ""
      "" "區" "" "" "" "" "" "雉" "" ""
      "魍" "" "）" "６" "" "" "" "" "" ""
      "" "" "" "" "" "涸" "" "奢" "" ""]
     ;; 43o
     ["毯" "" "" "惠" "" "" "韆" "" "" "鑪"
      "嶌" "" "揉" "撥" "" "" "" "" "" ""
      "" "贄" "卍" "學" "" "繹" "" "" "" ""
      "" "" "經" "" "絨" "" "" "" "" ""]
     ;; 43p
     ["" "" "" "" "" "" "" "" "" ""
      "" "沐" "" "" "" "譚" "" "" "" ""
      "掟" "" "" "朦" "" "" "" "嬌" "" "罹"
      "仄" "黎" "" "" "乖" "" "" "" "" ""]
     ;; 43a
     ["" "" "綺" "" "" "" "" "" "稟" ""
      "" "" "餡" "" "" "" "" "" "" "蛉"
      "" "咤" "" "" "" "" "" "" "" ""
      "" "" "" "餉" "" "" "" "" "" ""]
     ;; 43s
     ["" "" "" "" "" "嘔" "" "" "" ""
      "" "凰" "" "" "珀" "聲" "" "鑽" "" ""
      "" "" "埃" "" "" "" "" "嗜" "" ""
      "" "" "" "俯" "" "" "" "" "" ""]
     ;; 43d
     ["" "" "" "" "" "" "" "" "" "焙"
      "" "" "抒" "棘" "" "" "" "" "" ""
      "" "" "闊" "" "咥" "" "幇" "" "" ""
      "" "" "" "" "" "" "" "囮" "" ""]
     ;; 43f
     ["" "" "" "" "" "" "" "" "" ""
      "鵺" "" "" "" "" "" "昴" "" "" "愕"
      "" "聚" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "騙" "榮" "" "址"]
     ;; 43g
     ["" "" "" "" "" "亞" "" "謗" "" "笏"
      "" "應" "" "攪" "毬" "" "藝" "" "" ""
      "眞" "" "滲" "" "椒" "" "" "諍" "曰" "澁"
      "" "" "" "" "" "" "" "" "" "瞠"]
     ;; 43h
     ["" "" "熙" "" "" "" "竄" "" "" ""
      "" "" "７" "" "" "躾" "蹙" "" "" ""
      "３" "" "" "" "" "" "翹" "頽" "" ""
      "徨" "誦" "" "" "" "" "" "" "" ""]
     ;; 43j
     ["" "" "" "" "" "" "" "" "" ""
      "隕" "" "" "" "櫟" "" "" "" "壜" ""
      "" "栞" "" "" "" "" "鐵" "" "戌" "蠅"
      "" "" "菻" "" "" "" "" "" "" ""]
     ;; 43k
     ["" "蜩" "" "" "" "" "" "" "" ""
      "" "几" "（" "" "８" "穽" "" "" "" "顰"
      "" "" "" "０" "" "冰" "" "" "誅" ""
      "圓" "" "―" "９" "會" "" "裔" "" "" ""]
     ;; 43l
     ["" "閾" "咎" "灌" "" "" "" "" "" ""
      "" "" "" "國" "" "" "" "" "菫" ""
      "" "弌" "５" "" "舐" "籐" "" "" "泄" ""
      "" "" "" "" "" "" "" "" "" ""]
     ;; 43;
     ["" "" "" "" "" "" "" "" "" ""
      "檸" "" "斂" "" "" "" "" "谺" "" ""
      "縷" "姜" "１" "２" "" "" "" "" "憫" ""
      "" "" "" "" "" "" "狡" "" "躰" ""]
     ;; 43z
     ["" "" "" "" "" "噤" "絣" "砒" "" ""
      "" "" "樅" "" "苺" "" "" "" "戮" ""
      "琲" "" "" "" "" "" "橇" "" "" ""
      "" "拌" "齊" "" "" "" "" "" "" ""]
     ;; 43x
     ["" "" "" "" "" "逍" "" "" "" ""
      "棗" "" "猾" "" "" "" "" "" "" ""
      "" "柩" "" "澤" "" "" "" "淺" "" ""
      "" "" "" "" "" "" "茉" "祓" "" ""]
     ;; 43c
     ["" "" "" "" "" "" "" "" "渾" ""
      "" "" "" "" "" "" "" "" "" "鞦"
      "" "" "簒" "暈" "" "" "呻" "" "" ""
      "證" "" "" "" "" "瞼" "" "冪" "" "蝸"]
     ;; 43v
     ["" "" "" "" "" "鉤" "" "" "" "徘"
      "" "" "" "紆" "" "" "" "腱" "" "翡"
      "敲" "棹" "絆" "蜻" "" "" "" "烙" "贅" "膠"
      "頷" "" "" "" "" "" "" "" "" ""]
     ;; 43b
     ["" "" "" "" "" "" "" "屏" "" ""
      "" "" "寥" "" "" "屁" "櫂" "蠍" "" ""
      "" "捏" "" "" "" "繚" "" "" "翔" ""
      "％" "" "煌" "" "" "瀾" "" "" "游" "單"]
     ;; 43n
     ["憑" "" "" "" "" "" "" "" "" ""
      "" "苻" "" "" "" "" "遙" "" "囁" ""
      "" "" "" "貶" "" "" "" "" "瑙" ""
      "濱" "" "胚" "矮" "" "" "" "欒" "" ""]
     ;; 43m
     ["" "" "" "" "" "" "" "" "" ""
      "" "" "肛" "" "" "" "" "" "" ""
      "" "" "儘" "" "" "" "" "" "" ""
      "" "" "" "訛" "賣" "" "" "" "檜" ""]
     ;; 43,
     ["" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "埓" "" "" ""
      "" "萬" "" "" "" "" "" "燻" "踵" ""
      "" "瑪" "" "" "" "" "～" "" "拗" ""]
     ;; 43.
     ["" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "炬" "" "唸" "" ""
      "" "彷" "壺" "" "" "苹" "" "" "" ""
      "" "" "" "眩" "" "" "呟" "" "" "芻"]
     ;; 43/
     ["" "" "" "" "辯" "" "" "" "" ""
      "轢" "" "睨" "" "" "" "" "慟" "" "邊"
      "魑" "" "" "" "" "樂" "" "" "" "胱"
      "" "" "" "" "" "" "" "" "" ""]
     ]
    "" "" "" "" "" "" ""
    "啓" "掲" "携" "劇" "賢" "宗" "途" "筆" "逃" "勉"
    "兼" "嫌" "顕" "牽" "厳" "致" "貨" "招" "卸" "雲"
    "" "" "" "" "" "述" "脳" "豆" "辞" "箱"]
   ;; 5
   ["" "" "" "" "" "" "" "" "" ""
    "把" "伐" "避" "卑" "藩" "植" "複" "里" "寝" "罪"
    "菱" "紐" "描" "憤" "弊" "汎" "絡" "季" "阿" "窓"
    "" "" "" "" "" "朗" "老" "看" "献" "矢"]
   ;; 6
   ["" "" "" "" "" "" "" "" "" ""
    "酸" "貿" "攻" "盤" "汽" "" "" "" "" ""
    "桜" "典" "採" "君" "犯" "" "" "" "" ""
    "呼" "紀" "房" "去" "秒" "" "" "" "" ""]
   ;; 7
   ["" "" "" "" "" "" "" "" "" ""
    "昼" "捜" "焼" "帯" "換" "索" "冊" "皿" "賛" ""
    "瀬" "博" "謡" "純" "余" "衰" "趨" "垂" "粋" "寸"
    "幅" "破" "績" "疑" "範" "" "" "" "" ""]
   ;; 8
   ["" "" "" "" "" "" "" "" "" ""
    "炭" "異" "闘" "易" "延" "射" "需" "輯" "瞬" "盾"
    "鳥" "筋" "希" "副" "堀" "滋" "湿" "甚" "" "瞳"
    "歓" "郡" "識" "ぢ" "核" "" "" "" "" ""]
   ;; 9
   ["" "" "" "" "" "" "" "" "" ""
    "稲" "隣" "奈" "速" "雪" "濁" "詑" "蓄" "貯" "虫"
    "催" "忠" "仏" "盟" "肩" "沈" "添" "徹" "爪" "陶"
    "功" "抗" "属" "綿" "影" "" "" "" "" ""]
   ;; 0
   ["" "" "" "" "" "" "" "" "" ""
    "湯" "旧" "夕" "拡" "互" "慢" "迷" "戻" "羊" ""
    "障" "乳" "察" "標" "療" "己" "已" "巳" "巴" ""
    "盗" "幡" "衣" "離" "麻" "" "" "" "" ""]
   ;; q
   ["ヮ" "丑" "鬼" "孤" "奉" "湖" "端" "刷" "震" "弘"
    "果" "概" "武" "風" "細" "害" "撃" "浴" "積" "故"
    "収" "若" "指" "ぎ" "思" "病" "常" "寺" "停" "河"
    "徳" "械" "帝" "読" "族" "帰" "監" "竹" "ゅ" "志"]
   ;; w
   ["ヰ" "臼" "虚" "誇" "某" "礼" "飾" "寿" "扱" "痛"
    "告" "買" "残" "階" "古" "賃" "折" "秀" "程" "鉱"
    "際" "雄" "氏" "格" "術" "終" "張" "質" "領" "置"
    "渡" "刊" "始" "鈴" "丁" "庁" "寄" "注" "修" "抜"]
   ;; e
   ["ヱ" "宴" "狭" "黄" "貌" "著" "郵" "順" "片" "票"
    "策" "詳" "両" "能" "利" "整" "追" "糸" "断" "提"
    "太" "査" "丸" "次" "広" "起" "薬" "づ" "容" "供"
    "守" "訪" "了" "恐" "未" "昨" "裁" "介" "究" "航"]
   ;; r
   ["ヵ" "縁" "脅" "后" "卜" "移" "塩" "危" "札" "訴"
    "首" "由" "在" "論" "ペ" "軽" "隊" "春" "低" "児"
    "園" "ふ" "続" "習" "門" "路" "防" "港" "玉" "試"
    "登" "融" "極" "督" "才" "跡" "達" "具" "答" "層"]
   ;; t
   ["ヶ" "曳" "驚" "耕" "" "郷" "群" "砂" "乞" "遺"
    "農" "死" "!" "増" "ゃ" "評" "角" "幸" "減" "敷"
    "船" "賞" "ェ" "火" "聞" "越" "得" "条" "右" "席"
    "退" "雨" "熱" "況" "返" "ゲ" "芝" "失" "養" "深"]
   ;; y
   ["請" "尚" "舎" "布" "姿" "" "" "庶" "" "欄"
    "歩" "キ" "や" "コ" "ナ" "佐" "接" "記" "モ" "無"
    "中" "わ" "う" "あ" "本" "む" "ケ" "話" "べ" "期"
    "店" "全" "バ" "後" "問" "洗" "響" "司" "復" "担"]
   ;; u
   ["境" "賀" "喜" "苦" "絶" "" "星" "粧" "乃" "龍"
    "回" "せ" "出" "山" "金" "法" "備" "朝" "資" "石"
    "ス" "ラ" "4" "こ" "さ" "南" "式" "座" "民" "ゾ"
    "持" "じ" "部" "間" "ム" "羽" "忘" "迎" "並" "陸"]
   ;; i
   ["系" "岸" "幹" "圧" "密" "" "析" "丈" "如" "略"
    "務" "区" "タ" "者" "マ" "数" "最" "知" "士" "屋"
    "も" "東" ")" "6" "ら" "原" "戦" "線" "ソ" "歳"
    "町" "自" "六" "場" "七" "個" "討" "華" "浦" "巻"]
   ;; o
   ["探" "責" "丘" "恵" "秘" "" "遷" "称" "尼" "慮"
    "島" "百" "手" "発" "和" "郎" "急" "ワ" "費" "解"
    "お" "生" "十" "学" "高" "駅" "関" "ダ" "点" "強"
    "所" "議" "経" "ニ" "住" "医" "史" "許" "ユ" "競"]
   ;; p
   ["象" "漁" "糖" "固" "押" "" "宣" "蒸" "帳" "累"
    "開" "木" "保" "立" "女" "談" "験" "送" "ィ" "募"
    "定" "ろ" "リ" "月" "シ" "物" "男" "橋" "遇" "係"
    "ほ" "明" "動" "産" "北" "静" "環" "補" "冷" "護"]
   ;; a
   ["ゎ" "於" "奇" "巧" "" "償" "紅" "舗" "輪" "則"
    "報" "音" "案" "横" "崎" "服" "変" "限" "逆" "令"
    "種" "宅" "料" "受" "英" "勢" "輸" "基" "足" "婦"
    "件" "宮" "局" "向" "割" "億" "色" "左" "ぬ" "根"]
   ;; s
   ["ゐ" "汚" "既" "克" "" "欧" "傷" "充" "倒" "存"
    "紙" "王" "曲" "興" "白" "声" "審" "研" "企" "違"
    "岡" "熟" "土" "予" "ボ" "必" "形" "好" "草" "段"
    "友" "伊" "頭" "府" "ぶ" "録" "貸" "態" "展" "様"]
   ;; d
   ["ゑ" "乙" "菊" "懇" "" "努" "豪" "喫" "操" "倍"
    "館" "放" "情" "刺" "ぐ" "任" "改" "労" "精" "装"
    "結" "待" "活" "切" "加" "講" "助" "味" "築" "衛"
    "卒" "求" "配" "富" "番" "赤" "販" "花" "警" "独"]
   ;; f
   ["" "穏" "却" "困" "" "底" "維" "腕" "柄" "牛"
    "夜" "々" "引" "側" "官" "検" "昇" "統" "ざ" "然"
    "進" "取" "ね" "育" "室" "愛"
    ;; fj
    [
     ;; fj1
     ["" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""]
     ;; fj2
     ["" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""]
     ;; fj3
     ["" "" "" "" "" "" "" "" "" ""
      "Α" "Β" "Γ" "Δ" "Ε" "Ζ" "Η" "Θ" "Ι" "Κ"
      "Λ" "Μ" "Ν" "Ξ" "Ο" "Π" "Ρ" "Σ" "Τ" "Υ"
      "Φ" "Χ" "Ψ" "Ω" "" "" "" "" "" ""]
     ;; fj4
     ["" "" "" "" "" "" "" "" "" ""
      "Ａ" "Ｂ" "Ｃ" "Ｄ" "Ｅ" "Ｆ" "Ｇ" "Ｈ" "Ｉ" "Ｊ"
      "Ｋ" "Ｌ" "Ｍ" "Ｎ" "Ｏ" "Ｐ" "Ｑ" "Ｒ" "Ｓ" "Ｔ"
      "Ｕ" "Ｖ" "Ｗ" "Ｘ" "Ｙ" "Ｚ" "" "" "" ""]
     ;; fj5
     ["Э" "Ю" "Я" "" "" "" "" "" "" ""
      "А" "Б" "В" "Г" "Д" "Е" "Ё" "Ж" "З" "И"
      "Й" "К" "Л" "М" "Н" "О" "П" "Р" "С" "Т"
      "У" "Ф" "Х" "Ц" "Ч" "Ш" "Щ" "Ъ" "Ы" "Ь"]
     ;; fj6
     ["э" "ю" "я" "" "" "" "" "" "" ""
      "а" "б" "в" "г" "д" "е" "ё" "ж" "з" "и"
      "й" "к" "л" "м" "н" "о" "п" "р" "с" "т"
      "у" "ф" "х" "ц" "ч" "ш" "щ" "ъ" "ы" "ь"]
     ;; fj7
     ["" "" "" "" "" "" "" "" "" ""
      "ａ" "ｂ" "ｃ" "ｄ" "ｅ" "ｆ" "ｇ" "ｈ" "ｉ" "ｊ"
      "ｋ" "ｌ" "ｍ" "ｎ" "ｏ" "ｐ" "ｑ" "ｒ" "ｓ" "ｔ"
      "ｕ" "ｖ" "ｗ" "ｘ" "ｙ" "ｚ" "" "" "" ""]
     ;; fj8
     ["" "" "" "" "" "" "" "" "" ""
      "α" "β" "γ" "δ" "ε" "ζ" "η" "θ" "ι" "κ"
      "λ" "μ" "ν" "ξ" "ο" "π" "ρ" "σ" "τ" "υ"
      "φ" "χ" "ψ" "ω" "" "" "" "" "" ""]
     ;; fj9
     ["" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""]
     ;; fj0
     ["" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""]
     ;; fjq
     ["" "" "" "" "" "肱" "腔" "膏" "砿" "閤"
      "叡" "餌" "荏" "云" "噂" "鴻" "劫" "壕" "濠" "轟"
      "穎" "盈" "瑛" "洩" "嬰" "麹" "鵠" "漉" "甑" "忽"
      "奄" "堰" "厭" "榎" "頴" "惚" "狛" "此" "坤" "梱"]
     ;; fjw
     ["" "" "" "" "" "梧" "檎" "瑚" "醐" "鯉"
      "窺" "鵜" "卯" "迂" "烏" "佼" "倖" "勾" "喉" "垢"
      "蔚" "欝" "唄" "嘘" "碓" "宏" "巷" "庚" "昂" "晃"
      "閏" "瓜" "厩" "姥" "鰻" "杭" "梗" "浩" "糠" "紘"]
     ;; fje
     ["" "" "" "" "" "捲" "硯" "鍵" "鹸" "絃"
      "郁" "亥" "謂" "萎" "畏" "舷" "諺" "乎" "姑" "狐"
      "允" "鰯" "茨" "溢" "磯" "糊" "袴" "股" "胡" "菰"
      "吋" "蔭" "胤" "淫" "咽" "虎" "跨" "鈷" "伍" "吾"]
     ;; fjr
     ["" "" "" "" "" "袈" "祁" "圭" "珪" "慧"
      "鮎" "綾" "絢" "飴" "虻" "桂" "畦" "稽" "繋" "罫"
      "闇" "按" "袷" "粟" "或" "荊" "詣" "頚" "戟" "隙"
      "椅" "惟" "夷" "杏" "鞍" "桁" "訣" "倦" "喧" "拳"]
     ;; fjt
     ["" "" "" "" "" "矩" "躯" "駈" "駒" "喰"
      "葵" "姶" "挨" "娃" "唖" "寓" "串" "櫛" "釧" "屑"
      "葦" "旭" "渥" "穐" "茜" "窟" "沓" "轡" "窪" "熊"
      "姐" "斡" "梓" "鯵" "芦" "隈" "粂" "栗" "鍬" "卦"]
     ;; fjy
     ["庇" "匪" "蕃" "磐" "挽" "" "" "" "" ""
      "簸" "樋" "誹" "緋" "斐" "蒼" "鎗" "捉" "袖" "其"
      "柊" "眉" "琵" "毘" "枇" "揃" "遜" "汰" "唾" "柁"
      "肘" "膝" "髭" "疋" "稗" "舵" "楕" "陀" "騨" "堆"]
     ;; fju
     ["媛" "桧" "逼" "畢" "弼" "" "" "" "" ""
      "廟" "豹" "瓢" "彪" "謬" "岱" "戴" "腿" "苔" "黛"
      "鰭" "蛭" "蒜" "鋲" "錨" "鯛" "醍" "鷹" "瀧" "啄"
      "冨" "埠" "瀕" "斌" "彬" "托" "琢" "鐸" "茸" "凧"]
     ;; fji
     ["葡" "撫" "阜" "芙" "斧" "" "" "" "" ""
      "淵" "蕗" "葺" "楓" "蕪" "蛸" "只" "叩" "辰" "巽"
      "焚" "扮" "吻" "鮒" "弗" "竪" "辿" "狸" "鱈" "樽"
      "碧" "僻" "頁" "蔽" "糞" "坦" "旦" "歎" "湛" "箪"]
     ;; fjo
     ["娩" "篇" "箆" "蔑" "瞥" "" "" "" "" ""
      "輔" "甫" "圃" "鋪" "鞭" "綻" "耽" "蛋" "檀" "弛"
      "庖" "峯" "呆" "菩" "戊" "智" "蜘" "馳" "筑" "註"
      "蓬" "萌" "烹" "朋" "捧" "酎" "樗" "瀦" "猪" "苧"]
     ;; fjp
     ["鉾" "鵬" "鳳" "鋒" "蜂" "" "" "" "" ""
      "釦" "穆" "睦" "頬" "吠" "凋" "喋" "寵" "帖" "暢"
      "哩" "昧" "幌" "殆" "勃" "牒" "蝶" "諜" "銚" "捗"
      "鱒" "柾" "鮪" "枕" "槙" "椎" "槌" "鎚" "栂" "掴"]
     ;; fja
     ["" "" "" "" "" "蒐" "讐" "蹴" "酋" "什"
      "鰹" "葛" "恰" "鰍" "梶" "戎" "夙" "峻" "竣" "舜"
      "兜" "鞄" "樺" "椛" "叶" "駿" "楯" "淳" "醇" "曙"
      "噛" "鎌" "釜" "蒲" "竃" "渚" "薯" "藷" "恕" "鋤"]
     ;; fjs
     ["" "" "" "" "" "叱" "嫉" "悉" "蔀" "篠"
      "柿" "蛙" "馨" "浬" "骸" "偲" "柴" "屡" "蕊" "縞"
      "撹" "廓" "劃" "鈎" "蛎" "紗" "杓" "灼" "錫" "惹"
      "橿" "樫" "笠" "顎" "赫" "腫" "呪" "綬" "洲" "繍"]
     ;; fjd
     ["" "" "" "" "" "燦" "珊" "纂" "讃" "餐"
      "恢" "廻" "駕" "蛾" "臥" "斬" "仔" "屍" "孜" "斯"
      "凱" "蟹" "芥" "晦" "魁" "獅" "爾" "痔" "而" "蒔"
      "鎧" "蓋" "碍" "崖" "咳" "汐" "鴫" "竺" "宍" "雫"]
     ;; fjf
     ["" "" "" "" "" "埼" "碕" "鷺" "咋" "朔"
      "嘉" "伽" "俺" "牡" "桶" "柵" "窄" "鮭" "笹" "匙"
      "蝦" "茄" "苛" "禾" "珂" "拶" "" "薩" "皐" "鯖"
      "峨" "俄" "霞" "迦" "嘩" "捌" "錆" "鮫" "晒" "撒"]
     ;; fjg
     ["" "" "" "" "" "痕" "艮" "些" "叉" "嵯"
      "艶" "燕" "焔" "掩" "怨" "沙" "瑳" "裟" "坐" "挫"
      "旺" "甥" "鴛" "薗" "苑" "哉" "塞" "采" "犀" "砦"
      "臆" "荻" "鴎" "鴬" "襖" "冴" "阪" "堺" "榊" "肴"]
     ;; fjh
     ["迄" "沫" "俣" "亦" "桝" "" "" "" "" ""
      "蜜" "箕" "蔓" "麿" "侭" "槻" "佃" "柘" "辻" "蔦"
      "牟" "粍" "稔" "蓑" "湊" "綴" "鍔" "椿" "潰" "壷"
      "牝" "姪" "冥" "椋" "鵡" "嬬" "紬" "吊" "剃" "悌"]
     ;; fjj
     ["孟" "摸" "麺" "緬" "棉" "" "" "" "" ""
      "餅" "勿" "杢" "儲" "蒙" "挺" "梯" "汀" "碇" "禎"
      "悶" "貰" "籾" "" "尤" "諦" "蹄" "鄭" "釘" "鼎"
      "弥" "耶" "爺" "冶" "也" "擢" "鏑" "溺" "轍" "填"]
     ;; fjk
     ["佑" "愈" "鑓" "薮" "靖" "" "" "" "" ""
      "涌" "湧" "柚" "揖" "宥" "纏" "甜" "貼" "顛" "澱"
      "傭" "輿" "邑" "祐" "猷" "兎" "堵" "妬" "屠" "杜"
      "蓉" "耀" "熔" "楊" "妖" "菟" "賭" "鍍" "砥" "砺"]
     ;; fjl
     ["螺" "淀" "沃" "慾" "遥" "" "" "" "" ""
      "蘭" "藍" "嵐" "洛" "莱" "塘" "套" "宕" "嶋" "梼"
      "葎" "裡" "璃" "梨" "李" "淘" "涛" "燈" "祷" "董"
      "侶" "琉" "溜" "劉" "掠" "蕩" "鐙" "憧" "撞" "萄"]
     ;; fj;
     ["稜" "瞭" "梁" "凌" "亮" "" "" "" "" ""
      "琳" "燐" "淋" "遼" "諒" "鴇" "涜" "禿" "栃" "橡"
      "嶺" "伶" "瑠" "麟" "鱗" "椴" "鳶" "苫" "寅" "酉"
      "漣" "憐" "苓" "玲" "怜" "瀞" "噸" "惇" "敦" "沌"]
     ;; fjz
     ["" "" "" "" "" "岨" "曾" "曽" "楚" "狙"
      "僅" "粁" "桐" "尭" "饗" "疏" "蘇" "遡" "叢" "爽"
      "禽" "欽" "欣" "錦" "巾" "宋" "匝" "惣" "掻" "槍"
      "玖" "狗" "倶" "衿" "芹" "漕" "痩" "糟" "綜" "聡"]
     ;; fjx
     ["" "" "" "" "" "脊" "蹟" "碩" "蝉" "尖"
      "禦" "鋸" "渠" "笈" "灸" "撰" "栴" "煎" "煽" "穿"
      "匡" "兇" "僑" "侠" "亨" "箭" "羨" "腺" "舛" "詮"
      "蕎" "怯" "彊" "喬" "卿" "賎" "閃" "膳" "糎" "噌"]
     ;; fjc
     ["" "" "" "" "" "諏" "厨" "逗" "翠" "錐"
      "誼" "蟻" "祇" "妓" "亀" "瑞" "嵩" "雛" "椙" "菅"
      "橘" "桔" "吃" "鞠" "掬" "頗" "雀" "裾" "摺" "凄"
      "汲" "仇" "黍" "杵" "砧" "棲" "栖" "醒" "脆" "戚"]
     ;; fjv
     ["" "" "" "" "" "丞" "擾" "杖" "穣" "埴"
      "玩" "巌" "舘" "韓" "諌" "拭" "燭" "蝕" "尻" "晋"
      "伎" "雁" "贋" "翫" "癌" "榛" "疹" "秦" "芯" "塵"
      "徽" "稀" "畿" "毅" "嬉" "壬" "腎" "訊" "靭" "笥"]
     ;; fjb
     ["" "" "" "" "" "哨" "嘗" "妾" "娼" "庄"
      "粥" "萱" "茅" "栢" "鴨" "廠" "捷" "昌" "梢" "樟"
      "桓" "柑" "姦" "侃" "苅" "樵" "湘" "菖" "蒋" "蕉"
      "莞" "翰" "竿" "潅" "澗" "裳" "醤" "鉦" "鍾" "鞘"]
     ;; fjn
     ["呂" "蓮" "聯" "簾" "煉" "" "" "" "" ""
      "弄" "婁" "賂" "櫓" "魯" "遁" "頓" "呑" "那" "乍"
      "聾" "篭" "狼" "牢" "榔" "凪" "薙" "謎" "灘" "捺"
      "倭" "肋" "禄" "麓" "蝋" "鍋" "楢" "馴" "畷" "楠"]
     ;; fjm
     ["亘" "亙" "鷲" "脇" "歪" "" "" "" "" ""
      "椀" "蕨" "藁" "詫" "鰐" "汝" "迩" "匂" "賑" "虹"
      "哺" "刹" "傲" "丼" "碗" "廿" "韮" "濡" "禰" "祢"
      "彙" "毀" "嘲" "嗅" "喩" "葱" "捻" "撚" "廼" "埜"]
     ;; fj,
     ["拉" "憬" "慄" "惧" "恣" "" "" "" "" ""
      "璧" "鬱" "楷" "曖" "摯" "嚢" "膿" "覗" "蚤" "播"
      "羞" "緻" "籠" "箋" "瘍" "杷" "琶" "罵" "芭" "盃"
      "辣" "踪" "貪" "諧" "訃" "牌" "楳" "煤" "狽" "這"]
     ;; fj.
     ["" "" "" "" "錮" "" "" "" "" ""
      "" "" "" "" "" "蝿" "秤" "矧" "萩" "剥"
      "" "" "" "" "" "柏" "箔" "粕" "曝" "莫"
      "" "" "" "" "" "駁" "函" "硲" "箸" "肇"]
     ;; fj/
     ["" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "筈" "櫨" "畠" "溌" "醗"
      "" "" "" "" "" "筏" "鳩" "噺" "塙" "蛤"
      "" "" "" "" "" "隼" "叛" "斑" "氾" "釆"]
     ]
    "宝" "観" "額"
    "初" "技" "黒" "直" "望" "想" "編" "栄" "型" "止"]
   ;; g
   ["" "" "享" "昏" "" "亜" "脱" "暴" "魚" "釈"
    "位" "応" "職" "覚" "球" "豊" "芸" "役" "印" "確"
    "真" "科" "参" "池" "少" "管" "流" "争" "言" "渋"
    "慣" "写" "院" "倉" "元" "消" "仕" "ザ" "誰" "堂"]
   ;; h
   ["盛" "益" "康" "邦" "衆" "" "鼠" "" "" ""
    "給" "分" "7" "き" "上" "美" "宿" "セ" "神" "優"
    "3" "ー" "い" "。" "で" "要" "連" "デ" "車" "主"
    "行" "通" "だ" "新" "事" "支" "先" "調" "組" "銀"]
   ;; j
   ["革" "援" "徒" "舞" "節" "" "曹" "" "" ""
    "員" "よ" "か" "っ" "く" "題" "制" "運" "び" "公"
    "と" "し" "、"
    ;; jf
    [
     ;; jf1
     ["" "" "" "" "！" "１" "" "" "" ""
      "" "" "" "" "" "" "" "┯" "" ""
      "" "" "" "" "" "┠" "" "" "┿" "┨"
      "" "" "" "" "" "" "" "┷" "" ""]
     ;; jf2
     ["" "" "" "" "＠" "２" "" "" "" ""
      "" "" "" "" "" "┏" "" "┳" "" "┓"
      "" "" "" "" "" "┣" "┃" "━" "╋" "┫"
      "" "" "" "" "" "┗" "" "┻" "" "┛"]
     ;; jf3
     ["" "" "" "" "＃" "３" "" "" "" ""
      "" "" "" "" "" "┌" "" "┬" "" "┐"
      "" "" "" "" "" "├" "│" "─" "┼" "┤"
      "" "" "" "" "" "└" "" "┴" "" "┘"]
     ;; jf4
     ["" "" "" "" "＄" "４" "" "" "" ""
      "" "" "" "" "" "" "" "┰" "" ""
      "" "" "" "" "" "┝" "" "" "╂" "┥"
      "" "" "" "" "" "" "" "┸" "" ""]
     ;; jf5
     ["" "" "" "" "％" "５" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""]
     ;; jf6
     ["" "" "" "" "＾" "６" "" "" "" ""
      "￣" "＾" "｀" "゜" "" "" "" "" "" ""
      "＿" "¨" "´" "゛" "" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""]
     ;; jf7
     ["" "" "" "" "＆" "７" "" "" "" ""
      "℃" "″" "′" "°" "Å" "" "" "" "" ""
      "£" "¢" "＄" "￥" "‰" "" "" "" "" ""
      "" "" "" "" "" "" "" "" "" ""]
     ;; jf8
     ["" "" "" "" "＊" "８" "" "" "" ""
      "＜" "≦" "≧" "＞" "≠" "" "" "" "" ""
      "−" "÷" "×" "＋" "＝" "" "" "" "" ""
      "≪" "≒" "≡" "≫" "±" "" "" "" "" ""]
     ;; jf9
     ["" "" "" "" "（" "９" "" "" "" ""
      "√" "♀" "♂" "∴" "∞" "" "" "" "" ""
      "⌒" "⊥" "∠" "∵" "∽" "" "" "" "" ""
      "∬" "∫" "∇" "∂" "∝" "" "" "" "" ""]
     ;; jf0
     ["" "" "" "" "）" "０" "" "" "" ""
      "⊂" "⊆" "⊇" "⊃" "⇒" "" "" "" "" ""
      "∈" "∋" "∩" "∪" "⇔" "" "" "" "" ""
      "∃" "∀" "∧" "∨" "¬" "" "" "" "" ""]
     ;; jfq
     ["" "" "Θ" "" "Ｑ" "ｑ" "" "θ" "" ""
      "冠" "乾" "刈" "且" "轄" "焦" "症" "礁" "祥" "肖"
      "寛" "堪" "喚" "勧" "勘" "衝" "訟" "詔" "鐘" "冗"
      "緩" "汗" "款" "棺" "憾" "剰" "壌" "嬢" "浄" "畳"]
     ;; jfw
     ["" "" "Ω" "" "Ｗ" "ｗ" "" "ω" "" ""
      "嚇" "垣" "該" "涯" "慨" "巡" "遵" "緒" "叙" "徐"
      "隔" "郭" "穫" "獲" "殻" "匠" "升" "召" "奨" "宵"
      "褐" "滑" "渇" "括" "喝" "床" "彰" "抄" "掌" "晶"]
     ;; jfe
     ["" "" "Ε" "" "Ｅ" "ｅ" "" "ε" "" ""
      "蚊" "菓" "箇" "稼" "禍" "醜" "柔" "汁" "獣" "銃"
      "悔" "怪" "塊" "餓" "雅" "叔" "淑" "粛" "塾" "俊"
      "劾" "皆" "拐" "戒" "懐" "准" "循" "旬" "殉" "潤"]
     ;; jfr
     ["" "" "Ρ" "" "Ｒ" "ｒ" "" "ρ" "" ""
      "猿" "煙" "炎" "閲" "謁" "勺" "爵" "酌" "寂" "殊"
      "沖" "翁" "殴" "凹" "鉛" "狩" "珠" "趣" "儒" "囚"
      "架" "寡" "嫁" "佳" "憶" "愁" "臭" "舟" "襲" "酬"]
     ;; jft
     ["" "" "Τ" "" "Ｔ" "ｔ" "" "τ" "" ""
      "緯" "尉" "威" "偉" "握" "諮" "賜" "雌" "侍" "慈"
      "韻" "姻" "芋" "逸" "壱" "璽" "軸" "漆" "疾" "赦"
      "悦" "疫" "鋭" "詠" "渦" "斜" "煮" "遮" "蛇" "邪"]
     ;; jfy
     ["" "" "Ψ" "" "Ｙ" "ｙ" "" "ψ" "" ""
      "沿" "液" "泳" "飲" "暗" "泥" "摘" "滴" "哲" "撤"
      "荷" "歌" "仮" "恩" "往" "迭" "殿" "吐" "塗" "斗"
      "閣" "貝" "絵" "灰" "芽" "奴" "怒" "凍" "唐" "塔"]
     ;; jfu
     ["" "" "Υ" "" "Ｕ" "ｕ" "" "υ" "" ""
      "弓" "吸" "貴" "旗" "机" "悼" "搭" "桃" "棟" "痘"
      "訓" "鏡" "胸" "泣" "救" "筒" "到" "謄" "踏" "透"
      "穴" "潔" "敬" "径" "兄" "騰" "洞" "胴" "峠" "匿"]
     ;; jfi
     ["" "" "Ι" "" "Ｉ" "ｉ" "" "ι" "" ""
      "穀" "鋼" "皇" "孝" "犬" "篤" "凸" "屯" "豚" "曇"
      "枝" "姉" "蚕" "菜" "祭" "鈍" "縄" "軟" "弐" "尿"
      "似" "飼" "詩" "詞" "至" "妊" "忍" "寧" "猫" "粘"]
     ;; jfo
     ["" "" "Ο" "" "Ｏ" "ｏ" "" "ο" "" ""
      "拾" "尺" "謝" "捨" "磁" "悩" "濃" "覇" "婆" "廃"
      "署" "暑" "縮" "祝" "縦" "排" "杯" "輩" "培" "媒"
      "臣" "森" "城" "松" "昭" "賠" "陪" "伯" "拍" "泊"]
     ;; jfp
     ["" "" "Π" "" "Ｐ" "ｐ" "" "π" "" ""
      "舌" "誠" "聖" "晴" "仁" "舶" "薄" "漠" "縛" "肌"
      "像" "祖" "銭" "染" "泉" "鉢" "髪" "罰" "閥" "伴"
      "損" "孫" "束" "息" "臓" "帆" "搬" "畔" "煩" "頒"]
     ;; jfa
     ["" "" "Α" "" "Ａ" "ａ" "" "α" "" ""
      "憩" "契" "傾" "薫" "勲" "措" "疎" "租" "粗" "阻"
      "鶏" "蛍" "茎" "継" "渓" "僧" "双" "喪" "壮" "掃"
      "圏" "剣" "倹" "傑" "鯨" "挿" "槽" "燥" "荘" "葬"]
     ;; jfs
     ["" "" "Σ" "" "Ｓ" "ｓ" "" "σ" "" ""
      "吟" "謹" "襟" "菌" "琴" "拙" "摂" "窃" "仙" "扇"
      "隅" "偶" "虞" "愚" "駆" "栓" "潜" "旋" "薦" "践"
      "桑" "繰" "靴" "掘" "屈" "銑" "漸" "禅" "繕" "塑"]
     ;; jfd
     ["" "" "Δ" "" "Ｄ" "ｄ" "" "δ" "" ""
      "凶" "距" "拠" "拒" "糾" "枢" "据" "澄" "畝" "是"
      "狂" "挟" "恭" "峡" "叫" "姓" "征" "牲" "誓" "逝"
      "斤" "暁" "凝" "仰" "矯" "斉" "隻" "惜" "斥" "籍"]
     ;; jff
     ["" "" "Φ" "" "Ｆ" "ｆ" "" "φ" "" ""
      "儀" "偽" "騎" "飢" "輝" "尋" "尽" "迅" "酢" "吹"
      "犠" "欺" "擬" "戯" "宜" "帥" "" "炊" "睡" "遂"
      "窮" "朽" "虐" "脚" "詰" "酔" "錘" "随" "髄" "崇"]
     ;; jfg
     ["" "" "Γ" "" "Ｇ" "ｇ" "" "γ" "" ""
      "鑑" "貫" "艦" "肝" "缶" "醸" "錠" "嘱" "殖" "辱"
      "幾" "岐" "頑" "陥" "閑" "侵" "唇" "娠" "慎" "浸"
      "軌" "祈" "棄" "棋" "忌" "紳" "薪" "診" "辛" "刃"]
     ;; jfh
     ["" "" "Η" "" "Ｈ" "ｈ" "" "η" "" ""
      "兆" "柱" "宙" "暖" "誕" "蛮" "妃" "扉" "披" "泌"
      "弟" "頂" "腸" "．" "潮" "疲" "碑" "罷" "微" "匹"
      "灯" "刀" "冬" "笛" "敵" "姫" "漂" "苗" "浜" "賓"]
     ;; jfj
     ["" "" "" "" "Ｊ" "ｊ" "" "" "" ""
      "燃" "届" "毒" "銅" "童" "頻" "敏" "瓶" "怖" "扶"
      "拝" "俳" "，" "" "馬" "浮" "符" "腐" "膚" "譜"
      "畑" "麦" "梅" "』" "肺" "賦" "赴" "附" "侮" "封"]
     ;; jfk
     ["" "" "Κ" "" "Ｋ" "ｋ" "" "κ" "" ""
      "肥" "悲" "晩" "飯" "班" "伏" "覆" "沸" "噴" "墳"
      "腹" "貧" "氷" "俵" "鼻" "紛" "雰" "丙" "塀" "幣"
      "墓" "陛" "閉" "粉" "奮" "壁" "癖" "偏" "遍" "穂"]
     ;; jfl
     ["" "" "Λ" "" "Ｌ" "ｌ" "" "λ" "" ""
      "幕" "妹" "牧" "棒" "亡" "慕" "簿" "倣" "俸" "峰"
      "勇" "油" "鳴" "…" "脈" "崩" "抱" "泡" "砲" "縫"
      "覧" "卵" "翌" "幼" "預" "胞" "芳" "褒" "飽" "乏"]
     ;; jf;
     ["" "" "" "" "：" "；" "" "" "" ""
      "零" "隷" "林" "緑" "律" "傍" "剖" "坊" "妨" "帽"
      "劣" "暦" "齢" "麗" "霊" "忙" "冒" "紡" "肪" "膨"
      "炉" "錬" "廉" "裂" "烈" "謀" "僕" "墨" "撲" "朴"]
     ;; jfz
     ["" "" "Ζ" "" "Ｚ" "ｚ" "" "ζ" "" ""
      "傘" "擦" "撮" "錯" "搾" "漬" "坪" "釣" "亭" "偵"
      "嗣" "伺" "暫" "桟" "惨" "貞" "呈" "堤" "廷" "抵"
      "脂" "肢" "紫" "祉" "旨" "締" "艇" "訂" "逓" "邸"]
     ;; jfx
     ["" "" "Ξ" "" "Ｘ" "ｘ" "" "ξ" "" ""
      "鎖" "詐" "唆" "魂" "紺" "弔" "彫" "懲" "挑" "眺"
      "砕" "栽" "彩" "宰" "債" "聴" "脹" "超" "跳" "勅"
      "削" "咲" "剤" "載" "斎" "朕" "珍" "鎮" "陳" "墜"]
     ;; jfc
     ["" "" "Χ" "" "Ｃ" "ｃ" "" "χ" "" ""
      "酵" "郊" "購" "貢" "衡" "胆" "鍛" "壇" "弾" "恥"
      "獄" "酷" "拷" "剛" "項" "痴" "稚" "畜" "逐" "秩"
      "昆" "恨" "婚" "墾" "腰" "窒" "嫡" "抽" "衷" "鋳"]
     ;; jfv
     ["" "" "" "" "Ｖ" "ｖ" "" "" "" ""
      "孔" "坑" "侯" "碁" "悟" "泰" "滞" "胎" "逮" "滝"
      "洪" "控" "拘" "慌" "恒" "卓" "拓" "濯" "託" "諾"
      "肯" "絞" "稿" "硬" "溝" "但" "奪" "棚" "嘆" "淡"]
     ;; jfb
     ["" "" "Β" "" "Ｂ" "ｂ" "" "β" "" ""
      "遣" "軒" "謙" "懸" "堅" "藻" "遭" "霜" "騒" "憎"
      "枯" "弧" "玄" "弦" "幻" "贈" "促" "俗" "賊" "堕"
      "娯" "呉" "鼓" "顧" "雇" "妥" "惰" "駄" "耐" "怠"]
     ;; jfn
     ["" "" "Ν" "" "Ｎ" "ｎ" "" "ν" "" ""
      "漏" "浪" "楼" "廊" "露" "没" "奔" "翻" "凡" "盆"
      "" "湾" "枠" "惑" "賄" "摩" "磨" "魔" "埋" "膜"
      "" "" "" "" "" "抹" "繭" "漫" "魅" "岬"]
     ;; jfm
     ["" "" "Μ" "" "Ｍ" "ｍ" "" "μ" "" ""
      "；" "：" "‥" "｜" "‖" "妙" "眠" "矛" "霧" "婿"
      "〆" "仝" "〃" "／" "＼" "娘" "銘" "滅" "妄" "猛"
      "ヾ" "ヽ" "ゞ" "ゝ" "‐" "盲" "網" "耗" "黙" "紋"]
     ;; jf,
     ["［" "｛" "｝" "］" "＜" "，" "" "" "" ""
      "〔" "【" "】" "〕" "" "匁" "厄" "躍" "柳" "愉"
      "《" "〈" "〉" "》" "『" "癒" "諭" "唯" "幽" "悠"
      "“" "‘" "’" "”" "" "憂" "猶" "裕" "誘" "誉"]
     ;; jf.
     ["＃" "＆" "＊" "＠" "＞" "．" "" "" "" ""
      "♪" "♭" "♯" "†" "‡" "庸" "揚" "揺" "擁" "溶"
      "☆" "△" "□" "○" "◯" "窯" "踊" "抑" "翼" "羅"
      "　" "▽" "◇" "◎" "" "裸" "雷" "酪" "濫" "吏"]
     ;; jf/
     ["" "" "" "" "？" "／" "" "" "" ""
      "←" "↓" "↑" "→" "¶" "痢" "硫" "粒" "隆" "虜"
      "★" "▲" "■" "●" "§" "僚" "涼" "猟" "糧" "陵"
      "〓" "▼" "◆" "※" "〒" "倫" "厘" "塁" "涙" "励"]
     ]
    "は" "設" "鉄" "現" "成" "映"
    "ド" "カ" "り" "」" "田" "協" "多" "混" "選" "以"]
   ;; k
   ["突" "周" "景" "雑" "杉" "" "奏" "" "" ""
    "ど" "ル" "(" "日" "8" "井" "集" "ツ" "打" "品"
    "〇" "た" "の" "0" "に" "水" "教" "エ" "天" "書"
    "円" "社" "—" "9" "会" "用" "商" "ポ" "党" "ヌ"]
   ;; l
   ["温" "域" "処" "漢" "肉" "" "尊" "" "" ""
    "代" "千" "ト" "国" "え" "洋" "安" "特" "勤" "語"
    "て" "一" "5" "・" "な" "藤" "力" "他" "世" "可"
    "小" "野" "め" "子" "前" "表" "ハ" "決" "択" "営"]
   ;; ;
   ["捕" "荒" "ぜ" "緊" "除" ";" "" "" "" ""
    "レ" "ア" "れ" "二" "年" "実" "画" "谷" "ャ" "演"
    "る" "が" "1" "2" "を" "有" "ベ" "度" "文" "へ"
    "ジ" "同" "大" "五" "そ" "正" "交" "ミ" "体" "治"]
   ;; z
   ["" "" "" "" "" "禁" "絹" "批" "就" "綱"
    "欠" "財" "従" "適" "母" "爆" "陽" "ァ" "殺" "券"
    "ヒ" "及" "投" "込" "転" "素" "毛" "等" "板" "伝"
    "ヨ" "判" "済" "説" "休" "図" "之" "州" "例" "字"]
   ;; x
   ["" "" "" "" "" "硝" "被" "慶" "駐" "潟"
    "夏" "針" "骨" "類" "奥" "仲" "構" "導" "負" "悪"
    "江" "久" "義" "沢" "空" "兵" "永" "浅" "客" "庭"
    "誤" "規" "吉" "週" "省" "挙" "末" "払" "満" "材"]
   ;; c
   ["" "" "" "" "" "樹" "源" "渉" "揮" "創"
    "彼" "裏" "厚" "御" "因" "茶" "旅" "認" "何" "秋"
    "別" "蔵" "算" "軍" "性" "専" "申" "頃" "師" "課"
    "証" "感" "ゆ" "号" "央" "険" "ぼ" "乗" "津" "過"]
   ;; v
   ["" "" "" "" "" "句" "願" "竜" "丹" "背"
    "妻" "居" "顔" "宇" "酒" "率" "施" "健" "履" "非"
    "考" "早" "半" "青" "使" "親" "袋" "落" "税" "着"
    "含" "値" "器" "葉" "福" "ゼ" "街" "庫" "準" "諸"]
   ;; b
   ["" "" "" "" "" "礎" "臨" "併" "鮮" "皮"
    "善" "差" "量" "推" "伸" "比" "曜" "尾" "般" "便"
    "権" "造" "県" "清" "級" "寮" "良" "命" "飛" "坂"
    "%" "ギ" "照" "派" "毎" "波" "免" "状" "遊" "単"]
   ;; n
   ["依" "織" "譲" "激" "測" "" "" "" "" ""
    "相" "付" "内" "九" "サ" "昔" "遠" "序" "耳" "示"
    "ッ" "ロ" "ん" "け" "業" "ホ" "私" "村" "ノ" "近"
    "海" "当" "不" "委" "気" "ヤ" "再" "団" "戸" "身"]
   ;; m
   ["繊" "父" "ヘ" "干" "血" "" "" "" "" ""
    "家" "プ" "工" "名" "建" "短" "ォ" "振" "授" "即"
    "人" "ク" "ま" "イ" "時" "共" "ゴ" "ガ" "完" "外"
    "道" "理" "合" "化" "売" "心" "ネ" "計" "ひ" "ピ"]
   ;; ,
   ["借" "枚" "模" "彦" "散" "," "" "" "" ""
    "的" "ば" "八" "川" "パ" "岩" "将" "練" "版" "難"
    "三" "万" "ン" "す" "「" "ブ" "来" "製" "重" "米"
    "ず" "メ" "面" "ビ" "下" "界" "〜" "夫" "ょ" "勝"]
   ;; .
   ["須" "乱" "降" "均" "笑" "." "" "" "" ""
    "対" "ュ" "テ" "機" "第" "巨" "ぞ" "念" "効" "普"
    "京" "方" "つ" "電" "長" "平" "信" "校" "約" "ョ"
    "西" "ウ" "政" "目" "都" "意" "口" "食" "価" "反"]
   ;; /
   ["訳" "香" "走" "又" "弁" "/" "" "" "" ""
    "歴" "作" "見" "チ" "入" "敗" "塚" "働" "視" "辺"
    "ち" "フ" "四" "地" "み" "楽" "午" "ご" "各" "光"
    "げ" "グ" "オ" "市" "株" "今" "台" "総" "与" "ズ"]
   ]
  "*TT-code table.")

;;
;; Variables
;;

(defvar ttt--state nil "State of TT-code decoder.")
(make-variable-buffer-local 'ttt--state)

;;
;; Functions
;;

(defun ttt--index (str substr)
  "Return index of start of first match for SUBSTR in STR, or nil."
  (let ((case-fold-search nil))
    (string-match-p (regexp-quote substr) str)))

(defun ttt--reset()
  "Reset state of TT-code decoder."
  (setq ttt--state ttt-table))

(defun ttt--trans (c)
  "Take character C as input to TT-code decoder, get output and return it."
  (let* ((ch (string c))
         (k (ttt--index ttt-keys ch)))
    (if (or (null k) (not (arrayp ttt--state)) (<= (length ttt--state) k))
        (progn (ttt--reset) ch)
      (setq ttt--state (aref ttt--state k))
      (cond ((stringp ttt--state) (prog1 ttt--state (ttt--reset)))
            ((vectorp ttt--state) "")
            (t (progn (ttt--reset) ""))))))

(defun ttt--decode-string (str)
  "Decode TT-code string STR to Japanese string."
  (ttt--reset)
  (mapconcat 'ttt--trans (string-to-list str) ""))

(defun ttt--decode-substring (str)
  "Scan backward in STR, skipping tail (i.e. non-TT-code string) and then
getting body (i.e. TT-code string), decode body and return resulting
list (DECODED-BODY BODY-LEN TAIL-LEN)."
  (let* ((keys (string-to-list ttt-keys))
         (ls (string-to-list str))
         (len (length ls))
         (i (1- len))
         j dst)
    (while (and (<= 0 i) (not (memq (nth i ls) keys)))
      (setq i (1- i)))
    (setq j i)
    (while (and (<= 0 j) (memq (nth j ls) keys))
      (setq j (1- j)))
    (setq dst (ttt--decode-string (substring str (1+ j) (1+ i))))
    (if (and (<= 0 j) (= (nth j ls) ttt-delimiter))
        (setq j (1- j)))
    (list dst (- i j) (- len i 1))))

;;;###autoload
(defun ttt-do-ttt ()
  "Do ttt."
  (interactive)
  (let* ((src (buffer-substring (point-at-bol) (point)))
         (len (length src))
         (ls (ttt--decode-substring src))
         (dst (car ls))
         (body-len (car (cdr ls)))
         (tail-len (car (cdr (cdr ls))))
         (end (- (point) tail-len))
         (beg (- end body-len)))
    (save-excursion
      (goto-char beg)
      (insert dst)
      (delete-region (point) (+ (point) body-len)))))

;;;###autoload
(defun ttt-isearch-do-ttt ()
  "Do ttt in isearch."
  (interactive)
  (let* ((i 0)
         (src isearch-string)
         (len (length src))
         (ls (ttt--decode-substring src))
         (dst (car ls))
         (body-len (car (cdr ls)))
         (tail-len (car (cdr (cdr ls))))
         (n (+ body-len tail-len)))
    (while (< i n)
      (isearch-pop-state)
      (setq i (1+ i)))
    (setq i 0)
    (while (< i (length dst))
      (isearch-process-search-string (substring dst i (1+ i))
                                     (substring dst i (1+ i)))
      (setq i (1+ i)))
    (setq i (- len tail-len))
    (while (< i len)
      (isearch-process-search-string (substring src i (1+ i))
                                     (substring src i (1+ i)))
      (setq i (1+ i)))))

;;
;; ttt-jump-to-char-forward, ttt-jump-to-char-backward
;;

(defun ttt--read-char (prompt)
  "Read TT-code keys until they make a valid code, and return decoded character."
  (let (c ch)
    (ttt--reset)
    (while (string-equal "" (setq ch (ttt--trans (setq c (read-char prompt)))))
      (setq prompt (concat prompt (string c) " ")))
    ch))

(defvar ttt-jump--char nil "Last searched character.")
(defvar ttt-jump--sign 1 "Search direction; 1 for forward, -1 for backward.")

(defun ttt-jump--sub (char count)
  (let ((case-fold-search nil)
        (forward-move nil))
    (cond
     ((<= 0 count)
      (when (eq (char-after (point)) char)
        (forward-char)
        (setq forward-move t))
      (when (or (search-forward (char-to-string char) nil t count)
                forward-move)
        (backward-char))
      (setq ttt-jump--char char))
     ((< count 0)
      (search-backward (char-to-string char) nil t (- count))
      (setq ttt-jump--char char)))))

(defun ttt-jump--repeat (count)
  (cond
   ((eq nil ttt-jump--char) (message "No jump yet"))
   (t (ttt-jump--sub ttt-jump--char count)
      (message "Repeating jump to char %c" ttt-jump--char))))

(defun ttt-jump--main (count jump jump-reverse sign prompt prompt-ttt)
  (let ((mark-set nil) char)
    (cond ((eq last-command jump)
           (ttt-jump--repeat (* ttt-jump--sign count)))
          ((eq last-command jump-reverse)
           (setq ttt-jump--sign (- ttt-jump--sign))
           (ttt-jump--repeat (* ttt-jump--sign count)))
          (t
           (setq ttt-jump--sign sign)
           (unless mark-active (push-mark) (setq mark-set t))
           (setq char (read-char (propertize prompt 'face 'minibuffer-prompt)))
           (if (eq char ?\C-m)
               (setq char (string-to-char
                           (ttt--read-char
                            (propertize prompt-ttt 'face 'minibuffer-prompt)))))
           (if (not (characterp char))  ; XXX: command call key as input char
               (ttt-jump--repeat (* ttt-jump--sign count))
             (ttt-jump--sub char (* ttt-jump--sign count)))))
    (if (< count 0) (setq ttt-jump--sign (- ttt-jump--sign)))
    (if mark-set (message "Mark set"))))

;;;###autoload
(defun ttt-jump-to-char-forward (&optional count)
  "Jump forward to COUNT-th occurrence of input char.
Repeat use of this command repeats last jump.
If input char is RET (or C-m), then read TT-code keys as input char."
  (interactive "p")
  (ttt-jump--main count 'ttt-jump-to-char-forward 'ttt-jump-to-char-backward
                  1 "Jump to char: " "Jump to char by ttt: "))

;;;###autoload
(defun ttt-jump-to-char-backward (&optional count)
  "Jump backward to COUNT-th occurrence of input char.
Repeat use of this command repeats last jump.
If input char is RET (or C-m), then read TT-code keys as input char."
  (interactive "p")
  (ttt-jump--main count 'ttt-jump-to-char-backward 'ttt-jump-to-char-forward
                  -1 "Back to char: " "Back to char by ttt: "))

(provide 'ttt)

;;; ttt.el ends here
