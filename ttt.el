;;; ttt.el --- Tiny TT-code Translation              -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2022  YUSE Yosihiro

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

;; ttt is another modeless Japanese input for Emacs, or a simple and
;; minimal implementation of TT-code, one of kanji direct input
;; methods.

;; ttt provides an easy way of inputting English-Japanese mixed text
;; without switching modes.  Inputting Japanese characters is done by
;; means of simple decode of TT-code, rather than complex process such
;; as in kana-kanji conversion.

;; Setup:

;; (require 'ttt)
;; (define-key global-map (kbd "M-j") 'ttt-do-ttt)
;; (define-key isearch-mode-map (kbd "M-j") 'ttt-isearch-do-ttt)
;; (define-key isearch-mode-map (kbd "M-t") 'ttt-isearch-toggle-ttt)
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
;; Japanese text.

;; TT-code string scanning goes backward from the cursor to the
;; beginning of the line, or until a white space, any non-TT-code
;; character or `ttt-delimiter' found.  `ttt-delimiter' is removed
;; after decode.

;;; Code:

;;; ttt

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

(defcustom ttt-delimiter ":"
  "Delimiter between TT-code and non-TT-code text."
  :type 'string
  :group 'ttt)

(defcustom ttt-remove-space nil
  "*If non-nil, remove space between alphanumeric string and Japanese string."
  :type 'boolean
  :group 'ttt)

(defcustom ttt-remove-space-regexp
  ;; "[a-zA-Z0-9/]$"
  ;; "*Space after string matching the regexp is removed when `ttt-remove-space'."
  "[a-zA-Z0-9/]"
  "*Space after string ending with the regexp is removed when `ttt-remove-space'."
  :type 'regexp
  :group 'ttt)

;;
;; ttt-table
;;

(defvar ttt--tta "
*********************鄙***蛛**************
***************************************甦
**********瑕鴉**賽*******瞰嵌*匣**儚*礫****藪*哭？*
*************遽*****寃*******聘*靄*********筐
************辟痺*************悸************
********************櫻*******************
********************漱*****皺******訝疇*****
**********痰***焉輻****************熾*******
*******************蟲**佛**眈********翳*****
***********舊*絋*饅**痒******杞*祀爬*****靡*****
**隗**********颯****癪********峙佇****讀*孵檻*揄*
**墟**禮*躊****懺偕*凭**逞******疼*躓************
**鋏**躇******魎*莉*********廣*媚*************
*渕*****澹***釉*********彿****膀***橙*****燵***
*******炒****！*揶********螢*僭*條**褪*********
*********爛***珈****檬*鍮*謳嗚*****麒**薔*******
竟*****彗*孕朧徊**********薇４琥****罠**靱*癇*翅****
***軋**晰*茹**區*****雉**魍*）６***********涸*奢**
毯**惠**韆**鑪嶌*揉撥*******贄卍學*繹******經*絨*****
***********沐***譚****掟**朦***嬌*罹仄黎**乖*****
**綺*****稟***餡******蛉*咤***********餉******
*****嘔*****凰**珀聲*鑽****埃****嗜*****俯******
*********焙**抒棘********闊*咥*幇**********囮**
**********鵺*****昴**愕*聚**************騙榮*址
*****亞*謗*笏*應*攪毬*藝***眞*滲*椒**諍曰澁*********瞠
**熙***竄*****７**躾蹙***３*****翹頽**徨誦********
**********隕***櫟***壜**栞****鐵*戌蠅**菻*******
*蜩*********几（*８穽***顰***０*冰**誅*圓*―９會*裔***
*閾咎灌*********國****菫**弌５*舐籐**泄*****剪*****
**********檸*斂****谺**縷姜１２****憫*******狡*躰*
*****噤絣砒****樅*苺***戮*琲*****橇****拌齊*******
*****逍****棗*猾********柩*澤***淺********茉祓**
********渾**********鞦**簒暈**呻***證****瞼*冪*蝸
*****鉤***徘***紆***腱*翡敲棹絆蜻***烙贅膠頷*********
*******屏****寥**屁櫂蠍***捏***繚**翔*％*煌**瀾**游單
憑**********苻****遙*囁****貶****瑙*濱*胚矮***欒**
************肛*********儘**********訛賣***檜*
****************埓****萬*****燻踵**瑪****～*拗*
***************炬*唸***彷壺**苹*******眩**呟**芻
****辯*****轢*睨****慟*邊魑****樂***胱**********
" "TT-code assoc table in string format.")

(defvar ttt--ttl "
****************************************
****************************************
**********ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ******
**********ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ****
ЭЮЯ*******АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬ
эюя*******абвгдеёжзийклмнопрстуфхцчшщъыь
**********ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ****
**********αβγδεζηθικλμνξοπρστυφχψω******
****************************************
****************************************
*****肱腔膏砿閤叡餌荏云噂鴻劫壕濠轟穎盈瑛洩嬰麹鵠漉甑忽奄堰厭榎頴惚狛此坤梱
*****梧檎瑚醐鯉窺鵜卯迂烏佼倖勾喉垢蔚欝唄嘘碓宏巷庚昂晃閏瓜厩姥鰻杭梗浩糠紘
*****捲硯鍵鹸絃郁亥謂萎畏舷諺乎姑狐允鰯茨溢磯糊袴股胡菰吋蔭胤淫咽虎跨鈷伍吾
*****袈祁圭珪慧鮎綾絢飴虻桂畦稽繋罫闇按袷粟或荊詣頚戟隙椅惟夷杏鞍桁訣倦喧拳
*****矩躯駈駒喰葵姶挨娃唖寓串櫛釧屑葦旭渥穐茜窟沓轡窪熊姐斡梓鯵芦隈粂栗鍬卦
庇匪蕃磐挽*****簸樋誹緋斐蒼鎗捉袖其柊眉琵毘枇揃遜汰唾柁肘膝髭疋稗舵楕陀騨堆
媛桧逼畢弼*****廟豹瓢彪謬岱戴腿苔黛鰭蛭蒜鋲錨鯛醍鷹瀧啄冨埠瀕斌彬托琢鐸茸凧
葡撫阜芙斧*****淵蕗葺楓蕪蛸只叩辰巽焚扮吻鮒弗竪辿狸鱈樽碧僻頁蔽糞坦旦歎湛箪
娩篇箆蔑瞥*****輔甫圃鋪鞭綻耽蛋檀弛庖峯呆菩戊智蜘馳筑註蓬萌烹朋捧酎樗瀦猪苧
鉾鵬鳳鋒蜂*****釦穆睦頬吠凋喋寵帖暢哩昧幌殆勃牒蝶諜銚捗鱒柾鮪枕槙椎槌鎚栂掴
*****蒐讐蹴酋什鰹葛恰鰍梶戎夙峻竣舜兜鞄樺椛叶駿楯淳醇曙噛鎌釜蒲竃渚薯藷恕鋤
*****叱嫉悉蔀篠柿蛙馨浬骸偲柴屡蕊縞撹廓劃鈎蛎紗杓灼錫惹橿樫笠顎赫腫呪綬洲繍
*****燦珊纂讃餐恢廻駕蛾臥斬仔屍孜斯凱蟹芥晦魁獅爾痔而蒔鎧蓋碍崖咳汐鴫竺宍雫
*****埼碕鷺咋朔嘉伽俺牡桶柵窄鮭笹匙蝦茄苛禾珂拶*薩皐鯖峨俄霞迦嘩捌錆鮫晒撒
*****痕艮些叉嵯艶燕焔掩怨沙瑳裟坐挫旺甥鴛薗苑哉塞采犀砦臆荻鴎鴬襖冴阪堺榊肴
迄沫俣亦桝*****蜜箕蔓麿侭槻佃柘辻蔦牟粍稔蓑湊綴鍔椿潰壷牝姪冥椋鵡嬬紬吊剃悌
孟摸麺緬棉*****餅勿杢儲蒙挺梯汀碇禎悶貰籾*尤諦蹄鄭釘鼎弥耶爺冶也擢鏑溺轍填
佑愈鑓薮靖*****涌湧柚揖宥纏甜貼顛澱傭輿邑祐猷兎堵妬屠杜蓉耀熔楊妖菟賭鍍砥砺
螺淀沃慾遥*****蘭藍嵐洛莱塘套宕嶋梼葎裡璃梨李淘涛燈祷董侶琉溜劉掠蕩鐙憧撞萄
稜瞭梁凌亮*****琳燐淋遼諒鴇涜禿栃橡嶺伶瑠麟鱗椴鳶苫寅酉漣憐苓玲怜瀞噸惇敦沌
*****岨曾曽楚狙僅粁桐尭饗疏蘇遡叢爽禽欽欣錦巾宋匝惣掻槍玖狗倶衿芹漕痩糟綜聡
*****脊蹟碩蝉尖禦鋸渠笈灸撰栴煎煽穿匡兇僑侠亨箭羨腺舛詮蕎怯彊喬卿賎閃膳糎噌
*****諏厨逗翠錐誼蟻祇妓亀瑞嵩雛椙菅橘桔吃鞠掬頗雀裾摺凄汲仇黍杵砧棲栖醒脆戚
*****丞擾杖穣埴玩巌舘韓諌拭燭蝕尻晋伎雁贋翫癌榛疹秦芯塵徽稀畿毅嬉壬腎訊靭笥
*****哨嘗妾娼庄粥萱茅栢鴨廠捷昌梢樟桓柑姦侃苅樵湘菖蒋蕉莞翰竿潅澗裳醤鉦鍾鞘
呂蓮聯簾煉*****弄婁賂櫓魯遁頓呑那乍聾篭狼牢榔凪薙謎灘捺倭肋禄麓蝋鍋楢馴畷楠
亘亙鷲脇歪*****椀蕨藁詫鰐汝迩匂賑虹哺刹傲丼碗廿韮濡禰祢彙毀嘲嗅喩葱捻撚廼埜
拉憬慄惧恣*****璧鬱楷曖摯嚢膿覗蚤播羞緻籠箋瘍杷琶罵芭盃辣踪貪諧訃牌楳煤狽這
****錮**********蝿秤矧萩剥*****柏箔粕曝莫*****駁函硲箸肇
***************筈櫨畠溌醗*****筏鳩噺塙蛤*****隼叛斑氾釆
" "TT-code left table in string format.")

(defvar ttt--ttr "
****！１***********┯*******┠**┿┨*******┷**
****＠２*********┏*┳*┓*****┣┃━╋┫*****┗*┻*┛
****＃３*********┌*┬*┐*****├│─┼┤*****└*┴*┘
****＄４***********┰*******┝**╂┥*******┸**
****％５**********************************
****＾６****￣＾｀゜******＿¨´゛****************
****＆７****℃″′°Å*****£¢＄￥‰***************
****＊８****＜≦≧＞≠*****−÷×＋＝*****≪≒≡≫±*****
****（９****√♀♂∴∞*****⌒⊥∠∵∽*****∬∫∇∂∝*****
****）０****⊂⊆⊇⊃⇒*****∈∋∩∪⇔*****∃∀∧∨¬*****
**Θ*Ｑｑ*θ**冠乾刈且轄焦症礁祥肖寛堪喚勧勘衝訟詔鐘冗緩汗款棺憾剰壌嬢浄畳
**Ω*Ｗｗ*ω**嚇垣該涯慨巡遵緒叙徐隔郭穫獲殻匠升召奨宵褐滑渇括喝床彰抄掌晶
**Ε*Ｅｅ*ε**蚊菓箇稼禍醜柔汁獣銃悔怪塊餓雅叔淑粛塾俊劾皆拐戒懐准循旬殉潤
**Ρ*Ｒｒ*ρ**猿煙炎閲謁勺爵酌寂殊沖翁殴凹鉛狩珠趣儒囚架寡嫁佳憶愁臭舟襲酬
**Τ*Ｔｔ*τ**緯尉威偉握諮賜雌侍慈韻姻芋逸壱璽軸漆疾赦悦疫鋭詠渦斜煮遮蛇邪
**Ψ*Ｙｙ*ψ**沿液泳飲暗泥摘滴哲撤荷歌仮恩往迭殿吐塗斗閣貝絵灰芽奴怒凍唐塔
**Υ*Ｕｕ*υ**弓吸貴旗机悼搭桃棟痘訓鏡胸泣救筒到謄踏透穴潔敬径兄騰洞胴峠匿
**Ι*Ｉｉ*ι**穀鋼皇孝犬篤凸屯豚曇枝姉蚕菜祭鈍縄軟弐尿似飼詩詞至妊忍寧猫粘
**Ο*Ｏｏ*ο**拾尺謝捨磁悩濃覇婆廃署暑縮祝縦排杯輩培媒臣森城松昭賠陪伯拍泊
**Π*Ｐｐ*π**舌誠聖晴仁舶薄漠縛肌像祖銭染泉鉢髪罰閥伴損孫束息臓帆搬畔煩頒
**Α*Ａａ*α**憩契傾薫勲措疎租粗阻鶏蛍茎継渓僧双喪壮掃圏剣倹傑鯨挿槽燥荘葬
**Σ*Ｓｓ*σ**吟謹襟菌琴拙摂窃仙扇隅偶虞愚駆栓潜旋薦践桑繰靴掘屈銑漸禅繕塑
**Δ*Ｄｄ*δ**凶距拠拒糾枢据澄畝是狂挟恭峡叫姓征牲誓逝斤暁凝仰矯斉隻惜斥籍
**Φ*Ｆｆ*φ**儀偽騎飢輝尋尽迅酢吹犠欺擬戯宜帥*炊睡遂窮朽虐脚詰酔錘随髄崇
**Γ*Ｇｇ*γ**鑑貫艦肝缶醸錠嘱殖辱幾岐頑陥閑侵唇娠慎浸軌祈棄棋忌紳薪診辛刃
**Η*Ｈｈ*η**兆柱宙暖誕蛮妃扉披泌弟頂腸．潮疲碑罷微匹灯刀冬笛敵姫漂苗浜賓
****Ｊｊ****燃届毒銅童頻敏瓶怖扶拝俳，*馬浮符腐膚譜畑麦梅』肺賦赴附侮封
**Κ*Ｋｋ*κ**肥悲晩飯班伏覆沸噴墳腹貧氷俵鼻紛雰丙塀幣墓陛閉粉奮壁癖偏遍穂
**Λ*Ｌｌ*λ**幕妹牧棒亡慕簿倣俸峰勇油鳴…脈崩抱泡砲縫覧卵翌幼預胞芳褒飽乏
****：；****零隷林緑律傍剖坊妨帽劣暦齢麗霊忙冒紡肪膨炉錬廉裂烈謀僕墨撲朴
**Ζ*Ｚｚ*ζ**傘擦撮錯搾漬坪釣亭偵嗣伺暫桟惨貞呈堤廷抵脂肢紫祉旨締艇訂逓邸
**Ξ*Ｘｘ*ξ**鎖詐唆魂紺弔彫懲挑眺砕栽彩宰債聴脹超跳勅削咲剤載斎朕珍鎮陳墜
**Χ*Ｃｃ*χ**酵郊購貢衡胆鍛壇弾恥獄酷拷剛項痴稚畜逐秩昆恨婚墾腰窒嫡抽衷鋳
****Ｖｖ****孔坑侯碁悟泰滞胎逮滝洪控拘慌恒卓拓濯託諾肯絞稿硬溝但奪棚嘆淡
**Β*Ｂｂ*β**遣軒謙懸堅藻遭霜騒憎枯弧玄弦幻贈促俗賊堕娯呉鼓顧雇妥惰駄耐怠
**Ν*Ｎｎ*ν**漏浪楼廊露没奔翻凡盆*湾枠惑賄摩磨魔埋膜*****抹繭漫魅岬
**Μ*Ｍｍ*μ**；：‥｜‖妙眠矛霧婿〆仝〃／＼娘銘滅妄猛ヾヽゞゝ‐盲網耗黙紋
［｛｝］＜，****〔【】〕*匁厄躍柳愉《〈〉》『癒諭唯幽悠“‘’”*憂猶裕誘誉
＃＆＊＠＞．****♪♭♯†‡庸揚揺擁溶☆△□○◯窯踊抑翼羅　▽◇◎*裸雷酪濫吏
****？／****←↓↑→¶痢硫粒隆虜★▲■●§僚涼猟糧陵〓▼◆※〒倫厘塁涙励
" "TT-code right table in string format.")

(defvar ttt--ttc "
**********ヲゥヴヂヅ簡承快包唱ぱぴぷぺぽ朱陣眼執岳ぁぃぅぇぉ欲迫留替還
**********哀逢宛囲庵徴章否納暮慰為陰隠胃遅鶴繁紹刑*****巣災列沼更
**********暇牙壊較寒触候歯頼憲我掛敢甘患甲鹿誌夢弱瓦****茂恋刻?占
**※*******啓掲携劇賢宗途筆逃勉兼嫌顕牽厳致貨招卸雲*****述脳豆辞箱
**********把伐避卑藩植複里寝罪菱紐描憤弊汎絡季阿窓*****朗老看献矢
**********酸貿攻盤汽*****桜典採君犯*****呼紀房去秒*****
**********昼捜焼帯換索冊皿賛*瀬博謡純余衰趨垂粋寸幅破績疑範*****
**********炭異闘易延射需輯瞬盾鳥筋希副堀滋湿甚*瞳歓郡識ぢ核*****
**********稲隣奈速雪濁詑蓄貯虫催忠仏盟肩沈添徹爪陶功抗属綿影*****
**********湯旧夕拡互慢迷戻羊*障乳察標療己已巳巴*盗幡衣離麻*****
ヮ丑鬼孤奉湖端刷震弘果概武風細害撃浴積故収若指ぎ思病常寺停河徳械帝読族帰監竹ゅ志
ヰ臼虚誇某礼飾寿扱痛告買残階古賃折秀程鉱際雄氏格術終張質領置渡刊始鈴丁庁寄注修抜
ヱ宴狭黄貌著郵順片票策詳両能利整追糸断提太査丸次広起薬づ容供守訪了恐未昨裁介究航
ヵ縁脅后卜移塩危札訴首由在論ペ軽隊春低児園ふ続習門路防港玉試登融極督才跡達具答層
ヶ曳驚耕*郷群砂乞遺農死!増ゃ評角幸減敷船賞ェ火聞越得条右席退雨熱況返ゲ芝失養深
請尚舎布姿**庶*欄歩キやコナ佐接記モ無中わうあ本むケ話べ期店全バ後問洗響司復担
境賀喜苦絶*星粧乃龍回せ出山金法備朝資石スラ4こさ南式座民ゾ持じ部間ム羽忘迎並陸
系岸幹圧密*析丈如略務区タ者マ数最知士屋も東)6ら原戦線ソ歳町自六場七個討華浦巻
探責丘恵秘*遷称尼慮島百手発和郎急ワ費解お生十学高駅関ダ点強所議経ニ住医史許ユ競
象漁糖固押*宣蒸帳累開木保立女談験送ィ募定ろリ月シ物男橋遇係ほ明動産北静環補冷護
ゎ於奇巧*償紅舗輪則報音案横崎服変限逆令種宅料受英勢輸基足婦件宮局向割億色左ぬ根
ゐ汚既克*欧傷充倒存紙王曲興白声審研企違岡熟土予ボ必形好草段友伊頭府ぶ録貸態展様
ゑ乙菊懇*努豪喫操倍館放情刺ぐ任改労精装結待活切加講助味築衛卒求配富番赤販花警独
*穏却困*底維腕柄牛夜々引側官検昇統ざ然進取ね育室愛▽宝観額初技黒直望想編栄型止
**享昏*亜脱暴魚釈位応職覚球豊芸役印確真科参池少管流争言渋慣写院倉元消仕ザ誰堂
盛益康邦衆*鼠***給分7き上美宿セ神優3ーい。で要連デ車主行通だ新事支先調組銀
革援徒舞節*曹***員よかっく題制運び公とし、▲は設鉄現成映ドカり」田協多混選以
突周景雑杉*奏***どル(日8井集ツ打品〇たの0に水教エ天書円社—9会用商ポ党ヌ
温域処漢肉*尊***代千ト国え洋安特勤語て一5・な藤力他世可小野め子前表ハ決択営
捕荒ぜ緊除;****レアれ二年実画谷ャ演るが12を有ベ度文へジ同大五そ正交ミ体治
*****禁絹批就綱欠財従適母爆陽ァ殺券ヒ及投込転素毛等板伝ヨ判済説休図之州例字
*****硝被慶駐潟夏針骨類奥仲構導負悪江久義沢空兵永浅客庭誤規吉週省挙末払満材
*****樹源渉揮創彼裏厚御因茶旅認何秋別蔵算軍性専申頃師課証感ゆ号央険ぼ乗津過
*****句願竜丹背妻居顔宇酒率施健履非考早半青使親袋落税着含値器葉福ゼ街庫準諸
*****礎臨併鮮皮善差量推伸比曜尾般便権造県清級寮良命飛坂%ギ照派毎波免状遊単
依織譲激測*****相付内九サ昔遠序耳示ッロんけ業ホ私村ノ近海当不委気ヤ再団戸身
繊父ヘ干血*****家プ工名建短ォ振授即人クまイ時共ゴガ完外道理合化売心ネ計ひピ
借枚模彦散,****的ば八川パ岩将練版難三万ンす「ブ来製重米ずメ面ビ下界〜夫ょ勝
須乱降均笑.****対ュテ機第巨ぞ念効普京方つ電長平信校約ョ西ウ政目都意口食価反
訳香走又弁/****歴作見チ入敗塚働視辺ちフ四地み楽午ご各光げグオ市株今台総与ズ
" "TT-code center table in string format.")

(defun ttt--make-subtable (ttx fn)
  "Make 40x40 array subtable from string TTX with function FN on each element."
  (save-match-data
    (cl-map 'vector
       (lambda (str) (cl-map 'vector fn (split-string str "" t)))
       (split-string ttx "\n" t "\n"))))

(defun ttt--make-table ()
  "Make TT-code table."
  (let* ((tta (ttt--make-subtable ttt--tta
                                  (lambda (ch) (if (string= ch "*") "" ch))))
         (ttl (ttt--make-subtable ttt--ttl
                                  (lambda (ch) (if (string= ch "*") "" ch))))
         (ttr (ttt--make-subtable ttt--ttr
                                  (lambda (ch) (if (string= ch "*") "" ch))))
         (ttc (ttt--make-subtable ttt--ttc
                                  (lambda (ch)
                                    (cond ((string= ch "*") "")
                                          ((string= ch "※") tta)
                                          ((string= ch "▽") ttl)
                                          ((string= ch "▲") ttr)
                                          (t ch))))))
    ttc))

(defvar ttt-table (ttt--make-table)
  "TT-code table.")

;;
;; Variables
;;

(defvar ttt--state nil "State of TT-code decoder.")
(make-variable-buffer-local 'ttt--state)

(defvar ttt--remain nil "Remain of TT-code decode.")
(make-variable-buffer-local 'ttt--remain)

;;
;; Functions
;;

(defun ttt--index (str substr)
  "Return index of start of first match for SUBSTR in STR, or nil."
  (let ((case-fold-search nil))
    (string-match-p (regexp-quote substr) str)))

(defun ttt--string-to-keyseq (str)
  "Return list of key numbers for string STR."
  (mapcar (apply-partially #'ttt--index ttt-keys) (split-string str "" t)))

(defun ttt--keyseq-to-string (keyseq)
  "Return string for KEYSEQ, list of key numbers."
  (mapconcat (lambda (k) (substring ttt-keys k (1+ k))) keyseq ""))

(defun ttt--reset()
  "Reset state of TT-code decoder."
  (setq ttt--remain "")
  (setq ttt--state ttt-table))

(defun ttt--trans (c &optional prefer-remain)
  "Take character C as input to TT-code decoder, get output and return it.
If PREFER-REMAIN is non-nil and result is empty string, output remain instead."
  (let* ((ch (string c))
         (k (ttt--index ttt-keys ch)))
    (if (or (null k) (not (arrayp ttt--state)) (<= (length ttt--state) k))
        (prog1 (concat ttt--remain ch) (ttt--reset))
      (setq ttt--remain (concat ttt--remain ch))
      (setq ttt--state (aref ttt--state k))
      (cond ((and prefer-remain (stringp ttt--state) (string= ttt--state ""))
             (prog1 ttt--remain (ttt--reset)))
            ((stringp ttt--state) (prog1 ttt--state (ttt--reset)))
            ((vectorp ttt--state) "")
            (t (progn (ttt--reset) ""))))))

(defun ttt--decode-string (str)
  "Decode TT-code string STR to Japanese string."
  (ttt--reset)
  (mapconcat 'ttt--trans (string-to-list str) ""))

(defun ttt--regexp-opt-charset-complemented (chars)
  "Return a regexp to match a character not in CHARS."
  (save-match-data
    (let ((str (regexp-opt-charset chars)))
      (if (not (string-match "^\\(\\[\\)\\(.+\\)\\(]\\)$" str))
        (error "Unexpected regexp: %s" str) ; XXX
      (concat (match-string 1 str)
              "^"
              (match-string 2 str)
              (match-string 3 str))))))

(defun ttt--make-pattern ()
  "Return regexp for extracting HEAD, SRC, BODY, and TAIL for ttt."
  (concat "\\(.*?\\)\\(\\("
          (regexp-quote ttt-delimiter)
          "\\)?\\("
          (regexp-opt-charset (string-to-list ttt-keys))
          "+\\)\\)\\("
          (ttt--regexp-opt-charset-complemented (string-to-list ttt-keys))
          "*\\)$"))

(defun ttt--backward (str)
  "Scan TT-code backward in STR and decode it.
Scanning is done with skipping tail (i.e. non-TT-code string) and then
getting body (i.e. TT-code string).
Return resulting list (DECODED-BODY BODY-LEN TAIL-LEN)."
  (save-match-data
    (let ((case-fold-search nil))
      (if (not (string-match (ttt--make-pattern) str))
          (list "" 0 0)
        (let* ((head (match-string-no-properties 1 str))
               (src (match-string-no-properties 2 str))
               (delimiter (match-string-no-properties 3 str))
               (body (match-string-no-properties 4 str))
               (tail (match-string-no-properties 5 str))
               (dst (ttt--decode-string body)))
          (if (and ttt-remove-space (null delimiter)
                   (string-match-p (concat ttt-remove-space-regexp " $") head))
              (list dst (1+ (length src)) (length tail))
            (list dst (length src) (length tail))))))))

(defun ttt--region (beg end)
  "Decode region between BEG and END.
Return beginning and end position of decoded string as (BEG . END)."
  (let* ((src (buffer-substring beg end))
         (dst (ttt--decode-string src)))
    (goto-char beg)
    (delete-region beg end)
    (insert dst)
    (cons beg (+ beg (length dst)))))

;;;###autoload
(defun ttt-do-ttt ()
  "Do ttt.
Return beginning and end position of decoded string as (BEG . END)."
  (interactive)
  (if (region-active-p)
      (ttt--region (region-beginning) (region-end))
    (let* ((src (buffer-substring (point-at-bol) (point)))
           (ls (ttt--backward src))
           (dst (car ls))
           (body-len (car (cdr ls)))
           (tail-len (car (cdr (cdr ls))))
           (end (- (point) tail-len))
           (beg (- end body-len)))
      (save-excursion
        (goto-char beg)
        (insert dst)
        (delete-region (point) (+ (point) body-len))
        (cons beg (+ beg (length dst)))))))

;;; isearch

;;;###autoload
(defun ttt-isearch-do-ttt ()
  "Do ttt in isearch."
  (interactive)
  (let* ((i 0)
         (src isearch-string)
         (len (length src))
         (ls (ttt--backward src))
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

;;; jump

;;
;; ttt-jump-to-char-forward, ttt-jump-to-char-backward
;;

(defun ttt--read-char (prompt)
  "Read TT-code keys with PROMPT until they make a valid code.
Return decoded character."
  (let (c ch)
    (ttt--reset)
    (while (string-equal "" (setq ch (ttt--trans (setq c (read-char prompt)))))
      (setq prompt (concat prompt (string c) " ")))
    ch))

(defvar ttt-jump--char nil "Last searched character.")
(defvar ttt-jump--sign 1 "Search direction; 1 for forward, -1 for backward.")

(defun ttt-jump--sub (char count)
  "Jump to COUNTth occurrence of char CHAR."
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
  "Repeat last jump to `ttt-jump--char' COUNT times."
  (cond
   ((eq nil ttt-jump--char) (message "No jump yet"))
   (t (ttt-jump--sub ttt-jump--char count)
      (message "Repeating jump to char %c" ttt-jump--char))))

(defun ttt-jump--main (count jump jump-reverse sign prompt prompt-ttt)
  "Jump to COUNTth occurrence of input char.
Do jump by `ttt-jump-repeat' if `last-command' is JUMP or JUMP-REVERSE,
or by `ttt-jump--sub' after reading char with PROMPT or PROMPT-TTT,
Search is done in same direction if SIGN is positive,
or in reverse direction if SIGN is negative."
  (let (char point)
    (cond ((eq last-command jump)
           (ttt-jump--repeat (* ttt-jump--sign count)))
          ((eq last-command jump-reverse)
           (setq ttt-jump--sign (- ttt-jump--sign))
           (ttt-jump--repeat (* ttt-jump--sign count)))
          (t
           (setq ttt-jump--sign sign)
           (setq point (point))
           (setq char (read-char (propertize prompt 'face 'minibuffer-prompt)))
           (if (eq char ?\C-m)
               (setq char (string-to-char
                           (ttt--read-char
                            (propertize prompt-ttt 'face
                                        'minibuffer-prompt)))))
           (if (not (characterp char))  ; XXX: command call key as input char
               (ttt-jump--repeat (* ttt-jump--sign count))
             (ttt-jump--sub char (* ttt-jump--sign count)))
           (when (and (/= point (point))
                      (not mark-active))
             (push-mark point nil nil))))
    (if (< count 0) (setq ttt-jump--sign (- ttt-jump--sign)))))

;;;###autoload
(defun ttt-jump-to-char-forward (&optional count)
  "Jump forward to COUNTth occurrence of input char.
Repeat use of this command repeats last jump.
If input char is RET (or C-m), then read TT-code keys as input char."
  (interactive "p")
  (ttt-jump--main count 'ttt-jump-to-char-forward 'ttt-jump-to-char-backward
                  1 "Jump to char: " "Jump to char by ttt: "))

;;;###autoload
(defun ttt-jump-to-char-backward (&optional count)
  "Jump backward to COUNTth occurrence of input char.
Repeat use of this command repeats last jump.
If input char is RET (or C-m), then read TT-code keys as input char."
  (interactive "p")
  (ttt-jump--main count 'ttt-jump-to-char-backward 'ttt-jump-to-char-forward
                  -1 "Back to char: " "Back to char by ttt: "))

;;; titeto

;;
;; titeto --- isearch a la migemo
;;

(defcustom ttt-isearch-enable-p t
  "*Enable the ttt feature on isearch or not."
  :group 'ttt
  :type 'boolean)

;;;###autoload
(defun ttt-isearch-toggle-ttt ()
  "Toggle ttt in migemo isearch."
  (interactive)
  (unless (or isearch-regexp)
    (discard-input)
    (setq ttt-isearch-enable-p (not ttt-isearch-enable-p)))
  (when (fboundp 'isearch-lazy-highlight-new-loop)
      (let ((isearch-lazy-highlight-last-string nil))
        (condition-case nil
            (isearch-lazy-highlight-new-loop)
          (error
           (isearch-lazy-highlight-new-loop nil nil)))))
  (isearch-message))

(defun ttt--decode-expand (str)
  "Decode TT-code string STR, get DST and STATE and return (DST . STATE)."
  (ttt--reset)
  (cons (mapconcat (lambda (c) (ttt--trans c t)) (string-to-list str) "")
        ttt--state))

(defun ttt--vector-flatten-concat (vec)
  "Take a nested vector VEC and return its contents as a single, flat string."
  (cond ((and (vectorp vec) (= 0 (length vec)))
         "")
        ((vectorp vec)
         (concat (ttt--vector-flatten-concat (aref vec 0))
                 (ttt--vector-flatten-concat (substring vec 1))))
        (t
         vec)))

(defun ttt--generate-regexp-str (pattern &optional with-paren-p)
  "Get regexp for pattern PATTERN.
Returned regexp is put in parentheses if WITH-PAREN-P is non-nil."
  (let* ((decode-expand (ttt--decode-expand pattern))
         (dst (car decode-expand))
         (state (cdr decode-expand))
         (state (if (eq state ttt-table) [] state))
         ;; XXX: ttt-table の末端の要素は空文字列か長さ 1 の文字列と仮定
         (char-class (ttt--vector-flatten-concat state))
         (char-class (if (string= "" char-class) ""
                       (regexp-opt-charset (string-to-list char-class))))
         (re (concat (regexp-quote dst) char-class))
         (re (concat (regexp-quote pattern) "\\|" re))
         (re (if with-paren-p (concat "\\(" re "\\)") re)))
    re))

(defun ttt--get-pattern (pattern)
  "Get regexp for pattern PATTERN."
  (let* ((segments (save-match-data (split-string pattern " "))))
    (if (<= (length segments) 1)
        (ttt--generate-regexp-str pattern nil)
      (let ((re-str1 (ttt--generate-regexp-str pattern t))
            (re-str2 (mapconcat (lambda (p) (ttt--generate-regexp-str p t))
                                segments " *")))
        (concat re-str1 "\\|" re-str2)
        ))))

(defadvice isearch-message-prefix (after ttt-status activate)
  "Adviced by ttt."
  (let ((ret ad-return-value)
        (str "[ttt]"))
    (when (and (boundp 'migemo-isearch-enable-p)
               migemo-isearch-enable-p
               ttt-isearch-enable-p
               (not (or isearch-regexp)))
      (setq ad-return-value (concat str " " ret)))))

;; advice で migemo-get-pattern を書き換える
(defadvice migemo-get-pattern (around ttt--get-pattern-ad activate)
  "Adviced by ttt."
  (if (and ttt-isearch-enable-p)
      (setq ad-return-value (ttt--get-pattern (ad-get-arg 0)))
    ad-do-it))

;;; ttt-rev

(defvar ttt-rev--plist nil "Property list.")
(defvar ttt-rev--obarray (make-vector 4999 nil) "Object array.") ; 4999: prime

(defun ttt-rev--make-revtable (table &optional prefix)
  "Make reverse table. PREFIX is prefix of code, TABLE is subtable."
  (let* ((k 0)
         (prefix (or prefix nil)))
    (while (< k (length ttt-keys))
      (let* ((tbl (aref table k))
             (revcode (cons k prefix)))
        (cond ((vectorp tbl)
               (ttt-rev--make-revtable tbl revcode))
              ((and (stringp tbl) (not (string= "" tbl)))
               (let* ((codes (plist-get ttt-rev--plist
                                        (intern-soft tbl ttt-rev--obarray)))
                      (codes (cons (reverse revcode) codes)))
                 (setq ttt-rev--plist
                       (plist-put ttt-rev--plist (intern tbl ttt-rev--obarray)
                                  codes))))
              (t nil)))
      (setq k (1+ k))))
  ttt-rev--plist)

(defvar ttt-rev--revtable (ttt-rev--make-revtable ttt-table nil)
  "TT-code reverse table.")

(defun ttt-rev--lookup-keyseq (ch)
  "Look up one char string CH in reverse table and return list of code keyseq."
  (plist-get ttt-rev--revtable (intern-soft ch ttt-rev--obarray)))

(defun ttt-rev--lookup-string (ch)
  "Look up one char string CH in reverse table and return list of code string."
  (mapcar #'ttt--keyseq-to-string (ttt-rev--lookup-keyseq ch)))

;;; ttt-code-help

(defface ttt-paren-face '((t (:inherit font-lock-comment-face)))
  "Face for paren of code help.")

(defface ttt-code-face '((t (:inherit font-lock-string-face)))
  "Face for code of code help.")

(defun ttt--make-code-help (code)
  "Make code help from CODE."
  (concat (propertize "<" 'face 'ttt-paren-face)
          (if code (propertize code 'face 'ttt-code-face) "--")
          (propertize ">" 'face 'ttt-paren-face)))

(defun ttt--code-help-ch (ch &optional certain)
  "Return code help of one char string CH.
Does not include code for char included in string CERTAIN."
  (let ((codes (or (ttt-rev--lookup-string ch) '(nil))))
    (if (and certain (ttt--index certain ch))
        ch
      (concat ch (mapconcat #'ttt--make-code-help codes "")))))

(defun ttt--code-help-str (str &optional certain)
  "Return code help of string STR.
Does not include code for char included in string CERTAIN."
  (let* ((list (save-match-data (split-string str "" t)))
         (code-help-ch-no-certain (lambda (ch) (ttt--code-help-ch ch certain)))
         (help-list (mapcar code-help-ch-no-certain list))
         (str (mapconcat #'identity help-list "")))
    str))

;;; ttt-kkc

(require 'kkc)

;;;###autoload
(defun ttt-do-ttt-with-kkc (&optional n)
  "Convert string until caret or active region.
If optional perfix arg N is given, use `kkc-region' to Kana Kanji Conversion.

hrq.ydhr,. M-j       → きゅうきょ
hrq.ydhr,. C-u M-j   → 急遽
きゅうきょ C-u 5 M-j → 急遽"
  (interactive "P")
  (if (null n) (ttt-do-ttt)
    (save-match-data
      (let* ((left (if (region-active-p)
                       (buffer-substring (region-beginning) (region-end))
                     (buffer-substring (point-at-bol) (point))))
             (pattern (if (and (boundp 'kkc-maze-enable-maze-p)
                               kkc-maze-enable-maze-p)
                          ;; "[^ -~\t\r\n]+$"
                          "\\cj+$"              ; \cj matches Japanese char
                        "[ぁ-んヴヵヶー・]+$"))
             ret yomi)
        (when (string-match
               (concat (regexp-opt-charset (string-to-list ttt-keys)) "+$")
               left)
          (setq ret (ttt-do-ttt))
          (setq left (buffer-substring (car ret) (cdr ret))))
        (when (string-match pattern left)
          (setq yomi (match-string 0 left)))
        (when yomi
          (and (region-active-p)
               (goto-char (+ (region-beginning) (length yomi))))
          (let* ((len (kkc-region
                       (- (point) (or (and (numberp n) n) (length yomi)))
                       (point)))
                 (str (buffer-substring (- (point) len) (point))))
            (message (ttt--code-help-str str yomi))))))))

;;; ttt-userdef

;; 2022-11-30 必要な時だけ ttt-userdef-save-file を実行
;; 2022-11-29 ttt-table を上書きする方式
;; 2022-11-29 ttt.el の逆引きテーブルの仕様変更に対応
;; 2022-11-29 ttt-userdef-alist が nil の時も ttt-userdef-file-name に保存する
;; 2022-11-29 (or (not (eq (length ch) 1)) ...)
;; 2022-11-28 ttt-userdef.el - ttt-table を上書きせず変換時等は ttt-table と ttt-userdef-alist の両方を見る方式

;;; Known issues:

;; - ttt-table の“葉”の部分にしか定義できない
;; - 1 文字しか定義できない (熟語を定義した時の動作は未定義: 要チェック)
;; - - → 1 文字チェックは入れた - 2022-11-29
;; - migemo ttt isearch (titeto) 時にユーザ定義や上書き定義が反映されない
;; - - ttt-table を上書きしないと難しい
;; - - - → ttt-table を上書きする方式にした
;; - - ttt-table を上書きするなら ^ - [ ] などの文字が定義された場合を考慮する必要
;; - - - → 考慮されてるみたい (regexp-opt-charset により)
;; - - ttt-table を上書きするなら ttt--rev-table の整合性を保つ必要
;; - - - → たぶん整合性を保ってる
;; - ttt.el: ttt-keys の変更にコードヘルプが追随しない (Dvorak なのに の<kd> 等)
;; - - ttt--rev-table のデータ形式を変更する必要
;; - - - → データ形式を変更し ttt-rev--revtable とした。これで追随するはず
;; - ttt-userdef-define で定義した文字は ttt-userdef-undefine できるが、 ttt-table にもとからある文字は ttt-userdef-undefine できない

;;; ttt--define

(defun ttt--set (table keyseq ch)
  "Set KEYSEQ position of array TABLE to one char string CH."
  (if (= (length keyseq) 1)
      (aset table (car keyseq) ch)
    (ttt--set (aref table (car keyseq)) (cdr keyseq) ch)))

(defun ttt--get (table keyseq)
  "Get KEYSEQ position of array TABLE."
  (if (= (length keyseq) 1)
      (aref table (car keyseq))
    (ttt--get (aref table (car keyseq)) (cdr keyseq))))

(defun ttt-rev--set (revtable keyseq ch)
  "Set value for CH in plist REVTABLE to KEYSEQ."
  (let* ((old-ch (ttt--get ttt-table keyseq))
         (rev (plist-member revtable (intern-soft old-ch ttt-rev--obarray))))
    (if (and old-ch (= (length old-ch) 1) rev)
        (setcar (cdr rev)
                (cl-remove-if (apply-partially #'equal keyseq) (cadr rev)))))
  (when (= (length ch) 1)
    (plist-put revtable (intern ch ttt-rev--obarray)
               (cons keyseq
                     (plist-get revtable (intern-soft ch ttt-rev--obarray))))))

(defun ttt--define (keyseq ch)
  "Set KEYSEQ to CH."
  (ttt-rev--set ttt-rev--revtable keyseq ch)
  (ttt--set ttt-table keyseq ch))

;;; ttt-userdef--set-string

(defvar ttt-userdef-alist nil "Alist of keyseq to ch.")

(defvar ttt-userdef-alist-copy nil "Copy of `ttt-userdef-alist'")

(defun ttt-userdef--set (keyseq ch)
  "Set KEYSEQ, list of key numbers, to one char string CH."
  (setf (alist-get keyseq ttt-userdef-alist nil nil #'equal) ch)
  (ttt--define keyseq ch))

(defun ttt-userdef--unset (keyseq)
  "Remove definition for KEYSEQ, list of key numbers."
  (setf (alist-get keyseq ttt-userdef-alist nil 'remove #'equal) nil) ; XXX
  (ttt--define keyseq ""))              ; XXX

(defun ttt-userdef--get (keyseq)
  "Look up KEYSEQ, list of key numbers and return definition."
  (alist-get keyseq ttt-userdef-alist nil nil #'equal))

(defun ttt-userdef--set-string (code ch)
  "Set string CODE to one char string CH."
  (ttt-userdef--set (ttt--string-to-keyseq code) ch))

(defun ttt-userdef--unset-string (code)
  "Remove definition for string CODE."
  (ttt-userdef--unset (ttt--string-to-keyseq code)))

(defun ttt-userdef--get-string (code)
  "Return definition for CODE."
  (ttt-userdef--get (ttt--string-to-keyseq code)))

;;; save file

(defvar ttt-userdef-file-name (locate-user-emacs-file "ttt-userdef-init.el")
  "*File name for user definitions.")

(defun ttt-userdef-save-file ()
  "Write to file specified by `ttt-userdef-file-name'."
  (let ((coding-system-for-write 'utf-8)
        (print-length nil))
    (when (and ttt-userdef-file-name
               (not (equal ttt-userdef-alist ttt-userdef-alist-copy))
               (or ttt-userdef-alist (file-exists-p ttt-userdef-file-name)))
      (write-region (format "(setq ttt-userdef-alist '%S)"
                            ttt-userdef-alist)
                    nil
                    ttt-userdef-file-name))))

(add-hook 'kill-emacs-hook 'ttt-userdef-save-file)

;;; load file

(if (and ttt-userdef-file-name
         (file-readable-p ttt-userdef-file-name))
    (condition-case nil
        (progn (load-file ttt-userdef-file-name)
               (setq ttt-userdef-alist-copy (copy-alist ttt-userdef-alist)))
      (ttt-userdef-error "Invalid data in %s" ttt-userdef-file-name)))

(if (fboundp 'define-error)
    (define-error 'ttt-userdef-error nil)
  (put 'ttt-userdef-error 'error-conditions '(ttt-userdef-error error)))

(defun ttt-userdef-error (&rest args)
  "Signal error `ttt-userdef-error' with message ARGS."
  (signal 'ttt-userdef-error (apply #'format-message args)))

(let ((alist (reverse ttt-userdef-alist)))
  (while alist
    (ttt--define (caar alist) (cdar alist))
    (setq alist (cdr alist))))

;;; define/undefine

;;;###autoload
(defun ttt-userdef-undefine ()
  "Undefine user defined code interactively."
  (interactive)
  (let* (code-str old-def)
    (setq code-str (read-string "Undefine code: ")
          old-def (ttt-userdef--get-string code-str))
    (while (or (string= code-str "")
               (null old-def)
               (and old-def
                    (not (y-or-n-p (format "Undefine %s%s? "
                                           old-def
                                           (ttt--make-code-help code-str))))))
      (setq code-str (read-string "Undefine code: ")
            old-def (ttt-userdef--get-string code-str)))
    (ttt-userdef--unset-string code-str)
    (message (format "Undefined %s%s" old-def (ttt--make-code-help code-str)))))

;;;###autoload
(defun ttt-userdef-define ()
  "Define user defined code interactively."
  (interactive)
  (let* ((ch (read-string "Define character: "))
         codes-str prompt code-str old-def)
    (if (string= ch "")
        (ttt-userdef-undefine)
      (while (or (not (eq (length ch) 1))
                 (and (setq codes-str (ttt-rev--lookup-string ch))
                      (not (y-or-n-p (format "Redefine %s? "
                                             (ttt--code-help-ch ch))))))
        (setq ch (read-string "Define character: ")))
      (setq prompt (format "Define %s to code: " ch)
            code-str (read-string prompt)
            old-def (progn (ttt--reset)
                           (mapconcat (lambda (c) (ttt--trans c t))
                                      (string-to-list code-str) "")))
      (while (or (string= code-str "")
                 (not (eq ttt--state ttt-table))
                 (and (not (string= old-def code-str))
                      (not (y-or-n-p
                            (format "Overwrite %s%s? "
                                    old-def
                                    (ttt--make-code-help code-str))))))
        (setq code-str (read-string prompt)
              old-def (progn (ttt--reset)
                             (mapconcat (lambda (c) (ttt--trans c t))
                                        (string-to-list code-str) ""))))
      (ttt-userdef--set-string code-str ch)
      (message (format "Defined %s%s" ch (ttt--make-code-help code-str))))))

;;; provide

(provide 'ttt)
;;; ttt.el ends here
