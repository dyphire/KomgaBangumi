// ==UserScript==
// @name         KomgaBangumi
// @namespace    https://github.com/dyphire/KomgaBangumi
// @version      2.9.7
// @description  Komga 漫画服务器元数据刮削器，使用 Bangumi API，并支持自定义 Access Token
// @author       eeezae, ramu, dyphire
// @include      http://localhost:25600/*
// @include      *://在此处填入你的komga地址/*
// @icon         https://komga.org/img/logo.svg
// @grant        GM_xmlhttpRequest
// @grant        GM_download
// @grant        GM_setValue
// @grant        GM_getValue
// @grant        GM_deleteValue
// @grant        GM_registerMenuCommand
// @require      https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js
// @downloadURL  https://raw.githubusercontent.com/dyphire/KomgaBangumi/master/KomgaBangumi.user.js
// @updateURL    https://raw.githubusercontent.com/dyphire/KomgaBangumi/master/KomgaBangumi.meta.js
// @license      MIT
// ==/UserScript==

'use strict';

// 添加loading关键帧
$('head').append(`
  <style>
    @keyframes spin {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
    }
  </style>`);

const maxReqBooks = 500;
const sourceLabels = ['Btv', 'Bof']; // Btv now uses API
const btvApiUrl = 'https://api.bgm.tv';
const btvLegacyUrl = 'https://bangumi.tv'; // Still used for direct subject links
const bofUrl = 'https://bookof.moe';
const tagLabels = '架空,搞笑,欢乐,欢乐向,热血,运动,恋爱,轻改,后宫,校园,青年,少年,少女,青年向,少年向,少女向,英雄,青春,友情,治愈,邪道,战斗,魔法,科幻,冒险,推理,悬疑,侦探,竞技,体育,励志,职场,社会,史诗,历史,战争,机战,末世,意识流,宗教,神鬼,妹控,奇幻,异界,轮回,穿越,重生,恐怖,短篇,反转,萌系,百合,日常,旅行，异世界,偶像,转生,伦理,黑暗,亲情,家庭,暴力,复仇,血腥,兄妹,生命,哲学,废土,致郁,性转,兄控,颜艺,感动,地下城,篮球,足球,棒球,网球,排球,高尔夫,保龄球,滑板,滑雪,滑冰,射击,赛车,赛马,拳击,摔跤,格斗,武术,游泳,健身,骑行,登山,攀岩,射箭,钓鱼,烹饪,麻将,围棋,象棋,桥牌,扑克,美食,魔术,占卜,跳舞,唱歌,乐器,绘画,书法,摄影,雕塑,篆刻,陶艺,服装,舞蹈,戏剧,电影,成长,童年,反套路,犯罪,校园霸凌,校园欺凌,外星人,色气,自然主义,将棋,工口,武士,超能力,游戏,街机,梦想,怪物,冷战,社会主义,摇滚,音乐,环保,猎奇,民俗,幽默,僵尸,动物,农业,生活,心理,生存,短篇集,师生,卖肉,连载,连载中,完结,已完结,停刊,长期休载,停止连载,休刊';
const equalLabels = ['治愈,治癒', '校园欺凌,校园霸凌', '轻改,轻小说改', '工口,色气,卖肉'];

const defaultReqHeaders = { // Renamed to avoid conflict with local var 'defaultHeaders' in asyncReq
  'content-type': 'application/json;charset=UTF-8',
};

const BANGUMI_ACCESS_TOKEN_KEY = 'komga_bangumi_access_token'; // 用于存储Bangumi Access Token的键名
const BANGUMI_MATCH_TYPE_KEY = "bangumi_match_type"; // 用于存储匹配类型的键名

const bangumiApiHeaders = {
    'User-Agent': `${GM_info.script.name}/${GM_info.script.version} (UserScript; ${GM_info.script.namespace})`,
    'Accept': 'application/json'
    // Authorization 如果令牌存在，将被动态添加
};

// 获取已存储的Bangumi Access Token
function getBangumiAccessToken() {
    return GM_getValue(BANGUMI_ACCESS_TOKEN_KEY, null);
}

// 通过提示框设置/更新Bangumi Access Token
function setBangumiAccessToken() {
    const currentToken = getBangumiAccessToken();
    const newToken = prompt(
        "请输入您的 Bangumi API Access Token (用于提高请求频率或访问 NSFW 条目)。\n留空并确定则清除已保存的 Token。\n\n你可以在 https://next.bgm.tv/demo/access-token 生成一个 Access Token",
        currentToken || ""
    );

    if (newToken !== null) { // 用户按了确定，而不是取消
        if (newToken.trim() === "") {
            GM_deleteValue(BANGUMI_ACCESS_TOKEN_KEY);
            showMessage("Bangumi Access Token 已清除。", "info");
        } else {
            GM_setValue(BANGUMI_ACCESS_TOKEN_KEY, newToken.trim());
            showMessage("Bangumi Access Token 已保存。", "success");
        }
    } else {
        showMessage("设置Bangumi Access Token操作已取消。", "warning");
    }
}

// 读取匹配类型，默认返回"漫画"
function getBangumiMatchType() {
    return GM_getValue(BANGUMI_MATCH_TYPE_KEY, "漫画");
}

// 通过提示框设置/更新匹配类型
async function setBangumiMatchType() {
    const currentType = getBangumiMatchType();

    const result = await customConfirm(
        `当前匹配类型是：${currentType}\n请选择匹配类型：`,
        "漫画",
        "小说"
    );

    if (result === "keep") {
        return;
    }

    const newType = result === "confirm" ? "漫画" : "小说";
    GM_setValue(BANGUMI_MATCH_TYPE_KEY, newType);
    showMessage(`匹配类型已设置为：${newType}`, "success");
}

// 定义常用样式
const btnStyle = {
  position: 'absolute',
  bottom: '10px',
  'border-radius': '50%',
  'background-color': 'orange',
  border: 'none',
  color: '#efefef',
  'font-size': '16px',
  'font-weight': 'bold',
  'z-index': '10',
  opacity: '0',
  'pointer-events': 'none',
  transition: 'opacity 0.2s ease-in-out',
  cursor: 'pointer',
  display: 'flex',
  'align-items': 'center',
  'justify-content': 'center',
};

const maskStyle = {
  position: 'absolute',
  top: 0,
  left: 0,
  'background-color': 'white',
  opacity: 0.9,
  display: 'flex',
  alignItems: 'center',
  justifyContent: 'center',
  textAlign: 'center',
  'z-index': 5,
};

const selPanelStyle = {
  position: 'fixed',
  top: '50%',
  left: '50%',
  transform: 'translate(-50%, -50%)',
  width: '400px',
  height: 'auto',
  maxHeight: '80vh',
  overflowY: 'auto',
  backgroundColor: '#f5f5dc',
  border: '1px solid #ccc',
  boxShadow: '0 0 10px rgba(0,0,0,0.3)',
  display: 'grid',
  gridTemplateColumns: 'repeat(auto-fill, minmax(110px, 1fr))',
  gap: '10px',
  padding: '15px',
  alignItems: 'start',
  justifyContent: 'center',
  zIndex: '100',
  borderRadius: '8px',
};

const selPanelBtnStyle = {
  display: 'flex',
  flexDirection: 'column',
  alignItems: 'center',
  justifyContent: 'center',
  width: '100%',
  minWidth: '110px',
  height: '160px',
  textAlign: 'center',
  backgroundColor: '#4CAF50',
  color: 'white',
  borderRadius: '10px',
  padding: '8px',
  border: 'none',
  overflow: 'hidden',
  cursor: 'pointer',
  fontSize: '14px',
  wordBreak: 'break-word',
  transition: 'background-color 0.2s ease',
};
selPanelBtnStyle[':hover'] = {
    backgroundColor: '#45a049',
};

const $msgBoxes = $('<div>').attr('id', 'msg-boxes').css({
  display: 'flex',
  alignItems: 'center',
  justifyContent: 'center',
  flexDirection: 'column',
  gap: '10px',
  position: 'fixed',
  top: '40px',
  right: 0,
  width: '500px',
  height: 'auto',
  'z-index': '10000',
});

// 自定义确认弹窗
function customConfirm(title, btn1Text = "确定", btn2Text = "取消") {
    return new Promise((resolve) => {
        const overlay = document.createElement("div");
        overlay.style.cssText = `
            position: fixed;
            inset: 0;
            background-color: rgba(0,0,0,0.5);
            display: flex;
            justify-content: center;
            align-items: center;
            z-index: 99999;
            font-family: "Segoe UI", Tahoma, Geneva, Verdana, sans-serif;
        `;

        const dialog = document.createElement("div");
        dialog.style.cssText = `
            position: relative;
            max-width: 320px;
            width: 90%;
            padding: 20px 30px;
            border-radius: 12px;
            box-shadow: 0 8px 24px rgba(0,0,0,0.2);
            text-align: center;
            user-select: none;
            transition: background-color 0.3s, color 0.3s;
        `;

        const textElem = document.createElement("div");
        textElem.textContent = title;
        textElem.style.cssText = `
            font-size: 18px;
            margin-bottom: 24px;
            white-space: pre-wrap;
        `;

        // 关闭按钮
        const closeBtn = document.createElement("button");
        closeBtn.innerHTML = "&times;";
        closeBtn.title = "关闭";
        closeBtn.style.cssText = `
            position: absolute;
            top: 8px;
            right: 12px;
            background: transparent;
            border: none;
            font-size: 24px;
            cursor: pointer;
            user-select: none;
            transition: color 0.2s;
        `;

        const btnContainer = document.createElement("div");
        btnContainer.style.cssText = `
            display: flex;
            justify-content: space-around;
            gap: 20px;
        `;

        function styleButton(btn) {
            btn.style.cssText = `
                flex: 1;
                padding: 10px 0;
                border-radius: 8px;
                border: none;
                font-size: 16px;
                cursor: pointer;
                user-select: none;
                transition: background-color 0.2s ease;
            `;
        }

        const btn1 = document.createElement("button");
        btn1.textContent = btn1Text;
        styleButton(btn1);

        const btn2 = document.createElement("button");
        btn2.textContent = btn2Text;
        styleButton(btn2);

        const darkTheme = window.matchMedia("(prefers-color-scheme: dark)").matches;

        if (darkTheme) {
            overlay.style.backgroundColor = "rgba(255,255,255,0.15)";

            dialog.style.backgroundColor = "#222";
            dialog.style.color = "#eee";
            dialog.style.boxShadow = "0 8px 24px rgba(0,0,0,0.8)";

            closeBtn.style.color = "#ccc";
            closeBtn.onmouseenter = () => (closeBtn.style.color = "#fff");
            closeBtn.onmouseleave = () => (closeBtn.style.color = "#ccc");

            btn1.style.backgroundColor = "#388e3c";
            btn1.style.color = "#fff";
            btn1.onmouseenter = () => (btn1.style.backgroundColor = "#2e7d32");
            btn1.onmouseleave = () => (btn1.style.backgroundColor = "#388e3c");

            btn2.style.backgroundColor = "#d32f2f";
            btn2.style.color = "#fff";
            btn2.onmouseenter = () => (btn2.style.backgroundColor = "#b71c1c");
            btn2.onmouseleave = () => (btn2.style.backgroundColor = "#d32f2f");
        } else {
            // 浅色主题
            overlay.style.backgroundColor = "rgba(0,0,0,0.5)";

            dialog.style.backgroundColor = "#fff";
            dialog.style.color = "#333";
            dialog.style.boxShadow = "0 8px 24px rgba(0,0,0,0.2)";

            closeBtn.style.color = "#666";
            closeBtn.onmouseenter = () => (closeBtn.style.color = "#000");
            closeBtn.onmouseleave = () => (closeBtn.style.color = "#666");

            btn1.style.backgroundColor = "#4caf50";
            btn1.style.color = "white";
            btn1.onmouseenter = () => (btn1.style.backgroundColor = "#45a049");
            btn1.onmouseleave = () => (btn1.style.backgroundColor = "#4caf50");

            btn2.style.backgroundColor = "#f44336";
            btn2.style.color = "white";
            btn2.onmouseenter = () => (btn2.style.backgroundColor = "#e53935");
            btn2.onmouseleave = () => (btn2.style.backgroundColor = "#f44336");
        }

        closeBtn.onclick = () => {
            cleanup();
            resolve("keep");
        };

        const btn1Click = () => {
            cleanup();
            resolve("confirm");
        };
        btn1.onclick = btn1Click;

        const btn2Click = () => {
            cleanup();
            resolve("cancel");
        };
        btn2.onclick = btn2Click;

        function cleanup() {
            document.body.removeChild(overlay);
            document.removeEventListener("keydown", keyListener);
        }

        function keyListener(e) {
            if (e.key === "Escape") {
                cleanup();
                resolve("keep");
            }
        }
        document.addEventListener("keydown", keyListener);

        btnContainer.appendChild(btn1);
        btnContainer.appendChild(btn2);
        dialog.appendChild(closeBtn);
        dialog.appendChild(textElem);
        dialog.appendChild(btnContainer);
        overlay.appendChild(dialog);
        document.body.appendChild(overlay);
    });
}

// ************************************** 工具相关 **************************************
//<editor-fold desc="工具相关">
function s2t(cc) { let str = '', ss = jtpy(), tt = ftpy(); for (let i = 0; i < cc.length; i++) { let c = cc.charAt(i); if (c.charCodeAt(0) > 10000 && ss.indexOf(c) !== -1) str += tt.charAt(ss.indexOf(c)); else str += c; } return str; }
function t2s(cc) { let str = '', ss = jtpy(), tt = ftpy(); for (let i = 0; i < cc.length; i++) { let c = cc.charAt(i); if (c.charCodeAt(0) > 10000 && tt.indexOf(c) !== -1) str += ss.charAt(tt.indexOf(c)); else str += c; } return str; }
function jtpy() { return '经验值别皑蔼碍爱翱袄奥坝罢摆败颁办绊帮绑镑谤剥饱宝报鲍辈贝钡狈备惫绷笔毕毙闭边编贬变辩辫鳖瘪濒滨宾摈饼拨钵铂驳卜补参蚕残惭惨灿苍舱仓沧厕侧册测层诧搀掺蝉馋谗缠铲产阐颤场尝长偿肠厂畅钞车彻尘陈衬撑称惩诚骋痴迟驰耻齿炽冲虫宠畴踌筹绸丑橱厨锄雏础储触处传疮闯创锤纯绰辞词赐聪葱囱从丛凑窜错达带贷担单郸掸胆惮诞弹当挡党荡档捣岛祷导盗灯邓敌涤递缔点垫电淀钓调迭谍叠钉顶锭订东动栋冻斗犊独读赌镀锻断缎兑队对吨顿钝夺鹅额讹恶饿儿尔饵贰发罚阀珐矾钒烦范贩饭访纺飞废费纷坟奋愤粪丰枫锋风疯冯缝讽凤肤辐抚辅赋复负讣妇缚该钙盖干赶秆赣冈刚钢纲岗皋镐搁鸽阁铬个给龚宫巩贡钩沟构购够蛊顾剐关观馆惯贯广规硅归龟闺轨诡柜贵刽辊滚锅国过骇韩汉阂鹤贺横轰鸿红后壶护沪户哗华画划话怀坏欢环还缓换唤痪焕涣黄谎挥辉毁贿秽会烩汇讳诲绘荤浑伙获货祸击机积饥讥鸡绩缉极辑级挤几蓟剂济计记际继纪夹荚颊贾钾价驾歼监坚笺间艰缄茧检碱硷拣捡简俭减荐槛鉴践贱见键舰剑餞渐溅涧浆蒋桨奖讲酱胶浇骄娇搅铰矫侥脚饺缴绞轿较秸阶节茎惊经颈静镜径痉竞净纠厩旧驹举据锯惧剧鹃绢杰洁结诫届紧锦仅谨进晋烬尽劲荆觉决诀绝钧军骏开凯颗壳课垦恳抠库裤夸块侩宽矿旷况亏岿窥馈溃扩阔蜡腊莱来赖蓝栏拦篮阑兰澜谰揽览懒缆烂滥捞劳涝乐镭垒类泪篱离里鲤礼丽厉励砾历沥隶俩联莲连镰怜涟帘敛脸链恋炼练粮凉两辆谅疗辽镣猎临邻鳞凛赁龄铃凌灵岭领馏刘龙聋咙笼垄拢陇楼娄搂篓芦卢颅庐炉掳卤虏鲁赂禄录陆驴吕铝侣屡缕虑滤绿峦挛孪滦乱抡轮伦仑沦纶论萝罗逻锣箩骡骆络妈玛码蚂马骂吗买麦卖迈脉瞒馒蛮满谩猫锚铆贸么霉没镁门闷们锰梦谜弥觅绵缅庙灭悯闽鸣铭谬谋亩钠纳难挠脑恼闹馁腻撵捻酿鸟聂啮镊镍柠狞宁拧泞钮纽脓浓农疟诺欧鸥殴呕沤盘庞国爱赔喷鹏骗飘频贫苹凭评泼颇扑铺朴谱脐齐骑岂启气弃讫牵扦钎铅迁签谦钱钳潜浅谴堑枪呛墙蔷强抢锹桥乔侨翘窍窃钦亲轻氢倾顷请庆琼穷趋区躯驱龋颧权劝却鹊让饶扰绕热韧认纫荣绒软锐闰润洒萨鳃赛伞丧骚扫涩杀纱筛晒闪陕赡缮伤赏烧绍赊摄慑设绅审婶肾渗声绳胜圣师狮湿诗尸时蚀实识驶势释饰视试寿兽枢输书赎属术树竖数帅双谁税顺说硕烁丝饲耸怂颂讼诵擞苏诉肃虽绥岁孙损笋缩琐索獭挞抬摊贪瘫滩坛谭谈叹汤烫涛绦腾誊锑题体屉条贴铁厅听烃铜统头图涂团颓蜕脱鸵驮椭洼袜弯湾顽万网韦违围爲潍维苇伟伪纬谓卫温闻纹稳问乌诬芜吴坞雾务误锡牺习习铣戏细虾辖峡侠狭厦锨鲜纤咸贤衔闲显险现献县馅羡宪线厢镶乡详响项萧销晓啸蝎协挟携胁谐写泻谢锌衅兴汹绣绣虚嘘须许绪续轩悬选癣绚学勋询寻驯训讯逊压鸦鸭哑亚讶阉烟盐严颜阎艳厌砚彦谚验鸯杨扬疡阳痒养样瑶摇尧遥窑谣药爷页业叶医铱颐遗仪彝蚁艺亿忆义诣议谊译异绎荫阴银饮樱婴鹰应缨莹萤营荧蝇颖哟拥佣痈踊咏涌优忧邮铀犹游诱舆鱼渔娱与屿语吁御狱誉预驭鸳渊辕园员圆缘远愿约跃钥岳粤悦阅云郧匀陨运蕴酝晕韵杂灾载攒暂赞赃脏凿枣灶责择则泽贼赠扎札轧铡闸诈斋债毡盏斩辗崭栈战绽张涨帐账胀赵蛰辙锗这贞针侦诊镇阵挣睁狰帧郑证织职执纸挚掷帜质钟终种肿众诌轴皱昼骤猪诸诛烛瞩嘱贮铸筑驻专砖转赚桩庄装妆壮状锥赘坠缀谆浊兹资渍踪综总纵邹诅组钻致钟么为只凶准启板里雳余链泄'; }
function ftpy() { return '経験値別皚藹礙愛翺襖奧壩罷擺敗頒辦絆幫綁鎊謗剝飽寶報鮑輩貝鋇狽備憊繃筆畢斃閉邊編貶變辯辮鼈癟瀕濱賓擯餅撥缽鉑駁蔔補參蠶殘慚慘燦蒼艙倉滄廁側冊測層詫攙摻蟬饞讒纏鏟産闡顫場嘗長償腸廠暢鈔車徹塵陳襯撐稱懲誠騁癡遲馳恥齒熾沖蟲寵疇躊籌綢醜櫥廚鋤雛礎儲觸處傳瘡闖創錘純綽辭詞賜聰蔥囪從叢湊竄錯達帶貸擔單鄲撣膽憚誕彈當擋黨蕩檔搗島禱導盜燈鄧敵滌遞締點墊電澱釣調叠諜疊釘頂錠訂東動棟凍鬥犢獨讀賭鍍鍛斷緞兌隊對噸頓鈍奪鵝額訛惡餓兒爾餌貳發罰閥琺礬釩煩範販飯訪紡飛廢費紛墳奮憤糞豐楓鋒風瘋馮縫諷鳳膚輻撫輔賦複負訃婦縛該鈣蓋幹趕稈贛岡剛鋼綱崗臯鎬擱鴿閣鉻個給龔宮鞏貢鈎溝構購夠蠱顧剮關觀館慣貫廣規矽歸龜閨軌詭櫃貴劊輥滾鍋國過駭韓漢閡鶴賀橫轟鴻紅後壺護滬戶嘩華畫劃話懷壞歡環還緩換喚瘓煥渙黃謊揮輝毀賄穢會燴彙諱誨繪葷渾夥獲貨禍擊機積饑譏雞績緝極輯級擠幾薊劑濟計記際繼紀夾莢頰賈鉀價駕殲監堅箋間艱緘繭檢堿鹼揀撿簡儉減薦檻鑒踐賤見鍵艦劍餞漸濺澗漿蔣槳獎講醬膠澆驕嬌攪鉸矯僥腳餃繳絞轎較稭階節莖驚經頸靜鏡徑痙競淨糾廄舊駒舉據鋸懼劇鵑絹傑潔結誡屆緊錦僅謹進晉燼盡勁荊覺決訣絕鈞軍駿開凱顆殼課墾懇摳庫褲誇塊儈寬礦曠況虧巋窺饋潰擴闊蠟臘萊來賴藍欄攔籃闌蘭瀾讕攬覽懶纜爛濫撈勞澇樂鐳壘類淚籬離裏鯉禮麗厲勵礫曆瀝隸倆聯蓮連鐮憐漣簾斂臉鏈戀煉練糧涼兩輛諒療遼鐐獵臨鄰鱗凜賃齡鈴淩靈嶺領餾劉龍聾嚨籠壟攏隴樓婁摟簍蘆盧顱廬爐擄鹵虜魯賂祿錄陸驢呂鋁侶屢縷慮濾綠巒攣孿灤亂掄輪倫侖淪綸論蘿羅邏鑼籮騾駱絡媽瑪碼螞馬罵嗎買麥賣邁脈瞞饅蠻滿謾貓錨鉚貿麽黴沒鎂門悶們錳夢謎彌覓綿緬廟滅憫閩鳴銘謬謀畝鈉納難撓腦惱鬧餒膩攆撚釀鳥聶齧鑷鎳檸獰甯擰濘鈕紐膿濃農瘧諾歐鷗毆嘔漚盤龐國愛賠噴鵬騙飄頻貧蘋憑評潑頗撲鋪樸譜臍齊騎豈啓氣棄訖牽扡釺鉛遷簽謙錢鉗潛淺譴塹槍嗆牆薔強搶鍬橋喬僑翹竅竊欽親輕氫傾頃請慶瓊窮趨區軀驅齲顴權勸卻鵲讓饒擾繞熱韌認紉榮絨軟銳閏潤灑薩鰓賽傘喪騷掃澀殺紗篩曬閃陝贍繕傷賞燒紹賒攝懾設紳審嬸腎滲聲繩勝聖師獅濕詩屍時蝕實識駛勢釋飾視試壽獸樞輸書贖屬術樹豎數帥雙誰稅順說碩爍絲飼聳慫頌訟誦擻蘇訴肅雖綏歲孫損筍縮瑣鎖獺撻擡攤貪癱灘壇譚談歎湯燙濤縧騰謄銻題體屜條貼鐵廳聽烴銅統頭圖塗團頹蛻脫鴕駝橢窪襪彎灣頑萬網韋違圍爲濰維葦偉僞緯謂衛溫聞紋穩問烏誣蕪吳塢霧務誤錫犧襲習銑戲細蝦轄峽俠狹廈鍁鮮纖鹹賢銜閑顯險現獻縣餡羨憲線廂鑲鄉詳響項蕭銷曉嘯蠍協挾攜脅諧寫瀉謝鋅釁興洶鏽繡虛噓須許緒續軒懸選癬絢學勳詢尋馴訓訊遜壓鴉鴨啞亞訝閹煙鹽嚴顔閻豔厭硯彥諺驗鴦楊揚瘍陽癢養樣瑤搖堯遙窯謠藥爺頁業葉醫銥頤遺儀彜蟻藝億憶義詣議誼譯異繹蔭陰銀飲櫻嬰鷹應纓瑩螢營熒蠅穎喲擁傭癰踴詠湧優憂郵鈾猶遊誘輿魚漁娛與嶼語籲禦獄譽預馭鴛淵轅園員圓緣遠願約躍鑰嶽粵悅閱雲鄖勻隕運蘊醞暈韻雜災載攢暫贊贓髒鑿棗竈責擇則澤賊贈紮劄軋鍘閘詐齋債氈盞斬輾嶄棧戰綻張漲帳賬脹趙蟄轍鍺這貞針偵診鎮陣掙睜猙幀鄭證織職執紙摯擲幟質鍾終種腫衆謅軸皺晝驟豬諸誅燭矚囑貯鑄築駐專磚轉賺樁莊裝妝壯狀錐贅墜綴諄濁茲資漬蹤綜總縱鄒詛組鑽緻鐘麼為隻兇準啟闆裡靂餘鍊洩'; }
function capitalize(str) { return str ? str.replace(/\b[a-z]/g, (char) => char.toUpperCase()) : ''; }
//</editor-fold>

// ************************************** Dom加载 **************************************
//<editor-fold desc="Dom加载">
function partLoadingStart($dom) {
  if (!$dom || $dom.length === 0) {
      console.warn("partLoadingStart: Target DOM element not found.");
      return;
  }
  if ($dom.find('.loadMask').length > 0) return;
  const $imageDiv = $dom.find('div.v-image').first();
  const loadingWidth = $imageDiv.length > 0 ? $imageDiv.width() : $dom.width();
  const loadingHeight = $imageDiv.length > 0 ? $imageDiv.height() : $dom.height();
  const diameter = Math.min(loadingWidth, loadingHeight) / 3;
  const $loadMask = $('<div class="loadMask"></div>').css({
    ...maskStyle,
    width: loadingWidth,
    height: loadingHeight,
  });
  const $loading = $('<div></div>').css({
    width: diameter,
    height: diameter,
    'border-radius': '50%',
    border: '5px solid #f3f3f3',
    'border-top': '5px solid #3498db',
    animation: 'spin 1s linear infinite',
  });
  $loadMask.append($loading);
  $dom.append($loadMask);
}

function partLoadingEnd($dom) {
  if ($dom && $dom.length > 0) {
    $dom.find('.loadMask').remove();
  } else {
      $('.loadMask').remove();
  }
}

function loadMessage() {
  let msgBoxesIntervalId = setInterval(function () {
    let $app = $('div#app');
    if ($app.length !== 0) {
      $app.append($msgBoxes);
      clearInterval(msgBoxesIntervalId);
    }
  }, 200);
}

function showMessage(msgContent, msgType = 'success', duration = 5000) {
  let msgBgColor;
  switch (msgType) {
    case 'success': msgBgColor = '#4CAF50'; break;
    case 'warning': msgBgColor = '#ff9800'; break;
    case 'error': msgBgColor = '#f44336'; break;
    case 'info': msgBgColor = '#2196F3'; break;
    default: msgBgColor = '#4CAF50'; break;
  }
  let $msgTxt = $('<span>').attr('class', 'message').css({
    display: 'block',           // 改为块级元素，方便多行换行
    textAlign: 'left',
    fontSize: '16px',
    color: '#ffffff',
    overflowWrap: 'break-word', // 防止单词过长溢出
    whiteSpace: 'normal',       // 自动换行
    fontWeight: 'bold',
    maxHeight: '100px',         // 限制最大高度
    overflowY: 'auto',          // 超过高度显示滚动条
  });
  $msgTxt.text(msgContent);

  let $msgBox = $('<div>').attr('class', 'message-box').append($msgTxt).css({
    height: 'auto',
    minHeight: '46px',
    width: '360px',
    borderRadius: '4px',
    transform: 'translateX(110%)',
    backgroundColor: msgBgColor,
    boxShadow: '0 0 10px rgba(0,0,0,0.3)',
    padding: '10px 15px',
    zIndex: '9999',
    transition: 'transform 0.5s ease-out',
    display: 'flex',
    alignItems: 'flex-start',  // 改成顶部对齐，避免多行时垂直居中不自然
    justifyContent: 'flex-start',
    opacity: 0.95,
  });

  $msgBoxes.prepend($msgBox);
  setTimeout(() => { $msgBox.css({ transform: 'translateX(-10px)' }); }, 50);
  setTimeout(() => {
    $msgBox.css({ transform: 'translateX(110%)' });
    setTimeout(() => { $msgBox.remove(); }, 500);
  }, duration);
}

function findDomElementForSeries(komgaSeriesId) {
    let $dom = $(`div.v-card[komgaseriesid="${komgaSeriesId}"]`);
    if ($dom.length > 0) return $dom.first();
    $dom = $(`div.my-2.mx-2[komgaseriesid="${komgaSeriesId}"]`);
    if ($dom.length > 0) return $dom.first();
    return null;
}

function loadSearchBtn($dom, komgaSeriesId) {
    $dom.attr('komgaSeriesId', komgaSeriesId);
    const width = $dom.width();
    const btnDia = Math.max(width / 5.5, 34);
    let $syncInfo = $('<button title="仅更新元数据"></button>').attr('komgaSeriesId', komgaSeriesId);
    let $syncAll = $('<button title="更新元数据和封面"></button>').attr('komgaSeriesId', komgaSeriesId);
    const currentBtnStyle = { ...btnStyle, width: btnDia, height: btnDia };

    // 检查当前是否在 '/collections' 或 '/readlists' 页面
    const leftSidePages = ['/collections', '/readlists'];
    const isLeftSidePage = leftSidePages.some(path => window.location.pathname.includes(path));

    if (isLeftSidePage) {
        // 在 '/collections' 或 '/readlists' 页面，图标移动到左侧
        $syncInfo.css({ ...currentBtnStyle, left: '10px' });
        $syncAll.css({ ...currentBtnStyle, left: btnDia + 15 + 'px' });
    } else {
        // 其他页面，图标保持在右侧
        $syncAll.css({ ...currentBtnStyle, right: '10px' });
        $syncInfo.css({ ...currentBtnStyle, right: btnDia + 15 + 'px' });
    }

    $syncAll.append('<i aria-hidden="true" class="v-icon notranslate mdi mdi-image-sync-outline theme--light" style="font-size: inherit;"></i>');
    $syncInfo.append('<i aria-hidden="true" class="v-icon notranslate mdi mdi-file-document-edit-outline theme--light" style="font-size: inherit;"></i>');

    $syncAll.add($syncInfo).on('mouseenter', function () {
        $(this).css({ 'background-color': 'yellow', color: '#3c3c3c' });
    }).on('mouseleave', function () {
        $(this).css({ 'background-color': 'orange', color: '#efefef' });
    });

    $syncInfo.on('click', async (e) => {
        e.stopPropagation();
        await handleSearchClick(komgaSeriesId, 'meta', $dom);
    });

    $syncAll.on('click', async (e) => {
        e.stopPropagation();
        await handleSearchClick(komgaSeriesId, 'all', $dom);
    });

    $dom.append($syncAll).append($syncInfo);

    $dom.on('mouseenter', function () {
        $syncAll.add($syncInfo).css({ opacity: '1', 'pointer-events': 'auto' });
    }).on('mouseleave', function () {
        $syncAll.add($syncInfo).css({ opacity: '0', 'pointer-events': 'none' });
    });
}

function showBookSelectionPanel(seriesListRes) {
    return new Promise((resolve) => {
        const $selBookPanel = $('<div></div>').css({ ...selPanelStyle }); // selPanelStyle 需要在全局定义
        seriesListRes.forEach((series) => {
            const $selBookBtn = $('<button></button>')
                .attr('resSeriesId', series.id)
                .css({ ...selPanelBtnStyle }); // selPanelBtnStyle 需要在全局定义
            // 内部容器，用于更好的内容布局和padding
            const $contentWrapper = $('<div></div>').css({
                display: 'flex',
                flexDirection: 'column',
                justifyContent: 'center', // 垂直居中内容
                alignItems: 'center',    // 水平居中内容
                height: '100%',          // 撑满按钮高度
                padding: '5px',          // 按钮内边距
                boxSizing: 'border-box',
                position: 'relative',    // 为内部元素的z-index服务
                zIndex: 2                // 确保内容在背景图遮罩之上
            });
            // 1. 显示主标题 (通常是 series.name_cn)
            $contentWrapper.append(
                $('<div></div>').css({
                    fontWeight: 'bold',
                    fontSize: '14px', // 可以根据按钮大小调整
                    marginBottom: '5px',
                    wordBreak: 'break-word', // 防止长标题溢出
                    maxHeight: '2.8em', // 限制标题高度，约两行
                    overflow: 'hidden',
                    textOverflow: 'ellipsis',
                    // lineHeight: '1.4em' // 可选，用于精确控制行高
                }).text(series.title) // 使用 .text() 以避免HTML注入
            );
            // 2. 显示原始标题 (series.name / series.orititle)，如果与主标题不同且存在
            if (series.orititle && series.orititle.trim() !== '' && series.orititle.toLowerCase() !== series.title.toLowerCase()) {
                $contentWrapper.append(
                    $('<div></div>').css({
                        fontSize: '11px',
                        color: '#e0e0e0', // 在深色背景/遮罩上应该可见
                        marginBottom: '4px',
                        fontStyle: 'italic',
                        wordBreak: 'break-word',
                        maxHeight: '1.3em', // 限制一行
                        overflow: 'hidden',
                        textOverflow: 'ellipsis',
                    }).text(series.orititle)
                );
            }
            // 3. 显示作者 (排除 "取消选择" 按钮)
            if (series.id !== -1) { // series.id === -1 是取消按钮
                const authorText = series.author?.trim() || '未知作者';
                $contentWrapper.append(
                    $('<div></div>').css({
                        fontSize: '12px',
                        color: '#f0f0f0', // 确保在背景图上可读
                        marginBottom: '4px',
                        wordBreak: 'break-word',
                        maxHeight: '1.4em', // 限制一行
                        overflow: 'hidden',
                        textOverflow: 'ellipsis',
                    }).text(authorText)
                );
            }
            // 4. 显示别名 (排除 "取消选择" 按钮，且别名存在)
            if (series.id !== -1 && series.aliases && series.aliases.trim() !== '') {
                $contentWrapper.append(
                    $('<div></div>').css({
                        fontSize: '10px', // 别名用更小的字号
                        color: '#cccccc', // 浅灰色
                        marginTop: '2px',
                        wordBreak: 'break-word',
                        maxHeight: '2.4em', // 限制约两行的高度
                        overflow: 'hidden',
                        textOverflow: 'ellipsis',
                        lineHeight: '1.2em', // 调整行高以适应两行
                        // whiteSpace: 'normal' // 确保能换行
                    }).text(`别名: ${series.aliases}`)
                );
            } else if (series.id !== -1 && (!series.aliases || series.aliases.trim() === '')) {
                // 如果不是取消按钮，但没有别名，可以添加一个占位符以帮助对齐，如果需要的话
                // $contentWrapper.append($('<div></div>').css({ height: '1.2em', marginTop: '2px' }));
            }
            // 如果是 "取消选择" 按钮的特殊处理 (清空内容，只显示标题)
            if (series.id === -1) {
                $contentWrapper.empty(); // 清空之前可能添加的内容
                $contentWrapper.append(
                    $('<div></div>').css({
                        fontWeight: 'bold',
                        fontSize: '16px', // 取消按钮的文字可以大一些
                        color: 'white'    // 确保在红色背景上白色文字清晰
                    }).text(series.title) // "取消选择"
                );
                $selBookBtn.css({ // 取消按钮的特定样式
                    backgroundColor: '#dc3545', // 红色背景
                    color: 'white',
                    backgroundImage: 'none', // 无背景图片
                    textShadow: 'none',      // 无文字阴影
                    minHeight: '60px',       // 保持一个最小高度
                    height: 'auto'           // 高度自适应内容
                });
            }
            $selBookBtn.append($contentWrapper);
            // 背景图片和遮罩逻辑 (仅对非取消按钮)
            if (series.cover && series.id !== -1) {
                $selBookBtn.css({
                    'background-image': `url(${series.cover})`,
                    'background-size': 'cover',
                    'background-position': 'center',
                    'background-repeat': 'no-repeat',
                    'text-shadow': '1px 1px 3px #000, -1px -1px 3px #000, 1px -1px 3px #000, -1px 1px 3px #000', // 增强文字对比度
                    'color': 'white', // 确保文字默认为白色，在深色遮罩上可读
                    'position': 'relative', // 为遮罩定位
                    // 'border': '1px solid rgba(255,255,255,0.2)', // 可选的边框
                });
                // 添加遮罩层，确保它在背景图之上，内容在遮罩之上
                const $overlay = $('<div></div>').css({
                    position: 'absolute',
                    top: 0, left: 0,
                    width: '100%', height: '100%',
                    backgroundColor: 'rgba(0, 0, 0, 0.65)', // 增加遮罩不透明度
                    borderRadius: '10px', // 与按钮圆角一致
                    zIndex: 1 // 遮罩在背景图之上
                });
                $selBookBtn.prepend($overlay); // Prepend 使其在 $contentWrapper 之下
            }
            $selBookBtn.on('click', function (e) {
                e.stopPropagation();
                const seriesIdRes = parseInt($(this).attr('resSeriesId'));
                $selBookPanel.remove(); // 关闭面板
                resolve(seriesIdRes);   // 返回选择的 ID
            });
            $selBookPanel.append($selBookBtn);
        });
        $selBookPanel.appendTo('body');
    });
}

async function performSearchAndHandleResults(searchTerm, komgaSeriesId, $dom, searchType) {
    if (!searchTerm || searchTerm.trim() === '') {
        showMessage('搜索词不能为空', 'warning');
        return Promise.reject('Empty search term');
    }
    showMessage('正在查找《' + searchTerm + '》', 'info', 2000);
    try {
        let seriesListRes = await fetchBookByName(searchTerm, searchType); // searchType is 'btv' or 'bof'
        let seriesIdRes = 0;
        if (seriesListRes.length > 0) {
            if (seriesListRes.length > 8) seriesListRes = seriesListRes.slice(0, 8); // Limit to 8 results + Cancel
            seriesListRes.push({ id: -1, title: '取消选择', author: '' }); // Add cancel option
            seriesIdRes = await showBookSelectionPanel(seriesListRes);
            if (seriesIdRes === -1) { // User cancelled
                showMessage('检索《' + searchTerm + '》已取消', 'warning');
                return Promise.reject('Selection cancelled');
            } else if (seriesIdRes > 0) { // Valid selection
                partLoadingStart($dom);
                // seriesIdRes is the ID from the external source (BTV or BOF)
                await fetchBookByUrl(komgaSeriesId, seriesIdRes, '', searchType); // searchType is 'btv' or 'bof'
                return Promise.resolve();
            } else {
                return Promise.reject('Invalid selection ID');
            }
        } else {
             showMessage('检索《' + searchTerm + '》未找到', 'error', 4000);
             return Promise.reject('No results found');
        }
    } catch (error) {
        console.error(`Error during search/fetch process for "${searchTerm}":`, error);
        showMessage(`处理《${searchTerm}》时出错: ${error.message || error}`, 'error');
        partLoadingEnd($dom); // Ensure loading ends on error
        return Promise.reject(error);
    }
    // partLoadingEnd($dom); // This was here, but fetchBookByUrl handles its own partLoadingEnd now.
}

function extractSeriesTitles(seriesName, limitCount) {
    const parts = [...seriesName.matchAll(/\[([^\[\]]+)\]/g)].map(m => m[1]);

    let title = '';
    let authors = [];

    const authorCandidate = parts.find(p => /[×]/.test(p));
    if (authorCandidate) {
        authors = authorCandidate.split(/[×]/).map(s => t2s(s.trim()));
        for (let p of parts) {
            if (p !== authorCandidate && !title) {
                title = t2s(p.trim());
            }
        }
    } else {
        if (parts.length > 0) title = t2s(parts[0].trim());
        if (parts.length > 1) authors = [t2s(parts[1].trim())];
    }

    const titleParts = title ? title.split('_').map(t => t.trim()) : [];
    const combinedParts = [...titleParts, ...authors];

    let cleanedTitles = combinedParts.map(t =>
        t2s(t.trim())
    );

    const minimalProcessedTitle = t2s(seriesName
        .replace(/\[.*?\]/g, '')
        .replace(/【.*?】/g, '')
        .replace(/[（()）]/g, ' ')
        .replace(/[\(\[【（]?单行本[\)\]】）]?\s*$/g, '')
        .replace(/[\(\[【（]?\d+卷[\)\]】）]?\s*$/g, '')
        .replace(/[_-]?\s*$/g, '')
        .trim()
    );

    const finalTitles = [...new Set(
        cleanedTitles
            .map(t => t
                .replace(/[:：•·․,，。'’?？!！~⁓～]/g, ' ')
                .replace(/／/g, '/')
                .trim()
            )
            .filter(Boolean)
    )];

    if (minimalProcessedTitle && !finalTitles.includes(minimalProcessedTitle)) {
        finalTitles.unshift(minimalProcessedTitle);
    }

    if (typeof limitCount === 'number') {
        if (limitCount <= 0) return [];
        return finalTitles.slice(0, limitCount);
    }

    return finalTitles;
}

async function selectSeriesTitle(komgaSeriesId, $dom) {
    return new Promise(async (resolve, reject) => {
        const komgaMeta = await getKomgaSeriesMeta(komgaSeriesId).catch(() => null);
        const oriTitle = await getKomgaOriTitle(komgaSeriesId).catch(() => '');
        const seriesName = (komgaMeta?.title?.trim()) || oriTitle;

        if (!seriesName) {
            showMessage(`无法获取系列 ${komgaSeriesId} 的标题`, 'error');
            return reject('Failed to get original title');
        }

        const selTitles = extractSeriesTitles(seriesName);
        const $selTitlePanel = $('<div></div>').css({ ...selPanelStyle });
        const searchType = localStorage.getItem(`STY-${komgaSeriesId}`); // 'btv' or 'bof'

        if (selTitles.length > 0) {
            selTitles.forEach((title) => {
                let $btn = $('<button></button>').text(title).css(selPanelBtnStyle);
                $btn.on('click', async function (e) {
                    e.stopPropagation();
                    $selTitlePanel.remove();
                    try {
                        await performSearchAndHandleResults(title, komgaSeriesId, $dom, searchType);
                        resolve();
                    } catch (err) {
                        reject(err);
                    }
                });
                $selTitlePanel.append($btn);
            });
        } else {
            const $msg = $('<div style="grid-column: 1 / -1; text-align: center; padding: 10px; color: #555;">未能自动提取关键词，请手动输入。</div>');
            $selTitlePanel.append($msg);
        }

        const $manualInputContainer = $('<div></div>').css({
            gridColumn: '1 / -1',
            display: 'flex',
            gap: '10px',
            marginTop: '15px',
            padding: '10px',
            borderTop: '1px solid #ccc'
        });

        const $manualInput = $('<input type="text">').attr('id', 'manualSearchInput').css({
            flexGrow: 1,
            padding: '8px 10px',
            border: '1px solid #ccc',
            borderRadius: '4px',
            fontSize: '14px'
        }).attr('placeholder', '或在此手动输入搜索词');

        const $manualSearchBtn = $('<button>手动搜索</button>').css({
            ...selPanelBtnStyle,
            width: 'auto',
            height: 'auto',
            padding: '8px 15px',
            backgroundColor: '#007bff',
            flexShrink: 0
        });

        $manualInput.on('keydown', function (e) {
            if (e.key === 'Enter') $manualSearchBtn.click();
        });

        $manualSearchBtn.on('click', async function (e) {
            e.stopPropagation();
            const manualTerm = $('#manualSearchInput').val().trim();
            if (!manualTerm) {
                showMessage('请输入手动搜索词', 'warning');
                return;
            }
            $selTitlePanel.remove();
            try {
                await performSearchAndHandleResults(manualTerm, komgaSeriesId, $dom, searchType);
                resolve();
            } catch (err) {
                reject(err);
            }
        });

        $manualInputContainer.append($manualInput).append($manualSearchBtn);
        $selTitlePanel.append($manualInputContainer);

        const $cancelBtn = $('<button>取消搜索</button>').css({
            ...selPanelBtnStyle,
            gridColumn: '1 / -1',
            marginTop: '10px',
            backgroundColor: '#dc3545',
            minHeight: '50px',
            height: 'auto',
            padding: '10px'
        });

        $cancelBtn.on('click', function (e) {
            e.stopPropagation();
            $selTitlePanel.remove();
            showMessage('搜索已取消', 'warning');
            reject('Title selection cancelled');
        });

        $selTitlePanel.append($cancelBtn);
        $selTitlePanel.appendTo('body');
        setTimeout(() => $manualInput.focus(), 100);
    });
}

function showBatchProgress(current, total, stats) {
    let bar = document.getElementById('batchProgressBar');
    if (!bar) {
        bar = document.createElement('div');
        bar.id = 'batchProgressBar';
        bar.style.position = 'fixed';
        bar.style.bottom = '0';
        bar.style.left = '0';
        bar.style.width = '100%';
        bar.style.zIndex = '9999';
        bar.style.fontFamily = 'Arial, sans-serif';

        bar.innerHTML = `
            <div style="background:#222; color:#fff; padding:6px 12px; font-size:13px; text-align:center; user-select:none;">
                <div id="batchProgressText"></div>
                <div style="background:#444; height:8px; border-radius:4px; overflow:hidden; margin-top:6px; box-shadow: inset 0 0 5px #000;">
                    <div id="batchProgressFill" style="
                        height: 100%;
                        width: 0%;
                        border-radius: 4px;
                        background: linear-gradient(270deg, #4caf50, #81c784, #a5d6a7, #4caf50);
                        background-size: 400% 400%;
                        animation: gradientShift 3s ease infinite;
                    "></div>
                </div>
            </div>
            <style>
                @keyframes gradientShift {
                    0% { background-position: 0% 50%; }
                    50% { background-position: 100% 50%; }
                    100% { background-position: 0% 50%; }
                }
            </style>
        `;
        document.body.appendChild(bar);
    }
    let percent = ((current / total) * 100).toFixed(1);
    document.getElementById('batchProgressText').textContent =
        `批量匹配进度: ${current}/${total} | 成功: ${stats.successCount} 失败: ${stats.failureCount} 跳过: ${stats.skippedCount}`;
    document.getElementById('batchProgressFill').style.width = percent + '%';
}

function hideBatchProgress() {
    const bar = document.getElementById('batchProgressBar');
    if (bar) bar.remove();
}
//</editor-fold>

// ************************************** 事件处理 **************************************
//<editor-fold desc="事件处理">
async function handleSearchClick(komgaSeriesId, type, $dom) {
  // type is 'meta' or 'all' (sync type for Komga)
  localStorage.setItem(`SID-${komgaSeriesId}`, type); // Store Komga sync type
  await search(komgaSeriesId, $dom);
}
//</editor-fold>

// ************************************** 数据处理 **************************************
//<editor-fold desc="数据处理">
async function filterSeriesMeta(komgaSeriesId, seriesMeta) {
    const komgaMeta = await getKomgaSeriesMeta(komgaSeriesId);
    if (!komgaMeta) {
        // If no existing Komga meta, return the new meta as is
        return seriesMeta;
    }
    // Links: Merge and keep unique by label (case-insensitive)
    const existingLinks = komgaMeta.links || [];
    const newLinks = seriesMeta.links || [];
    const combinedLinks = [...existingLinks, ...newLinks];
    seriesMeta.links = combinedLinks.filter(
        (link, index, self) =>
        link.label && self.findIndex((t) => t.label && t.label.toLowerCase() === link.label.toLowerCase()) === index
    );
    // Tags: Merge, convert s2t, normalize, and keep unique
    let combinedTags = [...(komgaMeta.tags || []), ...(seriesMeta.tags || [])].map((t) => t2s(t)); // s2t on all tags
    combinedTags = combinedTags.map((t) => {
        const matchingLabel = equalLabels.find((labels) => labels.split(',').includes(t));
        return matchingLabel ? matchingLabel.split(',')[0] : t; // Normalize
    }).filter(Boolean); // Remove empty tags
    seriesMeta.tags = Array.from(new Set(combinedTags)); // Unique
    // Alternate Titles: Merge, sort (原名 first, 别名 last), and keep unique by title (case-insensitive)
    let combinedAltTitles = [...(komgaMeta.alternateTitles || []), ...(seriesMeta.alternateTitles || [])];
    combinedAltTitles.sort((a, b) => { // Custom sort: "原名" first, "别名" tends to be less specific
        if (a.label === '原名') return -1;
        if (b.label === '原名') return 1;
        if (a.label === '别名') return 1; // Put "别名" after more specific ones if not "原名"
        if (b.label === '别名') return -1;
        return 0;
    });
    seriesMeta.alternateTitles = combinedAltTitles.filter(
        (altTitle, index, self) =>
        altTitle.title && self.findIndex((t) => t.title && t.title.toLowerCase() === altTitle.title.toLowerCase()) === index
    );
    // Respect Komga's lock fields
    for (const keyName in seriesMeta) {
        if (komgaMeta[keyName + 'Lock'] === true) {
             // console.log(`KomgaBangumi: Field "${keyName}" is locked for series ${komgaSeriesId}. Skipping update.`);
             delete seriesMeta[keyName]; // Remove from payload if locked
             delete seriesMeta[keyName + 'Lock']; // Also remove the lock field itself from payload if it was carried over
        }
    }
    return seriesMeta;
}

function extractAndNormalizeTitle(str) {
    const title = extractSeriesTitles(String(str), 1)[0] || '';
    return title
        .replace(/[:：•·․,，。、'’?？!！~⁓～]/g, ' ')
        .replace(/\s+/g, '')
        .trim()
        .toLowerCase();
}

function chineseToArabic(chineseNum) {
    const cnNums = {
        '零': 0, '〇': 0,
        '一': 1, '二': 2, '两': 2, '俩': 2,
        '三': 3, '四': 4, '五': 5,
        '六': 6, '七': 7, '八': 8,
        '九': 9,
    };
    const cnUnits = {
        '十': 10,
        '百': 100,
        '千': 1000,
    };

    let result = 0;
    let unit = 1;     // 当前单位，默认是个位
    let section = 0;  // 当前节的累加结果
    let number = 0;   // 当前数字
    let str = chineseNum.split('');

    for (let i = str.length - 1; i >= 0; i--) {
        const char = str[i];
        if (cnUnits[char]) {
            unit = cnUnits[char];
            // 如果前面没有数字，比如“十二”，默认前面是1
            if (number === 0) {
                number = 1;
            }
            section += number * unit;
            number = 0;
            unit = 1;
        } else if (cnNums.hasOwnProperty(char)) {
            number = cnNums[char];
            if (i === 0) {
                // 如果是首字符是数字，要乘上当前单位
                section += number * unit;
            }
        } else {
            return NaN; // 非法字符
        }
    }

    result += section;
    return result;
}

// 辅助函数：规范化日期字符串
function normalizeDate(dateStr) {
    if (!dateStr) return undefined;

    dateStr = dateStr.trim();
    let match;

    match = dateStr.match(/^(\d{4})年(\d{1,2})月(\d{1,2})日$/);
    if (match) {
        const [, y, m, d] = match;
        return `${y}-${m.padStart(2, '0')}-${d.padStart(2, '0')}`;
    }

    match = dateStr.match(/^(\d{4})年(\d{1,2})月$/);
    if (match) {
        const [, y, m] = match;
        return `${y}-${m.padStart(2, '0')}-01`;
    }

    match = dateStr.match(/^(\d{4})-(\d{1,2})-(\d{1,2})$/);
    if (match) {
        const [, y, m, d] = match;
        return `${y}-${m.padStart(2, '0')}-${d.padStart(2, '0')}`;
    }

    match = dateStr.match(/^(\d{4})-(\d{1,2})$/);
    if (match) {
        const [, y, m] = match;
        return `${y}-${m.padStart(2, '0')}-01`;
    }

    return undefined;
}

function extractVolumeNumber(name) {
    if (!name) return null;

    let match = name.match(/Vol[.\s](\d{1,4})$/);
    if (match) return parseInt(match[1], 10);

    match = name.match(/\((\d+)\)$/);
    if (match) return parseInt(match[1], 10);

    match = name.match(/\s(\d{1,4})$/);
    if (match) return parseInt(match[1], 10);

    match = name.match(/第(\d{1,4})卷$/);
    if (match) {
        return parseInt(match[1], 10);
    }

    return null;
}

function normalizeVolNum(raw) {
    const num = extractVolumeNumber(raw);

    return num ? String(num) : '';

}
//</editor-fold>

// ************************************** API封装 **************************************
//<editor-fold desc="基本请求封装">
function asyncReq(url, method, data_ry = {}, headers = null, responseType = 'text') {
    return new Promise((resolve, reject) => {
        let requestHeaders = { ...headers }; // 从传入的 headers 开始
        let requestData = data_ry;

        if (data_ry instanceof FormData) {
            // 对于 FormData, Content-Type 由浏览器设置
        } else if (method !== "GET" && typeof data_ry === 'object') {
            requestData = JSON.stringify(data_ry);
            requestHeaders = { ...defaultReqHeaders, ...requestHeaders }; // 与默认值合并，传入的 headers 优先
        } else if (method === "GET") {
            requestData = undefined;
            // 对于 GET, 通常不需要 Content-Type
            delete requestHeaders['content-type']; // 确保 GET 请求没有默认的 content-type
        }

        // 如果适用，添加 Bangumi 特定请求头和 Authorization 令牌
        if (url.startsWith(btvApiUrl)) {
             requestHeaders = { ...bangumiApiHeaders, ...requestHeaders }; // Bangumi 基础请求头优先，然后是特定调用的请求头

             const accessToken = getBangumiAccessToken();
             if (accessToken) {
                 requestHeaders['Authorization'] = `Bearer ${accessToken}`;
                 // console.log("正在为请求使用Bangumi Access Token:", url.substring(0,60));
             } else {
                 // console.log("未找到用于请求的Bangumi Access Token:", url.substring(0,60));
             }
        }

        let requestUrl = url;
        // 为 GET 请求添加缓存清除参数，除非是不喜欢它的API (例如外部API)
        if (method === 'GET' && !url.startsWith(btvApiUrl) && !url.startsWith(bofUrl)) {
            requestUrl += (url.includes('?') ? '&' : '?') + '_=' + Date.now();
        }

        GM_xmlhttpRequest({
            method: method,
            url: requestUrl,
            headers: requestHeaders,
            data: requestData,
            responseType: responseType,
            timeout: 30000, // 30 秒超时
            onload: (response) => {
                if (response.status >= 200 && response.status < 300) {
                    resolve(responseType === 'text' || responseType === 'json' ? response.responseText : response.response);
                } else if (response.status === 401 && url.startsWith(btvApiUrl)) { // Bangumi API 认证失败
                    console.error(`[asyncReq] Bangumi API 授权错误 (401): ${method} ${requestUrl.substring(0,100)}...`, response.statusText, response.responseText?.substring(0, 200));
                    // 检查是否存在已配置的 Access Token
                    const currentToken = getBangumiAccessToken();
                    if (currentToken) {
                        showMessage(
                            `Bangumi API认证失败(401)。您配置的Access Token可能已失效或不正确。请通过油猴脚本菜单更新Token。`,
                            'error',
                            15000 // 显示更长时间
                        );
                    } else {
                        showMessage(
                            `Bangumi API认证失败(401)。如果您想使用Access Token，请通过油猴脚本菜单进行配置。`,
                            'error',
                            10000
                        );
                    }
                    reject(new Error(`HTTP Error ${response.status}: ${response.statusText || 'Unauthorized'}. Bangumi Access Token might be invalid or expired.`));
                }
                else {
                    console.error(`[asyncReq] HTTP Error (${response.status}): ${method} ${requestUrl.substring(0,100)}...`, response.statusText, response.responseText?.substring(0, 200));
                    showMessage(`请求错误 (${response.status}): ${method} ${requestUrl.substring(0, 60)}...`, 'error', 7000);
                    reject(new Error(`HTTP Error ${response.status}: ${response.statusText || 'Unknown error'}`));
                }
            },
            onerror: (error) => {
                console.error(`[asyncReq] Network Error: ${method} ${requestUrl.substring(0,100)}...`, error);
                showMessage(`网络请求失败: ${method} ${requestUrl.substring(0, 60)}...`, 'error', 7000);
                reject(new Error('Network request failed'));
            },
            ontimeout: () => {
                console.error(`[asyncReq] Timeout: ${method} ${requestUrl.substring(0,100)}...`);
                showMessage(`请求超时: ${method} ${requestUrl.substring(0, 60)}...`, 'error', 7000);
                reject(new Error('Request timed out'));
            }
        });
    });
}

async function asyncPool(items, asyncFn, limit = 5) {
    const ret = [];
    const executing = new Set();

    for (let i = 0; i < items.length; i++) {
        const item = items[i];
        // 调用异步函数，得到 Promise
        const p = Promise.resolve().then(() => asyncFn(item, i));
        ret.push(p);

        // 添加到执行中集合
        executing.add(p);

        // 当执行中任务达到最大并发数，等待最先完成的一个
        if (executing.size >= limit) {
            await Promise.race(executing);
        }

        // 一旦 Promise 完成，从执行集合移除
        p.finally(() => executing.delete(p));
    }

    // 等待剩余所有任务完成
    return Promise.all(ret);
}
//</editor-fold>

//<editor-fold desc="API封装-系列">
// 单页获取符合条件的系列
async function getAllSeries(payload) {
    const url = `${location.origin}/api/v1/series/list`;
    const params = new URLSearchParams({
        unpaged: "true",
        sort: "lastModified,desc"
    });
    try {
        const respText = await asyncReq(`${url}?${params.toString()}`, 'POST', payload);
        const data = JSON.parse(respText);
        return data?.content || [];
    } catch (e) {
        console.error(`[getAllSeries] 获取系列数据失败:`, e);
        throw e;
    }
}

async function getSeriesWithLibraryId(libraryId) {
    const payload = {
        condition: {
            allOf: [
                { libraryId: { operator: "is", value: libraryId } },
                { deleted: { operator: "isFalse" } }
            ]
        }
    };
    showMessage(`正在获取数据库 #${libraryId} 所有系列...`, 'info');

    try {
        const allSeries = await getAllSeries(payload);
        showMessage(`数据库 #${libraryId} 系列列表获取完毕，共 ${allSeries.length} 个`, 'success');
        return allSeries;
    } catch (e) {
        showMessage(`获取数据库 #${libraryId} 系列失败: ${e.message || e}`, 'error', 5000);
        return [];
    }
}

async function getSeriesWithCollection(collectionIds) {
    const ids = Array.isArray(collectionIds) ? collectionIds : [collectionIds];
    const allSeries = [];

    for (const id of ids) {
        const payload = {
            condition: {
                allOf: [
                    { collectionId: { operator: "is", value: String(id) } },
                    { deleted: { operator: "isFalse" } }
                ]
            }
        };

        try {
            const seriesList = await getAllSeries(payload);
            allSeries.push(...seriesList);
            showMessage(`收藏夹 #${id} 系列列表获取完毕，共 ${seriesList.length} 个`, 'success');
        } catch (error) {
            console.error(`获取收藏夹 #${id} 系列时出错:`, error);
            showMessage(`获取收藏夹 #${id} 系列失败：${error.message || error}`, 'error', 5000);
        }
    }

    return allSeries;
}

async function getLatestSeries(libraryIds = null, page = 0) {
    const params = new URLSearchParams({
        size: 30,
        page,
        deleted: "false"
    });
    if (libraryIds) {
        const ids = Array.isArray(libraryIds) ? libraryIds : [libraryIds];
        ids.forEach(id => params.append("library_id", id));
    }
    try {
        const resText = await asyncReq(`${location.origin}/api/v1/series/latest?${params.toString()}`, 'GET');
        const data = JSON.parse(resText);
        showMessage(`已获取最近系列：共 ${data.content?.length || 0} 项`, 'success', 3000);
        return data?.content || [];
    } catch (error) {
        console.error("获取最近系列时出错:", error);
        showMessage(`获取最近系列失败：${error.message || error}`, 'error', 5000);
        return [];
    }
}

async function getKomgaSeriesData(komgaSeriesId) {
    const seriesUrl = `${location.origin}/api/v1/series/${komgaSeriesId}`;
    try {
        const seriesResStr = await asyncReq(seriesUrl, 'GET');
        return JSON.parse(seriesResStr);
    } catch (error) {
        console.error(`[getKomgaSeriesData] Failed for ID ${komgaSeriesId}:`, error);
        showMessage(`获取系列 ${komgaSeriesId} 数据失败`, 'error');
        return null;
    }
}

async function getKomgaSeriesMeta(komgaSeriesId) {
    const seriesData = await getKomgaSeriesData(komgaSeriesId);
    return seriesData ? seriesData.metadata : null;
}

async function getKomgaOriTitle(komgaSeriesId) { // This gets series.name (folder name)
    const seriesData = await getKomgaSeriesData(komgaSeriesId);
    return seriesData ? seriesData.name : null;
}

async function updateKomgaSeriesMeta(komgaSeriesId, komgaSeriesName, komgaSeriesMeta) {
    const bookMetaUrl = `${location.origin}/api/v1/series/${komgaSeriesId}/metadata`;
    // Filter out null or empty string values before sending, but allow empty arrays (for tags, links etc.)
    const cleanMeta = Object.fromEntries(
        Object.entries(komgaSeriesMeta).filter(([_, v]) => v !== null && v !== '' || (Array.isArray(v)))
    );
    if (Object.keys(cleanMeta).length === 0) {
        // console.log(`[updateKomgaSeriesMeta] No metadata to update for ${komgaSeriesName}.`);
        return;
    }
    try {
        await asyncReq(bookMetaUrl, 'PATCH', cleanMeta); // Komga API handles empty arrays correctly (e.g. clearing tags)
        showMessage(`《${komgaSeriesName}》系列信息已更新`, 'success', 1500);
    } catch (e) {
        console.error(`[updateKomgaSeriesMeta] Failed for ${komgaSeriesName}:`, e);
        showMessage(`《${komgaSeriesName}》系列信息更新失败`, 'error', 5000);
    }
}

async function getKomgaSeriesCovers(komgaSeriesId) {
    let allSeriesCoverUrl = `${location.origin}/api/v1/series/${komgaSeriesId}/thumbnails`;
    try {
        const coversStr = await asyncReq(allSeriesCoverUrl, 'GET');
        return JSON.parse(coversStr);
    } catch (e) {
        console.error(`[getKomgaSeriesCovers] Failed for ID ${komgaSeriesId}:`, e);
        return []; // Return empty array on error
    }
}

async function updateKomgaSeriesCover(komgaSeriesId, komgaSeriesName, orderedImageUrls) {
    if (!orderedImageUrls || orderedImageUrls.length === 0) {
        showMessage(`《${komgaSeriesName}》系列封面URL列表为空，跳过更新`, 'warning');
        return false;
    }
    await cleanKomgaSeriesCover(komgaSeriesId, komgaSeriesName);

    let blob;
    let validTried = false;

    for (let i = 0; i < orderedImageUrls.length; i++) {
        const imgUrl = orderedImageUrls[i];
        const imageSizeLabel = i === 0 ? "首选" : (i === 1 ? "中尺寸" : (i === 2 ? "通用尺寸" : "小尺寸"));
        try {
            showMessage(`《${komgaSeriesName}》尝试上传 ${imageSizeLabel} 系列封面...`, 'info', 2000);
            blob = await asyncReq(imgUrl, 'GET', undefined, {}, 'blob');

            if (!blob || blob.size === 0) {
                console.warn(`[updateKomgaSeriesCover] 下载图片 ${imgUrl} 失败或为空 blob。`);
                throw new Error("下载图片 blob 失败或为空");
            }

            if (blob.size < 60 * 1024) {
                console.warn(`[updateKomgaSeriesCover] 跳过 ${imageSizeLabel} 封面，文件太小: ${blob.size} bytes`);
                showMessage(`《${komgaSeriesName}》${imageSizeLabel} 封面太小(${(blob.size / 1024).toFixed(1)}kB)，跳过`, 'warning', 2000);
                continue;
            }

            validTried = true;
            let updateSeriesCoverUrl = `${location.origin}/api/v1/series/${komgaSeriesId}/thumbnails`;
            const seriesCoverFormdata = new FormData();
            const fileName = `series_cover_${komgaSeriesId}.jpg`;
            const seriesCoverFile = new File([blob], fileName, { type: blob.type || 'image/jpeg' });
            seriesCoverFormdata.append('file', seriesCoverFile);
            seriesCoverFormdata.append('selected', 'true');
            await asyncReq(updateSeriesCoverUrl, 'POST', seriesCoverFormdata);

            showMessage(`《${komgaSeriesName}》系列封面 (${imageSizeLabel}) 已更新`, 'success', 2500);
            return true;
        } catch (e) {
            const errorMessage = e.message || String(e);
            if (errorMessage.includes("HTTP Error 413")) {
                console.warn(`[updateKomgaSeriesCover] 《${komgaSeriesName}》上传 ${imageSizeLabel} 封面 (${imgUrl}) 失败 (413 Payload Too Large). 大小: ${blob ? blob.size + ' bytes' : '未知'}. 尝试下一个尺寸...`);
                showMessage(`《${komgaSeriesName}》${imageSizeLabel} 封面过大(413)，尝试更小尺寸...`, 'warning', 3000);
                if (i === orderedImageUrls.length - 1 && validTried) {
                    showMessage(`《${komgaSeriesName}》所有尺寸系列封面均因过大(413)上传失败。请检查服务器配置。`, 'error', 7000);
                }
            } else {
                console.error(`[updateKomgaSeriesCover] 《${komgaSeriesName}》上传 ${imageSizeLabel} 封面 (${imgUrl}) 失败:`, e);
                showMessage(`《${komgaSeriesName}》系列封面 (${imageSizeLabel}) 更新失败: ${errorMessage}`, 'error', 5000);
                return false;
            }
        }
    }

    if (!validTried) {
        showMessage(`《${komgaSeriesName}》所有封面文件均小于 60kB，未上传封面`, 'error', 4000);
    } else {
        console.error(`[updateKomgaSeriesCover] 《${komgaSeriesName}》所有尝试均未能成功上传系列封面。`);
    }

    return false;
}

async function cleanKomgaSeriesCover(komgaSeriesId, komgaSeriesName) {
    const thumbs = await getKomgaSeriesCovers(komgaSeriesId);
    // Filter for thumbnails that are USER_UPLOADED and NOT currently selected
    const thumbsToClean = thumbs?.filter((thumb) => thumb.type === 'USER_UPLOADED' && thumb.selected === false) || [];
    if (thumbsToClean.length === 0) return;
    const cleanSeriesCoverUrlBase = `${location.origin}/api/v1/series/${komgaSeriesId}/thumbnails/`;
    for (const thumb of thumbsToClean) {
        try {
            await asyncReq(cleanSeriesCoverUrlBase + thumb.id, 'DELETE');
            // showMessage(`《${komgaSeriesName}》旧封面 (ID: ${thumb.id}) 已清理`, 'info', 1000);
        } catch (e) {
            console.error(`[cleanKomgaSeriesCover] Failed to delete thumb ${thumb.id} for ${komgaSeriesName}:`, e);
            showMessage(`《${komgaSeriesName}》系列封面清理失败 (ID: ${thumb.id})`, 'error', 5000);
        }
    }
}
//</editor-fold>

//<editor-fold desc="API封装-话卷">
async function getKomgaSeriesBooks(komgaSeriesId) {
    const url = `${location.origin}/api/v1/books/list`;

    const payload = {
        condition: {
            allOf: [
                {
                    seriesId: {
                        operator: "is",
                        value: String(komgaSeriesId)
                    }
                },
                {
                    deleted: {
                        operator: "isFalse"
                    }
                }
            ]
        }
    };

    const params = new URLSearchParams({
        unpaged: "true",
        sort: "metadata.numberSort,asc"
    });

    try {
        const resText = await asyncReq(`${url}?${params.toString()}`, 'POST', payload);
        const data = JSON.parse(resText);
        const allBooks = data?.content || [];

        return {
            content: allBooks,
            numberOfElements: allBooks.length
        };
    } catch (e) {
        console.error(`[getKomgaSeriesBooks] 获取系列 ${komgaSeriesId} 书籍失败:`, e);
        showMessage(`获取系列 ${komgaSeriesId} 的书籍失败`, 'error');
        return {
            content: [],
            numberOfElements: 0
        };
    }
}

async function updateKomgaBookMeta(book, komgaSeriesName, bookMeta) {
    // Filter out null or empty string values before sending
    const cleanMeta = Object.fromEntries(Object.entries(bookMeta).filter(([_, v]) => v !== null && v !== ''));
    if (Object.keys(cleanMeta).length === 0) {
        return; // No actual metadata to update
    }
    try {
        await asyncReq(`${location.origin}/api/v1/books/${book.id}/metadata`, 'PATCH', cleanMeta);
        showMessage(`《${komgaSeriesName}》第 ${book.number} 卷信息已更新`, 'success', 1000);
    } catch (e) {
        console.error(`[updateKomgaBookMeta] Failed for ${komgaSeriesName} Vol ${book.number}:`, e);
        showMessage(`《${komgaSeriesName}》第 ${book.number} 卷信息更新失败`, 'error', 5000);
    }
}

async function updateKomgaBookCover(book, komgaSeriesName, bookNumberForDisplay, orderedImageUrls) {
    if (!orderedImageUrls || orderedImageUrls.length === 0) {
        return false;
    }

    let blob;
    let validTried = false;

    for (let i = 0; i < orderedImageUrls.length; i++) {
        const imgUrl = orderedImageUrls[i];
        const imageSizeLabel = i === 0 ? "首选" : (i === 1 ? "中等" : (i === 2 ? "通用" : "较小"));
        try {
            showMessage(`《${komgaSeriesName}》卷 ${bookNumberForDisplay} 尝试上传 ${imageSizeLabel} 封面...`, 'info', 1500);
            blob = await asyncReq(imgUrl, 'GET', undefined, {}, 'blob');

            if (!blob || blob.size === 0) throw new Error("下载图片 blob 失败");

            if (blob.size >= 1024 * 1024) {
                console.warn(`[updateKomgaBookCover] 跳过 ${imageSizeLabel} 封面，文件太大: ${blob.size} bytes`);
                showMessage(`《${komgaSeriesName}》卷 ${bookNumberForDisplay} ${imageSizeLabel} 封面太大(${(blob.size / 1024).toFixed(1)}kB)，跳过`, 'warning', 2000);
                continue;
            }

            if (blob.size < 30 * 1024) {
                console.warn(`[updateKomgaBookCover] 跳过 ${imageSizeLabel} 封面，文件太小: ${blob.size} bytes`);
                showMessage(`《${komgaSeriesName}》卷 ${bookNumberForDisplay} ${imageSizeLabel} 封面太小(${(blob.size / 1024).toFixed(1)}kB)，跳过`, 'warning', 2000);
                continue;
            }

            validTried = true;
            let updateBookCoverUrl = `${location.origin}/api/v1/books/${book.id}/thumbnails`;
            let bookCoverFormdata = new FormData();
            let bookCoverName = `vol_${bookNumberForDisplay}_cover.jpg`;
            let bookCoverFile = new File([blob], bookCoverName, { type: blob.type || 'image/jpeg' });
            bookCoverFormdata.append('file', bookCoverFile);
            bookCoverFormdata.append('selected', 'true');
            await asyncReq(updateBookCoverUrl, 'POST', bookCoverFormdata);

            showMessage(`《${komgaSeriesName}》卷 ${bookNumberForDisplay} 封面 (${imageSizeLabel}版本) 已更新`, 'success', 1500);
            return true;
        } catch (e) {
            const errorMessage = e.message || String(e);
            if (errorMessage.includes("HTTP Error 413")) {
                console.warn(`[updateKomgaBookCover] 《${komgaSeriesName}》卷 ${bookNumberForDisplay} 上传 ${imageSizeLabel} 封面失败 (413): 大小 ${blob ? blob.size + ' bytes' : '未知'}，尝试下一个`);
                showMessage(`《${komgaSeriesName}》卷 ${bookNumberForDisplay} ${imageSizeLabel} 封面过大(413)，尝试更小...`, 'warning', 2500);
                if (i === orderedImageUrls.length - 1 && validTried) {
                    showMessage(`《${komgaSeriesName}》卷 ${bookNumberForDisplay} 所有尺寸封面均因过大(413)上传失败。`, 'error', 6000);
                }
            } else {
                console.error(`[updateKomgaBookCover] 《${komgaSeriesName}》卷 ${bookNumberForDisplay} 上传 ${imageSizeLabel} 封面失败:`, e);
                showMessage(`《${komgaSeriesName}》卷 ${bookNumberForDisplay} 封面 (${imageSizeLabel}版本) 更新失败: ${errorMessage}`, 'error', 5000);
                return false;
            }
        }
    }

    if (!validTried) {
        showMessage(`《${komgaSeriesName}》卷 ${bookNumberForDisplay} 所有封面均小于 60kB，未上传`, 'error', 4000);
    } else {
        console.error(`[updateKomgaBookCover] 《${komgaSeriesName}》卷 ${bookNumberForDisplay} 所有尝试均未能成功上传封面。`);
    }

    return false;
}

async function updateKomgaBookAll(komgaSeriesId, seriesBooks, seriesName, bookAuthors, bookVolumeCoverSets, volumeMates = []) {
    // bookVolumeCoverSets 结构: [{ coverUrls: [urlL, urlM, urlC] }, { coverUrls: [...] }, ...]
    // 或者是空数组 [] (无封面或只更新作者)
    if (!seriesBooks || seriesBooks.numberOfElements === 0) return;

    if (seriesBooks.numberOfElements >= maxReqBooks) {
        showMessage(`系列《${seriesName}》的书籍数量 (${seriesBooks.numberOfElements}) 达到或超过 ${maxReqBooks} 的限制，跳过书籍处理。`, 'warning', 6000);
        return;
    }

    const series = await getKomgaSeriesData(komgaSeriesId);
    if (series.oneshot) {
        const existingVol = (volumeMates && volumeMates.length > 0) ? volumeMates[0] : {};
        const mergedVol = {
            ...existingVol,
            ...series,
        };

        if (series.metadata?.summary) {
            mergedVol.summary = series.metadata.summary;
        }

        volumeMates = [mergedVol];
    }

    const booksToProcess = seriesBooks.content || [];
    const bookUpdateNeeded = bookAuthors?.length > 0 || volumeMates.length > 0;

    const coverUpdateNeeded = bookVolumeCoverSets &&
                             bookVolumeCoverSets.length > 0 &&
                             bookVolumeCoverSets.some(set => set && set.coverUrls && set.coverUrls.length > 0);

    if (!bookUpdateNeeded && !coverUpdateNeeded) return;

    const volumeTitlePattern = /(?:vol(?:ume)?s?|巻|卷|册|冊|第)(?!.*(?:话|話|章|回|迴|篇|期|辑|輯|节|節|页|頁|部))[\W_]*?(?<volNum>\d+|[一二三四五六七八九十百千零〇两俩]+)\s*(?:巻|卷|册|冊|集)?/i;

    for (let i = 0; i < booksToProcess.length; i++) {
        const book = booksToProcess[i];
        const bookname = book.name ?? '';
        const booktitle = book.metadata?.title ?? '';
        let volNum = 0;

        // 用正则提取卷号数字
        const match = bookname.match(volumeTitlePattern) || booktitle.match(volumeTitlePattern);
        if (match?.groups?.volNum) {
            const volStr = match.groups.volNum;
            volNum = /^\d+$/.test(volStr) ? parseInt(volStr, 10) : chineseToArabic(volStr);
        }

        const bookNumberForDisplay = volNum.toString().padStart(2, '0');

        try {
            // 查找volumeMates中匹配的卷
            const volNumInt = parseInt(volNum, 10);
            let mate = null;

            if (series.oneshot) {
                mate = volumeMates.length > 0 ? volumeMates[0] : null;
            } else if (volumeMates.length > 0 && volNumInt > 0) {
                mate = volumeMates.find(m => parseInt(m.num, 10) === volNumInt) || null;
            }

            if (bookUpdateNeeded) {
                const bookMeta = {
                    authors: bookAuthors,
                    title: (volumeTitlePattern.test(bookname) || volumeTitlePattern.test(booktitle)) ? `卷 ${bookNumberForDisplay}` : undefined,
                    number: (volumeTitlePattern.test(bookname) || volumeTitlePattern.test(booktitle)) ? bookNumberForDisplay : undefined,
                };

                if (mate) {
                    if (mate.summary) bookMeta.summary = mate.summary;
                    if (mate.releaseDate) bookMeta.releaseDate = mate.releaseDate;
                    if (mate.isbn) bookMeta.isbn = mate.isbn;
                    if (mate.metadata?.links) bookMeta.links = mate.metadata.links;
                    if (mate.metadata?.tags && mate.metadata.tags.length > 0) {
                        bookMeta.tags = mate.metadata.tags.map(tag => tag.trim()).filter(tag => tag !== '');
                    };
                }

                await updateKomgaBookMeta(book, seriesName, bookMeta);
            }

            if (coverUpdateNeeded) {
                let coverUrls = null;
                if (volumeMates.length > 0) {
                    // volumeMates 存在时，按匹配的卷号找对应封面
                    if (mate) {
                        const mateIndex = volumeMates.indexOf(mate);
                        if (bookVolumeCoverSets[mateIndex]?.coverUrls?.length > 0) {
                            coverUrls = bookVolumeCoverSets[mateIndex].coverUrls;
                        }
                    }
                } else {
                    // volumeMates 不存在时，按原索引匹配封面
                    if (bookVolumeCoverSets[i]?.coverUrls?.length > 0) {
                        coverUrls = bookVolumeCoverSets[i].coverUrls;
                    }
                }

                if (coverUrls) {
                    await updateKomgaBookCover(book, seriesName, bookNumberForDisplay, coverUrls);
                }
            }
        } catch (bookError) {
            console.error(`[updateKomgaBookAll] Error processing book ${book.id} (Vol ${bookNumberForDisplay}) for series "${seriesName}":`, bookError);
        }
    }
}

function ifUpdateBook(seriesBooks, bookAuthors) {
    // This function decides if book authors should be updated.
    // It's a heuristic based on the format of the last book's title.
    if (!bookAuthors || bookAuthors.length === 0) return false; // No authors to update with
    if (!seriesBooks || !seriesBooks.content || seriesBooks.content.length === 0) return false; // No books in Komga
    const seriesBooksContent = seriesBooks.content;
    const lastBook = seriesBooksContent[seriesBooksContent.length - 1]; // Check the last book
    const lastBookMeta = lastBook.metadata;
    // If last book has no metadata, update
    if (!lastBookMeta || !lastBookMeta.title || !lastBookMeta.summary || !lastBookMeta.releaseDate || !lastBookMeta.isbn) return true;
    const lastBookTitle = lastBookMeta.title;
    // If title is very long, or contains typical filename patterns, it's likely not manually set
    if (lastBookTitle.length > 16) return true; // Arbitrary length, adjust if needed
    if (lastBookTitle.includes('[') || lastBookTitle.includes(']')) return true;
    if (lastBookTitle.toLowerCase().includes('.zip') || lastBookTitle.toLowerCase().includes('.cbz')) return true;
    // If title looks like "Vol XX" or "卷 XX", it might be okay, but we still might want to ensure authors are set.
    // If it does NOT look like a standard volume title, it's probably a filename, so update.
    if (!/^vol(ume)?s?\s*\d+/i.test(lastBookTitle) && !/^(?:巻|卷|册|第)\s*\d+/i.test(lastBookTitle)) return true;
    // If Komga authors for the last book are empty, update
    const lastBookKomgaAuthors = lastBookMeta.authors || [];
    if (lastBookKomgaAuthors.length === 0) return true;
    // TODO: Could add a check to see if Komga authors match the new authors.
    // For now, if authors exist, and title isn't a clear filename, assume it might be okay.
    // The current logic is more aggressive towards updating if the title isn't "卷 XX" or "Vol XX"
    // or if authors are missing.
    return false; // Default to not updating if none of the above "bad title" conditions are met and authors exist
}

function getVolumeNumsNeedUpdate(seriesBooks) {
    if (!seriesBooks || !seriesBooks.content || seriesBooks.content.length === 0) return new Set();

    const volumeTitlePattern = /(?:vol(?:ume)?s?|巻|卷|册|冊|第)(?!.*(?:话|話|章|回|迴|篇|期|辑|輯|节|節|页|頁|部))[\W_]*?(?<volNum>\d+|[一二三四五六七八九十百千零〇两俩]+)\s*(?:巻|卷|册|冊|集)?/i;
    const needUpdateVolumeNums = new Set();

    for (const book of seriesBooks.content) {
        const title = book?.metadata?.title || book?.name || "";

        if (!volumeTitlePattern.test(title)) continue;

        const match = title.match(volumeTitlePattern);
        const volNum = match?.groups?.volNum || null;

        if (!volNum) continue;

        const meta = book?.metadata;

        if (!meta || !meta.title || !meta.summary || !meta.releaseDate || !meta.isbn) {
            needUpdateVolumeNums.add(Number(volNum));
        }
    }
    return needUpdateVolumeNums;
}
//</editor-fold>

//<editor-fold desc="API封装-收藏夹">
const MANUAL_MATCH_COLLECTION_NAME = "手动匹配";
let _manualMatchCollectionId = null;
let _manualMatchCollectionExistingSeriesIds = []; // Cache existing series IDs in the collection

async function getAllCollections() {
    const url = `${location.origin}/api/v1/collections?unpaged=true`;
    try {
        const resText = await asyncReq(url, 'GET');
        const data = JSON.parse(resText);
        return data?.content || [];
    } catch (e) {
        console.error('[getAllCollections] 获取收藏夹失败:', e);
        throw e;
    }
}

async function createCollection(name, seriesIds = [], ordered = false) {
    const url = `${location.origin}/api/v1/collections`;
    const payload = { name, seriesIds, ordered };
    try {
        const resText = await asyncReq(url, 'POST', payload);
        const data = JSON.parse(resText);
        return data;
    } catch (e) {
        console.error(`[createCollection] 创建收藏夹 "${name}" 失败:`, e);
        throw e;
    }
}

async function updateCollectionSeries(collectionId, seriesIds) {
    if (!collectionId) throw new Error('collectionId 不能为空');
    const url = `${location.origin}/api/v1/collections/${collectionId}`;
    try {
        await asyncReq(url, 'PATCH', { seriesIds });
    } catch (e) {
        console.error(`[updateCollectionSeries] 更新收藏夹 ${collectionId} 失败:`, e);
        throw e;
    }
}

async function ensureManualMatchCollectionExists(initialSeriesIdForCreation = null) {
    if (_manualMatchCollectionId) return true; // Already found/created

    try {
        const collections = await getAllCollections();
        const collection = collections.find(c => c.name === MANUAL_MATCH_COLLECTION_NAME);

        if (collection) {
            _manualMatchCollectionId = collection.id;
            _manualMatchCollectionExistingSeriesIds = collection.seriesIds || [];
            console.log(`[收藏夹] 已找到 "${MANUAL_MATCH_COLLECTION_NAME}" (ID:${_manualMatchCollectionId})。包含 ${_manualMatchCollectionExistingSeriesIds.length} 个系列`);
            return true;
        }

        if (!initialSeriesIdForCreation) {
            console.log(`[收藏夹] "${MANUAL_MATCH_COLLECTION_NAME}" 不存在，且未提供初始系列ID，将等待实际失败系列出现时创建`);
            return false;
        }

        console.log(`[收藏夹] "${MANUAL_MATCH_COLLECTION_NAME}" 不存在，使用系列ID "${initialSeriesIdForCreation}" 创建`);
        const created = await createCollection(MANUAL_MATCH_COLLECTION_NAME, [initialSeriesIdForCreation]);
        _manualMatchCollectionId = created.id;
        _manualMatchCollectionExistingSeriesIds = created.seriesIds || [initialSeriesIdForCreation];
        showMessage(`[收藏夹] 已使用系列ID ${initialSeriesIdForCreation} 创建 "${MANUAL_MATCH_COLLECTION_NAME}" (ID:${_manualMatchCollectionId})`, 'success', 3500);
        return true;

    } catch (error) {
        console.error(`[ensureManualMatchCollectionExists] 操作 "${MANUAL_MATCH_COLLECTION_NAME}" 失败:`, error);
        showMessage(`操作 "${MANUAL_MATCH_COLLECTION_NAME}" 收藏夹失败: ${error.message || error}`, 'error', 7000);
        _manualMatchCollectionId = null;
        _manualMatchCollectionExistingSeriesIds = [];
        return false;
    }
}

async function addSeriesToManualMatchCollectionImmediately(seriesIdToAdd, seriesNameToAdd) {
    if (!seriesIdToAdd) return false;
    let collectionReady = _manualMatchCollectionId ? true : false;
    if (!collectionReady) {
        collectionReady = await ensureManualMatchCollectionExists(seriesIdToAdd);
    }

    if (!collectionReady || !_manualMatchCollectionId) {
        showMessage(`[收藏夹] 因 "${MANUAL_MATCH_COLLECTION_NAME}" 未就绪/创建失败，无法添加《${seriesNameToAdd || seriesIdToAdd}》。`, 'error', 4000);
        return false;
    }

    if (_manualMatchCollectionExistingSeriesIds.includes(seriesIdToAdd)) {
        return true;
    }

    const newSeriesList = [..._manualMatchCollectionExistingSeriesIds, seriesIdToAdd];
    try {
        await updateCollectionSeries(_manualMatchCollectionId, newSeriesList);
        _manualMatchCollectionExistingSeriesIds.push(seriesIdToAdd);
        showMessage(`《${seriesNameToAdd || seriesIdToAdd}》已添加至 "${MANUAL_MATCH_COLLECTION_NAME}"。`, 'success', 3000);
        return true;
    } catch (error) {
        console.error(`[addSeriesToCollImm] 添加系列 ${seriesIdToAdd} ("${seriesNameToAdd}") 到收藏夹 ${_manualMatchCollectionId} 失败:`, error);
        showMessage(`添加《${seriesNameToAdd || seriesIdToAdd}》至 "${MANUAL_MATCH_COLLECTION_NAME}" 失败: ${error.message || error}`, 'error', 5000);
        return false;
    }
}

async function removeSeriesFromManualMatchCollectionIfExists(seriesIdToRemove, seriesNameToRemove) {
    if (!seriesIdToRemove || !_manualMatchCollectionId) return false;

    if (!_manualMatchCollectionExistingSeriesIds.includes(seriesIdToRemove)) {
        return false; // Not in collection, nothing to remove
    }

    const newSeriesList = _manualMatchCollectionExistingSeriesIds.filter(id => id !== seriesIdToRemove);
    try {
        await updateCollectionSeries(_manualMatchCollectionId, newSeriesList);
        _manualMatchCollectionExistingSeriesIds = newSeriesList;
        console.info(`[收藏夹] 《${seriesNameToRemove || seriesIdToRemove}》已从 "${MANUAL_MATCH_COLLECTION_NAME}" 移除 (匹配成功)`);
        return true;
    } catch (error) {
        console.error(`[removeSeriesFromColl] 从收藏夹移除系列 ${seriesIdToRemove} ("${seriesNameToRemove}") 失败:`, error);
        return false;
    }
}
//</editor-fold>

// ************************************* 第三方请求 (Bangumi API and bookof.moe) *************************************
//<editor-fold desc="第三方请求">
async function fetchBookByName(seriesName, source, limit = 8) {
  source = source ? source.toLowerCase() : 'btv'; // Default to btv (Bangumi API)
  try {
      switch (source) {
        case 'btv': return await fetchBtvSubjectByNameAPI(seriesName, limit);
        case 'bof': return await fetchMoeBookByName(seriesName, limit); // Stays as is (scraping)
        default:    return await fetchBtvSubjectByNameAPI(seriesName, limit);
      }
  } catch (error) {
      console.error(`[fetchBookByName] Error searching "${seriesName}" on ${source}:`, error);
      showMessage(`在 ${source.toUpperCase()} 搜索 《${seriesName}》 失败: ${error.message || error}`, 'error');
      return []; // Return empty array on error
  }
}

async function fetchBookByUrl(komgaSeriesId, reqSeriesId, reqSeriesUrl = '', source = 'btv') {
    // reqSeriesId is the ID from BTV or BOF
    // reqSeriesUrl is if a direct URL was already known (e.g. from Komga links)
    source = source ? source.toLowerCase() : 'btv';
    const $dom = findDomElementForSeries(komgaSeriesId) || $('body'); // Fallback to body for loading indicator if DOM not found

    try {
        switch (source) {
            case 'btv':
                await fetchBtvSubjectByUrlAPI(komgaSeriesId, reqSeriesId, reqSeriesUrl);
                break;
            case 'bof':
                await fetchMoeBookByUrl(komgaSeriesId, reqSeriesId, reqSeriesUrl); // Stays as is (scraping)
                break;
            default:
                await fetchBtvSubjectByUrlAPI(komgaSeriesId, reqSeriesId, reqSeriesUrl);
                break;
        }
    } catch (error) {
         console.error(`[fetchBookByUrl] Overall error fetching/processing for KomgaID ${komgaSeriesId} from ${source.toUpperCase()}:`, error);
         showMessage(`处理系列 ${komgaSeriesId} (${source.toUpperCase()}) 时发生错误: ${error.message || error}`, 'error', 10000);
    } finally {
        partLoadingEnd($dom); // Ensure loading indicator is removed
    }
}

// 辅助函数：尝试从 infobox 数组中提取特定 key 的值
function parseInfobox(infoboxArray, targetKey) {
    if (!infoboxArray || !Array.isArray(infoboxArray)) return null;
    const item = infoboxArray.find(i => i.key === targetKey);
    if (!item) return null;
    if (typeof item.value === 'string') return item.value;
    if (Array.isArray(item.value)) { // e.g., [{v: "value1"}, {v: "value2"}] or simple array of strings
        return item.value.map(v => (typeof v === 'object' && v.v !== undefined) ? v.v : v).filter(v => typeof v === 'string').join('、');
    }
    if (typeof item.value === 'object' && item.value.v !== undefined) return item.value.v; // Single object like {v: "value"}
    return null;
}

// 辅助函数：提取任意结构中的别名信息
function extractAliases(infoboxArray) {
    const aliases = new Set();
    if (!infoboxArray || !Array.isArray(infoboxArray)) return "";

    for (const item of infoboxArray) {
        // 1. 处理直接别名项（支持简体和繁体）
        const isAliasKey = item.key === "别名" || item.key === "別名";
        if (isAliasKey) {
            // 处理所有可能的值类型
            if (typeof item.value === "string") {
                aliases.add(item.value.trim());
            } else if (Array.isArray(item.value)) {
                item.value.forEach(v => {
                    if (typeof v === "string") {
                        aliases.add(v.trim());
                    } else if (v?.v && typeof v.v === "string") {
                        aliases.add(v.v.trim());
                    }
                });
            } else if (item.value?.v && typeof item.value.v === "string") {
                aliases.add(item.value.v.trim());
            }
        }
        
        // 2. 处理嵌套别名项（支持简体和繁体）
        if (Array.isArray(item.value)) {
            for (const subItem of item.value) {
                // 检查子项是否是别名（支持简体和繁体）
                const isSubAlias = subItem?.k === "别名" || subItem?.k === "別名";
                if (isSubAlias && typeof subItem.v === "string") {
                    aliases.add(subItem.v.trim());
                } 
                // 处理嵌套的别名数组
                else if (isSubAlias && Array.isArray(subItem.v)) {
                    subItem.v.forEach(alias => {
                        if (typeof alias === "string") {
                            aliases.add(alias.trim());
                        } else if (alias?.v) {
                            aliases.add(alias.v.trim());
                        }
                    });
                }
            }
        }
    }
    
    return Array.from(aliases).filter(a => a).join(" / ");
}

async function fetchBtvSubjectByNameAPI(seriesName, limit = 8) {
    const searchUrl = `${btvApiUrl}/v0/search/subjects?limit=20`;
    const requestBody = {
      keyword: seriesName,
      sort: "match",
      filter: {
        type: [1], // 1 for Books (漫画, 画集, 轻小说)
        nsfw: true // 搜索结果包含 nsfw 条目，需要设置 Bangumi API Access Token
      }
    };

    try {
      const searchResStr = await asyncReq(searchUrl, 'POST', requestBody, {});
      const searchRes = JSON.parse(searchResStr);

      if (!searchRes || !searchRes.data || searchRes.data.length === 0) {
          console.log(`[fetchBtvSubjectByNameAPI] 搜索 "${seriesName}" (type: 书籍) 未找到任何结果。`);
          return [];
      }

      const matchType = getBangumiMatchType();
      const filteredData = searchRes.data.filter(item => item.platform === matchType);

      if (filteredData.length === 0) {
          console.log(`[fetchBtvSubjectByNameAPI] 搜索 "${seriesName}" 未找到 platform 为 "${matchType}" 的条目。`);
          return [];
      }

      const results = filteredData.map(item => {
          let authorName = "未知作者";
          let aliasesString = ""; // 用于存储处理后的别名字符串

          if (item.infobox) {
              // 提取作者 (优先作画，其次作者，再次原作)
              const authorFromInfo = parseInfobox(item.infobox, "作画") ||
                                     parseInfobox(item.infobox, "作者") ||
                                     parseInfobox(item.infobox, "原作") ||
                                     parseInfobox(item.infobox, "脚本");
              if (authorFromInfo) {
                  authorName = authorFromInfo.split(/[、→・×]/)[0].replace(/[《【（\[\(][^》】）\]\)]*[》】）\]\)]\s*$/, '').trim(); // 取第一个作为主要作者
              }

              // 提取并处理别名
              aliasesString = item.infobox ? extractAliases(item.infobox) : "";
          }

          return {
              id: item.id,
              title: item.name_cn || item.name, // 优先中文名
              orititle: item.name, // 原始名
              author: authorName,
              aliases: aliasesString, // 别名
              cover: item.image || item.images?.medium || item.images?.common || item.images?.small || null, // 优先 image (通常是主封面)
          };
      });

      return (typeof limit === 'number' && limit > 0) ? results.slice(0, limit) : results; // Limit results if too many

    } catch (error) {
        // asyncReq already shows a message and logs the error
        console.error(`[fetchBtvSubjectByNameAPI] POST 请求 "${seriesName}" 失败:`, error);
        throw error; // Re-throw to be caught by caller
    }
}

async function fetchBtvSubjectByUrlAPI(komgaSeriesId, reqSeriesId, reqSeriesUrl = '') {
    const komgaSeries = await getKomgaSeriesData(komgaSeriesId);
    const subjectId = reqSeriesId || (reqSeriesUrl.match(/subject\/(\d+)/) ? reqSeriesUrl.match(/subject\/(\d+)/)[1] : null);
    if (!subjectId) {
        throw new Error("Bangumi Subject ID is missing.");
    }
    const apiUrl = `${btvApiUrl}/v0/subjects/${subjectId}`;
    const seriesResStr = await asyncReq(apiUrl, 'GET', undefined, {}); // API call
    const btvData = JSON.parse(seriesResStr);

    let seriesMeta = {
        title: '', titleLock: false, titleSort: '', titleSortLock: false,
        status: '', statusLock: false, tags: [], tagsLock: false,
        links: [{ label: 'Btv', url: `${btvLegacyUrl}/subject/${subjectId}` }], linksLock: false,
        publisher: '', publisherLock: false, totalBookCount: null, totalBookCountLock: false,
        summary: '', summaryLock: false, alternateTitles: [], authors: [], authorsLock: false,
    };

    seriesMeta.title = btvData.name_cn && t2s(btvData.name_cn) || t2s(btvData.name);
    seriesMeta.titleSort = seriesMeta.title;
    if (btvData.name && btvData.name !== seriesMeta.title) { // If original name differs from CN name
        seriesMeta.alternateTitles.push({ label: '原名', title: capitalize(btvData.name) });
    }
    seriesMeta.summary = (btvData.summary || '')
      .replace(/\r\n|\r/g, '\n')        // 统一换行
      .split('\n')
      .map(line => line.replace(/^[\s\u3000]+|[\s\u3000]+$/g, ''))  // 去除每行前后空格（含全角空格）
      .join('\n')
      .trim();
  
    seriesMeta.totalBookCount = btvData.volumes || btvData.eps || btvData.total_episodes || null;

    seriesMeta.genres = komgaSeries.genres || [];
    seriesMeta.genres.push(btvData.platform);

    const statusTags = ["连载", "连载中", "完结", "已完结", "停刊", "长期休载", "停止连载", "休刊"];

    if (btvData.tags && btvData.tags.length > 0) {
        const rawApiTags = btvData.tags
            .map(t => ({ name: t.name, count: t.count }))
            .filter(tag => tagLabels.includes(tag.name + ',') && !statusTags.includes(tag.name));

        if (rawApiTags.length > 0) {
            let validTags = rawApiTags
                .filter(tag => tagLabels.includes(tag.name + ",") && !statusTags.includes(tag.name))
                .sort((a, b) => b.count - a.count);
            const maxTagCount = Math.max(1, ...validTags.map(tag => tag.count));

            let thresholdTagCount = 3;
            if (maxTagCount > 200) {
                thresholdTagCount = 35;
            } else if (maxTagCount > 125) {
                thresholdTagCount = 25;
            } else if (maxTagCount > 60) {
                thresholdTagCount = 15;
            } else if (maxTagCount > 30) {
                thresholdTagCount = 10;
            } else if (maxTagCount > 10) {
                thresholdTagCount = 5;
            }

            let finalTags = validTags.filter(tag => tag.count >= thresholdTagCount);

            if (finalTags.length < 10) {
                finalTags = validTags.slice(0, 10);
            }

            seriesMeta.tags = finalTags.map(tag => tag.name);
        } else {
            seriesMeta.tags = [];
        }
    }

    // 追加识别系列文件夹名称中的出版社/汉化信息
    const publisherKeywords = [
        '台湾角川', '台湾东贩', '尖端', '青文', '东立', '长鸿', '尚禾', '大然', '龙成',
        '群英', '未来数位', '新视界', '玉皇朝', '天下', '传信', '天闻角川', 'bili', 
        'bilibili', '哔哩哔哩', '汉化', '生肉', '日版', '原版', '正版', '官方',
        '中文版',  '简中', '繁中', '简体中文', '繁体中文', '简体', '繁体',
    ];

    const seriesName = komgaSeries.name || '';
    const matchedKeyword = publisherKeywords.find(keyword => 
        t2s(seriesName).includes(keyword)
    );

    if (matchedKeyword && !seriesMeta.tags.includes(matchedKeyword)) {
        seriesMeta.tags.push(matchedKeyword);
    }

    if (btvData.rating && typeof btvData.rating.score === 'number' && btvData.rating.score > 0) {
        seriesMeta.tags.push(`${Math.round(btvData.rating.score)}分`);
    }

    if (seriesMeta.tags && seriesMeta.tags.length > 0) {
        const hasCompleted = seriesMeta.tags.includes("已完结") || seriesMeta.tags.includes("完结");
        if (hasCompleted) {
            let keepTag = seriesMeta.tags.includes("已完结") ? "已完结" : "完结";
            seriesMeta.tags = seriesMeta.tags.filter(
                t => !statusTags.includes(t) || t === keepTag
            );
        }
    }

    const infobox = btvData.infobox || [];
    let resAuthors = [];
    let seriesIndividualAliases = []; // For aliases from infobox

    let publisherVal = parseInfobox(infobox, '出版社') || parseInfobox(infobox, '连载杂志') || parseInfobox(infobox, '制作');
    if (publisherVal) {
        seriesMeta.publisher = t2s(publisherVal.split(/[、→×]/)[0].trim()); // Take first publisher, convert to simplified
    } else if (matchedKeyword && !seriesMeta.publisher) {
        seriesMeta.publisher = matchedKeyword;
    }

    // Define author roles mapping for Komga
    const authorRoles = {
        '作者': 'writer', '原作': 'writer', '分镜': 'writer', '脚本·分镜': 'writer', '脚本': 'writer', '漫画家': 'writer', 
        '作画': 'penciller', '插图': 'illustrator', '插画家': 'illustrator', '人物原案': 'conceptor', '人物设定': 'designer',
        '原案': 'story', '系列构成': 'scriptwriter', '铅稿': 'penciller', '上色': 'colorist'
        // Add more roles as needed and map them to Komga's supported roles
    };
    for (const [key, role] of Object.entries(authorRoles)) {
        let val = parseInfobox(infobox, key);
        console.log(`[baseAsyncReq] Success (${val}...`);
        if (val) {
            val.split(/[、→・×]/).forEach(name => { // Handle multiple authors for the same role
                const trimmedName = name.replace(/[《【（\[\(][^》】）\]\)]*[》】）\]\)]\s*$/, '').trim();
                if (trimmedName && !resAuthors.some(a => a.name === trimmedName && a.role === role)) {
                    resAuthors.push({ name: t2s(trimmedName), role: role });
                }
            });
        }
    }
    const hasWriter = resAuthors.some(a => a.role === 'writer');
    const hasPenciller = resAuthors.some(a => a.role === 'penciller');
    if (!hasWriter) {
        const pencillers = resAuthors.filter(a => a.role === 'penciller');
        for (const p of pencillers) {
            const alreadyAdded = resAuthors.some(a => a.name === p.name && a.role === 'writer');
            if (!alreadyAdded) {
                resAuthors.push({ name: p.name, role: 'writer' });
            }
        }
    }
    if (!hasPenciller && btvData.platform === '漫画') {
        const writers = resAuthors.filter(a => a.role === 'writer');
        for (const w of writers) {
            const alreadyAdded = resAuthors.some(a => a.name === w.name && a.role === 'penciller');
            if (!alreadyAdded) {
                resAuthors.push({ name: w.name, role: 'penciller' });
            }
        }
    }

    seriesMeta.authors = resAuthors;

    // Extract aliases from infobox ("别名")
    const aliasStr = extractAliases(infobox);
    if (aliasStr) {
        seriesIndividualAliases.push(...aliasStr.split(' / ').filter(Boolean));
    }
    seriesIndividualAliases.forEach(alias => {
        const aliasLower = alias.toLowerCase();
        const titleLower = seriesMeta.title ? seriesMeta.title.toLowerCase() : '';
        const oriNameLower = btvData.name ? btvData.name.toLowerCase() : '';
        // Add if not empty, not same as title, not same as original name, and not already in alternateTitles
        if (alias && aliasLower !== titleLower && aliasLower !== oriNameLower &&
            !seriesMeta.alternateTitles.some(at => at.title.toLowerCase() === aliasLower)) {
            seriesMeta.alternateTitles.push({ label: '别名', title: capitalize(alias) });
        }
    });

    // Status from infobox (keys like "状态", "连载状态", "刊行状态")
    let statusVal = parseInfobox(infobox, '状态') || parseInfobox(infobox, '连载状态') || parseInfobox(infobox, '刊行状态');
    if (!statusVal) {
        const foundStatus = seriesMeta.tags.find(tag => statusTags.includes(tag));
        if (foundStatus) {
            statusVal = foundStatus;
        }
    }
    if (statusVal) {
        statusVal = t2s(statusVal.toLowerCase()); // Convert to simplified Chinese and lower case for matching
        if (statusVal.includes('休刊') || statusVal.includes('停刊')  || statusVal.includes('停止连载') || statusVal.includes('长期休载')) seriesMeta.status = 'HIATUS';
        else if (statusVal.includes('连载中') || statusVal.includes('连载')) seriesMeta.status = 'ONGOING';
        else if (statusVal.includes('完结') || statusVal.includes('已完结')) seriesMeta.status = 'ENDED';
        // else if (statusVal.includes('宣布动画化')) seriesMeta.status = 'ONGOING'; // Or some other appropriate status
    }

    if (parseInfobox(infobox, '结束') || parseInfobox(infobox, '完结') || (seriesMeta.totalBookCount && seriesMeta.totalBookCount > 0)) {
        seriesMeta.status = 'ENDED';
    }

    let finalMeta = await filterSeriesMeta(komgaSeriesId, seriesMeta);
    // Filter out null or empty string values, but allow empty arrays (for tags, links etc.)
    finalMeta = Object.fromEntries(Object.entries(finalMeta).filter(([_, v]) => v !== null && v !== '' || Array.isArray(v)));

    const seriesNameForDisplay = finalMeta.title || btvData.name_cn || btvData.name || '未知系列';
    await updateKomgaSeriesMeta(komgaSeriesId, seriesNameForDisplay, finalMeta);

    // 匹配成功后，如果系列在手动匹配收藏夹中，则自动移除
    await ensureManualMatchCollectionExists(); // 确保收藏夹ID已初始化
    await removeSeriesFromManualMatchCollectionIfExists(komgaSeriesId, seriesNameForDisplay);

    // --- 获取系列和卷的多种封面尺寸 ---
    const seriesCoverUrls = [];
    if (btvData.images) { // 主条目的图片
        if (btvData.images.large) seriesCoverUrls.push(btvData.images.large);
        if (btvData.images.medium) seriesCoverUrls.push(btvData.images.medium);
        if (btvData.images.common) seriesCoverUrls.push(btvData.images.common);
        // if (btvData.images.small) seriesCoverUrls.push(btvData.images.small); // Usually too small
    }
    if (btvData.image && !seriesCoverUrls.includes(btvData.image)) { // `image` field is often the primary cover.
        seriesCoverUrls.unshift(btvData.image); // Add to front as highest priority if different
    }
    const uniqueSeriesCoverUrls = [...new Set(seriesCoverUrls.filter(Boolean))]; // 去重和去空

    const fetchSeriesType = localStorage.getItem(`SID-${komgaSeriesId}`);
    const seriesBooks = await getKomgaSeriesBooks(komgaSeriesId);
    const updateAuthorsFlag = finalMeta.authors && finalMeta.authors.length > 0 && ifUpdateBook(seriesBooks, finalMeta.authors);
    const needUpdateVolumeNums = getVolumeNumsNeedUpdate(seriesBooks);

    // 过滤单行本卷，排序
    const relatedSubjectsApiUrl = `${btvApiUrl}/v0/subjects/${subjectId}/subjects`;
    const relatedSubjectsStr = await asyncReq(relatedSubjectsApiUrl, 'GET', undefined, {});
    const relatedSubjects = JSON.parse(relatedSubjectsStr);
    const volumes = relatedSubjects
        .filter(rel => rel.relation === "单行本")
        .sort((a, b) => {
            const nameA = a.name_cn || a.name;
            const nameB = b.name_cn || b.name;
            const numA = extractVolumeNumber(nameA);
            const numB = extractVolumeNumber(nameB);

            if (numA !== null && numB !== null && numA !== numB) return numA - numB;
            return a.id - b.id;
        });

    // 获取 volumeMates
    const volumeMatesFetcher = async (vol, index) => {
        const volCoverUrlsList = [];
        if (vol.images) {
            if (vol.images.large) volCoverUrlsList.push(vol.images.large);
            if (vol.images.medium) volCoverUrlsList.push(vol.images.medium);
            if (vol.images.common) volCoverUrlsList.push(vol.images.common);
        }
        if (vol.image && !volCoverUrlsList.includes(vol.image)) {
            volCoverUrlsList.unshift(vol.image);
        }
        const uniqueVolCoverUrls = [...new Set(volCoverUrlsList.filter(Boolean))];
    
        let num = extractVolumeNumber(vol.name_cn || vol.name) || (index + 1);
        // 判断当前卷号是否需要更新元数据
        const isNeedUpdate = needUpdateVolumeNums.has(num);
    
        let summary = '', releaseDate = '', isbn = '';
        if (isNeedUpdate) {
            try {
                const volDetailStr = await asyncReq(`${btvApiUrl}/v0/subjects/${vol.id}`, 'GET', undefined, {});
                const volDetail = JSON.parse(volDetailStr);
    
                summary = (volDetail.summary || '')
                    .replace(/\r\n|\r/g, '\n')
                    .split('\n')
                    .map(line => line.replace(/^[\s\u3000]+|[\s\u3000]+$/g, ''))
                    .join('\n')
                    .trim();
    
                const dateStr = parseInfobox(volDetail.infobox || [], '发售日') || parseInfobox(volDetail.infobox || [], '放送开始');
                if (dateStr) {
                    releaseDate = normalizeDate(dateStr);
                }
    
                const isbnVal = parseInfobox(volDetail.infobox || [], 'ISBN');
                if (isbnVal) isbn = isbnVal;
            } catch (e) {
                console.warn(`[BtvAPI] 获取单行本详情失败 (${vol.id}):`, e);
            }
        }
    
        return {
            num,
            summary,
            releaseDate,
            isbn,
            coverUrls: uniqueVolCoverUrls,
        };
    };
    
    const volumeMatesWithCovers = await asyncPool(volumes, volumeMatesFetcher, 10);
    const bookVolumeCoverSets = volumeMatesWithCovers.map(v => ({ coverUrls: v.coverUrls }));
    let volumeMates = volumeMatesWithCovers.map(({ coverUrls, ...meta }) => meta);

    // --- 更新系列封面 ---
    if (fetchSeriesType === 'all') {
        if (uniqueSeriesCoverUrls.length > 0) {
            await updateKomgaSeriesCover(komgaSeriesId, seriesNameForDisplay, uniqueSeriesCoverUrls);
        } else {
            showMessage(`《${seriesNameForDisplay}》未能获取系列主封面 (BGM API)`, 'warning');
        }

        await updateKomgaBookAll(komgaSeriesId, seriesBooks, seriesNameForDisplay, updateAuthorsFlag ? finalMeta.authors : [], bookVolumeCoverSets, volumeMates);
    } else if (updateAuthorsFlag || (needUpdateVolumeNums && needUpdateVolumeNums.size > 0)) { // 'meta' only sync, but authors need update
        console.log(`[fetchBtvSubjectByUrlAPI] 更新系列 ${komgaSeriesId} 的作者或卷信息`);
        await updateKomgaBookAll(komgaSeriesId, seriesBooks, seriesNameForDisplay, finalMeta.authors, [], volumeMates); // Pass empty cover sets
    }
}

async function fetchMoeBookByName(seriesName, limit = 8) {
  // This function remains unchanged as it's for bookof.moe (scraping)
  const moeSeriesName = s2t(seriesName); // Convert to traditional for BoF search
  const searchUrl = `${bofUrl}/data_list.php?s=${encodeURIComponent(moeSeriesName)}&p=1`; // Search on page 1
  try {
      const searchRes = await asyncReq(searchUrl, 'GET', undefined, {}, 'text'); // Explicitly text for regex
      // Regex to find datainfo-B entries (which often contain book info)
      // datainfo-B=[分类],[ID],[标题],[作者],[出版日期]
      const idxRe = /datainfo-B=[^,]+,(\d+),(.*?),([^,]*?),[\d-]+/g; // Made author group more flexible
      // BoF script content might be split. Concatenate or iterate.
      // The original split by '<script>' and iterated. This should be fine.
      const scriptArr = searchRes.split('<script>');
      const results = [];
      scriptArr.forEach((scriptContent) => {
          let match;
          while ((match = idxRe.exec(scriptContent)) !== null) {
              const [_, id, title, author] = match;
              if (id && title) {
                  results.push({
                      id: id.trim(),
                      title: t2s(title.trim()), // Convert title back to simplified
                      author: t2s(author.trim() || '未知作者'), // Convert author, provide default
                      cover: null // BoF search results don't have covers directly
                  });
              }
          }
      });
      return (typeof limit === 'number' && limit > 0) ? results.slice(0, limit) : results;
  } catch (error) {
      console.error(`[fetchMoeBookByName] Failed for "${seriesName}":`, error);
      // showMessage handled by caller
      throw error;
  }
}

async function fetchMoeBookByUrl(komgaSeriesId, reqSeriesId, reqSeriesUrl = '') {
    const moeSeriesUrl = reqSeriesUrl || `${bofUrl}/b/${reqSeriesId}.htm`;
    const urlMatch = moeSeriesUrl.match(/https:\/\/bookof\.moe\/b\/(.*?)\.htm/);
    const moeSeriesId = urlMatch ? urlMatch[1] : null;
    if (!moeSeriesId) throw new Error(`Could not extract BoF ID from URL: ${moeSeriesUrl}`);

    const seriesRes = await asyncReq(moeSeriesUrl, 'GET', undefined, {}, 'text');
    const resEle = document.createElement('div');
    resEle.innerHTML = seriesRes.toString();

    let seriesMeta = {
        title: '', titleLock: false, titleSort: '', titleSortLock: false, status: '', statusLock: false,
        tags: [], tagsLock: false, links: [{ label: 'Bof', url: moeSeriesUrl }], linksLock: false,
        publisher: '', publisherLock: false, totalBookCount: null, totalBookCountLock: false,
        summary: '', summaryLock: false, alternateTitles: [], authors: [], authorsLock: false,
        ageRating: '', ageRatingLock: false,
    };
    let authors = [];
    let bookCoverFrameUrl = null; // 这个是 data_vol.php 或 temp_... 的 URL

    resEle.querySelectorAll('script').forEach(script => {
        const scriptContent = script.textContent || '';
        const coverUrlMatch = scriptContent.match(/window\.iframe_action\.location\.href\s*=\s*"(.*?)"/);
        if (coverUrlMatch && coverUrlMatch[1]) {
            let pathOrUrl = coverUrlMatch[1];
            if (pathOrUrl.toLowerCase().startsWith('http://') || pathOrUrl.toLowerCase().startsWith('https://')) {
                bookCoverFrameUrl = pathOrUrl;
            } else {
                bookCoverFrameUrl = bofUrl + (pathOrUrl.startsWith('/') ? pathOrUrl : '/' + pathOrUrl);
            }
        }
    });

    const mainEle = resEle.querySelector('td.author');
    if (!mainEle) throw new Error('Could not find main content element (td.author) on BoF page');

    const r18Img = mainEle.querySelector('img#logo_r18');
    seriesMeta.ageRating = (r18Img ? r18Img.style.display !== 'none' : false) ? 18 : null;

    const seriesOriNameElement = mainEle.querySelector('.name_main');
    const seriesOriName = seriesOriNameElement ? seriesOriNameElement.textContent.trim() : '';
    if (!seriesOriName) throw new Error('Could not parse main title (.name_main) from BoF page');

    seriesMeta.title = t2s(seriesOriName); // Simplified Chinese title
    seriesMeta.titleSort = seriesMeta.title;
    seriesMeta.alternateTitles.push({ label: '原名', title: seriesOriName }); // Original (Traditional Chinese) title

    const nameSubtElements = mainEle.querySelectorAll('.name_subt');
    let infoEleText = ''; // For status and tags
    if (nameSubtElements.length > 0) {
        const seriesNameStr = nameSubtElements[0].textContent || '';
        const seriesNameArr = seriesNameStr.match(/\(([^)]+)\)\s*(.*)/); // (EnglishName) OtherInfo
        const seriesEngName = seriesNameArr ? seriesNameArr[1]?.trim() : null;
        if (seriesEngName && seriesEngName.toLowerCase() !== seriesMeta.title.toLowerCase() && seriesEngName !== seriesOriName) {
            seriesMeta.alternateTitles.push({ label: '别名', title: capitalize(seriesEngName) });
        }
        infoEleText = (nameSubtElements.length > 1) ? (nameSubtElements[1].textContent || '') : seriesNameStr;
    }

    const seriesDescElement = resEle.querySelector('div#div_desctext');
    let seriesDesc = seriesDescElement ? seriesDescElement.textContent.trim() : '';
    seriesMeta.summary = t2s(seriesDesc.replace(/[\r\n]+/g, '\n').replace(/\【.*?\】$/,'').trim());

    mainEle.querySelectorAll('a[href^="https://bookof.moe/s/AUT"]').forEach(link => {
        const authorName = t2s(link.textContent?.trim());
        if (authorName) authors.push({ name: authorName, role: 'writer' }); // BoF doesn't usually specify roles beyond "author"
    });
    seriesMeta.authors = authors;


    if (infoEleText) {
        const statusMatch = infoEleText.match(/狀態：(.*?)(?:\s|分类：|$)/);
        const status = statusMatch ? t2s(statusMatch[1].trim()) : '';
        switch (status) {
            case '连载': seriesMeta.status = 'ONGOING'; break;
            case '完结': seriesMeta.status = 'ENDED'; break;
            case '停更': case '休刊': seriesMeta.status = 'HIATUS'; break;
            case '弃坑': case '放弃': seriesMeta.status = 'ABANDONED'; break;
        }

        const tagsMatch = infoEleText.match(/分類：(.*?)(?:\n|$)/);
        if (tagsMatch && tagsMatch[1]) {
            seriesMeta.tags = tagsMatch[1].trim().split(/\s+/).filter(Boolean).map(t => t2s(t));
        }

        const publisherKeywords = [
            '台湾角川', '台湾东贩', '尖端', '青文', '东立', '长鸿', '尚禾', '大然', '龙成',
            '群英', '未来数位', '新视界', '玉皇朝', '天下', '传信', '天闻角川', '角川', '东贩',
            '集英', '讲谈社', '小学馆', 'bili', 'bilibili', '哔哩哔哩', '汉化', '生肉', '日版',
            '原版', '正版', '官方', '中文版',  '简中', '繁中', '简体中文', '繁体中文', '简体', '繁体', 
        ];

        const komgaSeries = await getKomgaSeriesData(komgaSeriesId);
        const seriesName = komgaSeries.name || '';
        const matchedKeyword = publisherKeywords.find(keyword => 
            t2s(seriesName).includes(keyword)
        );

        if (matchedKeyword && !seriesMeta.tags.includes(matchedKeyword)) {
            seriesMeta.tags.push(matchedKeyword);
        }

        const publisherVal = infoEleText.match(/版本：(.*?)(?:\s|最後出版：|$)/);
        if (publisherVal && publisherVal[1]) {
            seriesMeta.publisher = t2s(publisherVal[1].trim());
        } else if (matchedKeyword && !seriesMeta.publisher) {
            seriesMeta.publisher = matchedKeyword;
        }
    }

    seriesMeta.links.push({ label: 'Mox', url: `https://kox.moe/c/${moeSeriesId}.htm` }); // Add Mox link

    const seriesNameForDisplay = seriesMeta.title || '未知系列';
    let finalMeta = await filterSeriesMeta(komgaSeriesId, seriesMeta);
    finalMeta = Object.fromEntries(Object.entries(finalMeta).filter(([_, v]) => v !== null && v !== '' || Array.isArray(v) ));
    await updateKomgaSeriesMeta(komgaSeriesId, seriesNameForDisplay, finalMeta);

    // 匹配成功后，如果系列在手动匹配收藏夹中，则自动移除
    await ensureManualMatchCollectionExists(); // 确保收藏夹ID已初始化
    await removeSeriesFromManualMatchCollectionIfExists(komgaSeriesId, seriesNameForDisplay);

    const fetchSeriesType = localStorage.getItem(`SID-${komgaSeriesId}`);
    const seriesBooks = await getKomgaSeriesBooks(komgaSeriesId);
    const updateAuthorsFlag = finalMeta.authors && finalMeta.authors.length > 0 && ifUpdateBook(seriesBooks, finalMeta.authors);
    const needUpdateVolumeNums = getVolumeNumsNeedUpdate(seriesBooks);

    if (fetchSeriesType === 'all') {
        let bofVolumeRawCoverUrls = [];
        if (bookCoverFrameUrl) {
            try {
                const coverFrameRes = await asyncReq(bookCoverFrameUrl, 'GET', undefined, {}, 'text');
                const coverRe = /datainfo-V=\d+,[^,]+,[^,]+,[^,]+,([^,]+),[^,]+/g; // datainfo-V=ID,Title,Type,Unknown,CoverURL,Unknown
                let coverMatch;
                while ((coverMatch = coverRe.exec(coverFrameRes)) !== null) {
                    if (coverMatch[1]) { // coverMatch[1] 是 CoverURL 部分
                        let imageUrlFromFrame = coverMatch[1];
                        let finalImageUrl;
                        // console.log("[BoF] Raw image URL from datainfo-V:", imageUrlFromFrame); // 调试
                        if (imageUrlFromFrame.toLowerCase().startsWith('http://') || imageUrlFromFrame.toLowerCase().startsWith('https://')) {
                            // 如果 imageUrlFromFrame 已经是绝对 URL (例如 https://img.mxomo.com/...)
                            finalImageUrl = imageUrlFromFrame;
                        } else {
                            // 否则，假定它是相对于 bofUrl 的路径 (例如 /pic/V/123.jpg)
                            finalImageUrl = bofUrl + (imageUrlFromFrame.startsWith('/') ? imageUrlFromFrame : '/' + imageUrlFromFrame);
                        }
                        // console.log("[BoF] Processed finalImageUrl for volume:", finalImageUrl); // 调试
                        bofVolumeRawCoverUrls.push(finalImageUrl);
                    }
                }
            } catch (coverError) {
                console.error(`[BoF] Error fetching/parsing cover frame for ${seriesNameForDisplay} from ${bookCoverFrameUrl}:`, coverError);
                showMessage(`《${seriesNameForDisplay}》BoF 封面数据获取/解析失败: ${coverError.message || coverError}`, 'error');
            }
        } else {
            showMessage(`《${seriesNameForDisplay}》未找到 BoF 封面配置脚本，无法更新封面`, 'warning');
        }

        // 更新系列主封面和卷封面
        if (bofVolumeRawCoverUrls.length > 0) {
            await updateKomgaSeriesCover(komgaSeriesId, seriesNameForDisplay, [bofVolumeRawCoverUrls[0]]); // 使用第一个卷的封面作为系列封面
            const bofBookVolumeCoverSets = bofVolumeRawCoverUrls.map(url => ({ coverUrls: [url] })); // BoF usually has one cover per vol
            await updateKomgaBookAll(komgaSeriesId, seriesBooks, seriesNameForDisplay, updateAuthorsFlag ? finalMeta.authors : [], bofBookVolumeCoverSets);
        } else {
            if (bookCoverFrameUrl) { // If frame URL existed but parsing failed
                 showMessage(`《${seriesNameForDisplay}》未能从 BoF 封面数据中解析出有效封面链接`, 'warning');
            }
            if (updateAuthorsFlag || (needUpdateVolumeNums && needUpdateVolumeNums.size > 0)) { // 即使没有封面，也可能需要更新作者
                 await updateKomgaBookAll(komgaSeriesId, seriesBooks, seriesNameForDisplay, finalMeta.authors, []);
            }
        }
    } else if (updateAuthorsFlag || (needUpdateVolumeNums && needUpdateVolumeNums.size > 0)) { // 'meta' only sync, but authors need update
        await updateKomgaBookAll(komgaSeriesId, seriesBooks, seriesNameForDisplay, finalMeta.authors, []);
    }
}
//</editor-fold>

async function search(komgaSeriesId, $dom) {
    if (!komgaSeriesId) { showMessage('Komga Series ID 无效', 'error'); return; }
    if (!$dom || $dom.length === 0) {
         $dom = findDomElementForSeries(komgaSeriesId);
         if (!$dom) { showMessage(`无法找到系列 ${komgaSeriesId} 的界面元素`, 'error'); return; }
    }

    const komgaMeta = await getKomgaSeriesMeta(komgaSeriesId);
    // No early exit if !komgaMeta, selectSeriesTitle will use series.name
    const komgaMetaLinks = komgaMeta?.links || []; // Handle if komgaMeta is null

    const $selSourcePanel = $('<div></div>').css({ ...selPanelStyle });
    sourceLabels.forEach((label) => { // 'Btv', 'Bof'
        const $selSourceBtn = $('<button></button>')
            .append('<div>' + label + '</div>')
            .attr('sourceLabel', label).css({ ...selPanelBtnStyle });

        const linkObj = komgaMetaLinks.find((link) => link.label && link.label.toLowerCase() === label.toLowerCase());
        if (linkObj) {
             $selSourceBtn.append('<div style="font-size: 10px; color: lightgreen;">(链接已存在)</div>');
             $selSourceBtn.attr('existingUrl', linkObj.url);
        }

        $selSourceBtn.on('click', async function (e) {
            e.stopPropagation();
            const sourceLabel = $(this).attr('sourceLabel'); // 'Btv' or 'Bof'
            const existingUrl = $(this).attr('existingUrl'); // e.g., https://bangumi.tv/subject/XXXX or https://bookof.moe/b/YYYY.htm
            localStorage.setItem(`STY-${komgaSeriesId}`, sourceLabel.toLowerCase()); // Store 'btv' or 'bof'
            $selSourcePanel.remove();

            if (existingUrl) {
                showMessage(`找到 ${sourceLabel} 链接，直接获取...`, 'info');
                partLoadingStart($dom); // Start loading before fetchBookByUrl
                // For BTV, existingUrl is like bangumi.tv/subject/ID. We need the ID for API.
                // For BOF, existingUrl is fine.
                let sourceId = 0; // For BTV API, this will be the subject ID. For BOF, it's part of the URL.
                if (sourceLabel.toLowerCase() === 'btv') {
                    const idMatch = existingUrl.match(/subject\/(\d+)/);
                    if (idMatch && idMatch[1]) {
                        sourceId = idMatch[1]; // This is the reqSeriesId for fetchBtvSubjectByUrlAPI
                    } else {
                         showMessage(`无法从现有 Btv 链接解析 ID: ${existingUrl}`, 'error');
                         partLoadingEnd($dom);
                         return; // Stop if ID cannot be parsed for Btv API
                    }
                }
                // fetchBookByUrl will handle its own partLoadingEnd
                await fetchBookByUrl(komgaSeriesId, sourceId, existingUrl, sourceLabel.toLowerCase());
                // partLoadingEnd($dom); // Moved to fetchBookByUrl's finally block
            } else {
                 // No existing link, proceed to title selection then search
                 try {
                     await selectSeriesTitle(komgaSeriesId, $dom);
                 } catch (selectionError) {
                     // Error message already shown by selectSeriesTitle or its callees
                     // console.warn("Title selection was cancelled or failed:", selectionError);
                 }
            }
        });
        $selSourcePanel.append($selSourceBtn);
    });

    // Add a general cancel button for the source selection panel
    const $cancelBtn = $('<button></button>').append('<div> 取消 </div>')
        .css({ ...selPanelBtnStyle, backgroundColor: '#f44336', height: 'auto', minHeight: '50px', padding: '10px' })
        .on('click', function (e) {
            e.stopPropagation();
            $selSourcePanel.remove();
            showMessage('操作已取消', 'warning');
        });
    $selSourcePanel.append($cancelBtn);
    $selSourcePanel.appendTo('body');
}

// ************************************** 批量匹配功能 **************************************
//<editor-fold desc="批量匹配功能">
async function preciseMatchSeries(komgaSeriesId, oriKomgaTitle, searchType = 'btv', syncType = 'meta', batchStats, currentIndex, totalCount) {
    const logPrefix = `[批量][${currentIndex}/${totalCount}]`;

    let $domForLoading = findDomElementForSeries(komgaSeriesId);
    if ($domForLoading) partLoadingStart($domForLoading);

    let matchResult = {
        success: false,
        seriesId: komgaSeriesId,
        name: oriKomgaTitle,
        skipped: false,
        error: 'Unknown error during precise match'
    };

    try {
        const komgaMeta = await getKomgaSeriesMeta(komgaSeriesId);
        const seriesName = (komgaMeta?.title?.trim()) || (oriKomgaTitle?.trim()) || (await getKomgaOriTitle(komgaSeriesId))?.trim();
        matchResult.name = seriesName || oriKomgaTitle;

        if (komgaMeta?.links?.find(l => l.label?.toLowerCase() === searchType.toLowerCase() && l.url)) {
            console.info(`${logPrefix} 《${matchResult.name}》已存在 ${searchType.toUpperCase()} 链接，跳过`);
            matchResult = { ...matchResult, success: true, skipped: true, reason: `Existing ${searchType.toUpperCase()} link` };
            if ($domForLoading) partLoadingEnd($domForLoading);
            return matchResult;
        }

        localStorage.setItem(`SID-${komgaSeriesId}`, syncType);
        localStorage.setItem(`STY-${komgaSeriesId}`, searchType);

        if (!seriesName) {
            console.log(`${logPrefix} 系列 ${komgaSeriesId} 无有效标题用于匹配`);
            matchResult.error = 'No valid title for matching';
        } else {
            const [searchTerm, backupTerm] = extractSeriesTitles(seriesName, 2);
            if (!searchTerm) {
                console.warn(`${logPrefix} 《${matchResult.name}》从"${seriesName}"解析搜索词失败`);
                matchResult.error = 'No valid search term parsed from title';
            } else {
                const normalize = extractAndNormalizeTitle;
                let preciseMatch = null;
                let matchedField = null;

                const findPreciseMatch = (list, searchNorm) => {
                    return list.find(item => {
                        if (item.title && normalize(item.title) === searchNorm) {
                            matchedField = 'title (name_cn/name)';
                            return true;
                        }
                        if (item.orititle && normalize(item.orititle) === searchNorm) {
                            matchedField = 'orititle (name)';
                            return true;
                        }
                        if (item.aliases && typeof item.aliases === 'string' && item.aliases.trim()) {
                            const aliases = item.aliases.split(' / ');
                            if (aliases.some(alias => normalize(alias) === searchNorm)) {
                                matchedField = 'alias';
                                return true;
                            }
                        }
                        return false;
                    });
                };

                let seriesListRes = await fetchBookByName(searchTerm, searchType, 0);
                preciseMatch = findPreciseMatch(seriesListRes, normalize(searchTerm));

                if (!preciseMatch && backupTerm) {
                    preciseMatch = findPreciseMatch(seriesListRes, normalize(backupTerm));
                    if (preciseMatch) {
                        matchedField = matchedField + ' (backupTerm)';
                    } else {
                        const backupSearchRes = await fetchBookByName(backupTerm, searchType, 0);
                        preciseMatch = findPreciseMatch(backupSearchRes, normalize(backupTerm));
                        if (preciseMatch) {
                            matchedField = matchedField + ' (backupTerm)';
                        }
                    }
                }

                if (preciseMatch) {
                    const displayMatchedTitle = preciseMatch.title || preciseMatch.orititle || "未知匹配标题";
                    console.info(`${logPrefix} 《${matchResult.name}》精确匹配 (${matchedField}):《${displayMatchedTitle}》。更新...`);
                    await fetchBookByUrl(komgaSeriesId, preciseMatch.id, '', searchType);
                    matchResult = {
                        ...matchResult,
                        success: true,
                        matchedWithTitle: displayMatchedTitle,
                        matchedSourceField: matchedField,
                        skipped: false
                    };
                    return matchResult;
                } else {
                    const msg = seriesListRes.length > 0 ? "无精确匹配 (标题, 原始标题或别名)" : "未找到结果";
                    console.warn(`${logPrefix} 《${matchResult.name}》(词:"${searchTerm}") ${msg}。`);
                    matchResult.error = seriesListRes.length > 0 ? 'No exact title, orititle, or alias match' : 'No search results from source';
                }
            }
        }
    } catch (error) {
        console.error(`${logPrefix} 处理系列 ${komgaSeriesId} ("${matchResult.name}") 出错:`, error);
        matchResult.error = error.message || String(error);
        matchResult.success = false;
    }

    // 失败自动加入手动匹配集合
    if (!matchResult.success && !matchResult.skipped && batchStats) {
        const added = await addSeriesToManualMatchCollectionImmediately(komgaSeriesId, matchResult.name);
        if (added) batchStats.addedToCollectionCount++;
    }

    if ($domForLoading) partLoadingEnd($domForLoading);
    return matchResult;
}               

async function batchMatchTarget(type, id, name) {
    showMessage(`开始对 ${name} 中的系列批量精确匹配...`, 'info', 3000);
    console.info(`批量匹配类型: ${type}, ID: ${id}`);

    _manualMatchCollectionId = null;
    _manualMatchCollectionExistingSeriesIds = [];
    let batchStats = { successCount: 0, failureCount: 0, skippedCount: 0, addedToCollectionCount: 0 };

    await ensureManualMatchCollectionExists();

    let seriesObjects = [];
    if (type === 'library') {
        seriesObjects = await getSeriesWithLibraryId(id);
    } else if (type === 'collection') {
        seriesObjects = await getSeriesWithCollection(id);
    } else if (type === 'recommended') {
        seriesObjects = await getLatestSeries(id);
    }

    if (!seriesObjects || seriesObjects.length === 0) {
        showMessage('未能获取系列ID，批量中止', 'warning');
        return;
    }

    showBatchProgress(0, seriesObjects.length, batchStats);

    for (let i = 0; i < seriesObjects.length; i++) {
        const seriesObj = seriesObjects[i];
        const seriesName = seriesObj.metadata.title || seriesObj.name;

        if (type !== 'collection' && _manualMatchCollectionExistingSeriesIds.includes(seriesObj.id)) {
            console.info(`[批量][${i + 1}/${seriesObjects.length}] 《${seriesName}》已在 "${MANUAL_MATCH_COLLECTION_NAME}" 收藏夹，跳过`);
            batchStats.skippedCount++;
            showBatchProgress(i + 1, seriesObjects.length, batchStats);
            continue;
        }

        console.info(`正在处理 ${i + 1}/${seriesObjects.length}：${seriesName} (${seriesObj.id})`);
        try {
            const result = await preciseMatchSeries(seriesObj.id, seriesName, 'btv', 'meta', batchStats, i + 1, seriesObjects.length);

            if (result.skipped) batchStats.skippedCount++;
            else if (result.success) {
                batchStats.successCount++;
            } else batchStats.failureCount++;
        } catch (err) {
            console.error(`[批量][${i + 1}/${seriesObjects.length}] 《${seriesName}》匹配出错:`, err);
            batchStats.failureCount++;
        }

        showBatchProgress(i + 1, seriesObjects.length, batchStats);

        await new Promise(resolve => setTimeout(resolve, 300));
    }

    hideBatchProgress();

    let summary = `批量精确匹配完成！${type === 'library' ? '库' : type === 'collection' ? '收藏夹' : type === 'readlist' ? '阅读列表' : type === 'recommended' ? '最近系列' : ''}#${id}：成功 ${batchStats.successCount}，失败 ${batchStats.failureCount}，跳过 ${batchStats.skippedCount}，共 ${seriesObjects.length}。`;
    if (batchStats.addedToCollectionCount > 0) {
        summary += ` ${batchStats.addedToCollectionCount} 个系列已添加/更新至 "${MANUAL_MATCH_COLLECTION_NAME}"。`;
    } else if (batchStats.failureCount > 0 && _manualMatchCollectionId) {
        summary += ` (失败系列或已在 "${MANUAL_MATCH_COLLECTION_NAME}" 中)。`;
    } else if (batchStats.failureCount > 0 && !_manualMatchCollectionId) {
        summary += ` (因收藏夹操作失败，未能记录失败系列)。`;
    }
    showMessage(summary, 'success', 10000);
    console.info(`批量精确匹配完成！${type}#${id}：成功 ${batchStats.successCount}，失败 ${batchStats.failureCount}，跳过 ${batchStats.skippedCount}，共 ${seriesObjects.length}。`);
}

function addBatchMatchButtonIfNeeded() {
    const path = window.location.pathname;
    const recMatch = path.match(/^\/libraries\/([a-zA-Z0-9]+(?:-sync)?)\/recommended$/);
    const libMatch = path.match(/^\/libraries\/(?!all(?:\/|$))([a-zA-Z0-9]+(?:-sync)?)(?:\/.*)?$/);
    const colMatch = path.match(/^\/collections\/([a-zA-Z0-9]+)$/);

    if (!recMatch && !libMatch && !colMatch) {
        $('#batchMatchLibraryBtn').remove();
        return;
    }

    if ($('#batchMatchLibraryBtn').length > 0) return;

    const $toolbar = $('header.v-app-bar .v-toolbar__content').first();
    if ($toolbar.length === 0) return;

    const isDark = $('div#app').hasClass('theme--dark');
    const themeClass = isDark ? 'theme--dark' : 'theme--light';

    const $btn = $(`
        <button id="batchMatchLibraryBtn" type="button"
                class="v-btn v-btn--flat ${themeClass} v-size--default mx-1"
                title="批量精确匹配 (元数据：Bangumi API)">
            <span class="v-btn__content">
                <i aria-hidden="true" class="v-icon notranslate mdi mdi-target-variant ${themeClass} left v-icon--left"></i>
                批量匹配
            </span>
        </button>
    `);

    $btn.on('click', async () => {
        const path = window.location.pathname;
        let pageType = null;
        let targetId = null;

        const recMatch = path.match(/^\/libraries\/([a-zA-Z0-9]+(?:-sync)?)\/recommended$/);
        const libMatch = path.match(/^\/libraries\/(?!all(?:\/|$))([a-zA-Z0-9]+(?:-sync)?)(?:\/.*)?$/);
        const colMatch = path.match(/^\/collections\/([a-zA-Z0-9]+)$/);


        if (recMatch) {
            pageType = 'recommended';
            targetId = recMatch[1].replace('-sync', '');
        } else if (libMatch) {
            pageType = 'library';
            targetId = libMatch[1].replace('-sync', '');
        } else if (colMatch) {
            pageType = 'collection';
            targetId = colMatch[1];
        }

        if (!pageType || !targetId) {
            alert('当前页面不支持批量匹配');
            return;
        }

        const pageTypeNameMap = {
            library: '本库',
            collection: '此收藏夹',
            readlist: '此阅读列表',
            recommended: '最近添加和更新'
        };
        const pageTypeName = pageTypeNameMap[pageType] || pageType;

        if (confirm(`即将对${pageTypeName}中的系列进行批量精确匹配。\n\n规则：\n- 元数据源：Bangumi API (Btv)\n- 更新类型：仅元数据 (不含封面)\n- 匹配方式：\n  - 若系列已有关联的 Btv 链接，则跳过。\n  - 否则，使用系列标题在 Btv 进行精确搜索。\n  - 精确匹配：搜索结果的中文名/原名/别名与系列标题完全一致。\n- 失败处理：匹配失败的系列将尝试添加至名为 "${MANUAL_MATCH_COLLECTION_NAME}" 的收藏夹中。\n\n是否继续？`)) {
            await batchMatchTarget(pageType, targetId, pageTypeName);
        }
    });

    let $point = $toolbar.find('.v-spacer').next('button, .v-btn').first() ||
                 $toolbar.find('.v-spacer').nextAll('button, .v-btn').first() ||
                 $toolbar.find('button:has(i.mdi-pencil), button:has(i.mdi-checkbox-multiple-marked-outline)').first();
    if ($point.length > 0) {
        $point.before($btn);
    } else {
        $toolbar.append($btn);
    }
}
//</editor-fold>

function main() {
    console.log("KomgaBangumi script started. Setting up observer.");

    const SERIES_CARD_SELECTOR = 'div[class*="v-card"], div.my-2.mx-2, .card-container';
    const DETAIL_CARD_SELECTOR = 'div.container > div > div > .v-card:first-child';

    function getSeriesIdFromHref(href) {
        if (!href) return null;
        let match = href.match(/\/series\/(\w+)/);
        if (match) return match[1];
        match = href.match(/\/oneshot\/(\w+)/);
        if (match) return match[1];
        return null;
    }

    function scanAndInjectButtons(root) {
        const $root = root ? $(root) : $(document);

        $root.find(SERIES_CARD_SELECTOR).addBack(SERIES_CARD_SELECTOR).each(function() {
            const $card = $(this);
            if ($card.find('button[komgaseriesid]').length) return;
            const bgStyle = $card.find('.v-image__image').attr('style') || '';
            if (bgStyle.includes('/thumbnails/')) return;
            const $link = $card.find('a[href]').first();
            const id = getSeriesIdFromHref($link.attr('href'));
            if (id) loadSearchBtn($card, id);
        });

        const $detailCard = $root.find(DETAIL_CARD_SELECTOR).addBack(DETAIL_CARD_SELECTOR).first();
        if ($detailCard.length > 0 && $detailCard.find('button[komgaseriesid]').length === 0) {
            const currentHref = location.href.replace(/%2F/g, '/');
            const id = getSeriesIdFromHref(currentHref);
            if (id) loadSearchBtn($detailCard, id);
        }
    }

    const observer = new MutationObserver(mutations => {
        const addedElements = [];
        mutations.forEach(mutation => {
            mutation.addedNodes.forEach(node => {
                if (node.nodeType === Node.ELEMENT_NODE) {
                    addedElements.push(node);
                }
            });
        });
        if (addedElements.length > 0) {
            addedElements.forEach(el => scanAndInjectButtons(el));
            addBatchMatchButtonIfNeeded();
        }
    });

    let observerIntervalId = setInterval(() => {
        const targetNode = document.getElementById('app') || document.body;
        if (targetNode) {
            observer.observe(targetNode, { childList: true, subtree: true });
            clearInterval(observerIntervalId);
            console.log("KomgaBangumi: Observer attached. Performing initial UI checks...");
            scanAndInjectButtons();
            addBatchMatchButtonIfNeeded();
        }
    }, 500);

    loadMessage();

    if (typeof GM_registerMenuCommand === "function") {
        GM_registerMenuCommand("选择匹配类型（漫画/小说）", setBangumiMatchType);
        GM_registerMenuCommand('设置Bangumi Access Token', setBangumiAccessToken, 'B');
    }

    console.log("KomgaBangumi main execution finished setting up.");
}

// Handle SPA navigation
const _wr = function (type) {
  const orig = history[type];
  return function () {
    const rv = orig.apply(this, arguments);
    const e = new Event(type);
    e.arguments = arguments;
    window.dispatchEvent(e);
    return rv;
  };
};
history.pushState = _wr('pushState');
history.replaceState = _wr('replaceState');

function handleNavigation() {
    // console.log("KomgaBangumi: Navigation detected (pushState/popstate), re-evaluating UI elements.");
    // Delay slightly to allow Komga's UI to render after navigation
    setTimeout(() => {
        // Buttons on cards/detail view are handled by MutationObserver.
        // Only need to explicitly re-check for the batch button here.
        addBatchMatchButtonIfNeeded();
    }, 700); // Adjust delay if needed
}

window.addEventListener('pushState', handleNavigation);
window.addEventListener('popstate', handleNavigation); // Handles browser back/forward

// Initial load
console.log("KomgaBangumi initial load, running main setup...");
setTimeout(() => {
    main();
}, 700); // Delay main execution slightly to ensure Komga's base UI is more likely to be ready
