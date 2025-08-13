# KomgaBangumi

Komga 漫画服务器元数据刮削器，使用 Bangumi API，并支持自定义 Access Token

用于自建 Komga 服务刮削漫画元数据，生成 Metadata 和封面

## 功能

* 从 Bangumi API 获取系列和卷的元数据及封面
* 支持 bookof.moe 作为备用数据源 (刮削)
* 在 Komga 界面添加刮削按钮
* 批量精确匹配库中的系列
* 失败的系列自动添加到 "手动匹配" 收藏夹
* 允许用户通过油猴菜单配置 Bangumi Access Token
* 当 Access Token 失效 (API 返回 401) 时提示用户更新

## 安装

1. 确保你已经安装了浏览器扩展 [Tampermonkey](https://www.tampermonkey.net/) (Chrome, Firefox, Edge, Safari 等均支持) 或兼容的用户脚本管理器
2. 点击以下链接安装脚本：
   [![Install KomgaPatcher](https://img.shields.io/badge/Install%20Directly-KomgaBangumi-blue.svg)](https://raw.githubusercontent.com/dyphire/KomgaBangumi/master/KomgaBangumi.user.js)

## 说明

刮削按钮在每本书封面处下方，会生成两个圆形按钮，按钮是默认隐藏的，只有移动到书籍封面上才会显示，包括书库和书籍详情页都会生成

左侧按钮用于只刮削 Metadata 信息，右侧按钮用于刮削 Metadata 信息和所有封面

## 使用方法

* 配置 Komga 服务域名或 `ip:port`地址用于脚本识别

  1. 打开油猴 Tampermonkey 的管理面板（Dashboard）
  2. 找到 KomgaBangumi 脚本，点击编辑按钮（铅笔图标）
  3. 切换到 "设置" (Settings) 标签页
  4. 找到 "包括/排除 (Includes/Excludes) " 部分
  5. 在 "用户包括 (User includes) " 或 "用户匹配 (User matches) " 中添加您的 Komga 服务域名匹配规则，例如 `https://komga.org/*`
  6. 保存设置
* 配置元数据刮削器的匹配目标

  1. 在浏览器中访问你的 Komga 服务网址
  2. 点击浏览器工具栏中的 Tampermonkey 图标
  3. 找到 KomgaBangumi 脚本
  4. 选择匹配类型（漫画/小说）。默认为：漫画
* 配置 Bangumi Access Token **(可选，用于搜索 NSFW 条目)**

  1. 在浏览器中访问你的 Komga 服务网址
  2. 点击浏览器工具栏中的 Tampermonkey 图标
  3. 找到 KomgaBangumi 脚本
  4. 选择 "设置 Bangumi Access Token"
  5. 在弹出的对话框中输入您的 Token。留空则清除
* 在库视图的顶部工具栏会添加 "批量匹配" 按钮
* 脚本会在 Komga 的系列卡片和系列详情页面的封面上添加 "仅更新元数据" 和 "更新元数据和封面" 按钮

## 具体操作

1. 点击刮削按钮
2. 选择刮削源
3. 选择要刮削的书名
4. 点击对应的书籍，则开始刮削，刮削完不需要刷新，Komga 会自动更新

> [!NOTE]
> 如果某些书籍无法正确获取本地书籍名，或者使用搜索无法正确检索到，可以手动到网站检索到正确书籍后，复制其 URL，编辑漫画的链接一栏添加对应的项即可
>
> 比如 `'Btv': 'https://xxxx'`，保存后再更新漫画选择该源即可
>
> 也可以直接修改 Komga 上的标题为正确标题后再次执行手动搜索

## 批量匹配

- 在库视图的顶部工具栏会有一个 "批量匹配" 按钮，可以用于执行当前库的批量精确匹配
- 在库视图的推荐页面执行批量匹配将自动匹配最近添加和更新的系列（30 个条目）
- 批量匹配操作默认按最后入库顺序来倒序执行


## 漫画命名建议

批量匹配支持自动识别以下文件夹命名格式：

`[漫画名称][作者][出版社][卷数][其他1][其他2]`

`[作者][漫画名称][出版社][卷数][其他1][其他2]`

- [漫画名称]：以漫画封面实际名称为准，繁体不必转简体。
- [作者]：作者名字亦以单行本所给名字为准

  - 繁体不转为简体，若有日文假名亦保留，如[島崎讓]、[天王寺きつね]；
  - 若作者为多人，则以 `×`符号连接各作者（**注意：不是英文 `x`**），将作画作者列于最后，如[矢立肇×有贺ヒトシ]、[手塚治虫×浦沢直树]、[堀田由美×小畑健]。
- [出版社]：例如[玉皇朝]、[青文]。
- [卷数]：例如[Vol.01-Vol.12]。
- [其他1]、[其他2]：其他信息。例如[完结]、[来源]。

例如：

```txt
[碧蓝之海][井上堅二×吉岡公威][Vol.01-Vol.18]
[相合之物][浅野伦][Vol.01-Vol.13]
[海王但丁][皆川亮二×泉福朗][Vol.01-Vol.13][境外版]
[鶴田洋久][嬌娃·夏生之危機][東立][hmml][1-18完]
[鶴田謙二][少年科学俱乐部_Spirit.of.Wonder][玉皇朝][C.C][1完]
[黑田洋介×林家志弦][拜託了!老師_星空的邂逅][2完]
```

_命名建议修改自某喵_

## 更新日志

[Changelog](Changelog.md)

## 相关项目

- [KomgaMylar](https://github.com/dyphire/KomgaMylar)

## 许可证

[MIT](LICENSE)
