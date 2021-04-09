# statusPlay

基于 Scala Playframework、Slick ORM、H2 Database 实现的个人信息统计服务: 

① **网站服务可用性**：通过 wsClient 进行网页可用性检查，默认 30min 检查间隔。 

② **个人健康记录**：通过 Apple Watch 检测每小时的身体活动：步数、静息和运动心率、卡路里、楼层、最大耗氧、心率变异、体重等。 

③ **个人物品管理系统**：跟踪物品标签上的二维码提供状态信息。 

④ **每日餐饮管理系统**：跟踪每天的餐饮并记录。

## 前端界面

### 基本的 HTML 视图

Play 后端使用 RESTful API 提供服务，前端使用 URL 进行简单交互，比如 mazhangjing.com/status 会自动跳转到 https://status.mazhangjing.com/status，通过 Basic 认证后，提供当前网站运行概要信息。

此外，对于个人健康、个人物品管理和餐饮管理，暴露了简单的使用 Bootstrap 搭建的列表和详情页面，比如 mazhangjing.com/CMNTE 会自动跳转到 https://status.mazhangjing.com/goods/CMNTE/details 界面，通过认证后会显示 CMNTE 条目详细信息，否则显示简略信息。https://status.mazhangjing.com/foods/details?week=true 可以返回最近一周的饮食情况分析等。

详情参见 app/views/{xxx}.scala.html 所提供的 HTML 文档。

![](http://static2.mazhangjing.com/20210409/e587e36_截屏2021-04-09上午10.12.54.png)

### iOS 快捷方式提供的快捷视图

基于 Play 的 RESTful API，通过 iOS 的 "快捷方式" APP 编写了常用的对于个人健康记录的上传（苹果健康）、个人物品管理的增、删、改、查（带照片记录）、每日餐饮的快速拍照记录等服务。因为此 APP 需要硬编码用户名和密码，因此不做分享，详情请联系 concat@mazhanjging.com。

![](http://static2.mazhangjing.com/20210409/e274cb1_截屏2021-04-09上午10.19.47.png)

### 基于 Flutter 的客户端视图

statusPlay 所提供的服务被整合到 Flutter 客户端 'CyberMe' 中，Flutter 提供了丰富的动画、反馈以及流畅的操作体验，此客户端会自动保存当前偏好设置、记录当前登录用户和秘钥、自动滚动到上次浏览位置、自动将访问地址复制到剪贴板、提供快速的个人物品信息修改、增加以及本地照片上传和拍摄的能力。

关于此 Flutter 项目参见 [CyberMe](https://gitee.com/corkine/cyberMe) 。

![](http://static2.mazhangjing.com/20210409/9ab241f_截屏2021-04-09上午10.24.13.png)

## 使用技术

![](http://static2.mazhangjing.com/badge/openjdk.png)
![](http://static2.mazhangjing.com/badge/scala.png)
![](http://static2.mazhangjing.com/badge/play.png)
![](http://static2.mazhangjing.com/badge/slick.png)
![](http://static2.mazhangjing.com/badge/akka.png)