# made by lc
library(xlsx)
library(shiny)
library(shinydashboard)
library(reshape)
library(DT)
library(cluster)
library(vegan)
library(ggplot2)
library(amap)
library(devtools) 
library(gganimate)
library(gapminder)
library(pheatmap)

library(ggradar)
library(REmap)# REmapOutpot 会与DT包的dataTableOutput产生js冲突，表格无法显示
# 所以只能在另一个网页上显示出来

rm(list=ls())
Sys.setlocale("LC_ALL","Chinese")
path <- getwd()
# -----------------------f e n g e x i a n --------------------------------------
#   --------------------------------  数据集的准备 ------------------------------
myfile1_name <- paste(path,"/","myself.xlsx", sep = "")
mydata1 <- read.xlsx(myfile1_name, sheetName = "ability")

file_county_data <- paste(path,"/","县级/co2.csv", sep = "")
county_data<-read.csv(file_county_data,skip = 0, header = FALSE, sep=",") #data.frame()
colnames(county_data) <- county_data[1,]
county_data <- county_data[-1, ] # md
county_data_temp <- as.data.frame(county_data)
county_data_temp <- as.data.frame(lapply(county_data_temp,as.numeric)) # df
s1 <- data.frame(apply(county_data_temp, 2,mean),
                 apply(county_data_temp, 2,max),
                 apply(county_data_temp, 2,min),
                 apply(county_data_temp, 2,sd),
                 apply(county_data_temp, 2, sum),
                 apply(county_data_temp, 2, median))
s1 <- t(s1)
row.names(s1)<-c("mean","max","min","sd", "sum", "median")
file_country_data2 <- paste(path,"/","县级/sequestration.csv", sep = "")
country_data2<-read.csv(file_country_data2,skip = 0, header = FALSE, sep=",") #data.frame()
colnames(country_data2) <- country_data2[1,]
country_data2 <- country_data2[-1, ] # md
# ---------------------- 上面是县级 -------------------------------
# -----------------------------------------------------------------
# ---------------------- 下面是省级 -------------------------------
years <- seq(from=2000, by=1, length=18)
for(year in years){
  file_name_pro <- paste(path,"/","省级", "/", as.character(year), "年30个省份排放清单.xlsx", sep = "")
  data1_pro <- read.xlsx(file_name_pro, sheetName = "Sum")
  data1_pro <- data1_pro[1:30, 1:20]
  assign(paste("data_pro_", as.character(year), sep = ""), data1_pro) # 以字符串作为变量名，使用assign(string, data)
}
dx_pro <- seq(from=1, by=1, length=18)
# 选择城市

# ------------------------分割 --------------------------------------------------
# -------------------------------------------------------------------------------
# -----------------------下面是国家级 -------------------------------------------
file_national <- paste(path,"/","国家级/clean_data.csv", sep = "")
data_national <- read.csv(file_national, header = FALSE, sep=",") #data.frame()
colnames(data_national) <- data_national[1,]
data_national <- data_national[-1, ]
# ------------ui------------ui----------------ui----------------ui---------------
ui <- dashboardPage(
  
  dashboardHeader(title = "Lin Che",
                  dropdownMenu(
                    type = "tasks", badgeStatus = "danger",
                               notificationItem(icon = icon("battery-full"), status = "danger",
                                     text = "对部分环境数据的可视化及分析", 
                               ),
                               notificationItem(icon = icon("user"), status = "info",
                                                text =  "作者：林澈"
                               ),
                               
                               notificationItem(icon = icon("envelope-square"), status = "danger",
                                                text = "邮箱：19clin1@stu.edu.cn", href="mailto:19clin1@stu.edu.cn"
                               ),
                               notificationItem(icon = icon("clock"),
                                                status = "success", 
                                                text = "当前时间", href = "http://www.daojishiqi.com/bjtime.asp"
                                            
                               ),
                               notificationItem(icon = icon("calendar-week"),
                                                status = "danger", text = "2021年6月"
                               )
                  )
                ),
  
  dashboardSidebar(
    
    
    sidebarMenu(
      menuItem("国家级数据", tabName = "national_level", icon = icon("line-chart"),
               badgeLabel = "nation", badgeColor = "red",selected=T),
      menuItem("省级数据", tabName = "provincial_level", icon = icon("line-chart"),
               badgeLabel = "province", badgeColor = "green"),
      menuItem("县级数据", tabName = "county_level", icon = icon("line-chart"),
               badgeLabel = "county", badgeColor = "orange"),
      menuItem("数据集", tabName = "data", icon = icon("table"),startExpanded=F,
               menuSubItem("国家数据",tabName = "national_data", icon = icon("blank")),
               menuSubItem("省级数据",tabName = "provincial_data", icon = icon("blank")),
               menuSubItem("县级数据",tabName = "county_data", icon = icon("blank")),
               menuSubItem("数据集介绍",tabName = "main_data", icon = icon("blank"))),
      menuItem("项目介绍", icon = icon("book"),startExpanded=T,
               menuSubItem("数据集介绍",tabName = "main_data", icon = icon("blank")),
               menuSubItem("CO2数据信息",tabName = "national_data_com", icon = icon("blank")),      
               menuSubItem("研究背景",tabName = "item_background", icon = icon("blank"))),
      menuItem("作者介绍", tabName = "author", icon = icon("user-edit"),
               badgeLabel = "author", badgeColor = "light-blue"),br(),hr(),
      menuItem("小建议", tabName = "tips", icon = icon("smile"))
    )
    ),
  
  dashboardBody(
    tabItems(
      
      # -------------------------------------  national --------------------------------------------
      tabItem(tabName = "national_level",fluidPage(
        tabBox(title = tagList(icon("gear"), "国家级"),width = 9,side="right",
               tabPanel(title = "折线图",imageOutput("line1")),
               tabPanel(title = "柱状图",imageOutput("bar1")),
               tabPanel(title = "散点图",imageOutput("point1")),
               tabPanel(title = "热图",imageOutput("hot1",height=800,width=1173)),
               tabPanel(title = '数据框',dataTableOutput("data_n_1"),dataTableOutput("data_n_2"))
                  
        ),
        #  ------------------------- 按钮  - -- - - -- - - - - - -- - -  -- - - - - - - - -
        box(title = "数据选择",width = 3,collapsible = T,
           verticalLayout(
             selectInput("method1",
                         label = "属性选择",
                         choices =  c("Total", "Raw Coal", "Cleaned Coal", "Other Washed Coal", "Briquettes",
                            "Coke", "Coke Oven Gas", "Other Gas", "Other Coking Products", "Crude Oil", "Gasoline",
                            "Kerosene", "Diesel Oil", "Fuel Oil", "LPG", "Refinery Gas", "Other Petroleum Products",
                            "Natural Gas", "Process"),
                         selected = "Total"
              
            ),
            selectInput("method1_2",
                        label = "起始年份选择",
                        choices =  list("2000年" = "2000", "2001年" = "2001", "2002年" = "2002", 
                                        "2003年" = "2003", "2004年" = "2004", "2005年" = "2005", 
                                        "2006年" = "2006", "2007年" = "2007", "2008年" = "2008", 
                                        "2009年" = "2009", "2010年" = "2010", "2011年" = "2011", 
                                        "2012年" = "2012", "2013年" = "2013", "2014年" = "2014", 
                                        "2015年" = "2015"),
                        selected = "2000年"
                        
            ),
            selectInput("method1_3",
                        label = "结束年份选择",
                        choices =  list("2000年" = "2000", "2001年" = "2001", "2002年" = "2002", 
                                        "2003年" = "2003", "2004年" = "2004", "2005年" = "2005", 
                                        "2006年" = "2006", "2007年" = "2007", "2008年" = "2008", 
                                        "2009年" = "2009", "2010年" = "2010", "2011年" = "2011", 
                                        "2012年" = "2012", "2013年" = "2013", "2014年" = "2014", 
                                        "2015年" = "2015"),
                        selected = "2015"
            ),
            actionButton("done1","确定")
        )
        ),
        box(title = tagList(icon("database"), "数据介绍"), width = 6,solidHeader = F,collapsible = T,
            sidebarLayout(
              sidebarPanel(
                p("数据采用2000年到2015年全国部分物质的排放量，数据中的排放物包括：原煤(Raw Coal)、精煤(Cleaned.Coal)、
                  其他精煤(Other Washed Coal)、煤球(Briquettes)、焦炭(Coke)、焦炉煤气(Coke Oven Gas)、其他煤气(Other Gas)、
                  其他焦化产品(Other Coking Products)、原油(Crude Oil)、汽油(Gasoline)、煤油(Kerosene)、柴油(Diesel Oil)、
                  燃料油(Fuel Oil)、液化石油气(LPG)、炼厂气(Refinery Gas)、其他石油产品(Other Petroleum Products)、天然气(Natural Gas)
                  燃烧后的排气以及工业排气(Process)。", size="16"),
                
                style="height:300px;width:700px;"),
              mainPanel(
              )
            )
        ),
        box(title = tagList(icon("file-word"), "分析"), width = 6,collapsible = T,
              sidebarLayout(
                sidebarPanel(
                  p("根据折线图可以看出，我国大部分省份在2000年到2013年的排放量都处于上升趋势。
                  这说明我国快速化城市进程所带来的城市大气污染问题的加剧。", size="12"), br(),
                  p("根据热图，可以直观地看出哪些数据是比其他数据要高的，说明该年份下该物质地排放量超出其他排放。", size="12"),
                  p("画热图的数据是有经过对数处理的，处理的公式为：。", size="12"),
                  withMathJax(),
                  p(HTML("$$x=log_{10}(x_0+1)$$")), # 
                  style="height:300px;width:700px;"),
                mainPanel(
                )
              )
        )
          
      )
      ),
      
      
      #  ------------------------------  provincial   ---------------------------
      tabItem(tabName = "provincial_level",fluidPage(
        tabBox(title = tagList(icon("gear"), "省级"),width = 9,side="right",
               tabPanel(title = "折线图",imageOutput("line2")),
               tabPanel(title = "柱状图",imageOutput("bar2")),
               tabPanel(title = "散点图",imageOutput("point2")),
               tabPanel(title = "热图",imageOutput("hot2",height=800,width=1173)),
               tabPanel(title = '地  图',plotOutput("map2"))
        ),
        box(title = "数据选择",width = 3,collapsible = T,
            verticalLayout(
              selectInput("method2_1",
                          label = "城市选择",
                          choices =  list("北京" = "Beijing", "天津" = "Tianjin", "河北" = "Hebei","山西" = "Shanxi",
                                       "内蒙古" = "InnerMongolia", "辽宁" = "Liaoning","吉林" = "Jilin", "黑龙江" = "Heilongjiang",
                                       "上海" = "Shanghai", "江苏" = "Jiangsu", "浙江" = "Zhejiang", "安徽" = "Anhui", 
                                       "福建"  = "Fujian", "江西" = "Jiangxi", "山东" = "Shandong", "河南" = "Henan", 
                                       "湖北" = "Hubei", "湖南" = "Hunan", "广东" = "Guangdong", "广西" = "Guangxi",
                                       "海南" = "Hainan", "重庆" = "Chongqing", "四川" = "Sichuan", "贵州" = "Guizhou",
                                       "云南" = "Yunnan", "陕西" = "Shaanxi", "甘肃" = "Gansu", "青海" = "Qinghai", 
                                       "宁夏" = "Ningxia", "新疆" = "Xinjiang"),
                          selected = "北京"
                          
              ),
              selectInput("method2_2",
                          label = "属性选择",
                          choices =  c("Total", "Raw.Coal", "Cleaned.Coal", "Other.Washed.Coal", "Briquettes",
                                       "Coke", "Coke.Oven.Gas", "Other.Gas", "Other.Coking.Products", "Crude.Oil", "Gasoline",
                                       "Kerosene", "Diesel.Oil", "Fuel.Oil", "LPG", "Refinery.Gas", "Other.Petroleum.Products",
                                       "Natural.Gas", "Process"),
                          selected = "Total"
                          
              ),
              selectInput("method2_3",
                          label = "年份选择(地图、热图)",
                          choices =  list("2000年" = "2000", "2001年" = "2001", "2002年" = "2002", 
                                          "2003年" = "2003", "2004年" = "2004", "2005年" = "2005", 
                                          "2006年" = "2006", "2007年" = "2007", "2008年" = "2008", 
                                          "2009年" = "2009", "2010年" = "2010", "2011年" = "2011", 
                                          "2012年" = "2012", "2013年" = "2013", "2014年" = "2014", 
                                          "2015年" = "2015", "2016年" = "2016", "2017年" = "2017" ),
                          selected = "2000年"
                          
              ),
              actionButton("done2","确定")
            )
        ),
        box(title = tagList(icon("database"), "数据介绍"), width = 6,solidHeader = F,collapsible = T,
            sidebarLayout(
              sidebarPanel(
                p("数据采用2000年到2017年的全国各省份排放清单数据，数据中的排放物包括：原煤(Raw Coal)、精煤(Cleaned.Coal)、
                  其他精煤(Other Washed Coal)、煤球(Briquettes)、焦炭(Coke)、焦炉煤气(Coke Oven Gas)、其他煤气(Other Gas)、
                  其他焦化产品(Other Coking Products)、原油(Crude Oil)、汽油(Gasoline)、煤油(Kerosene)、柴油(Diesel Oil)、
                  燃料油(Fuel Oil)、液化石油气(LPG)、炼厂气(Refinery Gas)、其他石油产品(Other Petroleum Products)、天然气(Natural Gas)
                  燃烧后的排气以及工业排气(Process)。", size="16"),
                
                style="height:300px;width:700px;"),
              mainPanel(
              )
            )
        ),
        box(title = tagList(icon("file-word"), "分析"), width = 6,collapsible = T,
            sidebarLayout(
              sidebarPanel(
                p("根据折线图可以看出，我国大部分省份在2000年到2010年的排放量都处于上升趋势。
                  这说明我国快速化城市进程所带来的城市大气污染问题的加剧。", size="12"), br(),
                p("根据热图，可以直观地看出哪些数据是比其他数据要高的，说明该年份下该物质地排放量超出其他排放。", size="12"),
                p("画热图的数据是有经过归一化处理的，归一化的公式为：", size="12"),
                withMathJax(),
                p(HTML("$$x=\\frac{x-x_{min}}{x_{max}-x_{min}}$$")), # 
                p("根据地图可视化，可以直观的对比相同排放物相同时间的情况下，各省份排放量的多少，其中颜色深的表示排放量大。。", size="12"),
                style="height:300px;width:700px;"),
              mainPanel(
              )
            )
        )
        
              ),
        ),
      # ----------------------   这 是 一 条 分 割 线 -----------------------------------
      
      #  -------------------------   县级  ------------- ------------- ------------ ------------- -------
      tabItem(tabName = "county_level",fluidPage(
        tabBox(title = tagList(icon("gear"), "县级"),width = 9,side="right",
               tabPanel(title = "折线图",imageOutput("line3")),
               tabPanel(title = "柱状图",imageOutput("bar3")),
               tabPanel(title = "散点图",imageOutput("point3"))
        ),
        box(title = "数据选择",width = 3,collapsible = T,
            sidebarLayout(
              sidebarPanel(
                p(strong("数据选择"), size="6"),
                p("提示: ",  style = "color:red", size="6"),
                p("数据的加载可能需要一定时间,请耐心等待...", style = "color:red"),
                selectInput("method3_4",
                            label = "选择类型选择",
                            choices =  c("CO2数据", "陆地植被固碳量数据"),
                            selected = "CO2数据"),
                actionButton("done3","点击显示图片")
                ,height=400, width=200
              ),
              mainPanel(selectInput("method3_1",
                                    label = "省份选择",
                                    choices =  unique(county_data$`Province Name`),
                                    selected = "北京市"
                                    
              ),
              uiOutput("method3_2"),
              uiOutput("method3_3")
                          
              )
            )
        ),
        box(title = tagList(icon("file-word"), "分析"), width = 6,collapsible = T,
            sidebarLayout(
              sidebarPanel(
                p("数据分别选择县级CO2数据和陆地植被固碳量数据，以各县级为单位，根据年份分别画出折线图、散点图和柱状图。", size="12"),
                p("从画出来的图像中可以看出，全国大部分的城市在2000年到2012年这几年间CO2的释放量呈现上升趋势，在之后都呈现下降趋势。说明中国的
                  大部分城市的城市环境在逐渐变好。", size="12"),
                p("而植被固碳量不是很稳定，波动很大。", size="12"),
                style="height:300px;width:700px;"),
              mainPanel(
              )
            )
            ),
        box(title = tagList(icon("database"), "数据介绍"), width = 6,collapsible = T,
            sidebarLayout(
              sidebarPanel(
                p("中国在碳减排战略中实施自上而下的制定方法，充分考虑地区差异是实现目标的关键。作为中国最基本的行政单位，县比省和地级市能更好地探究区域异质性。", size="12"),
                p("因此，评估县级二氧化碳排放有助于帮助决策者因地制宜，找到适合当地情况的战略政策。但是，由于方法难度和数据的限制，现有的中国碳排放研究大多停留于国家、省份或城市尺度。", size="12"),
                p("CEADs研究采用粒子群优化-反向传播（PSO-BP）算法统一DMSP / OLS和NPP / VIIRS卫星图像的规模，估算了1997-2017年中国2,735个县的CO2排放量。", size="12"),
                p("此外，由于植被具有隔离和减少CO2排放的显着能力，因此进一步计算了县级陆地植被的固碳量。研究结果弥补了当前存在的数据缺口，为制定中国碳减排策略提供数据支持。", size="12"),
                style="height:300px;width:700px;"),
              mainPanel(
              )
            )
        )
        
      ),
      ),
      
      # ------------------------ 数据集 ------------------------------------------
      tabItem(tabName = "county_data",
              box(title = "当前数据",solidHeader = T,width = 12,style = "height:1000px;width:1600px;",
                  dataTableOutput("mdat1_1"), dataTableOutput("mdat1_2")
              )
      ),
      tabItem(tabName = "provincial_data",
              box(title = "当前数据",solidHeader = T,width = 12,style = "height:1200px;width:1600px;",
                  verticalLayout(
                    selectInput("data_s_1",
                                label = "年份选择",
                                choices =  list("2000年" = "2000", "2001年" = "2001", "2002年" = "2002", 
                                             "2003年" = "2003", "2004年" = "2004", "2005年" = "2005", 
                                             "2006年" = "2006", "2007年" = "2007", "2008年" = "2008", 
                                             "2009年" = "2009", "2010年" = "2010", "2011年" = "2011", 
                                             "2012年" = "2012", "2013年" = "2013", "2014年" = "2014", 
                                             "2015年" = "2015", "2016年" = "2016", "2017年" = "2017" ),
                                selected = "2000年"
                                
                    )),
                  dataTableOutput("mdat2_1"), dataTableOutput("mdat2_2")
              )
      ),
      tabItem(tabName = "national_data",
              box(title = "当前数据",solidHeader = T,width = 12,style = "height:1200px;width:1600px;",
                  dataTableOutput("mdat3_1"), dataTableOutput("mdat3_2")
              )
      ),
      tabItem(tabName = "main_data",
              fluidPage(style="height:600px;width:800px;",
                        titlePanel("数据介绍"),
                        sidebarLayout(
                          sidebarPanel(
                            h2("数据来源介绍", align = "center", size="15"),
                            p("数据来源于", strong("CEADs"), size="12"),
                            p(strong("CEADs"),"在中国国家自然基金委员会、科技部重点研发计划、中国科学院、英国研究理事会、牛顿基金会等多家研究机构的共同支持下，聚集了来自中英欧美等多国研究机构的学者，共同编纂中国及发展中国家、地区的多尺度碳核算清单及社会经济与贸易数据库，提供全公开、全透明、全免费的数据下载，供学术研究使用。", style = "font-family: 'times'; font-si48pt"),
                            strong("网址为:  ", size="8"),
                            a(em("CEADs(点击跳转)"), href = "https://www.ceads.net.cn/data/process/", size="8"),
                            br(), br(), 
                            
                            code("该网址的数据都是免费的，注册后可直接下载。"),br(),
                            div("本次项目使用其数据集(2000年-2017年)分别为：中国能源清单、中国排放清单、中国碳排放清单、",
                                "30个省份排放清单、182个城市排放清单、县级CO2排放量、植被的固碳价值", style = "color:blue"),
                            br(),
                            p("本项目读取数据并展示数据基本信息的",
                              span("代码", style = "color:red"),
                              "样例为："),
                            code("path <- getwd()"),br(),
                            code("file <- paste(path,\"/\",\"县级/co2.csv\", sep = \"\") "),br(),
                            code("df<-read.csv(file,skip = 0, header = FALSE, sep=\",\")"),br(),
                            code("colnames(df) <- df[1,]"),br(),
                            code("df <- df[-1, ]"),br(),
                            code("summary(df) # 展示数据基本信息"),
                            br(), br(), 
                            strong("数据见《CO2数据信息》", size="12"),
                            style="height:600px;width:800px;"),
                          mainPanel(
                            
                            
                          )
                        )
                        
                        
              ),# flu
              
      ), # tab
      tabItem(tabName = "national_data_com",
        box(title = "CO2数据",solidHeader = T,width = 12,style = "height:1000px;width:1600px;",
            dataTableOutput("summary1_1"),dataTableOutput("summary1_2")
         ), 
      ),
      tabItem(tabName = "author",
              column(width = 3,
                     box(status = "warning",width = 16,height="400px",
                         solidHeader = T,collapsible = T,
                         title = "基本资料",
                         pre("姓名： 林澈          邮箱： 19clin1@stu.edu.cn",width = 12),
                         pre("性别：  男           年龄： 20",width = 12),
                         pre("学历： 本科          毕业院校： 汕头大学",width = 12),
                         pre("电话： 13923963263   薪资要求： 0",width = 12),
                     ),
              ),
              column(width = 3,
                     box(status = "danger",width = 16,height="400px",
                         solidHeader = T,collapsible = T,
                         title = "教育背景",
                         pre("毕业院校： 汕头大学工学院          最高学历：本科",width = 12),
                         pre("所学专业： 数据科学与大数据技术",width = 12)
                     )),
              column(width = 3,
                     box(status = "success",width = 16,height="400px",
                         solidHeader = T,collapsible = T,
                         title = "掌握技能",
                         p("对html、css、javascript、机器学习等具有一定的了解，掌握python、R、java等程序设计语言以及网络爬虫等计算机技能，熟练掌握c/c++，对数据结构和算法具有一定的研究。")
                     )),
              column(width = 3,
                     box(status = "primary",width = 16,height="400px",
                         solidHeader = T,collapsible = T,
                         title = "获奖经历",
                         p("2019年学业二等奖学金"),
                         p("第十二届蓝桥杯程序设计赛c/c++大学B组省赛一等奖"),
                         p("第十二届蓝桥杯程序设计赛c/c++大学B组国赛优秀奖"),
                         p("高校计算机能力挑战赛华南赛区二等奖(水赛)"),
                         p("平平无奇的获奖记录 T T")
                     )),
              column(width = 6,
                     box(title = "个人能力",status = "info",width = 16,height="550px",
                         tabBox(title = "能力图",width = 12,side="right",
                                tabPanel(title = "柱状图",plotOutput("myPlot1")),
                                tabPanel(title = "雷达图",plotOutput("myPlot2"))
                         ),
                         solidHeader = T,collapsible = T
                         
                     ))
              
              
              
      ),
      tabItem(tabName = "tips",
              fluidPage(style="height:300px;width:400px;",
                        titlePanel("一点小建议"),
                        sidebarLayout(
                          sidebarPanel(
                            p("教学方面老师是属于教学水平很高的了，在课上可以学到很多东西。而且学到的确实都是
                              有用的东西，课上选择用代码教学我是比较喜欢的，对我来说用过代码学习可以加快掌握知识的速度。
                            只是工具的选择对我来说不是很友好，这学期数据采集课用的是python+一点js，写了个网页用了html+css，
                              打acm用的是c/c++，可视化课又得用R语言。。有点难受。"),
                            p("我个人觉得python虽然可能在可视化上面不如R做的那么完美，但是现在他的库也已经非常完善了，用python
                              可以做到和R差不多，且python适用的范围比较广，语法也比R简单一些。"),
                            p("唯一感到不足的是去年的《数据结构》这门课的考试难度，我感觉学的算法，数据结构是相对而言比较多的，
                              但是在成绩上却不能与其他同学有点差距，大部分人都是90+的成绩。个人觉得数据结构这门课，考核内容应该
                              体现在代码能力上，期末考试或许可以改为在oj平台上做题，根据ac的题目给分，这样对数据结构和算法的考察
                              比较全面，同时也比较公平一些。"),
                            style="height:400px;width:500px;"),
                          mainPanel(
                            
                            
                          )
                        )
                        
                        
              ),# flu
              
      ),
      tabItem(tabName = "item_background",
              fluidPage(style="height:300px;width:400px;",
                        titlePanel("项目背景"),
                        sidebarLayout(
                          sidebarPanel(
                            p("随着我国经济的发展，有效利用能源、减少环境污染、降低安全生产事故频次
                            ，防止突发环境事件，确保生命安全的重要性日益凸显。制定并执行环保政策和措施，致在保护环境的同时改善人民的生活质量，
                              已经成为我国民生工程的关注点。保护环境不仅关乎人们的生存环境，也影响着经济发展。"),
                            p("通过对现有数据的可视化，可以更加直观地看出环境质量地变化趋势，可以更加有效地
                              分析未来环境有可能出现地变化，对环境地防治具有一定的意义。"),
                            style="height:300px;width:400px;"),
                          mainPanel(
                            
                            
                          )
                        )
                        
                        
              ),# flu
              
      )
  )

)
)


server <- function(input, output) {
  
  
  # data_s_1
  data_s_year <- "2000"
  observeEvent(input$data_s_1, {
    data_s_year <- input$data_s_1
    data_s_name <- paste("data_pro_", data_s_year, sep="")
    data_s <- get(data_s_name)
    output$mdat2_1<-renderDataTable({
      datatable(data_s[, 1:10],rownames = F)
    })
    output$mdat2_2<-renderDataTable({
      datatable(data_s[, 11:20],rownames = F)
    })
  })
  data_s_name <- paste("data_pro_", data_s_year, sep="")
  data_s <- get(data_s_name)
  # dictionary
  dictionart_p <- c("Beijing" = "北京" ,"Tianjin" =  "天津","Hebei" = "河北" ,"Shanxi" = "山西",
                    "InnerMongolia" = "内蒙古","Liaoning"  = "辽宁", "Jilin" = "吉林", "Heilongjiang" = "黑龙江",
                    "Shanghai" = "上海","Jiangsu" = "江苏", "Zhejiang" = "浙江", "Anhui" = "安徽", 
                    "Fujian" = "福建" , "Jiangxi" = "江西", "Shandong" = "山东", "Henan" = "河南", 
                    "Hubei" = "湖北", "Hunan" = "湖南", "Guangdong" = "广东", "Guangxi" = "广西",
                    "Hainan" = "海南", "Chongqing" = "重庆", "Sichuan" = "四川", "Guizhou" = "贵州",
                    "Yunnan" = "云南", "Shaanxi" = "陕西", "Gansu" = "甘肃", "Qinghai" = "青海", 
                    "Ningxia" = "宁夏", "Xinjiang" = "新疆")
  # ---------------------------------- plot ---------------------------------------
  
  output$summary1_1<-renderDataTable({
    datatable(s1[, 8:18])
  })
  output$summary1_2<-renderDataTable({
    datatable(s1[, 19:28])
  })
  output$mdat1_1<-renderDataTable({
    datatable(county_data[, 1:14],rownames = F)
  })
  output$mdat1_2<-renderDataTable({
    datatable(county_data[, 15:28],rownames = F)
  })
  output$mdat2_1<-renderDataTable({
    datatable(data_s[, 1:10],rownames = F)
  }) # 根据
  output$mdat2_2<-renderDataTable({
    datatable(data_s[, 11:20],rownames = F)
  })
  output$data_n_1<-renderDataTable({
    datatable(data_national[, 1:8])
  })
  output$data_n_2<-renderDataTable({
    datatable(data_national[, 9:18])
  })
  output$mdat3_1<-renderDataTable({
    datatable(data_national[, 1:8])
  })
  output$mdat3_2<-renderDataTable({
    datatable(data_national[, 9:18])
  })
  varis <- reactive({
    get_numeric_variable(input$data1)
  })
  # my
  
  output$myPlot1 <- renderPlot({
    ggplot(data = mydata1, aes(x = items, y = ability, fill = items)) + geom_bar(stat="identity")+ theme_grey(base_family = "STKaiti")
  })
  output$myPlot2 <- renderPlot({
    mydata1_1 <- data.frame(t(mydata1$ability ))
    names(mydata1_1) <- t(mydata1$items)
    ggradar(mydata1_1,base.size = 3, grid.max = 5, grid.min = 0)
  })
  output$method3_2 <- renderUI({
    pro_nnn <- input$method3_1
    dddd <- county_data[which(county_data$`Province Name` == pro_nnn),]
    selectInput("method3_2",
                label = "城市选择",
                choices =  unique(dddd$`City Name`),
                selected = ""
                
    )
  })
  output$method3_3 <- renderUI({
    pro_nnnn <- input$method3_2
    ddd <- county_data[which(county_data$`City Name` == pro_nnnn),]
    selectInput("method3_3",
                label = "区县选择",
                choices =  unique(ddd$`County Name`),
                selected = ""
                
    )
  })
  output$hot1 <- renderPlot({
    # new
    data_national_t <- data_national
    rownames(data_national_t) <- data_national_t[,1]
    data_national_t <- data_national_t[,-1]
    data_national_t <- as.data.frame(lapply(data_national_t,
                                          function(x) as.numeric(as.character(x))))
    pheatmap(log(data_national_t + 1))
    })
  observeEvent(input$done1,{
      if(is.null(input$method1)) name <<- "Total"
      s_y_1 <- 2000
      e_y_1 <- 2015
      s_y_1 <- as.numeric(input$method1_2)
      e_y_1 <- as.numeric(input$method1_3)
      if(s_y_1 > e_y_1) return();
      name_n <<- input$method1
      data_n <- data_national[, name_n]
      index_n <- seq(from=s_y_1, by=1, length=e_y_1 - s_y_1 + 1)
      data_n <- as.numeric(data_n)
      
      d_n <- data.frame(x = index_n, y = data_n[(s_y_1-2000 + 1):(e_y_1-2000 + 1)])
      
      output$line1 <- renderPlot({
        ggplot(d_n, aes(x, y)) + geom_line(color = d_n$x)
        
      })
     # if(input$ggan1 == F){
      output$bar1 <- renderPlot({
          ggplot(data = d_n, aes(x = x, y = y, color = x)) + geom_bar(stat="identity", fill = d_n$x)
      })
    
      output$point1 <- renderPlot({
        ggplot(d_n, aes(x, y)) + geom_point(color = d_n$x)
      })
  })
  observeEvent(input$done2,{
    if(is.null(input$method2_1)) pro_name <<- "Beijing"
    if(is.null(input$method2_2)) data_choice_p <<- "Total"
    if(is.null(input$method2_3)) pro_map_year <<- "2000"
    pro_name <<- input$method2_1
    data_choice_p <<- input$method2_2
    pro_map_year <<- input$method2_3
    
    for(year in years){
      data_name_pro <- paste("data_pro_", as.character(year), sep="")
      da_pro <- get(data_name_pro)
      da_pro <- da_pro[da_pro$province == pro_name,]
      dx_pro[year-1999] <- da_pro[data_choice_p]
    }
    
    ii <- seq(from=1, by=1, length=18)
    dd_pro <- vector()
    for (i in ii){
      dd_pro <- append(dd_pro, dx_pro[[i]])
    }
    yy_pro <- years[1:length(dd_pro)]
    d_p <- data.frame(x = yy_pro, y = dd_pro)
    data11_name <- paste("data_pro_", pro_map_year, sep = "")
    data11 <- get(data11_name)
    ddata1 <- data.frame(province = dictionart_p[data11$province], value = data11[, data_choice_p])
    ddata2 <- data.frame(province = dictionart_p[data11$province], data11)
    rownames(ddata2) <- ddata2[,2]
    ddata2 <- ddata2[,-1]
    ddata2 <- ddata2[,-1]
 #  View(ddata2)
    # plot
    output$line2 <- renderPlot({
      ggplot(d_p, aes(x = x, y = y)) + geom_line(color = yy_pro)
    })
    
    output$bar2 <- renderPlot({
      ggplot(data = d_p, aes(x = x, y = y, color = x)) + geom_bar(stat="identity", fill = d_p$x)
    })
    
    output$point2 <- renderPlot({
      ggplot(data = d_p, aes(x = x, y = y)) + geom_point(color = yy_pro)
    })
    
    output$hot2 <- renderPlot({
      ddata2 <- scale(ddata2, center = TRUE, scale = TRUE)
      pheatmap(ddata2 + 1)
    })
    
    output$map2 <- renderPlot({
      remapC(ddata1,
             maptype = 'china',
             markLineData = NULL,
             markPointData = NULL,
             color = c('#1e90ff','#f0ffff'),
             theme = get_theme("Bright"),
             title = "",
             subtitle = "",
             markLineTheme = markLineControl(),
             markPointTheme = markPointControl(),
             geoData = NA,
             mindata = NA,
             maxdata = NA)
    })
  }) # observe
  observeEvent(input$done3,{
    proname <- input$method3_1
    cityname <- input$method3_2
    countyname <- input$method3_3
    if(input$method3_4 == "CO2数据"){
      the_data <- county_data[which(county_data$`Province Name` == proname), ]
      the_data <- the_data[which(the_data$`City Name` == cityname), ]
      the_data <- the_data[which(the_data$`County Name` == countyname), ]
      d_xxx <- the_data[8:28]
      index_xxx <- seq(from=1997, by=1, length=21)
    }
    else{
      the_data <- country_data2[which(country_data2$`Province Name` == proname), ]
      the_data <- the_data[which(the_data$`City Name` == cityname), ]
      the_data <- the_data[which(the_data$`County Name` == countyname), ]
      d_xxx <- the_data[8:25]
      index_xxx <- seq(from=2000, by=1, length=18)
    }
    
    
    d_p_xxx <- data.frame(data = t(d_xxx), year = index_xxx)
    colnames(d_p_xxx) <- c("data", "year")
    output$line3 <- renderPlot({
      ggplot(d_p_xxx, aes(x = year, y = data, color = year)) + geom_line()
    })
    
    output$bar3 <- renderPlot({
      ggplot(d_p_xxx, aes(x = year, y = data, color = year)) + geom_bar(stat="identity", fill = d_p_xxx$year)
    })
    
    output$point3 <- renderPlot({
      ggplot(d_p_xxx, aes(x = year, y = data, color = year)) + geom_point()
    })
    
     
  })
    
}
  

shinyApp(ui, server)



