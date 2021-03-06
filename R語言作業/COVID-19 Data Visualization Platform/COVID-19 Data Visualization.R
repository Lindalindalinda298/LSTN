#####全球:選擇#####
Production = T 
options(scipen = 1000, expressions = 10000)
appVersion = "v2.0" #APP版本
appName = "COVID-19 Data Visualization Platform"  #APP名稱
appLongName = "COVID-19 Data Visualization Platform" #APP顯示出來的名稱
lastUpdate = "2020-04-06" #最後更新日期

#裝載員 標籤表
loader <- tagList(   
  waiter::spin_loaders(42), ##waiter屏幕
  br(),
  h3("Loading data")
)

jsToggleFS <- 'shinyjs.toggleFullScreen = function() {
var element = document.documentElement,
enterFS = element.requestFullscreen || element.msRequestFullscreen || element.mozRequestFullScreen || element.webkitRequestFullscreen,
exitFS = document.exitFullscreen || document.msExitFullscreen || document.mozCancelFullScreen || document.webkitExitFullscreen;
if (!document.fullscreenElement && !document.msFullscreenElement && !document.mozFullScreenElement && !document.webkitFullscreenElement) {
enterFS.call(element);
} else {
exitFS.call(document);
}
}'

source("appFiles/packageLoad.R")
source("appFiles/dataLoad.R")
source("appFiles/CSS.R", local = TRUE)
source("appFiles/dashboardPage.R", local = TRUE)
##### 使用者介面 #####
ui <- tagList( # 依存關係
  use_waiter(),
  useSweetAlert(),
  useShinyjs(),
  extendShinyjs(text = jsToggleFS),
  waiter::waiter_show_on_load(loader, color = "#000"),
# shows before anything else
  ##### CSS and style functions #####
  CSS, #CSS.R
  # Loading message
  argonDash::argonDashPage(
    title = appLongName,
    header = argonDash::argonDashHeader(
      gradient = T,
      color = NULL,
      top_padding = 2,
      bottom_padding = 0,
      background_img = "coronavirus.jpg",
      height = 70,
      argonRow(
        argonColumn(width = 8,
                    h4(appLongName, style = 'color:white;
                       text-align:left;
                       vertical-align: middle;
                       font-size:40px;')
                    ),
        argonColumn(
          width = 4,
          h6(HTML(paste0("Creator & Maintainer: <a href='https://www.shubhrampandey.com' target = '_blank'>Shubhram Pandey</a>")), style = 'color:white;
                                  text-align: right;
                                  font-size:15px;
                                  margin-bottom: 0em'),
          h6(HTML(paste0("<a href='https://www.3ai.in' target = '_blank'> - 3AI Ambassdor</a>")), style = 'color:white;text-align: right;font-size:15px;')
        ),
        fixedPanel(
          div(
            actionBttn("fullScreen",
                       style = "material-circle",
                       icon = icon("arrows-alt"),
                       size = "xs",
                       color = "warning"),
            bsPopover("fullScreen", title = NULL, content = "Click to view in full screen", placement = "left", trigger = "hover",
                      options = NULL),
            onclick = "shinyjs.toggleFullScreen();"
          ),
          top = 55,
          right = 10
          
        ),
        fixedPanel(
          div(
            actionBttn("kofi",
                       style = "material-circle",
                       icon = icon("coffee"),
                       size = "xs",
                       color = "success"),
            bsPopover("kofi", title = NULL, content = "Buy me a coffee", placement = "left", trigger = "hover",
                      options = NULL),
            onclick = "window.open('https://ko-fi.com/shubhrampandey', '_blank')"
          ),
          top = 55,
          right = 40
          
        ),
        fixedPanel(
          div(
            actionBttn("userGuide",
                       style = "material-circle",
                       icon = icon("info"),
                       size = "xs",
                       color = "royal"),
            bsPopover("userGuide", title = NULL, content = "Go to app help page", placement = "left", trigger = "hover",
                      options = NULL),
            onclick = "window.open('https://sites.google.com/view/covid-19-userguide/home', '_blank')"
          ),
          top = 55,
          right = 70
          
        ),
        fixedPanel(
          div(
            actionBttn("webSite",
                       style = "material-circle",
                       icon = icon("address-card"),
                       size = "xs",
                       color = "primary"),
            bsPopover("webSite", title = NULL, content = "About developer", placement = "left", trigger = "hover",
                      options = NULL),
            onclick = "window.open('https://www.shubhrampandey.com', '_blank')"
          ),
          top = 55,
          right = 100
          
        )
                    )
      
      
      ),
    sidebar = NULL,
    body = argonDashBody(
      tags$head( tags$meta(name = "viewport", content = "width=1600"),uiOutput("body")),
      tags$br(),
           dashboardUI,
      tags$hr(),
      h5("Important Note:",style = 'color:Red;font-size:20px;text-align:Left;'),
      p("1. The data used in this dashboard extracted from webscrapping. In case of any discrepnecy in the numbers please contact with me.",style = 'color:Red;font-size:15px;text-align:Left;'),
      p(paste0("2. Dashboard will be updated on daily basis at GMT 00:00. It could be a chance that daily numbers not match as per your local source but aggregare numbers will definitely match."),style = 'color:Red;font-size:15px;text-align:Left;'),
      p(paste0("3. Last update: ",lastUpdate),style = 'color:Red;font-size:15px;text-align:Left;')
    )
  )
  )

##### server #####
server <- function(input, output, session) {
  printLogJs = function(x, ...) {
    logjs(x)
    T
  }
  # addHandler(printLogJs)
  if (!Production) options(shiny.error = recover)
  options(shiny.sanitize.errors = TRUE, width = 160)
  
  session$onSessionEnded(function() {
    stopApp()
    # q("no")
  })
  source("appFiles/dashboardServer.R", local = TRUE)
  # Hide the loading message when the rest of the server function has executed
  waiter_hide() # will hide *on_load waiter
}

# Run the application
shinyApp(ui = ui, server = server)