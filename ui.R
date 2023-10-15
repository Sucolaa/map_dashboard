library(shiny)
source("share_loads.R")
walk(list.files("ui_scripts", full.names = TRUE), ~ source(.x))

dashboardPage(
  skin = "black",
  dashboardHeader(
    title = span("SU Dashboard",
                 style = "font-weight:bold;"),
    title_dropdown
  ),
  dashboardSidebar(
    sidebarMenu(
      baidu_menu
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        @media (max-width: 1366px) {
          /* 在小于等于1366px宽度的屏幕上应用以下样式 */
          .content-wrapper {
            max-width: 90%; /* 调整内容的最大宽度 */
          }
          .main-sidebar {
            width: 200px; /* 调整侧边栏的宽度 */
          }
        }
        @media (max-width: 1920px) {
          /* 在小于等于1920px宽度的屏幕上应用以下样式 */
          .content-wrapper {
            max-width: 100%; /* 恢复默认最大宽度 */
          }
          .main-sidebar {
            width: 250px; /* 恢复默认侧边栏宽度 */
          }
        }
      "))
    ),
    tabItems(
      baidu_item
    )
  )
)