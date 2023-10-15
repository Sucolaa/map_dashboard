date_box <- box(
  status = "warning",
  width = 2,
  height = "80px",
  dateRangeInput(inputId = "date_select",
                 label = "日期区间",
                 start = min(burberry_full$date),
                 end = max(burberry_full$date),
                 min = min(burberry_full$date),
                 max = max(burberry_full$date),
                 language = "zh-CN")
)

title_box <- box(
  status = "warning",
  width = 10,
  height = "80px",
  div(
    strong("地图商户通店铺流量数据分析"), 
    style = "text-align: center; vertical-align: middle;font-size: 26px; font-weight: bold;font-style: italic;"
  )
)

store_select_box <- box(
  width = 3,
  height = 380,
  style = "max-height: 90%; overflow-y: auto;",
  title = strong("门店选择"),
  div(
    id = "select_all_div",
    class = "form-group",
    checkboxInput(
      inputId = "select_all_checkbox",
      label = "全选",
      value = FALSE
    )),
  checkboxGroupInput(
    inputId = "store_select",
    label = NULL,
    selected = head(unique(burberry_full$name),5),
    choices = unique(unique(burberry_full$name))
  )
)
info_text_box <- box(
  width = 9,
  height = 380,
  column(12,
         column(4),
         column(4,
                infoBox(
                  title = "门店总数",
                  value = paste0(length(unique(burberry$name))),
                  icon = icon("store"),
                  color = "black",
                  width = 12
                )),
         column(4)),
  column(12,
         column(1),
         column(4,
                infoBox(
                  title = "百度商户通门店",
                  value = paste0(length(unique(burberry_baidu$name))),
                  icon = icon("map-pin"),
                  color = "light-blue",
                  width = 12
                ),
                infoBox(
                  title = "百度商户通开通率",
                  value = paste0(round(length(unique(burberry_baidu$name))/length(unique(burberry$name)),3)*100,"%"),
                  color = "light-blue",
                  icon = icon("map-pin"),
                  fill = TRUE,
                  width = 12
                )
                ),
         column(2),
         column(4,
                infoBox(
                  title = "腾讯商户通门店",
                  value = paste0(length(unique(burberry_tencent$name))),
                  icon = icon("location-dot"),
                  color = "maroon",
                  width = 12
                ),
                infoBox(
                  title = "百度商户通开通率",
                  value = paste0(round(length(unique(burberry_tencent$name))/length(unique(burberry$name)),3)*100,"%"),
                  color = "maroon",
                  icon = icon("location-dot"),
                  fill = TRUE,
                  width = 12
                )
                ),
         column(1))
)

basic_info_box <- box(
  status = "warning",
  width = 12,
  height = 450,
  store_select_box,
  info_text_box
)

radar_box <- box(
  width = 12,
  height = 500,
  status = "warning",
  tabBox(
    width = 3,
    title = strong("百度得分"),
    side = "right",
    selected = "总值",
    tabPanel("店均",
             highchartOutput("store_score",
                             width = "90%")),
    tabPanel("总值",
             highchartOutput("store_score_overall",
                             width = "90%"))
  ),
  tabBox(
    width = 9,
    title = strong("门店得分表"),
    side = "right",
    tabPanel("",
             DTOutput("score_rank_df"))
  )
)

funnel_box <- box(
  width = 3,
  height = 500,
  status = "warning",
  tabBox(
    width = 12,
    title = strong("漏斗"),
    side = "right",
    selected = "百度",
    tabPanel("腾讯",
             highchartOutput("tencent_funnel_plot")),
    tabPanel("百度",
             highchartOutput("baidu_funnel_plot"))
  )
)

char_plots_box <-tabBox(
    width = 6,
    height = 500,
    title = strong("曝光，意向，转换率"),
    side = "right",
    selected = "百度",
    tabPanel("腾讯",
             highchartOutput("tencent_show_intent")),
    tabPanel("百度",
             highchartOutput("baidu_show_intent"))
  )

store_rank_box <- tabBox(
    width = 6,
    height = 500,
    title = strong("门店排行"),
    side = "right",
    tabPanel("",
             DTOutput(outputId = "store_show_intent_rank_df",
                      height = "450"))
  )

collect_box <- box(
  width = 9,
  height = 500,
  status = "warning",
  tabBox(
    width = 12,
    side = "right",
    title = strong("店铺收藏数变化"),
    tabPanel("",
             highchartOutput("collect_plot"))
  )
)

baidu_item <- tabItem(
  tabName = "platforms",
  style = "min-height: 800px; overflow-y: auto;",
  column(
    12,
    date_box,
    title_box
  ),
  column(
    12,
    basic_info_box
  ),
  column(
    12,
    box(
      width = 12,
      status = "warning",
      char_plots_box,
      store_rank_box
    )
  ),
  column(
    12,
    funnel_box,
    collect_box,
  ),
  column(
    12,
    radar_box
  )
)