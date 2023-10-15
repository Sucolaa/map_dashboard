library(shiny)
function(input, output, session) {
#地图和城市柱状图不受门店选择的影响
  #地图
  # output$whole_map <- renderHchinamap({
  #   burberry %>%
  #     mutate(province = str_replace(province,"(市|省|壮族自治区|维吾尔自治区)","")) %>%
  #     group_by(province) %>%
  #     mutate(total = n()) %>%
  #     distinct(province, total) -> geo_dist
  #   hchinamap(
  #     name = geo_dist$province,
  #     value = geo_dist$total,
  #     itermName = "店铺数量"
  #   )
  # })
  
  #城市柱状图
  #这个不用了
  # output$store_cities_rank <- renderHighchart({
  #   burberry %>%
  #     distinct(name, .keep_all = TRUE) %>%
  #     select(city) %>%
  #     group_by(city) %>%
  #     mutate(total = n()) %>%
  #     distinct(city, .keep_all = TRUE) %>%
  #     ungroup() %>%
  #     arrange(desc(total)) %>%
  #     head(10) -> burberry_city_top
  # 
  #   highchart() %>%
  #     hc_xAxis(categories = burberry_city_top$city) %>%
  #     hc_add_series(
  #       data = burberry_city_top$total,
  #       type = "column",
  #       name = "门店数量"
  #     ) %>%
  #     hc_yAxis(labels = list(format = "{value:.0f}")) %>%
  #     hc_title(text = "门店数量TOP10城市",
  #              style = list(
  #                fontSize = "12px",
  #                fontWeight = "bold"
  #              )) %>%
  #     hc_legend(enabled = FALSE)
  # })
  
#门店选择checkbox
  observeEvent(input$select_all_checkbox, {
    if (input$select_all_checkbox) {
      updateCheckboxGroupInput(session, "store_select", selected = unique(burberry_full$name))
    } else {
      updateCheckboxGroupInput(session, "store_select", selected = character(0))
    }
  })

  observe({
    selected_choices <- head(unique(burberry_full$name), 5)
    updateCheckboxGroupInput(session, "store_select", selected = selected_choices)
  })  

# 门店、日期筛选
  store_select <- reactive({
    selected_stores <- input$store_select
    date_range <- input$date_select

    burberry_full %>% 
      filter(name %in% selected_stores) %>%
      filter(date >= date_range[1] & date <= date_range[2]) -> filtered_data

    return(filtered_data)
  })
  
#百度得分雷达图
  #每个门店
  output$store_score <- renderHighchart({
    store_select() %>% 
      select(name,show, travel, content, activity, interact) %>% 
      filter(!is.na(show)) %>% 
      group_by(name) %>% 
      mutate(曝光热度 = round(sum(show)/n(),2),
             出行信息 = round(sum(travel)/n(),2),
             经营内容 = round(sum(content)/n(),2),
             活跃度 = round(sum(activity)/n(),2),
             互动量 = round(sum(interact)/n(),2)) %>% 
      select(-c(show, travel, content, activity, interact)) %>% 
      distinct(name, .keep_all = TRUE) %>% 
      pivot_longer(
        cols = c(曝光热度,出行信息,经营内容,活跃度,互动量),
        names_to = "dimensions",
        values_to = "value"
      ) -> radar_store
    highchart() %>%
      hc_chart(polar = TRUE,
               type = "line") %>%
      hc_xAxis(
        categories = radar_store$dimensions,
        lineWidth = 0,
        tickmarkPlacement  = "on",
        labels = list(
          style = list(
            fontWeight = "bold"
          )
        )
      ) %>%
      hc_yAxis(
        gridLineInterpolation = "polygon",
        linewidth = 0,
        min = 0,
        max = 100,
        tickInterval = 20,
        labels = list(enabled = FALSE)
      ) %>%
      hc_add_series_list(
        lapply(unique(radar_store$name), function(name) {
          data_subset <- radar_store[radar_store$name == name, ]
          list(
            name = name,
            data = data_subset$value
          )
        })
      ) %>%
      hc_legend(
        # align  = "right",
        # layout = "vertical",
        enabled = FALSE
      ) %>%
      hc_tooltip(
        shared = FALSE,
        pointFormat = '<span style="color:{series.color}">{series.name}: <b>{point.y:,.2f}</b><br/>',
        valueDecimals = 2)%>%
      hc_add_theme(hc_theme_google())
  })
  #所选门店总
  output$store_score_overall <- renderHighchart({
    store_select() %>%
      select(show, travel, content, activity, interact) %>% 
      filter(!is.na(show)) %>% 
      mutate(曝光热度 = round(sum(show)/n(),2),
             出行信息 = round(sum(travel)/n(),2),
             经营内容 = round(sum(content)/n(),2),
             活跃度 = round(sum(activity)/n(),2),
             互动量 = round(sum(interact)/n(),2)) %>% 
      select(-c(show, travel, content, activity, interact)) %>% 
      distinct(曝光热度,出行信息,经营内容,活跃度,互动量) %>% 
      pivot_longer(
        cols = c(曝光热度,出行信息,经营内容,活跃度,互动量),
        names_to = "dimensions",
        values_to = "value"
      ) -> radar_full

    highchart() %>%
      hc_chart(polar = TRUE,
               type = "line") %>%
      hc_xAxis(
        categories = radar_full$dimensions,
        lineWidth = 0,
        tickmarkPlacement  = "on",
        labels = list(
          style = list(
            fontWeight = "bold"
          )
        )
      ) %>%
      hc_yAxis(
        gridLineInterpolation = "polygon",
        linewidth = 0,
        min = 0,
        max = 100,
        tickInterval = 20,
        labels = list(enabled = FALSE)
      ) %>%
      hc_add_series(
        data = radar_full,
        type = "line",
        name = "得分",
        hcaes(x = dimensions,
              y = value)
      ) %>%
      hc_legend(
        # align  = "right",
        # layout = "vertical",
        enabled = FALSE
      ) %>%
      hc_tooltip(
        shared = TRUE,
        pointFormat = '<span style="color:{series.color}">{series.name}: <b>{point.y:,.2f}</b><br/>')
  })
 #百度门店得分表
  output$score_rank_df <- renderDT({
    store_select() %>% 
      select(name,show, travel, content, activity, interact) %>% 
      filter(!is.na(show)) %>% 
      group_by(name) %>% 
      mutate(曝光热度 = round(sum(show)/n(),2),
             出行信息 = round(sum(travel)/n(),2),
             经营内容 = round(sum(content)/n(),2),
             活跃度 = round(sum(activity)/n(),2),
             互动量 = round(sum(interact)/n(),2),
             门店 = name) %>% 
      ungroup() %>% 
      select(-c(show, travel, content, activity, interact, name)) %>% 
      distinct(门店, .keep_all = TRUE) %>% 
      group_by(门店) %>% 
      mutate("总分(满分500)" = sum(曝光热度,出行信息,经营内容,活跃度,互动量)) %>% 
      select(门店, everything()) %>% 
      arrange(desc(`总分(满分500)`)) -> baidu_score_rank_df
    formattable(
      baidu_score_rank_df,
      list(
        `门店` = color_bar(color = "lightgray"),
        `曝光热度` = color_tile("white", "pink"),
        `出行信息` = color_tile("white", "pink"),
        `经营内容` = color_tile("white", "pink"),
        `活跃度` = color_tile("white", "pink"),
        `互动量` = color_tile("white", "pink"),
        `总分(满分500)` = color_tile("white", "lightblue")
      )
    ) %>% 
      as.datatable(escape = FALSE,
                   options = list(
                     scrollX = TRUE,
                     pageLength = 8,
                     dom = "Bfrtip"
                   ),
                   rownames = FALSE) %>% 
      formatStyle(columns = '门店', minWidth = '270px')
  })
  
  #漏斗图
    #百度漏斗图
  output$baidu_funnel_plot <- renderHighchart({
    store_select() %>% 
      select(曝光次数, 意向顾客数量,收藏次数当日值) %>% 
      mutate(曝光次数 = sum(曝光次数, na.rm = TRUE),
             意向顾客数量 = sum(意向顾客数量,na.rm = TRUE),
             收藏次数 = sum(收藏次数当日值, na.rm = TRUE)) %>% 
      distinct(曝光次数,意向顾客数量,收藏次数) %>% 
      pivot_longer(
        cols = c(曝光次数,意向顾客数量,收藏次数),
        names_to = "dimensions",
        values_to = "value"
      ) %>% 
      #漏斗图用这个固定比例，没有细调，总之就是得用固定比例，因为大部分门店数据都很差，highchart自己算的真实比例会很尴尬
      mutate(fixed_value = c(30,20,10)) -> baidu_funnel
    highchart() %>% 
      hc_legend(enabled = FALSE)%>%
      hc_add_series(name ="总值",
                    type = "funnel",
                    data = baidu_funnel,
                    hcaes(x= dimensions,y = fixed_value),
                    dataLabels = list(enabled = TRUE, format = "<b>{point.name}</b> ({point.value:,.0f})", distance = -100)) %>%
      hc_tooltip(pointFormat = "数值: {point.value}") %>% 
      hc_plotOptions(series = list(dataLabels = list(
        format = "<b>{point.name}</b> ({point.y:,.0f})"),
        neckWidth = "45%",
        neckHeight = "40%"
      ))
  })
    #腾讯漏斗图
  output$tencent_funnel_plot <- renderHighchart({
    store_select() %>% 
      select(name, shoptotalts, shoptotaltc, collect_daily) %>% 
      mutate("店铺曝光量" = sum(shoptotalts, na.rm = TRUE),
             "意向客户数" = sum(shoptotaltc, na.rm = TRUE),
             "收藏次数" = sum(collect_daily, na.rm = TRUE)) %>% 
      distinct(店铺曝光量,意向客户数,收藏次数) %>% 
      pivot_longer(
        cols = c(店铺曝光量,意向客户数,收藏次数),
        names_to = "dimensions",
        values_to = "value"
      ) %>% 
      mutate(fixed_value = c(30,20,10)) -> tencent_funnel
    highchart() %>% 
      hc_legend(enabled = FALSE)%>%
      hc_add_series(name ="总值",
                    type = "funnel",
                    data = tencent_funnel,
                    hcaes(x= dimensions,y = fixed_value),
                    dataLabels = list(enabled = TRUE, format = "<b>{point.name}</b> ({point.value:,.0f})", distance = -100)) %>%
      hc_tooltip(pointFormat = "数值: {point.value}") %>% 
      hc_plotOptions(series = list(dataLabels = list(
        format = "<b>{point.name}</b> ({point.y:,.0f})"),
        neckWidth = "45%",
        neckHeight = "40%"
      ))
  })
  
  #曝光意向图
    #百度曝光意向图
  output$baidu_show_intent <- renderHighchart({
    store_select() %>% 
      select(date, 曝光次数,意向顾客数量) %>% 
      group_by(date) %>% 
      mutate(show = sum(曝光次数, na.rm = TRUE),
             intent = sum(意向顾客数量,na.rm = TRUE)) %>% 
      ungroup() %>% 
      distinct(date, .keep_all = TRUE) %>% 
      mutate(ratio = round(intent/show,6)*100) %>% 
      mutate(ratio = ifelse(is.na(ratio), 0, ratio)) %>% 
      arrange(date) -> burberry_baidu_ts
    highchart() %>% 
      hc_xAxis(type = "datetime",
               dateTimeLabelFormats = list(day = "%Y-%m-%d"),
               ordinal = TRUE) %>% 
      hc_yAxis_multiples(
        list(
          top = "0%",
          height = "100%",
          offset = 0,
          visible = FALSE
        ),
        list(
          top = "0%",
          height = "100%",
          offset = 0,
          opposite = TRUE,
          max = max(burberry_baidu_ts$intent)*2,
          visible = FALSE
        ),
        list(top = "0%",
             height = "100%",
             offset = 0,
             max = max(burberry_baidu_ts$ratio)*10,
             visible = FALSE)
      ) %>% 
      hc_add_series(data = burberry_baidu_ts,
                    type = "line",
                    name = "曝光次数",
                    hcaes(x = date, y = show),
                    marker = list(enabled = FALSE),
                    yAxis = 0) %>% 
      hc_add_series(data = burberry_baidu_ts,
                    type = "line",
                    name = "意向人数",
                    hcaes(x = date, y = intent),
                    marker = list(enabled = FALSE),
                    yAxis = 1,
                    color = "pink") %>% 
      hc_add_series(data = burberry_baidu_ts,
                    type = "column",
                    name = "转化率",
                    hcaes(x = date, y = ratio),
                    yAxis = 2,
                    color = "grey") %>% 
      hc_tooltip(
        shared = TRUE,
        pointFormatter = JS("function() {
      if (this.series.name === '转化率') {
        return '<span style=\"color:' + this.series.color + '\">' + this.series.name + '</span>: <b>' + Highcharts.numberFormat(this.y, 2) + '%</b><br>';
      } else {
        return '<span style=\"color:' + this.series.color + '\">' + this.series.name + '</span>: <b>' + this.y + '</b><br>';
      }
    }")
      )
  })
    #腾讯曝光意向图
  output$tencent_show_intent <- renderHighchart({
    store_select() %>% 
      select(date, shoptotalts,shoptotaltc) %>% 
      group_by(date) %>% 
      mutate(show = sum(shoptotalts, na.rm = TRUE),
             intent = sum(shoptotaltc,na.rm = TRUE)) %>% 
      ungroup() %>% 
      distinct(date, .keep_all = TRUE) %>% 
      mutate(ratio = round(intent/show,6)*100) %>% 
      arrange(date) %>% 
      mutate(ratio = ifelse(is.na(ratio), 0, ratio)) -> burberry_tencent_ts
    highchart() %>% 
      hc_xAxis(type = "datetime",
               dateTimeLabelFormats = list(day = "%Y-%m-%d"),
               ordinal = TRUE) %>% 
      hc_yAxis_multiples(
        list(
          top = "0%",
          height = "100%",
          offset = 0,
          visible = FALSE
        ),
        list(
          top = "0%",
          height = "100%",
          offset = 0,
          opposite = TRUE,
          max = max(burberry_tencent_ts$intent)*2,
          visible = FALSE
        ),
        list(top = "0%",
             height = "100%",
             offset = 0,
             max = max(burberry_tencent_ts$ratio)*10,
             visible = FALSE)
      ) %>% 
      hc_add_series(data = burberry_tencent_ts,
                    type = "line",
                    name = "曝光次数",
                    hcaes(x = date, y = show),
                    marker = list(enabled = FALSE),
                    yAxis = 0) %>% 
      hc_add_series(data = burberry_tencent_ts,
                    type = "line",
                    name = "意向人数",
                    hcaes(x = date, y = intent),
                    marker = list(enabled = FALSE),
                    yAxis = 1,
                    color = "pink") %>% 
      hc_add_series(data = burberry_tencent_ts,
                    type = "column",
                    name = "转化率",
                    hcaes(x = date, y = ratio),
                    yAxis = 2,
                    color = "grey") %>% 
      hc_tooltip(
        shared = TRUE,
        pointFormatter = JS("function() {
      if (this.series.name === '转化率') {
        return '<span style=\"color:' + this.series.color + '\">' + this.series.name + '</span>: <b>' + Highcharts.numberFormat(this.y, 2) + '%</b><br>';
      } else {
        return '<span style=\"color:' + this.series.color + '\">' + this.series.name + '</span>: <b>' + this.y + '</b><br>';
      }
    }")
      )
  })
  
  #门店曝光，意向，转换率表
  output$store_show_intent_rank_df <- renderDT({
    store_select() %>% 
      select(date, name, 曝光次数, shoptotalts,意向顾客数量, shoptotaltc) %>% 
      group_by(name) %>% 
      mutate("百度曝光" = sum(曝光次数, na.rm = TRUE),
             "腾讯曝光" = sum(shoptotalts, na.rm = TRUE),
             "百度意向顾客数" = sum(意向顾客数量, na.rm = TRUE),
             "腾讯意向顾客数" = sum(shoptotaltc, na.rm = TRUE),
             "百度转化率" = round(百度意向顾客数/百度曝光, 3),
             "腾讯转化率" = round(腾讯意向顾客数/腾讯曝光, 4),
             "门店" = name) %>% 
      ungroup() %>% 
      select(门店, 百度曝光,百度意向顾客数,百度转化率, 腾讯曝光,腾讯意向顾客数,腾讯转化率) %>% 
      distinct(门店, .keep_all = TRUE) %>% 
      mutate(百度转化率 = ifelse(is.na(百度转化率),0,百度转化率),
             腾讯转化率 = ifelse(is.na(腾讯转化率),0,腾讯转化率)) %>% 
      mutate(百度转化率 = percent(百度转化率),
             腾讯转化率 = percent(腾讯转化率)) %>% 
      arrange(desc(百度曝光)) -> show_rank
    formattable(
      show_rank,
      list(
        `门店` = color_bar(color = "lightgrey"),
        `百度曝光` = color_tile("white","pink"),
        `百度意向顾客数` = color_tile("white","pink"),
        `百度转化率` =color_tile("white","pink"),
        `腾讯曝光` = color_tile("white", "lightblue"),
        `腾讯意向顾客数` = color_tile("white","lightblue"),
        `腾讯转化率` = color_tile("white","lightblue")
      )
    ) %>% 
      as.datatable(escape = FALSE,
                   options = list(
                     scrollX = TRUE,
                     pageLength = 8,
                     dom = "Bfrtip"
                   ),
                   rownames = FALSE) %>% 
      formatStyle(columns = '门店', minWidth = '270px')
  })
  
  output$collect_plot <- renderHighchart({
    store_select() %>% 
      select(date, 收藏次数累计值, collect_cnt) %>% 
      group_by(date) %>% 
      mutate(baidu_collect = sum(收藏次数累计值, na.rm = TRUE),
             tencent_collect = sum(collect_cnt, na.rm = TRUE)) %>% 
      select(-c(收藏次数累计值, collect_cnt)) %>% 
      ungroup() %>% 
      distinct(date, .keep_all = TRUE) %>% 
      arrange(date) -> collect_ts
    highchart() %>% 
      hc_xAxis(type = "datetime",
               dateTimeLabelFormats = list(day = "%Y-%m-%d"),
               ordinal = TRUE) %>% 
      hc_add_series(data = collect_ts,
                    type = "area",
                    name = "百度收藏数",
                    hcaes(x = date,
                          y = baidu_collect),
                    marker = list(enabled = FALSE)
      ) %>% 
      hc_add_series(data = collect_ts,
                    type = "area",
                    name = "腾讯收藏数",
                    hcaes(x = date,
                          y = tencent_collect),
                    color = "lightpink",
                    marker = list(enabled = FALSE)
      ) %>% 
      hc_tooltip(shared = TRUE,
                 pointFormat = '<span style="color:{series.color}">{series.name}: <b>{point.y:,.0f}</b><br/>'
      )
  })

  
# #门店排行图
#   #曝光
#   baidu_store_rank <- reactive({
#     store_select() %>% 
#       select(yid, name,updatetime,yesterday_show_cnt,yesterday_intent_cnt,yesterday_comment_cnt, yesterday_favor_cnt) %>% 
#       group_by(yid) %>% 
#       mutate(show = sum(yesterday_show_cnt),
#              intent = sum(yesterday_intent_cnt),
#              comment = sum(yesterday_comment_cnt),
#              favor = sum(yesterday_favor_cnt)) %>% 
#       distinct(yid, .keep_all = TRUE) %>% 
#       select(-c(yid,yesterday_show_cnt,yesterday_intent_cnt,yesterday_comment_cnt,yesterday_favor_cnt,updatetime)) %>% 
#       ungroup() -> baidu_store_rank
#     return(baidu_store_rank)
#   })
#   output$show_rank_plot <- renderHighchart({
#     baidu_store_rank() %>% 
#       arrange(desc(show)) %>% 
#       head(10) -> show_top10
#     highchart() %>% 
#       hc_chart(inverted = TRUE) %>% 
#       hc_xAxis(categories = show_top10$name) %>% 
#       hc_add_series(
#         data = show_top10$show,
#         name = "曝光总数",
#         type = "column"
#       ) %>% 
#       hc_legend(enabled = FALSE)
#   })
#   output$favor_rank_plot <- renderHighchart({
#     baidu_store_rank() %>% 
#       arrange(desc(favor)) %>% 
#       head(10) -> favor_top10
#     highchart() %>% 
#       hc_chart(inverted = TRUE) %>% 
#       hc_xAxis(categories = favor_top10$name) %>% 
#       hc_add_series(
#         data = favor_top10$favor,
#         name = "favor",
#         type = "column"
#       ) %>% 
#       hc_legend(enabled = FALSE)
#   })
#   output$intent_rank_plot <- renderHighchart({
#     baidu_store_rank() %>% 
#       arrange(desc(intent)) %>% 
#       head(10) -> intent_top10
#     highchart() %>% 
#       hc_chart(inverted = TRUE) %>% 
#       hc_xAxis(categories = intent_top10$name) %>% 
#       hc_add_series(
#         data = intent_top10$intent,
#         name = "intent",
#         type = "column"
#       ) %>% 
#       hc_legend(enabled = FALSE)
#   })
#   output$comment_rank_plot <- renderHighchart({
#     baidu_store_rank() %>% 
#       arrange(desc(comment)) %>% 
#       head(10) -> comment_top10
#     highchart() %>% 
#       hc_chart(inverted = TRUE) %>% 
#       hc_xAxis(categories = comment_top10$name) %>% 
#       hc_add_series(
#         data = comment_top10$comment,
#         name = "comment",
#         type = "column"
#       ) %>% 
#       hc_legend(enabled = FALSE)
#   })
#   output$sti_convert_rank_plot <- renderHighchart({
#     baidu_store_rank() %>% 
#       mutate(sti = round(intent/show,3)*100)-> sti_rank
#     sti_rank[is.na(sti_rank)] <- 0
#     sti_rank %>% 
#       arrange(desc(sti)) %>% 
#     head(10) -> sti_rank
#     highchart() %>% 
#       hc_chart(inverted = TRUE) %>% 
#       hc_xAxis(categories = sti_rank$name) %>% 
#       hc_add_series(
#         data = sti_rank$sti,
#         name = "转换率",
#         type = "column"
#       ) %>% 
#       hc_legend(enabled = FALSE) %>% 
#       hc_yAxis(
#         labels = list(
#           format = "{value}%"
#         )
#       ) %>% 
#       hc_tooltip(
#         pointFormat = "<span style='color:{series.color}'>{series.name}: <b>{point.y:.2f}%</b></span>"
#       )
#   })
#   
#   output$overall_rank <- renderDT({
#     store_select() %>% 
#       group_by(yid) %>%
#       mutate("曝光热度" = round(sum(show)/n(),digits = 2),
#              "出行信息" = round(sum(travel)/n(),digits = 2),
#              "经营内容" = round(sum(content)/n(),digits = 2),
#              "活跃度" = round(sum(activity)/n(),digits = 2),
#              "互动量" = round(sum(interact)/n(),digits = 2)) %>% 
#       mutate(rank = round(sum(rank)/n(),0),
#              total_cnt = round(sum(total_cnt)/n(),0),
#              "所在商圈内排名" = paste0(rank,"/",total_cnt)) %>% 
#       mutate("总分" = (曝光热度+出行信息+经营内容+活跃度+互动量)) %>% 
#       mutate(show_total = sum(yesterday_show_cnt),
#              favor_total = sum(yesterday_favor_cnt),
#              intent_total = sum(yesterday_intent_cnt),
#              comment_total = sum(yesterday_comment_cnt)) %>% 
#       ungroup() %>% 
#       distinct(yid,.keep_all = TRUE) %>% 
#       arrange(desc(show_total)) %>% 
#       mutate("曝光排名" = row_number()) %>% 
#       arrange(desc(favor_total)) %>% 
#       mutate("favor排名" = row_number()) %>% 
#       arrange(desc(intent_total)) %>% 
#       mutate("intent排名" = row_number()) %>% 
#       arrange(desc(comment_total)) %>% 
#       mutate("comment排名" = row_number()) %>% 
#       arrange(desc(总分)) %>% 
#       mutate("五项得分排名" = row_number()) %>% 
#       mutate("店铺"=name) %>% 
#       select(店铺,所在商圈内排名,曝光排名,favor排名,intent排名,comment排名,五项得分排名) -> overall_rank
#     
#     formattable(
#       overall_rank,
#       list(
#         `店铺` = color_bar("lightblue"),
#         `曝光排名` = color_tile("pink","white"),
#         `favor排名` = color_tile("pink","white"),
#         `intent排名` = color_tile("pink","white"),
#         `comment排名` = color_tile("pink","white"),
#         `五项得分排名` = color_tile("lightgrey","white")
#       )
#     ) %>% 
#       as.datatable(escape = FALSE,
#                    options = list(scrollX = TRUE),
#                    rownames = FALSE)
#   })
}
