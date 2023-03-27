library(shiny)
library(dplyr)
library(shinyBS)
library(stringr)


# 读取储存的文章数据
data <- read.csv("https://raw.githubusercontent.com/Yorks0n/TRxiv/main/data.csv")



shinyServer(function(input, output, session){
  
  # create selection for server
  output$server <- renderUI({
    server_list <- sort(unique(data$server))
    selectInput("server", "Choose the Server:",selected = server_list[1], as.list(server_list))
  })
  
  # create selection for category
  output$category <- renderUI({
    # 使用req确保这里先获得选择的server类型
    req(input$server)
    
    selected_data <- getServerData()
    category_list <- sort(unique(selected_data$category))
    selectInput("category", "Choose the Category:",selected = category_list[1], as.list(category_list))
  })
  
  # create selection for category
  output$timeframe <- renderUI({
    # timeframe_list <- c("1m", "1w", "3d", "1d")
    # c("Monthly", "Weekly", "Three Days", "One Day")
    selectInput("timeframe", "Choose the Timeframe:",
                selected = "1m",
                choices = list("Monthly" = "1m", "Weekly" = "1w",
                               "Three Days" = "3d", "Last Day" = "1d"))
  })
  
  # get selected server data
  getServerData <- reactive({
    selected_server <- req(input$server)
    selected_data <- filter(data, server == selected_server)
    selected_data
  })
  
  # Use the selections to filter the table
  getFilteredData <- reactive({
    # 先获得选中类别的结果
    selected_data <- req(getServerData())
    
    # 确保输入完成
    req(input$timeframe)
    req(input$category)
    
    # 根据其他选项进行过滤
    ## 先筛选出符合要求的行
    res_category <- selected_data %>% filter(category == input$category) 
    
    ## 再根据时间窗口，选择特定的列进行排序
    ## 需要手搓一个
    timeOfRes <- res_category[,paste0("X",input$timeframe)]
    # 排序并只取至多前10个结果
    out_category <- res_category[order(timeOfRes, decreasing = TRUE),] %>% head(10)
    out_category
  })
  
  #output$text <- renderText(input$timeframe)
  
  output$view <- renderTable({
    getFilteredData() 
  })
  
  title_output <- reactive({
    out_category <- getFilteredData()
    
    lapply(1:(nrow(out_category)), function(i) {
      renderText(out_category[i,1])
    })
  })
  
  
  output$title_output <- renderUI({
    #tagList(title_output())
    out_category <- getFilteredData()
    
    #popify(bsButton("pB2", out_category[1,"title"], style = "inverse"),
    #       title = out_category[1,"date"],
    #       content = out_category[1,"abstract"])
    
    lapply(1:(nrow(out_category)), function(i) {
      popify(bsButton(paste0("PB",i), out_category[i,"title"], style = "inverse", block = TRUE),
             title = out_category[i,"date"],
             content = out_category[i,"abstract"],
             trigger = "click")
    })
  })
  
  # create bsCollapse panels
  output$title_bsCollapse <- renderUI({
    # get the out table
    out_category <- getFilteredData()
    
    #  create the CollapsePanels
    panels <- lapply(1:(nrow(out_category)), function(i) {
      # Background(?=[A-Z])|Methods(?=[A-Z])|Results(?=[A-Z])|Discussion(?=[A-Z])
      abstract_format <- str_remove_all(out_category[i,"abstract"],
                                        "Background(?=[A-Z])|Methods(?=[A-Z])|Results(?=[A-Z])|Discussion(?=[A-Z])")
      bsCollapsePanel(out_category[i,"title"],
                      out_category[i,"authors"],
                      HTML(paste0("<br><a href='https://doi.org/", out_category[i,"doi"],"'>",out_category[i,"doi"],"</a><br>")),
                      abstract_format,
                      style = "info")
    })
    
    # call these panels 
    do.call(tagList, panels)
  })
  
  # the details of this tool
  output$detail_text <- renderText({
    HTML("<p>Altmetric热门预印本：<br/>
1. 利用Altmetric API请求timeframe=T内的X篇doi_prefix=10.1101的热门文章，其中包含bioRxiv和medRxiv的文章，目前计划分别请求1d、3d、1w、1m这几个时间的结果，并且合并在同一个表中。<br/>
2. 利用bioRxiv API，根据这些文章的doi获取文章的摘要、分类等信息<br/>
3. 将这些信息合并后存入数据库<br/>
4. 从数据库中，根据选择的来源、类别进行筛选，筛选结果利用对应时间内获得的Altmetric分数排序，取靠前的最多10个结果进行展示<br/>
5. 再过X时间，重复1~3过程。</p><br/>
<a href='https://www.altmetric.com/about-us/our-data/donut-and-altmetric-attention-score/'>How is the Altmetric score calculated?</a>")
  })
  
  # 显示上次更新时间
  output$updated_time <- renderUI({
    # 将特定UNIX时间转换为POSIXct对象
    last_update_time <- as.POSIXct(as.numeric(data$update_time[1]), origin="1970-01-01")
    
    # 计算时间差
    time_diff <- difftime(Sys.time(), last_update_time, units="hours")
    
    # 输出小时差
    
    
    renderText(paste("Last Update:",round(time_diff,1), "hours ago."))
  })
})

