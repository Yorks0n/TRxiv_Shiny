library(shiny)
library(shinyBS)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Trending bioRxiv and medRxiv according to Altmetric "),
  # Sidebar layout
  sidebarLayout(
    
    sidebarPanel(
      
      uiOutput("server"),
      uiOutput("category"),
      uiOutput("timeframe"),
      
      # a button to collapse all the panels
      # fail to set id for a bsCollapsePanel 
      # actionButton("p1Button", "Collapse All")
      
      # Describe the data processing process in detail
      actionButton("detailButton", "How it works"),
      uiOutput("updated_time"),
    ),
    
    mainPanel(
      
      uiOutput("title_bsCollapse"),
      
      # Modal to describe the data processing process in detail
      bsModal("modalDetail", "The data processing process in detail", "detailButton", size = "large",htmlOutput("detail_text"))
    ),
    
  ),
)