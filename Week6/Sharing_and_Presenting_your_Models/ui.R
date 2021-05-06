#install.packages("shiny")
#library(shiny)
#library(markovchain)
#setwd("C:/Users/-/Downloads/memoryless_dashboard")


shinyUI(
  pageWithSidebar(
    
    headerPanel("Memoryless Bar Dashboard"),
    
    sidebarPanel(
      textInput("month_num", "Please Enter The Number Of Months", "1")

    ),
    
    mainPanel(plotOutput("dispPlot"))
  )
  
)


