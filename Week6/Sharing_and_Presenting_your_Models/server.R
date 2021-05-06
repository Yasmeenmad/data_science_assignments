shinyServer(
  
    function(input, output, session){
      prob <- matrix(c(0.95,0.05,0,0,
                       0.75,0.2,0.05,0,
                       0.2,0.55,0.2,0.05,
                       0.2, 0.55,0.2,0.05),
                     nrow = 4, byrow = TRUE)
      
      soda_batches <- new("markovchain", states = c('0','1','2','3'),
                          transitionMatrix = prob, name= 'Inventory')
    
    output$dispPlot <- renderPlot({
      
      month <- as.numeric(input$month_num)
      soda_plot   <- plot(soda_batches^month)
    })
  }
  
)


