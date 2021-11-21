library(shiny)
library(datasets)
Data <- mtcars
Data$am <- factor(Data$am, labels = c("Automatic", "Manual"))

shinyServer(function(input, output) {
  
  formulaText <- reactive({
    paste("mpg ~", input$variable)
  })
  
  formulaPoint <- reactive({
    paste("mpg ~", "as.integer(", input$variable, ")")
  })
  
  fit <- reactive({
    lm(as.formula(formulaPoint()), data=Data)
  })
  
  output$caption <- renderText({
    formulaText()
  })
  
  output$BoxPlot <- renderPlot({
    boxplot(as.formula(formulaText()), 
            data = Data,
            outline = input$outliers)
  })
  
  output$fit <- renderPrint({
    summary(fit())
  })
  
  output$Plot <- renderPlot({
    with(Data, {
      plot(as.formula(formulaPoint()))
      abline(fit(), col=2)
    })
  })
  
})
