library(shiny)
library(ggplot2)
library(dplyr)
library(rsconnect)
# Select columns
diam <- diamonds[,c(1:4,7)]
# Server Logic
shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    # Select diamonds depending on  input
    diam <- filter(diamonds, grepl(input$cut, cut), grepl(input$col, color), grepl(input$clar, clarity))
    # Linear Model
    fit <- lm( price~carat, diam)
    # Price Prediction
    pred <- predict(fit, newdata = data.frame(carat = input$car,
                                              cut = input$cut,
                                              color = input$col,
                                              clarity = input$clar))
    # Plot
    plot <- ggplot(data=diam, aes(x=carat, y = price))+
      geom_point(aes(color = cut), alpha = 0.3)+
      geom_smooth(method = "lm")+
      geom_vline(xintercept = input$car, color = "yellow")+
      geom_hline(yintercept = pred, color = "blue")
    plot
  })
  output$result <- renderText({
    # Renders the text
    diam <- filter(diamonds, grepl(input$cut, cut), grepl(input$col, color), grepl(input$clar, clarity))
    fit <- lm( price~carat, diam)
    pred <- predict(fit, newdata = data.frame(carat = input$car,
                                              cut = input$cut,
                                              color = input$col,
                                              clarity = input$clar))
    res <- paste(round(pred, digits = 1.5),"$" )
    res
  })

})
