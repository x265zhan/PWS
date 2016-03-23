
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  mypws <- eventReactive(input$getpwsButton, {
    PWS::getPWS(lat = input$lat, lon = input$lon, zipcode = input$zipcode, city = input$city, state = ifelse(input$state == "", NA, input$state), country = input$country, distance = input$distance)
  })
  
  mycurrent <- eventReactive(input$getcurrentButton, {
    PWS::getPWS(lat = input$lat, lon = input$lon, zipcode = input$zipcode, city = input$city, state = ifelse(input$state == "", NA, input$state), country = input$country, distance = input$distance)
  })
  
  myweather <- eventReactive(input$getweatherButton, {
    PWS::getPWSweather(PWS::getPWS(lat = input$lat, lon = input$lon, zipcode = input$zipcode, city = input$city, state = ifelse(input$state == "", NA, input$state), country = input$country, distance = input$distance), start = input$starttime, end = input$endtime)
  })
  
  output$pwsPlot <- renderPlot({
    if (is.null(mypws())) return()
    PWS::plotPWS(mypws())
  })
  
  output$currentPlot <- renderPlot({
    if (is.null(mycurrent())) return()
    PWS::plotPWScurrent(mycurrent())
  })
  
  output$weatherPlot <- renderPlot({
    if(is.null(myweather())) return()
    PWS::plotPWSweather(myweather(), input$weather)
  })
})
