
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Personal Weather Station"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      numericInput("lat", "Latitude", value = NA),
      numericInput("lon", "Longitude", value = NA),
      numericInput("zipcode", "Zipcode", value = NA),
      textInput("city", "City", value = NA),
      textInput("state", "State", value = NA),
      textInput("country", "Country", value = NA),
      numericInput("distance", "Distance", 50),
      actionButton("getpwsButton", "Get Nearby PWS", style = "color: #ffffff; background-color: #66a3ff"),
      br(),
      br(),
      actionButton("getcurrentButton", "Get Current Weather", style = "color: #ffffff; background-color: #66a3ff"),
      br(),
      br(),
      selectInput("weather", "Weather Variable to Plot", c('Temperature', 'Humidity', 'Windspeed', 'Pressure')),
      textInput("starttime", "From: (HH:MM)", value = "1:00"),
      textInput("endtime", "To: (HH:MM)", value = "12:00"),
      actionButton("getweatherButton", "Plot Weather in Specified Time Range", style = "color: #ffffff; background-color: #66a3ff")
      ),

    # Show generated plots
    mainPanel(
      plotOutput("pwsPlot"),
      plotOutput("currentPlot"),
      plotOutput("weatherPlot")
    )
  )
))
