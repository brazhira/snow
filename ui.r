library(shiny)
library(leaflet)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Мониторинг снега", titleWidth = 300),
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "shinychat.css")
    ),
    width = 300,
    tabsetPanel(
      tabPanel("Comments",
               fluidPage(
                 fluidRow( 
                   uiOutput("chat"),
                   textInput("entry", ""),
                   actionButton("send", "Send"),
                   actionButton("submit","Mark")
                 )
               )
      ),
      tabPanel("Photos",
               fluidPage(
                 fluidRow( 
                   uiOutput("photofeed"),
                   fileInput("file","File") 
                 )
               )
      )
    )
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel("Карта",
               leafletOutput("murmanskmap",height = "90vh")),
      tabPanel("Статистика",
               actionButton("stats", label = "Убрать снег"),
               actionButton("filldata", label = "Заполнить данными"),
               actionButton("route", label="Нарисовать путь"),
               dataTableOutput("table")
               ))
    
  )
)
