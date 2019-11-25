<<<<<<< HEAD
library(shiny)
library(leaflet)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Мониторинга снега", titleWidth = 300),
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
=======
library(shiny)
library(leaflet)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Мониторинга снега", titleWidth = 300),
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
>>>>>>> d50e671a5ac45d56f0a3f2d466f228a88ad763dd
