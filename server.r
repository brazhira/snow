library(shiny)
library(leaflet)
library(DBI)
library(sp)
library(shinydashboard)

#----------------database connnect variable-------------------------
con <- dbConnect( RPostgres::Postgres(),
                  dbname = 'snow',    
                  host = 'localhost',
                  port = 5432,
                  user = 'postgres',
                  password = '1234567890');
#----------------------global variables-----------------------------
num_marks <<- c() #list of polygons id
cur <<- 0
v_count <<- 0 #quantity of polygon
#-------------------------creating polylines------------------------
#Полигоны, которые затронуты машрутами: 1, 3, 4, 5, 6, 9, 10, 11, 12
ids = c(1, 3, 4, 5, 6, 9, 10, 11, 12)

r1_longs = c(33.0715548989974, 33.0715870855056, 33.0696451661788, 33.0696880815231, 33.0726277826034)
r1_lats = c(68.9624429311109, 68.9625661767098, 68.9626624618544, 68.9629628688017, 68.9628396254213)

r2_longs = c(33.0754029750824, 33.0754137039185, 33.0753815174103, 33.0757248401642, 33.0760037899017, 33.0767977237701, 33.0768299102783, 33.0764973163605, 33.0755424499512)
r2_lats = c(68.9624313768006, 68.9625661767098, 68.9627625979585, 68.9631323273243, 68.9636946121769, 68.9641220931149, 68.9642568826819, 68.96428769161, 68.9639641957163)
#---------------------end: creating polylines------------------------

#-----------------------creating polygons----------------------------
con1 <- file("pol_input.txt", "r")
vector <- c()
while(TRUE) {
  lon <- ""
  lat <- ""
  line <- paste(readLines(con1, n=1))
  if (length(line) == 0) {
    break
  }
  temp <- unlist(strsplit(line, " "))
  for (i in 1:length(temp)){
    if (i%%2==1) {
      lon <- paste(lon, temp[i])
    }
    else {
      lat <- paste(lat, as.double(temp[i]))
    }
  }
  lat <- sub(","," ",sub(",","",sub(" ", ",", lat)))
  lon <- sub(","," ",sub(",","",sub(" ", ",", lon)))
  
  lon <- strsplit(lon, " ")
  lon <- as.numeric(lon[[1]])
  lat <- strsplit(lat, " ")
  lat <- as.numeric(lat[[1]])
  
  vector<- c(vector, Polygons(list(Polygon(cbind(lat, lon))), toString(v_count+1)))
  v_count <<- v_count+1
}
SpDf <<- SpatialPolygonsDataFrame(SpatialPolygons(vector, 1:v_count), data=data.frame(ID = list(1:v_count)[[1]]), match.ID = F)
#-----------------------end: creating polygons-----------------------


server <- function(input, output, session) {
  vars <- reactiveValues(chat = NULL, photos = NULL)
  binpal <<- colorBin("Reds", num_marks, 4, pretty = FALSE)
  
  #updating map
  update_map <- function(){
    leafletProxy("murmanskmap") %>% clearShapes() %>% 
      addPolygons(data=SpDf, layerId = SpDf$ID, fillColor = ~binpal(num_marks), color="white", fillOpacity = 0.4, opacity = 0.6, weight = 2, label = as.character(num_marks[SpDf$ID]),
                                                                       highlight=highlightOptions(weight = 5, color="#666", fillOpacity = 0.7, bringToFront = FALSE))
  }
  
  
  #uploading file
  observeEvent(input$file,{
    isolate({
      inFile <- input$file
      if (is.null(inFile))
        return()
      a <- dbGetQuery(con, "SELECT photo_path FROM photos;")
      a <- length(a$photo_path)
      print(paste("Число: ", a))
      file_name <- paste0("image",a+1,".png")
      print(paste0("INSERT INTO photos VALUES (",a+1,",", cur,", '",file_name,"');"))
      file.copy(inFile$datapath, file.path("/www", file_name))
      dbSendQuery(con, paste0("INSERT INTO photos VALUES (",a+1,",", cur,", '",file_name,"');"))
    })
    
    output$photofeed <- renderUI({
      a <- dbGetQuery(con,paste0("SELECT photo_path FROM photos WHERE polygon_id=", cur))
      print(a)
      print(length(a$photo_path))
      print(length(a$photo_path))
      if (length(a$photo_path) > 0) {
        lapply(1:length(a$photo_path), function(i){
          tags$div(class="well",tags$img(src=a$photo_path[i],height="100%",width="100%"))
        })
      }
    })
  })
  
  #uploading comment
  observeEvent(input$send, {
    temp <- input$entry
    query <- paste0("INSERT INTO comments(polygon_id, comment, photo_path) VALUES (", cur,",'", temp,"','');")
    dbSendQuery(con, query)
    output$chat <- renderUI({
      query <- paste0("SELECT comment FROM comments WHERE polygon_id=", cur, ";")
      result <- dbGetQuery(con, query)
      lapply (1:length(result[[1]]), function(i) {
        print(result[[1]][i])
        tags$div(class="well",HTML(result[[1]][i]))
      })
    })
    updateTextInput(session, "entry", value = paste(""))
    showNotification("Комментарий отправлен!")
  })
  
  #generate stat table
  observeEvent(input$stats, {
    tabledata <- reactiveVal()
    for (id in ids) {
      dbGetQuery(con, paste0("UPDATE polygon SET num_marks = 0 WHERE polygon_id = ", id))
      dbGetQuery(con, paste0("DELETE FROM comments WHERE polygon_id = ", id, ";"))
      num_marks[id] <<- 0
    } 
    d <- dbGetQuery(con, "select address, sum(num_marks) as sum_marks from polygon 
                    where num_marks>0 group by address order by sum_marks desc;")
    d <- as.data.frame(d)
    colnames(d)[1] <- "Адрес"
    colnames(d)[2] <- "Количество меток"
    tabledata(d)
    output$table <- renderDataTable(tabledata())
    binpal <- colorBin("Reds", num_marks, 4, pretty = FALSE)
    update_map()
    showNotification("Снег убран!")
  })
  
  
  #editing polygon's mark info
  observeEvent(input$filldata, {
    for (i in 1:length(num_marks)) {
      temp <- sample(0:30, 1)
      num_marks[i] <<- temp
      dbSendQuery(con, paste0("UPDATE polygon SET num_marks =", temp, "WHERE polygon_id = ", i, ";"))
    }
    update_map()
    showNotification("Данные внесены!")
  })
  
  #uploading mark
  observeEvent(input$submit, {
    query <- paste0("UPDATE polygon SET num_marks = num_marks+1 WHERE polygon_id=", cur,";")
    dbSendQuery(con, query)
    num_marks[cur] <<- num_marks[cur] + 1
    update2_map()
  })
  
  #------------------first map init-------------------
  output$murmanskmap <- renderLeaflet({
    query <- paste0("SELECT num_marks FROM polygon ORDER BY polygon_id;")
    a = dbGetQuery(con, query)
    a <- as.data.frame(a)
    for (i in 1:length(a$num_marks)) {
      num_marks <<- c(num_marks, a$num_marks[i])
    }
    leaflet() %>%
      addProviderTiles("Esri.WorldStreetMap", group = "Esri.WorldStreetMap") %>%
      setView(lng=33.07402833379321, lat=68.963424802011, zoom = 17) %>%
      addPolygons(data=SpDf, layerId = SpDf$ID, fillColor = ~binpal(num_marks), color="white", fillOpacity = 0.4, opacity = 0.6, weight = 2, label = as.character(a$num_marks[SpDf$ID]),
                  highlight=highlightOptions(weight = 5, color="#666", fillOpacity = 0.7, bringToFront = FALSE))
  })
  #----------------------end: first map init----------
  
  
  #drawing route
  observeEvent(input$route, {
    output$murmanskmap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("Esri.WorldStreetMap", group = "Esri.WorldStreetMap") %>%
        setView(lng=33.07402833379321, lat=68.963424802011, zoom = 17) %>%
        addPolygons(data=SpDf, layerId = SpDf$ID, fillColor = ~binpal(num_marks), color="white", fillOpacity = 0.4, opacity = 0.6, weight = 2, label = as.character(num_marks[SpDf$ID]),
                    highlight=highlightOptions(weight = 5, color="#666", fillOpacity = 0.7, bringToFront = FALSE))  %>%
        addPolylines(lng=r1_longs, lat=r1_lats, color = 'green', opacity = 0.6, weight = 7) %>%
        addPolylines(lng=r2_longs, lat=r2_lats, color = 'green', opacity = 0.6, weight = 7)
    })
    showNotification("Путь построен!")
  })
  
  #polygon click event
  observeEvent(input$murmanskmap_shape_click, {
    cur <<- input$murmanskmap_shape_click$id
    output$chat <- renderUI({
      result <- dbGetQuery(con, paste0("SELECT comment FROM comments WHERE polygon_id=", cur, ";"))
      sas <- dbGetQuery(con, paste0("SELECT * FROM comments WHERE polygon_id=", cur, ";"))
      print(length(sas$photo_path))
      if (length(sas$photo_path) != 0){
        lapply (1:length(result[[1]]), function(i) {
          tags$div(class="well",HTML(result[[1]][i]))
        })
        } else {
        tags$div(class="well", HTML("Комментариев нет!"))
       }}
    )
    
    output$photofeed <- renderUI({
      a <- dbGetQuery(con,paste0("SELECT photo_path FROM photos WHERE polygon_id=", cur))
      print(a)
      print(length(a$photo_path))
      print(length(a$photo_path))
      if (length(a$photo_path) > 0) {
        lapply(1:length(a$photo_path), function(i){
          tags$div(class="well",tags$img(src=a$photo_path[i],height="100%",width="100%"))
        })
      }
    })
  })
}