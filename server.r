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
#-------------------------------------------------------------------
num_marks <<- c() #global var
cur <<- 0
#Полигоны, которые затронуты машрутами: 3, 4, 5, 6, 9, 10, 11, 12
ids = c(1, 3, 4, 5, 6, 9, 10, 11, 12)

r1_longs = c(33.0715548989974, 33.0715870855056, 33.0696451661788, 33.0696880815231, 33.0726277826034)
r1_lats = c(68.9624429311109, 68.9625661767098, 68.9626624618544, 68.9629628688017, 68.9628396254213)

r2_longs = c(33.0754029750824, 33.0754137039185, 33.0753815174103, 33.0757248401642, 33.0760037899017, 33.0767977237701, 33.0768299102783, 33.0764973163605, 33.0755424499512)
r2_lats = c(68.9624313768006, 68.9625661767098, 68.9627625979585, 68.9631323273243, 68.9636946121769, 68.9641220931149, 68.9642568826819, 68.96428769161, 68.9639641957163)
#-----------------------creating polygons---------------------------
list <- c(1:17)

a_lats1 = c(68.9642145203354, 68.964210669209, 68.9645534168271, 68.9645380125541)
a_longs1 = c(33.0701172349654, 33.0696666238509, 33.0697846410476, 33.0701601503097)
lats_longs = Polygons(list(Polygon(cbind(a_longs1, a_lats1))), 'a1')

a_lats2 = c(68.9645380125541, 68.9629320580212, 68.9628781390517, 68.9645380125541)
a_longs2 = c(33.0728852746688, 33.0724453923904, 33.0732071397506, 33.0736041066848)
lats_longs = c(lats_longs, Polygons(list(Polygon(cbind(a_longs2, a_lats2))), 'a2'))

a_lats3 = c(68.9630707061941, 68.962870436331, 68.962735638283, 68.9629012471976)
a_longs3 = c(33.0694842336379, 33.0729067323409, 33.0728745458328, 33.0694520471298)
lats_longs = c(lats_longs, Polygons(list(Polygon(cbind(a_longs3, a_lats3))), 'a3'))

a_lats4 = c(68.9628896931277,68.9628627336076,68.9626894216196,68.9627009757945)
a_longs4 = c(33.0694735048019,33.0699670312606,33.0699241159164,33.0694735048019)
lats_longs = c(lats_longs, Polygons(list(Polygon(cbind(a_longs4, a_lats4))), 'a4'))

a_lats5 = c(68.9626855702266,68.9626393534581,68.9624621882812,68.9625507710477)
a_longs5 = c(33.0695271489822,33.0714261529647,33.0713725087844,33.069494962474)
lats_longs = c(lats_longs, Polygons(list(Polygon(cbind(a_longs5, a_lats5))), 'a5'))

a_lats6 = c(68.9626123936317,68.9625969880019,68.9624275253625,68.9624313768006)
a_longs6 = c(33.0714261529647,33.0717587468826,33.0717158315383,33.0714046952926)
lats_longs = c(lats_longs, Polygons(list(Polygon(cbind(a_longs6, a_lats6))), 'a6'))

a_lats7 = c(68.9627394896672,68.9627163813517,68.9624390796748,68.9624544854151)
a_longs7 = c(33.0723917482101,33.0730462072097,33.0728638169967,33.0723273751937)
lats_longs = c(lats_longs, Polygons(list(Polygon(cbind(a_longs7, a_lats7))), 'a7'))

d_lats1 = c(68.9628762133718,68.9627510438159,68.9626682389497,68.9627548951974)
d_longs1 = c(33.0728380618052,33.0728004919997,33.075339137429,33.0753176689688)
lats_longs = c(lats_longs, Polygons(list(Polygon(cbind(d_longs1, d_lats1))), 'd1'))

d_lats2 = c(68.9627548951974,68.9627394896672,68.9631207733755,68.9636715048629,68.9636984633935,68.9631361786392)
d_longs2 = c(33.0752743726459,33.0755426983621,33.0759612864794,33.0761759470524,33.075821757107,33.0755641644194)
lats_longs = c(lats_longs, Polygons(list(Polygon(cbind(d_longs2, d_lats2))), 'd2'))

d_lats3 = c(68.9626894216196,68.9626778674385,68.962408268162,68.962400565277)
d_longs3 = c(33.0752529065886,33.0755963635054,33.0755534313907,33.0752207075026)
lats_longs = c(lats_longs, Polygons(list(Polygon(cbind(d_longs3, d_lats3))), 'd3'))

d_lats4 = c(68.9636907609596,68.9636715048629,68.9641028373951,68.964268436035,68.9642145203354,68.9641336465386)
d_longs4 = c(33.0759074164731,33.0761757722268,33.0770237764082,33.0769486367972,33.0767339521943,33.0767661548848)
lats_longs = c(lats_longs, Polygons(list(Polygon(cbind(d_longs4, d_lats4))), 'd4'))

d_lats5 = c(68.9639796003903,68.9639141304512,68.9642453293228,68.964210669209,68.964279989382,68.9643185004951)
d_longs5 = c(33.075392435961,33.075607096534,33.0765408700264,33.0767340645421,33.0768950599718,33.0764872048832)
lats_longs = c(lats_longs, Polygons(list(Polygon(cbind(d_longs5, d_lats5))), 'd5'))

d_lats6 = c(68.964268436035,68.9643030960579,68.964545714692,68.9645495657599)
d_longs6 = c(33.0775819563758,33.0772169925508,33.0773350690824,33.0777644382883)
lats_longs = c(lats_longs, Polygons(list(Polygon(cbind(d_longs6, d_lats6))), 'd6'))

d_lats7 = c(68.9625777309494,68.9625584738801,68.9623389421006,68.9623389421006)
d_longs7 = c(33.0779039832801,33.0781616048036,33.078118667883,33.0778717805897)
lats_longs = c(lats_longs, Polygons(list(Polygon(cbind(d_longs7, d_lats7))), 'd7'))

d_lats8 = c(68.9626277992508,68.9625777309494,68.9625315139548,68.9626200964426)
d_longs8 = c(33.0779361859706,33.0779254517404,33.0789881405248,33.0789559378344)
lats_longs = c(lats_longs, Polygons(list(Polygon(cbind(d_longs8, d_lats8))), 'd8'))

d_lats9 = c(68.9626470562596,68.9626624618544,68.9635790753647,68.9643955225192,68.9642453293228,68.9635290092246)
d_longs9 = c(33.0782152759543,33.0779898571213,33.0783118840256,33.0791813566674,33.0793316358894,33.0786017082396)
lats_longs = c(lats_longs, Polygons(list(Polygon(cbind(d_longs9, d_lats9))), 'd9'))

SpP = SpatialPolygons(lats_longs, 1:length(lats_longs))

SpDf <<- SpatialPolygonsDataFrame(SpP, data=data.frame(ID=c(1:length(lats_longs))), match.ID = F)
#-----------------------end: creating polygons-----------------------

server <- function(input, output, session) {
  vars <- reactiveValues(chat = NULL, photos = NULL)
  #comments
  # Listen for input$send changes (i.e. when the button is clicked)
  #observe({
    #if(input$send < 1){
      #return()
    #}
    #isolate({
      # Add the current entry to the chat log.
      #if (input$entry!=""){
        #vars$chat <<- c(vars$chat, tagList(input$entry))}
    #})
    # Clear out the text entry field.
    #updateTextInput(session, "entry", value="")
  #})
  
  
  # Save the chat object
  #saveRDS(vars$chat, "chat.Rds")
  # Pass the chat log through as HTML
  #lapply(1:length(vars$chat), function(i) {
  #tags$div(class="well",HTML(vars$chat[[i]]))
  #})
  #})
  
  #photos
  output$photofeed <- renderUI({
    
    # # Save the photo object
    # #saveRDS(vars$photos, "photos.Rds")
    # a <- dbGetQuery(con, "SELECT photo_path FROM photos;")
    # print(a)
    # print(length(a$photo_path))
    # print(length(a$photo_path))
    # if (length(a$photo_path) > 0) {
    #   lapply(1:length(a$photo_path), function(i){
    #     tags$div(class="well",tags$img(src=a$photo_path[i],height="100%",width="100%"))
    #   })
    # }
  })
  observeEvent(input$file,{
    isolate({
      inFile <- input$file
      if (is.null(inFile))
        return()
      #random <- sample(0:65000, 1)
      a <- dbGetQuery(con, "SELECT photo_path FROM photos;")
      a <- length(a$photo_path)
      print(paste("Число: ", a))
     
      file_name <- paste0("image",a+1,".png")
      print(paste0("INSERT INTO photos VALUES (",a+1,",", cur,", '",file_name,"');"))
      file.copy(inFile$datapath, file.path("C:/Users/Николай/Desktop/Учеба/хакатон201119/Работа с полигонами/merge/www", file_name))
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
    output$murmanskmap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("Esri.WorldStreetMap", group = "Esri.WorldStreetMap") %>%
        setView(lng=33.07402833379321, lat=68.963424802011, zoom = 17) %>%
        addPolygons(data=SpDf, layerId = SpDf$ID, fillColor = ~binpal(num_marks), color="white", fillOpacity = 0.4, opacity = 0.6, weight = 2, label = as.character(num_marks[SpDf$ID]),
                    highlight=highlightOptions(weight = 5, color="#666", fillOpacity = 0.7, bringToFront = FALSE)) 
    })
    
    showNotification("Снег убран!")
  })
  
  observeEvent(input$filldata, {
    for (i in 1:length(num_marks)) {
      temp <- sample(0:30, 1)
      num_marks[i] <<- temp
      dbSendQuery(con, paste0("UPDATE polygon SET num_marks =", temp, "WHERE polygon_id = ", i, ";"))
    }
    output$murmanskmap <- renderLeaflet({
      binpal <- colorBin("Reds", num_marks, 4, pretty = FALSE)
      leaflet() %>%
        addProviderTiles("Esri.WorldStreetMap", group = "Esri.WorldStreetMap") %>%
        setView(lng=33.07402833379321, lat=68.963424802011, zoom = 17) %>%
        addPolygons(data=SpDf, layerId = SpDf$ID, fillColor = ~binpal(num_marks), color="white", fillOpacity = 0.4, opacity = 0.6, weight = 2, label = as.character(num_marks[SpDf$ID]),
                    highlight=highlightOptions(weight = 5, color="#666", fillOpacity = 0.7, bringToFront = FALSE)) 
    })
    showNotification("Данные внесены!")
  })
  
  observeEvent(input$submit, {
    query <- paste0("UPDATE polygon SET num_marks = num_marks+1 WHERE polygon_id=", cur,";")
    dbSendQuery(con, query)
    num_marks[cur] <<- num_marks[cur] + 1
    
    output$murmanskmap <- renderLeaflet({
      binpal <- colorBin("Reds", num_marks, 4, pretty = FALSE)
      leaflet() %>%
        addProviderTiles("Esri.WorldStreetMap", group = "Esri.WorldStreetMap") %>%
        setView(lng=33.07402833379321, lat=68.963424802011, zoom = 17) %>%
        addPolygons(data=SpDf, layerId = SpDf$ID, fillColor = ~binpal(num_marks), color="white", fillOpacity = 0.4, opacity = 0.6, weight = 2, label = as.character(num_marks[SpDf$ID]),
                    highlight=highlightOptions(weight = 5, color="#666", fillOpacity = 0.7, bringToFront = FALSE))  
    })
  })
  
  
  
  
  #------------------first map init-------------------
  output$murmanskmap <- renderLeaflet({
    query <- paste0("SELECT num_marks FROM polygon ORDER BY polygon_id;")
    a = dbGetQuery(con, query)
    a <- as.data.frame(a)
    
    for (i in 1:length(a$num_marks)) {
      num_marks <<- c(num_marks, a$num_marks[i])
    }
    binpal <- colorBin("Reds", num_marks, 4, pretty = FALSE)
    leaflet() %>%
      addProviderTiles("Esri.WorldStreetMap", group = "Esri.WorldStreetMap") %>%
      setView(lng=33.07402833379321, lat=68.963424802011, zoom = 17) %>%
      addPolygons(data=SpDf, layerId = SpDf$ID, fillColor = ~binpal(num_marks), color="white", fillOpacity = 0.4, opacity = 0.6, weight = 2, label = as.character(a$num_marks[SpDf$ID]),
                  highlight=highlightOptions(weight = 5, color="#666", fillOpacity = 0.7, bringToFront = FALSE))
  })
  #----------------------end: first map init----------
  
  observeEvent(input$route, {
    output$murmanskmap <- renderLeaflet({
      binpal <- colorBin("Reds", num_marks, 4, pretty = FALSE)
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
  
  
  observeEvent(input$murmanskmap_shape_click, {
    p <- input$murmanskmap_shape_click
    cur <<- p$id
    
    output$chat <- renderUI({
      query <- paste0("SELECT comment FROM comments WHERE polygon_id=", cur, ";")
      result <- dbGetQuery(con, query)
      #print(length(result$comments))
      sas <- dbGetQuery(con, paste0("SELECT * FROM comments WHERE polygon_id=", cur, ";"))
      print(length(sas$photo_path))
      #print((result['comment']))
      if (length(sas$photo_path) != 0){
        lapply (1:length(result[[1]]), function(i) {
          
          tags$div(class="well",HTML(result[[1]][i]))
        })
        }
       else {
        tags$div(class="well", HTML("Комментариев нет!"))
       }}
    )
    
    output$photofeed <- renderUI({
      
      # Save the photo object
      #saveRDS(vars$photos, "photos.Rds")
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