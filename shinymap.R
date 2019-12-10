#install.packages('rsconnect')
rsconnect::setAccountInfo(name='tianyizhao',
                          token='C8DA2F2F78A7CC4264F503968BCF8973',
                          secret='6W1+6NzXeEAAZ6XCnCj1G1OgEiCuhf9IdtCqV6db')



suppressWarnings(suppressPackageStartupMessages(require(shiny)))
suppressWarnings(suppressPackageStartupMessages(require(dplyr)))
suppressWarnings(suppressPackageStartupMessages(require(DT)))
suppressWarnings(suppressPackageStartupMessages(require(plotly)))
suppressWarnings(suppressPackageStartupMessages(require(geosphere)))
suppressWarnings(suppressPackageStartupMessages(require(leaflet)))
suppressWarnings(suppressPackageStartupMessages(require(RColorBrewer)))
suppressWarnings(suppressPackageStartupMessages(require(stringr)))
suppressWarnings(suppressPackageStartupMessages(require(parcoords)))
suppressWarnings(suppressPackageStartupMessages(require(ggplot2)))
suppressWarnings(suppressPackageStartupMessages(require(reshape2)))
suppressWarnings(suppressPackageStartupMessages(require(ggthemes)))
suppressWarnings(suppressPackageStartupMessages(require(formattable)))
suppressWarnings(suppressPackageStartupMessages(require(openxlsx)))

#setwd("~/Desktop/ALY6980/puma dataset/shiny_map")
TransactionHistory <- read.csv("TransactionHistory.csv",header = TRUE,stringsAsFactors = FALSE,sep="\t")
DimStore <- read.csv("DimStore.csv",header = TRUE,stringsAsFactors = FALSE,sep="\t")
us_zipcodes <- read.csv("us_zipcodes.csv",header = TRUE,stringsAsFactors = FALSE)
StoreRanking <- read.xlsx("StoreRanking.xlsx")
colnames(TransactionHistory)[1] <- "date"
colnames(us_zipcodes)[1] <- "type"
TransactionHistory <- dcast(data=TransactionHistory,InternationalStoreCode~Transaction.Type,value.var = "Transactions",sum)
us_zipcodes$ZipCode <- as.character(us_zipcodes$ZipCode)

data <- inner_join(DimStore,StoreRanking)
data <- inner_join(data,TransactionHistory)
data <- inner_join(data%>%select(-FTVolume,-APVolume,-ACVolume),us_zipcodes%>%select(ZipCode,Latitude,Longitude,StateFullName,City))
class(data) <- "data.frame"
dataget <- read.csv("final prob.csv",header = TRUE,stringsAsFactors = FALSE)
dataget$ZipCode <- as.character(dataget$ZipCode)
dataget <- inner_join(dataget,data)
tab1 <- tabPanel("",
                 tags$head(tags$style(
                   HTML(
                     "
                     .sidebar { height: 90vh; overflow-y: auto; }
                     .dataTables_wrapper { overflow-x: scroll; }
                     "
                   )
                   )),
                 tags$div(class= "tag",
                          selectInput("tag", label = "choice", choices = c("Channel","ZipCode","LocationType","DistrictName")),
                          uiOutput("a"),
                          DTOutput("table"),
                          #leaflet out 
                          leafletOutput("USAMap", width="100%", height= "300px"),
                          leafletOutput("USAMap_prob", width="100%", height= "300px")
                 ))


shinyApp(
  ui = shinyUI(navbarPage(title = strong("usaMap"),
                          tab1
  )),
  
  
  server = function(input, output, session) {
    output$a <- renderUI({
      switch (input$tag,
              'Channel' = selectInput("Channel", label = "Channel", choices = unique(data$Channel)),
              'ZipCode' = selectInput("ZipCode", label = "ZipCode", choices = unique(data$ZipCode)),
              'LocationType' = selectInput("LocationType", label = "LocationType", choices = unique(data$LocationType)),
              'DistrictName' = selectInput("DistrictName", label = "DistrictName", choices = unique(data$DistrictName)),
              
      ) 
    })
    
    
    
    
    output$table <- renderDT({
      switch (input$tag,
              'Channel' = data[data$Channel==input$Channel,],            
              'ZipCode' = data[data$ZipCode==input$ZipCode,],
              'LocationType' = data[data$LocationType==input$LocationType,],
              'DistrictName' = data[data$DistrictName==input$DistrictName,]
      )
    })
    
    Pass <- reactive({
      switch (input$tag,
              'Channel' = data[data$Channel==input$Channel,],            
              'ZipCode' = data[data$ZipCode==input$ZipCode,],
              'LocationType' = data[data$LocationType==input$LocationType,],
              'DistrictName' = data[data$DistrictName==input$DistrictName,]
      )
    })
    
    Pass1 <- reactive({
      switch (input$tag,
              'Channel' = dataget[dataget$Channel==input$Channel,],            
              'ZipCode' = dataget[dataget$ZipCode==input$ZipCode,],
              'LocationType' = dataget[dataget$LocationType==input$LocationType,],
              'DistrictName' = dataget[dataget$DistrictName==input$DistrictName,]
      )
    })
    
    
    
    
    
    output$USAMap <- renderLeaflet({
      Pass <- Pass()%>%select(Channel,ZipCode,LocationType,DistrictName,Rank)
      map = leaflet(Pass()) %>% setView(-98.35  , 39.48, zoom = 4) %>% 
        
        addProviderTiles(providers$Esri.WorldStreetMap)
      
      #load icon
      
      iconList = awesomeIconList(
        "A" = makeAwesomeIcon(icon = "home",markerColor = "green"),
        "AA" = makeAwesomeIcon(icon = "home",markerColor = "orange"),
        "AAA" = makeAwesomeIcon(icon = "home",markerColor = "red"),
        "B" = makeAwesomeIcon(icon = "home",markerColor = "purple"),
        "C" = makeAwesomeIcon(icon = "home",markerColor = "blue")
      )
      
      
      map <- map %>% addAwesomeMarkers(lng = ~Longitude, lat = ~Latitude, icon=~iconList[Rank], label =paste("Channel:",Pass$Channel,". ","ZipCode:",Pass$ZipCode,". ","LocationType:",Pass$LocationType,". ","Rank:",Pass$Rank) ,layerId = ~StateFullName) 
    })
    
    
    output$USAMap_prob <- renderLeaflet({
      Pass <- Pass1()%>%select(LR,RF,avg_prob,Rank)
      map = leaflet(Pass1()) %>% setView(-98.35 , 39.48, zoom = 4) %>% 
        
        addProviderTiles(providers$Esri.WorldStreetMap)
      
      
      #load icon
      
      iconList = awesomeIconList(
        "A" = makeAwesomeIcon(icon = "home",markerColor = "green"),
        "AA" = makeAwesomeIcon(icon = "home",markerColor = "orange"),
        "AAA" = makeAwesomeIcon(icon = "home",markerColor = "red"),
        "B" = makeAwesomeIcon(icon = "home",markerColor = "purple"),
        "C" = makeAwesomeIcon(icon = "home",markerColor = "blue")
      )
      
      
      map <- map %>% addAwesomeMarkers(lng = ~Longitude, lat = ~Latitude, icon=~iconList[Rank], label =paste("LR:",Pass$LR,",RF:",Pass$RF,",avgprob:",Pass$avg_prob,",Rank:",Pass$Rank) ,layerId = ~StateFullName) 
      
    })
  }
)


  
  
  