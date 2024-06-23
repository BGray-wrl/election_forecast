library(shiny)
library(dplyr)
library(leaflet)
library(stringr)


shinyServer(function(input, output) {
  
  # Wrangling & constructing my dataset
  # world_energy<-read.csv("world-energy.csv")
  # materials_data <- world_energy%>%
  #   filter(str_detect(Var, "cobalt")|str_detect(Var, "lith")|str_detect(Var, "graph")|str_detect(Var, "rare"))%>%
  #   filter(Year>=2022)%>%
  #   filter(!str_detect(Var,"res"))%>%
  #   mutate(rad = 3+4*sqrt(Value),
  #          Value = round(Value,2))
  # colnames(materials_data)[3] = "alpha3"
  # 
  # # read alpha 3 lat/long data to add to materials data & place on map
  # code_coords<-read.csv("code_coords.csv")
  # colnames(code_coords)[3] = "alpha3"
  # code_coords<-mutate(code_coords, alpha3=trimws(alpha3))
  # materials_data<-left_join(materials_data, code_coords, by = "alpha3")
  # 
  # colnames(materials_data)[17] = "Latitude"
  # colnames(materials_data)[18] = "Longitude"
  # # materials_data$Longitude <- jitter(materials_data$Longitude, amount = 2)  # Adjust amount as needed
  # # materials_data$Latitude <- jitter(materials_data$Latitude, amount = 2)
  # 
  # materials_data=filter(materials_data, Latitude != "NA") # removing NA values
  # 
  # # creating the content label
  # materials_data <- mutate(materials_data, cntnt=paste0('<strong>Country: </strong>',Country.x,
  #                                                       '<br><strong>Variable:</strong> ', Var,
  #                                                       '<br><strong>Amount:</strong> ', Value
  # ))
  # 
  # # making the datatable reactive and able to change given an input
  # materialsData <- reactive({
  #   if (is.null(input$categoryFilter)) {
  #     return(materials_data)
  #   }
  #   materials_data[materials_data$Var %in% input$categoryFilter, ]
  # })
  
  
  # creating a color palatte 
  # mypal <- colorFactor(pal = c( "#0047ab", "#41424C","#d31940",'#665647' ), domain = c("cobalt_kt", "graphite_kt", "lithium_kt","rareearths_kt"))
  
  # create the leaflet map  
  output$matmap <- renderLeaflet({
    # leaflet(materials_data) %>% 
    #   # addCircles(lng = ~Longitude, lat = ~Latitude) %>%
    #   addTiles() %>%
    #   addCircleMarkers(data = materialsData(), lat =  ~Latitude, lng =~Longitude, 
    #                    radius = ~rad, popup = ~as.character(cntnt),
    #                    color = ~mypal(Var),
    #                    stroke = FALSE, fillOpacity = 0.8)%>%
    #   addLegend(pal=mypal, values=materials_data$Var,opacity=1, na.label = "Not Available")%>%
    #   addEasyButton(easyButton(
    #     icon="fa-crosshairs", title="ME",
    #     onClick=JS("function(btn, map){ map.locate({setView: true}); }"))
    #   )
  })
  
  
})



