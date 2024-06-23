library(shiny)
library(leaflet)
library(leaflet.extras)
# library(sf)
library(dplyr)
library(maps)
library(ggplot2)
library(stringr)
library(tigris)
library(DT)

source("dataProcessing.R")

#### USER INTERFACE ####
ui <- navbarPage("US Election Forecast",id="main", theme = shinythemes::shinytheme("flatly"),
                 # position="fixed-top",
                 tabPanel("Map",
                          fluidRow(
                            # column(12, style = "position: relative;",
                                   leafletOutput("map", height = 1000),
                                   
                                   # absolutePanel(id = "controls", class = "panel panel-default",
                                   #               position=
                                   #               top = 75, left = 55, width = 250, fixed=TRUE,
                                   #               draggable = TRUE, height = "auto",
                                   #               
                                   #               h3(textOutput("sars_reactive_case_count"), align = "right"),
                                   #               h4(textOutput("sars_reactive_death_count"), align = "right"),
                                   #               h6(textOutput("sars_reactive_country_count"), align = "right"),
                                   #               plotOutput("sars_epi_curve", height="130px", width="100%"),
                                   #               plotOutput("sars_cumulative_plot", height="130px", width="100%"),
                                   #               span(("The final count appears to decrease as several cases initially classified as SARS were later re-assigned."),align = "left", style = "font-size:80%"),#tags$br(),
                                   #               span(("Circles show confirmed cases for COVID, SARS, and Ebola, and estimated deaths for H1N1."),align = "left", style = "font-size:80%"),
                                   #               
                                   # ), ## Copied code from an R gallery project. I may use some of it after changing.
                                   
                                   div(style = "position: absolute; top: 75px; width: 100%; display: flex; justify-content: center; align-items: center; z-index: 1000;",
                                       tags$img(src = "Trump.png", height = "100px"),
                                       tags$img(src = "election_results.png", height = "130px"),
                                       tags$img(src = "Biden.png", height = "100px")
                                   )
                            # )
                          )
                 ),
                 tabPanel("Election Results",DTOutput("table")),
                 tabPanel("About This Project",includeMarkdown("README.md"))
)

####


#### SERVER LOGIC ####

server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet(data = merged_data, #make the leaflet
            options = leafletOptions(zoomControl = FALSE, minZoom = 4, maxZoom = 7)) %>% #,dragging = FALSE
      # addTiles() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(zIndex = 1)) %>%  # Use CartoDB Positron without labels
      # addProviderTiles(providers$Stamen.Watercolor) %>%  # Use Stamen Toner tiles, alternative option
      # addProviderTiles(providers$Esri.WorldImagery) %>%  # Use Esri WorldStreetMap tiles, alternative option
      setView(lng = -96, lat = 40.8, zoom = 5) %>%  # Center the map on the contiguous US
      addPolygons(fillColor = ~fillColor, fillOpacity = 1, weight = 1, color = "black", # "#BDBDC3", Add the states!
        # label = ~state,  # Add simple hover information. I didn't like this so removed
        # labelOptions = labelOptions(style = list("color" = "black", "font-weight" = "bold"), # add the state label on click
        #                              textsize = "15px", direction = "auto"
        #                              ),
        popup = ~paste0("<strong>", NAME, # Add detailed popup information upon click
                        "</strong><br>", Electoral_College_Votes," Electoral Votes",
                        "</strong><br>Biden Win Chance: ", round(percentage * 100, 2), "%",
                        "<br>Trump Win Chance: ", 100-round(percentage * 100, 2), "%"),
        highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
        #   options = pathOptions(zIndex = 2),  # Ensure polygons have a higher z-index than the base tiles
        #   group = "Polygons" # these two lines were when I was trying to get the labels to go above the map. I didn't like how it looks.
        ) %>%
      addLegend(pal = palette, values = merged_data$percentage, title = "Percentage", position = "bottomright",
                labFormat = function(type, cuts, p) {  # Custom label formatting
                  paste0(c("Safe R (90-100%)","Likely R (75-90%)","Lean R (55-75%)","Toss-Up (45-55%)","Lean D (55-75%)","Likely D (75-90%)","Safe D (90-100%)"))
                },
                opacity = 1 # add a legend to the map
                )%>%
      addLabelOnlyMarkers(~longitude, ~latitude+0.2, label =  ~state, # add the state abbreviation labels
                          labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T,
                                                      style = list("color" = ~textColor,"font-weight" = "bold")
                                                      )
                          )%>%
      addLabelOnlyMarkers(~longitude, ~latitude-0.2, label = ~Electoral_College_Votes,  #~paste0(round(percentage,digits=4)*100,"%",sep=""),
                          labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T,
                                                      style = list("color" = ~textColor)
                                                      )
                          )
      #                     %>%
      # addOverlayImages(  ## this was a way to add an image to the map. I prefered adding it in the UI.
      #   imageUrl = "www/election_results.png",  # Path to your image
      #   bounds = list(c(30, -120), c(50, -70)),  # Adjust these coordinates to position the image correctly
      #   opacity = 0.5
      # )
                            
    
      # addMapPane("label", zIndex = 410) %>% # shown above everything ## This was how I added the labels above the election map.            
      # addProviderTiles(providers$CartoDB.PositronOnlyLabels,options =  pathOptions(pane = "label") )
    
  })
  
  output$table <- renderDT({
    datatable(merged_data, options = list(pageLength = 10, autoWidth = TRUE,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#f8f8f8', 'color': '#333'});",
        "}"
      )
    ))
  })
  
}


####

help("addLegend")


# Run the application
shinyApp(ui = ui, server = server)




