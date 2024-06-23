library(shiny)
library(leaflet)
library(dplyr)
library(stringr)


world_energy<-read.csv("world-energy.csv")
# materials_data <- world_energy%>%
#   filter(str_detect(Var, "cobalt")|str_detect(Var, "lith")|str_detect(Var, "graph")|str_detect(Var, "rare"))%>%
#   filter(Year>=2022)%>%
#   filter(!str_detect(Var,"res"))%>%
#   mutate(rad = 3+4*sqrt(Value),
#          Value = round(Value,2))

navbarPage("Location of Rare Earth Materials", id="main",
           tabPanel("Map",
                    # fluidRow(
                    #   column(2,selectInput("categoryFilter", "Select a Material:",
                    #                        choices = unique(materials_data$Var), selected = unique(materials_data$Var)[1])
                    #   )
                    # ),
                    fluidRow(
                      leafletOutput("matmap", height=1000)
                    )
           )
           # tabPanel("Read Me",includeMarkdown("readme.md"))
           )
