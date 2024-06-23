
library(shiny)
library(leaflet)
library(leaflet.extras)
# library(sf)
library(dplyr)
library(maps)
library(ggplot2)
library(stringr)
library(tigris)




#### SETUP and DATA WRANGLING
# Load the US states map from the maps package
# states_map <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

electoral_college<-read.csv("../Electoral_College.csv")


# Load US states shapefile from tigris
options(tigris_use_cache = TRUE)  # Cache the data for faster loading in future
states_map <- st_as_sf(states(cb = TRUE, year = 2020))
# filter(STUSPS%in%electoral_college$Abb_State)


# Read in election data
election_data <- read.csv("Forecast Results.csv")%>%
  mutate(states=tolower(states))

# Read in the "state centroid" locations in latitude and longitude
state_labels<-read.csv("statecenters4.csv")%>% #TODO make these centers better!
  mutate(state=tolower(state))
election_data<-election_data%>%
  left_join(state_labels,by=c("states"="state"))


# Set up bins + colors for the forecast map
bins <- c(0, 0.1, 0.25, 0.45, 0.55, 0.75, 0.9, 1)
palette <- colorBin(palette = c("#A03232","#FF5864","#FF8B98","#C9C09B","#89AFFF","#577CCC","#244999"), 
                    domain = election_data$percentage, 
                    bins = bins)

# Function to determine if a color should be black or white. Doesn't work :(
getTextColor <- function(fillColor) {
  ifelse(col2rgb(fillColor)["red", ] * 0.299 +
           col2rgb(fillColor)["green", ] * 0.587 +
           col2rgb(fillColor)["blue", ] * 0.114 > 186,
         "black", "white")
}


# Merge shapefile and election data
merged_data <- states_map %>%
  left_join(election_data, by = c("STUSPS" = "st"))%>%
  mutate(state = paste(toupper(str_sub(STUSPS,1,1)),str_sub(STUSPS,2),sep=""),
         fillColor = palette(percentage),
         textColor = sapply(fillColor, getTextColor))%>%
  filter(percentage<5)

#add in count of electoral college votes by state
merged_data <- left_join(merged_data,electoral_college[,2:3],by=c("STUSPS"="Abb_State"))

## And finally select only the things I care about
merged_data<-merged_data%>%
  # select(-LSAD)
  select(NAME,state,Electoral_College_Votes,latitude,longitude,bidenwins,trumpwins,percentage,fillColor,textColor,geometry)







