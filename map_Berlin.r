rm(list=ls())
#Get location of current script
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set working directory to script location
setwd(fileloc)

# Remove fileloc object
rm(fileloc)

# Set locale to English
Sys.setlocale("LC_ALL","English")

library(ggplot2)
library(corrplot)
library(dplyr)
library(leaflet)
#install.packages("leaflet")
library(htmlwidgets)
#install.packages("htmlwidgets")

### Loading the dataset 
airbnb <-  read.csv2("D:/AEEM/Cross-section and Panel data analysis/Project/berlin-airbnb-data/listings_clean_14_may.csv", sep= ",")
zip <- read.csv2("D:/AEEM/Cross-section and Panel data analysis/Project/berlin-airbnb-data/zipcoed_average.csv", sep=",")
zip<- as.data.frame(zip)
# Cleaning the data 
airbnb$log_price <- log(airbnb$price)  #adding a column in the data 

airbnb$name <- as.character(airbnb$name)
airbnb$host_location <- as.character(airbnb$host_location)
airbnb$host_name <- as.character(airbnb$host_name)
airbnb$last_review <- as.Date(airbnb$last_review)
airbnb$reviews_per_month <- as.numeric(airbnb$reviews_per_month)
airbnb$neighbourhood_group_cleansed <- as.character(airbnb$neighbourhood_group)
airbnb$cleaning_fee = as.character(airbnb$cleaning_fee)
airbnb$cleaning_fee = as.numeric(airbnb$cleaning_fee)
airbnb$bathrooms = as.numeric(airbnb$bathrooms)
airbnb$property_type<- as.character(airbnb$property_type)
airbnb$latitude <- as.character(airbnb$latitude)
airbnb$latitude <- as.numeric(airbnb$latitude)
airbnb$longitude <- as.character(airbnb$longitude)
airbnb$longitude <- as.numeric(airbnb$longitude)

airbnb$price2<- na.omit(airbnb$price2)  

#Divide prices into categories based on above frequencies
airbnb$price2 <- cut(airbnb$price, c(0,32,70,150,8000), c("Cheap", "Average", "Pricey", "Very Expensive"), include.lowest = TRUE, right = TRUE)

grouped <- airbnb %>%
  group_by(price2) %>%
  count(price2)
summary(airbnb$price2)

test <-  airbnb[1:500, ]
######################
#   Creating a map   #
######################
#install.packages("leaflet.extras")
library(leaflet.extras)
#Color Palette
pal <- colorFactor(palette = c("green", "yellow", "orange", "red"), 
                   levels = c("Cheap", "Average", "Pricey", "Very Expensive"))
library(scales)
#install.packages("scales")
map <- leaflet() %>%
  addTiles() %>%
  addProviderTiles(provider= "CartoDB")   %>% 
  setView(lng = 13.413215, lat =  52.521918, zoom = 12) %>% 
  #clearMarkers() %>% 
  addCircleMarkers(data= test, lng= test$longitude, lat = test$latitude, popup = test$price2, radius= 3, 
                   color = ~pal(price2)#,clusterOptions = markerClusterOptions())
  ) %>%
addLegend(pal = pal,  values = c( "Cheap","Average", "Pricey", "Very Expensive")) 

print(map)

airbnb$zipcode <- as.character(airbnb$zipcode)

#install.packages("sp")
library(sp)

print(map)

## Numeric colors
nc_pal <- colorNumeric("blue", domain = airbnb$price)


