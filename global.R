# load libraries
library(shiny)
library(data.table)
library(dplyr)
library(eeptools)
library(rgdal)
library(snakecase)
library(openxlsx)
library(leaflet)
library(shinyWidgets)


#import data file
results <- fread(file = 'data/results.csv')
totals <- fread(file = 'data/totals.csv')

# append serial numbers
results$sno <- results$sno |> as.character()
results$sno <- case_when(nchar(results$sno)==2
                         ~paste("0",results$sno,sep = ""),
                         TRUE
                         ~paste("00",results$sno,sep = ""))

# import the shape files
county_shp <-readOGR(dsn= "data/shp/county.shp",
                     layer = "county", 
                     verbose = FALSE, 
                     stringsAsFactors = FALSE)
