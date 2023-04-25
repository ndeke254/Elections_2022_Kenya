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
library(bslib)
library(shinydashboard)
library(shinyWidgets)
library(echarts4r)
library(shinycssloaders)
library(maps)
library(shinyBS)
library(shinydashboardPlus)
library(shiny.fluent)

#import data file
results <- fread(file = "data/results.csv")
totals <- fread(file = "data/totals.csv")

# append serial numbers
results$sno <- results$sno |> as.character()
results$sno <- case_when(nchar(results$sno) %in% 2
                         ~ paste("0", results$sno, sep = ""),
                         TRUE
                         ~ paste("00", results$sno, sep = ""))

# import the shape files
county_shp <- readOGR(dsn = "data/shp/county.shp",
                     layer = "county",
                     verbose = FALSE,
                     stringsAsFactors = FALSE)
# rename name column
colnames(county_shp@data)[colnames(county_shp@data) == "ADM1_EN"] <- "name"

# set zoom level for each county onclick
county_shp@data$zoom <- c(8.2, 8.7, 9.0, 9.0, 9.0, 9.2, 7.0, 9.0,
                          7.5, 7.5, 9.0, 9.3, 8.5, 7.6, 9.5, 10.4,
                          8.5, 7.5, 8.5, 8.0, 8.6, 9, 8.2, 7.7,
                          7.0, 9.2, 8.5, 11.0, 9.0, 10.0, 9.0, 8.0,
                          10.2, 9.2, 9.2, 8.2, 9, 7.6, 7.0, 9.0,
                          9.3, 7.0, 9.2, 10.0, 7.0, 8.0, 8.5)
# map colors
county_shp@data$col2022 <- c(results$col2022)

# transfer county codes
county_shp@data$code <- c(results$sno)

# loading spinner
loading <- function(x) {
  withSpinner(x, image = "choice.png",
              hide.ui = FALSE)
}
loading_2 <- function(x) {
  withSpinner(x, image = "name.gif",
              hide.ui = FALSE)
}
loading_1 <- function(x) {
  withSpinner(x, type = 8, size = 0.5,
              color = "#e6ffff",
              hide.ui = FALSE)
}
# graph colors
my_colors2022 <- c("blue", "yellow", "red", "green")