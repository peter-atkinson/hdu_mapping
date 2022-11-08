library(shiny); library(lubridate); library(DT); library(ggplot2); library(sf);
library(sp); library(cropgrowdays); library(RColorBrewer); library(dplyr); library(scales);
library(maptools); library(raster); library(sf); library(sp); library(rgeos); library(rgdal); library(devtools)
library(terra); library(rasterVis); library(tmap); library(tmaptools)



dseq <- seq(from = as.Date("01-01-2015", format = "%d-%m-%Y"), to = (as.Date(Sys.Date()-2, format = "%d-%m-%Y")), by = 1)
list <- readRDS("list")

postcodes.all <- readRDS("poa20152021max.RDS")
postcodes.all[nrow(postcodes.all)+(length(dseq) - nrow(postcodes.all)),] <- NA

currentmax.df <- readRDS("currentmax.RDS")
currentmax.df <- currentmax.df[nrow(currentmax.df),]

#postcodes.all <- data.frame(dseq, readRDS("poa20152021max.RDS"))

#column no. of capital cities
capital.codes <- c(35, 466, 671, 1365, 1798, 2139, 2523, 1)
capital.names <- c("Sydney", "Canberra", "Melbourne", "Brisbane", "Adelaide", "Perth", "Hobart", "Darwin")

capital.chdu <- currentmax.df[,((capital.codes))]
capital.chdu <- if_else(capital.chdu >= 130, 'On', 'Off')
preventatives <- if_else(capital.chdu=="On", "Preventatives required", "No")

