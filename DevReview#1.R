#Dev Review 
#Alexandra Gonzalez 

###(1) Background Graphs: 
library(sp) 
library(raster) 
library(rasterVis) 
library(maptools)
library(rgeos)
library(dismo)

#Retrieving base maps from Google with gmap function in package dismo

mymap <- gmap("Mexico")
plot(mymap)
select.area <- drawExtent()
plot(mymap)

##https://pakillo.github.io/R-GIS-tutorial/#getdata
#Downloading raster climate data from the internet 
tmin <- getData("worldclim", var = "tmin", res = 10)  # this will download 
tmin1 <- raster(paste(getwd(), "/wc10/tmin1.bil", sep = ""))  # Tmin for January
fromDisk(tmin1)
tmin1 <- tmin1/10  # Worldclim temperature data come in decimal degrees 
tmin1  # look at the info
 



#### (2) API Access - Normal data 
token <- "cf51b7524f618f5b23220e687b70c01d0d240cc82d179cf2c5b08b70fdfb83d4" 

library("rredlist")
#Importance of a token: 
no_token <- rl_search('Theloderma asperum') #leaves a data frame


##Searching for a specific info 

asperum <- rl_search('Theloderma asperum', key = token) #Dataframe, parse default = True 
asperum$result[[12]]
asperum2 <- rl_search('Theloderma asperum', key = token, parse = FALSE)  #List, parse = False 
asperum2


#Threat Info 
rl_threats(id = 59031, key = token) #with taxon id
scope <- rl_threats("Abies bracteata",key = token) #with name 

asperum_threats <- rl_threats("Theloderma asperum", key = token) #same as above
title_name <- asperum_threats$result[[2]]


### (3) Spatial Data - Waiting for API Access 
library(foreign)
getwd()
setwd("/Users/Alexandra_Gonzalez/Downloads/species_59031")
thelodera_data <- read.dbf("species_59031.dbf", as.is = FALSE)