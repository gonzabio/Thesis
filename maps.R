#Alexandra Gonzalez

#To see more spatial packages that are super important: https://cran.r-project.org/web/views/Spatial.html

#(1)Introduction
      #R is great for GIS analysis. GIS = Geographic information system

library(sp) #classes for spatial data 
#install.packages("raster") 
library(raster) #grids, rasters
#install.packages("rasterVis")
library(rasterVis) #raster visualisation 
library(maptools)
library(rgeos)

#(2)Generic Mapping 
      #Retrieving base maps from Google with gmap function in package dismo

#install.packages("dismo")
library(dismo)
?dismo #species distribution modeling. 
      vignette('sdm', 'dismo')

mymap <- gmap("Mexico")
plot(mymap)

?gmap #Get a google map
      #- retrieve a google map that can be used as a background for 
      #plotting points and other spatial data
mymap2 <- gmap("Mexico", type = "satellite")
plot(mymap2)

mymap <- gmap("Mexico", type = "satellite", exp = 1) #exp changes zoom. 10 = far away
plot(mymap)

      #save file: 
mymap <- gmap("Mexico", type = "satellite", filename = "Mexico_Practice_map.gmap")

mymap <- gmap("Europe")
plot(mymap)

select.area <- drawExtent()
mymap <- gmap(select.area)
plot(mymap)


#https://pakillo.github.io/R-GIS-tutorial/#spatial

  #got to look at the raser vignete
  
?vignette
vignette(package = 'rWBclimate')

