#Leaflet Intro/Practice & The Map Widget 

library("leaflet")
########
########
########Making Custom Markers 
########Adding Multiple Popups 

#Me trying to make a map based on this example youtube:
# https://www.youtube.com/watch?v=kqzTsxV_V1w

GonzalezIcon <- makeIcon(
  iconUrl = "https://s-media-cache-ak0.pinimg.com/originals/49/b1/0f/49b10f78b1c3f532cf5313e25419e71a.jpg",
  iconWidth = 31 * 215/230, iconHeight = 31,
  iconAnchorX = 31*215/230/2, iconAnchorY = 16
  )
              #Question: iconWidth, iconAnchorX (mess with the heights/widths) 

#DF with all of the coordinates: 
Alexandra_df <- data.frame(
  lat = c( 33.9024, 33.8121),
  lng = c(-118.1334, -117.9190)
)

#Messing with the labels of the coordinates: so far the labels are random
popupdf <- c("HEY", "BAY", "NAY")


Alexandra_df %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(icon = GonzalezIcon, popup = popupdf)

################
####### Mapping Clusters


#Sometimes you ight have so many points on a map that 
#it doesnt make sense to plot every marker. In these situations 
#leaflet allows you to plot clusters of markers using some ish
#when you zoom in on each cluster - the clusters will seperate 
# until you see the individual markers. 

addMarkers(clusterOptions = markerClusterOptions())

df <- data.frame( lat = runif (500, min = 39.25, max = 39.35),
                  lng = runif (500, min = -76.65, max = -76.55))
df %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())

##Mapping Circle Markers 

#Instead of adding markers or clusters you can easily add circle 
#markers using 'addCircleMarkers()' 

df2 <- data.frame (lat = runif(20, min = 39.25, max = 39.35),
                   lng = runif(20, min = -76.65, max = -76.55))
df2 %>% 
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers()
    #basically small blue circles

### Drawing Circles
# You can draw arbitrary shapes on the maps you create, 
# including circles and squares. the code below draws a map
# where the circle on each city is proportional to the populaiton
# of that city. 

#Could be useful for doing circles that represent how many criically engangered 
#speices are in a region. 

md_cities <- data.frame (name = c("Baltimore", "Frederick", "Rockville",
                                  "Gaithersburg", "Bowie", "Hagerstown",
                                  "Annapolis", "College Park", "Salisbury",
                                  "Laurel"),
                         pop = c(618493, 66169, 6234, 6105, 55232,
                                 39890, 38880, 30587, 30484, 25346),
                         lat = c(39.2920592, 39.4143921, 39.0840, 39.1434, 39.0068,
                                 39.6418, 38.9784, 38.9897, 38.3607, 39.0993),
                         lng = c(-76.6077852, -77.4204875, -77.1528, -77.2014, -76.7791,
                                 -77.7200, -76.4922, -76.9378, -75.5994, -76.8483))
                        #Pop  = the population in the area 
                        #lat/lng = the coordinates of a place 
                      
                         
md_cities %>%
  leaflet() %>% 
  addTiles() %>% 
  addCircles(weight = 1, radius = sqrt(md_cities$pop) * 30) 
  #weight is how thick the radius of the circle is. weight = 10 means 
  #a heavier outline to the circle. 

## Drawing Rectangles 
leaflet () %>% 
    addTiles() %>% 
    addRectangles( lat = 37.3858, lng1 = -122.0595, 
                   lat2 = 37.3890, lng2 = -122.0625)
df <- data.frame (lat = runif (20, min = 39.25, max = 39.35),
                  lng = runif (20, min = -76.65, max = -76.55),
                  col = sample (c("red", "blue", "green"), 20, replace = TRUE),
                  stringsAsFactors = FALSE)

### Adding a Legend 
df %>% 
  leaflet() %>% 
  addTiles() %>%
  addCircleMarkers(color = df$col) %>% 
  addLegend(labels = LETTERS[1:3], colors = c("blue", "red", "green"))


#LetsR Function?

data(PAM) # Phyllomedusa presence-absence matrix
require(maptools)
data(wrld_simpl) # World map
Brazil <- wrld_simpl[wrld_simpl$NAME == "Brazil", ] # Brazil (polygon)
# Check where is the variable name
# (in this case it is in "NAME" which will be my z value)
names(Brazil)


#CANT LOAD DATA IN (From IUCN spatial amphibain section)
#From: R Functions - A place to share and lear about R. Mainly destined to ecologists 
#Following code by Jose Hidasi 

#First, install and load 'sp' and 'rdgal' packages 
require("sp")
library(sp)
library(rgdal)

data <- readOGR("C:/Herpetology", "ANURA.shp")

#Error in opening file 
file.exists('ANURA.shp')
readOGR(dsn = path.expand(".", layer = "ANURA.shp"))
library(raster)
s <- shapefile("Anura.shp")
getwd()

#file.exists("/Users/Alexandra_Gonzalez/Desktop/SORT/Herpetology", "ANURA.shp")

#FIGURED OUT HOW TO DOWNLOAD DATA 
data <- readOGR(dsn = "/Users/Alexandra_Gonzalez/Downloads/ANURA/", layer = "ANURA.shp")
z <- readOGR("/Users/Alexandra_Gonzalez/Downloads/ANURA/ANURA.shp")
summary(z)

onerow <- z[1,]
summary(onerow)

class(onerow)
crs(onerow)
extent(onerow)
onerow


####
#### Notes from:http://neondataskills.org/R/open-shapefiles-in-R/
####
#Object Type: the class of the imported object
#Coordinate Reference System (CRS): the projection of the data
#extent: the spatial extent (geographic area that the shapefile covers) of the
        #shapefile. Note that the spatial extent for a shapefile represents
        #the extent of ALL spatial objects in the shapefile. 

######################################

#View just the class for the shapefile 
class(onerow)
    #its a polygon


    #options: point, line, polygon 

######################################
#view just the crs for the shapefile 

crs(onerow) 
  #CRS arguments:
    #+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
  ###### so - the imporant information here is "longlat" because that 
  ###### specifies the units 
######################################
#view just hte extent for the shapefile 

extent(onerow)
   #class       : Extent 
   #xmin        : 46.68178 
   #xmax        : 47.66114 
   #ymin        : -22.41103 
   #ymax        : -19.17633 


  ##### so - this is imporant because it specifies the extent of the shapefile
          #for ALLLLL objects in the shapefile 

#LEFT OFF ON - SPATIAL DATA ATTRIBUTES OF THE POSTED LINK


