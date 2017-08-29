#Leaflet Intro/Practice & The Map Widget 
#August 23, 2017 
#ASK ABOUT WHEN I SHOULD CREATE A NEW FILE/PUSH/PULL 

#GOAL: Introduce myself to leaflet 
#From widget page --- > 
library("leaflet")
#install.packages ("leaflet")

m <- leaflet () %>%
  addTiles() %>% #Add default OpenStreetMap map tiles 
  addMarkers(lng=174.768, lat=-36.852, popup="Im a savage dont you forget it fam")
m #prints the map 

#set value for the minZoom and maxZoom settings 
leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))
df = data.frame(Lat = 1:10, Long = rnorm(10))
leaflet(df) %>% addCircles()
leaflet(df) %>% addCircles(lng = ~Long, lat = ~Lat)


library(sp)
Sr1 = Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2)))
Sr2 = Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)))
Sr3 = Polygon(cbind(c(4, 4, 5, 10, 4), c(5, 3, 2, 5, 5)))
Sr4 = Polygon(cbind(c(5, 6, 6, 5, 5), c(4, 4, 3, 3, 4)), hole = TRUE)
Srs1 = Polygons(list(Sr1), "s1")
Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr4, Sr3), "s3/4")
SpP = SpatialPolygons(list(Srs1, Srs2, Srs3), 1:3)
leaflet(height = "300px") %>% addPolygons(data = SpP)

library(maps)
mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)

oerioerjgoreijg



library(raster)
library(leaflet)


#Leaflet is one of the most populat javascript libraries for creating interactive maps. 
mymap <- leaflet () %>%
  addTiles()
mymap #blank map 

#Add markers 

mymap <- mymap %>%
  addMarkers(lat=39.2980803, lng = -76.5898801,
             popup = "Jeff Leek's Office")
mymap #fixed it


#Addnig Many Markers 

# Adding one marker at a time is often not practical if you
# want to display many markers. If you have a data frame with
# columns 'lat' and 'lng' you can pip ethat data frame into 
# 'leaflet()' to add all the points at once. 

set.seed(2016-04-25)
df <- data.frame (lat = runif (20, min = 39.2, max = 39.3),
                  lng = runif (20, min = -76.6, max = -76.5))

df %>% 
  leaflet () %>% 
  addTiles() %>% 
  addMarkers()


#Making Custom Markers 
# Adding Multiple Popups 

#Me trying to make a map based on this example youtube 
# video 

GonzalezIcon <- makeIcon(
  iconUrl = "https://s-media-cache-ak0.pinimg.com/originals/49/b1/0f/49b10f78b1c3f532cf5313e25419e71a.jpg",
  iconWidth = 31 * 215/230, iconHeight = 31,
  iconAnchorX = 31*215/230/2, iconAnchorY = 16
  )

Alexandra_df <- data.frame(
  lat = c(-118.1334, -117.9190),
  lng = c(33.9024, 33.8121)
)

Alexandra_df %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(icon = GonzalezIcon)






