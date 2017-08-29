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
