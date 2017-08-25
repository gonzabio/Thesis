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

gonzalez_map <- leaflet ()%>%
  addTiles() %>% #Add default OpenStreetMap map tiles 
  addMarkers(lng = 33.8121, lat= 117.9190, popup = "The birthplace of gonzalez")
gonzalez_map #For some reason, these coordinates dont work? 

##########
########## The Map Widget 


#Initializing options 
#Set value for the minZoom and mazZoom settings
leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))
?leafletOptions

?setView
