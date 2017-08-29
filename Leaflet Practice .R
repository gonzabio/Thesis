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







