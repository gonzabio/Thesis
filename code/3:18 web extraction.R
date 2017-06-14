#Practice extracting data from websites


#Following the code in the book 
require(XML)
theURL <- "http://www.jaredlander.com/2012/02/another-kind-of-super-bowl-pool/"
bowlPool <- readHTMLTable(theURL, which = 1, header = FALSE, stringsasFactors = FALSE)
bowlPool


#bp-mAPS 

require(XML)
theURL2 <- "http://www.bd-maps.net/surveillance/s_country.asp"
data <- readHTMLTable(theURL2, which = 1, header = TRUE, stringsasFactors = TRUE)



install.packages("googleVis")




#googleVis: visualise data in a web browser using Google Visualisation API


data(Exports)    # a simple data frame
Geo <- gvisGeoMap(Exports, locationvar="Country", numvar="Profit", 
                  options=list(height=400, dataMode='regions'))
plot(Geo)