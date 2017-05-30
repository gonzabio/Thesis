#spatial data 


getwd()
setwd("/Users/Alexandra_Gonzalez/Downloads/species_59031")


## Source Jeffrey Hollister: 
##R function downloads shapefiles from a URL 
##https://www.r-bloggers.com/an-r-function-to-download-shapefiles/ 
download.shapefile<-function(shape_url,layer,outfile=layer)
{
  #written by: jw hollister
  #Oct 10, 2012
  
  #set-up/clean-up variables
  if(length(grep("/$",shape_url))==0)
  {
    shape_url<-paste(shape_url,"/",sep="")
  }
  #creates vector of all possible shapefile extensions
  shapefile_ext<-c(".shp",".shx",".dbf",".prj",".sbn",".sbx",
                   ".shp.xml",".fbn",".fbx",".ain",".aih",".ixs",
                   ".mxs",".atx",".cpg")
  #Check which shapefile files exist
  if(require(RCurl))
  {
    xurl<-getURL(shape_url)
    xlogic<-NULL
    for(i in paste(layer,shapefile_ext,sep=""))
    {
      xlogic<-c(xlogic,grepl(i,xurl))
    }
    #Set-up list of shapefiles to download
    shapefiles<-paste(shape_url,layer,shapefile_ext,sep="")[xlogic]
    #Set-up output file names
    outfiles<-paste(outfile,shapefile_ext,sep="")[xlogic]   }
  #Download all shapefiles
  if(sum(xlogic)>0)
  {
    for(i in 1:length(shapefiles))
    {
      download.file(shapefiles[i],outfiles[i],
                    method="auto",mode="wb")
    }
  } else
  {
    stop("An Error has occured with the input URL
         or name of shapefile")
  }
}



#Data from IUCN 
      #DBF file or 'Database file' is the underlying format of dBase (Importing data into R, II)
library(foreign)
getwd()
setwd("/Users/Alexandra_Gonzalez/Downloads/species_59031")
thelodera_data <- read.dbf("species_59031.dbf", as.is = FALSE)





#Raster or rgdal? 
##https://pakillo.github.io/R-GIS-tutorial/#getdata
#Downloading raster climate data from the internet 
tmin <- getData("worldclim", var = "tmin", res = 10)  # this will download 
tmin1 <- raster(paste(getwd(), "/wc10/tmin1.bil", sep = ""))  # Tmin for January
fromDisk(tmin1)
tmin1 <- tmin1/10  # Worldclim temperature data come in decimal degrees 
tmin1  # look at the info
plot(tmin1)

