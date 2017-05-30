#Alexandra Gonzalez 
#Week 2: Feb 29 - Feb 4 
# Feb 1, 2017 

#Notes from speaking with Dr. Francis-Lyon: 
# Try an R Markdown page that can be made into a notebook. 
#### Books that are available with this? Search current book. 

# R markdown --> notebook. --> HTML 
# Try pulling data from Genebank using R. 



#Biocundoctur package? 
#Goal for this class is to create a Notebook. 


#"An R Notebook is an R Markdown document with chunks that can be executed 
#independently and interactively, with output visible immediately beneath the input." 
# http://rmarkdown.rstudio.com/r_notebooks.html



#STEP 1: Practice scraping data from a website so I can then do it to ICNU Website. 
# Once I have scrapped the data from the ICNU Website, I know which frogs I want to 
# pull from GeneBank 

#This is the website I'm following to teach me how to pull data: 
#http://zevross.com/blog/2015/05/19/scrape-website-data-with-the-new-r-package-rvest/

#devtools::install_github("rstudio/leaflet")
library(dplyr)
#install.packages("rvest")
library(rvest)
#install.packages("ggmap")
library(ggmap)
#install.packages("leaflet")
library(leaflet)
library(RColorBrewer)


# URL for the Visit Ithaca website, wineries page
url<-html("http://www.visitithaca.com/attractions/wineries.html")

#Website that describes a package to scan ICUN database: 
#https://cran.r-project.org/web/packages/letsR/README.html
#install.packages("letsR")
library(letsR)

lets.iucn
lets.iucn.ha
lets.iucn.his