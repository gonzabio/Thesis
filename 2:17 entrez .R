#2:17
#https://cran.r-project.org/web/packages/rentrez/vignettes/rentrez_tutorial.html

library(rentrez)
entrez_db_summary("nuccore")

#Find out about a database
entrez_dbs() #lists all of the databases 
entrez_db_links("nuccore")  #all of the linked databases to nuccore
      #nucleotide database. not really sure what the difference is? 
entrez_db_searchable("nuccore")


#searching a database
practice_search <- entrez_search(db = "nuccore", term = "Rana") 
      #Note on Retmax: 
      #"2416415 hits (object contains 20 IDs and no web_history object)
      #Basically, the retmax = 20IDs limits how many IDs can come back because it is a 
      #lot of data. 

      #the bottom.. "Rana"[Organism] OR "Rana"[Organism] OR Rana[ALL F..] is a MeSH Term
      #This is to make sure your search was interpretted the way that you wanted. 

      #MAX NCBI IDs: Use the 'Web History' feature.. described belo.w 


entrez_search(db="nuccore",
              term="Tetrahymena thermophila[ORGN]") 
      #Term = its important to put [ORGN] because when you don't, you get lots of weird
      #things back. here is the comparison 

entrez_search (db = "nuccore", 
               term = "Tetrahymena thermophila")
      #The number of hits actually increases from 31413 (specifying it was an organism)
      #to 31819 hits.  Also, the MeSH term includes more boolean opeartors .. like 'or' 

#Adding search terms 
      #Can find out what you can add but looking through the terms, entrez_db_searchable
entrez_search (db = "nuccore",
               term = "Tetrahymena thermophila [ORGN] AND  2013:2015[PDAT]")

#Searching two different organisms, use an 'OR' 
entrez_search (db = "nuccore",
               term = "Theloderma asperum[ORGN] OR Rana [ORGN]")
      
#Identify trends of a buzzword 
#Plot <- good idea 

search_year <- function(year, term){
  query <- paste(term, "AND (", year, "[PDAT])")
  entrez_search(db="nuccore", term=query, retmax=0)$count
}

year <- 2008:2017
papers <- sapply(year, search_year, term="Theloderma", USE.NAMES=FALSE)

plot(year, papers, type = 'b', main="The Rise of the Theloderma")


#entrez_link()
all_the_links <- entrez_link(dbfrom='gene', id=351, db='all')
all_the_links$links








