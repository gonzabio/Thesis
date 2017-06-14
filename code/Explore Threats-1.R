#Exploratory Data Analysis: Part I 


library("rredlist")
token <- "cf51b7524f618f5b23220e687b70c01d0d240cc82d179cf2c5b08b70fdfb83d4" 

#Questions: Better to use a high or low level API? 


#When quering, you can not use a keyword like 'frog' to pull up the list 
#Have to go through taxonomy. I chose 'frogs' and was returned both 
#salamander and a squirrel 

#Exploratory Data Analysis: 
A.bracteata <- rl_threats("Abies bracteata",key = token) 


#Common Names 
name <- rl_common_names(name = "Theloderma", key = token)
?rl_countries

rl_threats(name = "Theloderma asperum", key = token)

#where this species occurs 
rl_occ_country(name = "Theloderma asperum", key = token)
rl_occ_country(name = "Ambystoma altamirani", key = token)
rl_threats(name = "Ambystoma altamirani", key = token )
rl_search(name = "Ambystoma altamirani", key = token) #-> here is wher eu find the category
countries <- rl_countries(key = token)
countries$results$isocode

#Critically Engangered (CR)
CR <- rl_sp_category("CR", key = token, parse = TRUE)
CR_names <- CR$result$scientific_name

#Find species in your taxa 
parse_list <- function(species_list){
  for (i in species_list)
    info <- rl_search(i, key = token)
    Amphibia_list<- ifelse(info["result"]["class"] == "AMPHIBIA", i, NA)
    newlist <- na.omit(Amphibia_list)
    return(newlist)
}

CR_names_Edit <- CR_names[1:10]

#testnames 
testdf <- c("Theloderma asperum","Ambystoma altamirani")
testdf <- data.frame(testdf, stringsAsFactors = FALSE) #not a character

result <- parse_list(testdf)
#keep getting errors 
#Breaking up larger function into smaller pieces: 
info <- rl_search(CR_names_Edit[1], key = token)
is.atomic(info)
ifelse(info$result$class == "AMPHIBIA", TRUE, FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#test number 2
parse_list <- function(species_list){
  for (i in species_list)
    info <- rl_search(i, key = token)
    print(info)
    Amphibia_list<- ifelse(info$result$class == "AMPHIBIA", i, NA)
  return(Amphibia_list)
}

test2 <- parse_list(CR_names_Edit) #only getting back 1 NA .. not a list of ten 
#### only getting one False.. not a list 


#Problem: for loop is not cycling, ony goes through 1 name ^ 



