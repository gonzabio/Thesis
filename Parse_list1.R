#Exploratory Data Analysis: Part I 
#This is all 'showing my work' 


library("rredlist")
token <- "cf51b7524f618f5b23220e687b70c01d0d240cc82d179cf2c5b08b70fdfb83d4" 

#Exploring the functions... 
A.bracteata <- rl_threats("Abies bracteata",key = token) 
name <- rl_common_names(name = "Theloderma", key = token) #Common Names
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
#Doesnt work 
parse_list <- function(species_list){
  for (i in species_list)
    info <- rl_search(i, key = token)
    Amphibia_list<- ifelse(info["result"]["class"] == "AMPHIBIA", i, NA)
    newlist <- na.omit(Amphibia_list)
    return(newlist)
}

CR_names_Edit <- CR_names[100:110]

#testnames 
testdf <- c("Theloderma asperum","Ambystoma altamirani")
testdf <- data.frame(testdf, stringsAsFactors = FALSE) #not a character

result <- parse_list(testdf)
#keep getting errors 
#Breaking up larger function into smaller pieces: 
info <- rl_search(CR_names_Edit[1], key = token)
is.atomic(info)
ifelse(info$result$class == "AMPHIBIA", TRUE, FALSE)

####### parse_list #######
####### Description: parse_list. Goes through a list of species and pulls out 
####### only Class = Amphibia 
#######
####### Beforehand, use rl_sp_category to choose which Red List Category 
####### you want. Returns a list of ~all~ the species. Refine it to your class.



parse_list <- function(species_list){
  for (elem in (species_list))
    info <- rl_search(elem, key = token)
    print(info)
    info$result$class[info$result$class == "AMPHIBIA"] 
  return(Amphibia_list)
}

#showing my work: 
f <- function (y,s)
  for (elem in v){
    y <- elem ^ 2 
    s <- c(s,y)
    print (s)
  }
  return (s) 

#showing my work: 
new_list <- ""
only_amphibians <- function (species_list, token, new_list){
  for (species in species_list){
    general_info <- rl_search(species, key = token)
    tax <- general_info$results$class
    if tax == "Amphibia"
      new_list <- c(newlist, species)
    print (new_list)
  }
  return (new_list)
}
CR_amphibians <- only_amphibians(CR_names_Edit)

#showing my work: 
new_list = ""
only_amphibians <- function (species_list, new_list){
  for (species in species_list){
      general_info <- rl_search(species, key = token)
      tax <- general_info$result$class ###Good up until here 
      print(tax)
      if (tax == "AMPHIBIA"){ #getting error message here 
        new_list <- c(new_list, species)
      }
    }
    return(new_list)
}
test1 <- only_amphibians(CR_names_Edit, new_list)

#showing my work: 
v <- ""
v2 <- ""
v3 <- data.frame(v,v2, stringsAsFactors = FALSE)
amphibians3 <- function (species_list, v3){
  for (species in species_list){
    general_info <- rl_search(species, key = token)
    tax <- general_info$result$class  
    v3 <- data.frame(v3, v = species, v2 = tax) #problems in how im formatting
  }
  return(v3)
}


#Pulling CR frogs 
CR <- rl_sp_category("CR", key = token, parse = TRUE)
CR_names <- CR$result$scientific_name
#FINAL FUNCTION: 
################## THIS IS THE FUCNTION THAT I NEED 
sp_taxonomy <- function (species_list){
  #initilize 
  amphibian_list <- vector(length = length(species_list),
                           mode = "character")
  i <- 1
  for (species in species_list){
    general_info <- rl_search(species, key = token)
    #Sys.sleep(2)
    tax <- general_info$result$class
    if (tax == "AMPHIBIA"){
      amphibian_list[i] <- species
    }
    i <- i+1
  }
  return(amphibian_list[amphibian_list != ""])
}

CR_names_10 <- CR_names[100:110]
sp_taxa.df <- sp_taxonomy(CR_names_10)
###################################
library("profvis")
profvis(sp_taxonomy(CR_names_10)) #how you check how long a function is 
#taking you 



#low level APIs are a lot faster 
library("jsonlite") 
jsonlite::fromJSON(rl_sp_category_("CR", key = token))
 

rl_threats("Theloderma asperum", key = token)


