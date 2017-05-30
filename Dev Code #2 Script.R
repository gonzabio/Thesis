#Alexandra Gonzalez 

library("profvis") #profvis(sp_taxonomy(CR_names_10)) #how you check how long a function is 
library("rredlist")
token <- "cf51b7524f618f5b23220e687b70c01d0d240cc82d179cf2c5b08b70fdfb83d4" 

#In my file when I parse through the species names, everything is in one long 
#character string and I wrote the function to accomodate how it comes direction from
#the IUCN. 
#Lines 13/14 download the data and convert the species names column to a character vector
  #Otherwise get error: length must equal 1 


data1 <- read.table(file = "cr_frogs.txt",sep = "", stringsAsFactors = FALSE)
copycr_frogs <- as.character(data1$x)





sp_threat_count <- function(species_list){
  threat_count <- vector(length = length(species_list), mode = "character")
  i <- 1
  for (species in species_list){
    threats <- rl_threats(species, key = token)
    Sys.sleep(2)
    code <- as.numeric(threats$result$code)
    code <- na.omit(code)
    threat_count[i] <- length(code)
    i <- i + 1
  }
  df <- data.frame(species_list, threat_count, stringsAsFactors = FALSE)
  return(df)
}


sp_all_family <- function(species_list){
  family_list <- vector(length = length(species_list), mode = "character")
  i <- 1 
  for (species in species_list){
    general_info <- rl_search(species, key = token)
    family_list[i] <- general_info$result$family
    i <- i + 1 
  }
  spfamily <- data.frame(species_list, family_list)
  return(spfamily)
}


cr_threat_count <- sp_threat_count(practice2)
cr_family <- sp_all_family(copycr_frogs)
