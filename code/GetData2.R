#Alexandra Gonzalez


library("profvis") #profvis(sp_taxonomy(CR_names_10)) #how you check how long a function is 
library("rredlist")
token <- "cf51b7524f618f5b23220e687b70c01d0d240cc82d179cf2c5b08b70fdfb83d4" 

#Functions: 
clean_data2 <- function(species_list){
  df<- as.data.frame(species_list)
  df[is.na(df)] <- 0
  only_species <- subset(df, (df$result.subspecies == 0) & 
                           (df$result.rank == 0) & (df$result.subpopulation == 0))
  only_species <- only_species$result.scientific_name
  return(only_species)
}

species_length <- function (species_list){
  fail_list <- vector(length = length(species_list), mode = "character")
  amphibian_list <- vector(length = length(species_list), mode = "character")
  i <- 1
  for (species in species_list){
    if (length(species) > 2){
      fail_list[i] <- species
    } else  { 
        amphibian_list[i] <- species 
    }
    i <- i + 1 
  }
  amph.fail <- data.frame(amphibian_list, fail_list)
  return(amph.fail)
}
sp_class <- function (species_list){
  #Des.: Traverses through a list, and pulls out Class = AMPHIBIA
  #Input: species_list is any list of species w/ and w/o amphibians 
  #Output: Vector of Amphibians 
  amphibian_list <- vector(length = length(species_list), 
                           mode = "character")  
  failed <- vector(length = length(species_list), mode = "character")
  i <- 1 
  for (species in species_list){
    general_info <- rl_search(species, key = token)
    tax <- general_info$result$class
    Sys.sleep(2) 
    if (general_info[1] == "0"){
      failed[i] <- species 
    }else if (tax == "AMPHIBIA"){
      amphibian_list[i] <- species
    }
    i <- i+1
  }
  amph <- amphibian_list[amphibian_list != ""]
  return(amph)
}  

sp_family <- function (species_list, desired.family){
  #Des.: Traverses through a list, and pulls out selected taxa 
  #Input: species_list is any list of species w/ and w/o amphibians
  #Input: desired.family = name of family you'd like to pull species names
  #Output: Vector of amphibian species of desired family  
  #Consider Adding Parameters: token 
  
  amphibian_list <- vector(length = length(species_list), 
                           mode = "character")  
  i <- 1 
  for (species in species_list){
    general_info <- rl_search(species, key = token)
    Sys.sleep(2)
    family <- general_info$result$family
    if (family == desired.family){
      amphibian_list[i] <- species
    }
    i <- i+1
  }
  return(amphibian_list[amphibian_list != ""])
}  
sp_genus <- function(species_list, desired.genus){
  amphibian_list <- vector(length = length(species_list), mode = "character")
  i <- 1
  for (species in species_list){
    general_info <- rl_search(species, key = token)
    Sys.sleep(2)
    genus <- general_info$result$genus
    if (genus == desired.genus){
      amphibian_list[i] <- species
    }
    i <- i + 1
  }
  return(amphibian_list[amphibian_list != ""])
}
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

sp_all_genus <- function(species_list){
  genus_list <- vector(length = length(species_list), mode = "character")
  i <- 1 
  for (species in species_list){
    general_info <- rl_search(species, key = token)
    Sys.sleep(2)
    genus_list[i] <- general_info$result$genus
    i <- i + 1 
  }
  spgenus <- data.frame(species_list, genus_list)
  return(spgenus)
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


# #Get Data: 
# LC <- rl_sp_category("LC", key = token)
# NT <- rl_sp_category("NT", key = token)
# VU <- rl_sp_category("VU", key = token)
# EN <- rl_sp_category("EN", key = token)
# CR <- rl_sp_category("CR", key = token)
# EW <- rl_sp_category("EW", key = token)
# EX <- rl_sp_category("EX", key = token)

# 
# LCC <- LC
# NTT <- NT
# VUU <- VU
# ENN <- EN
# CRR <- CR
# EWW <- EW
# EXX <- EX

#Data: 
lc <- clean_data2(LCC)
nt <- clean_data2(NTT) #done 
vu <- clean_data2(VUU) #done 
en <- clean_data2(ENN) #done 
cr <- clean_data2(CRR) #done 
ew <- clean_data2(EWW) #done
ex <- clean_data2(EXX) #done 


#Only want amphibians: 
ew_frogs <- sp_class(ew) 
cr_frogs <- sp_class(cr)
ex_frogs <- sp_class(ex)
en_frogs <- sp_class(en)
    #for the rest of the categories, used shortcut and longershortcut functions (GetData1)

#copy data 
copyew_frogs <- ew_frogs
copyex_frogs <- ex_frogs 
copycr_frogs <- cr_frogs
copyen_frogs <- en_frogs 
copyvu_frogs <- vu_frogs
copynt_frogs <- nt_frogs 




#Export data 
write.table(copynt_frogs, "/Users/Alexandra_Gonzalez/Desktop/Herpetology/nt_frogs.txt", sep="\t")




x <- read.table("nt_frogs.txt")

