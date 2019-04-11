######## Alexandra Gonzalez 
######## HS 617 Project
########
######## Analyzing IUCN Red List Amphibian Data Repository 


########
######## Packages & IUCN token
########  
library("profvis") 
library("rredlist")
library(RSQLite)
library(sqldf)
library(dplyr)
require(ggplot2)
require(reshape2)
require(scales)
token <- "cf51b7524f618f5b23220e687b70c01d0d240cc82d179cf2c5b08b70fdfb83d4" 



########
######## Functions(1/2): Preprocessing, amphibians only, species only, threat count 
########

sp_only <- function(species_list){
  #parameter name is confusing - takes a list of everything, 
  #returns species. filters out subpopulations, rank, etc. 
  df<- as.data.frame(species_list)
  df[is.na(df)] <- 0
  only_species <- subset(df, (df$result.subspecies == 0) & 
                           (df$result.rank == 0) & (df$result.subpopulation == 0))
  only_species <- only_species$result.scientific_name
  return(only_species)
}
species_length <- function (species_list){
  # Runs through a list of species and only pulls the failures 
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
  # Enter species list and desired genus, returns amphibain list of that genus 
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
  # generates threat count for each species in the list 
  threat_count <- vector(length = length(species_list), mode = "character")
  i <- 1
  thirdlevel <- c("2.1.1", "2.1.2", "2.1.3", "2.1.4", "2.2.1", "2.2.2", "2.2.3",
                  "2.3.1", "2.3.2", "2.3.3", "2.3.4", "2.4.1", "2.4.2", "2.4.3", 
                  "5.1.1", "5.1.2", "5.1.3", "5.1.4", "5.2.1", "5.2.2", "5.2.3",
                  "5.2.4", "5.3.1", "5.3.2", "5.3.3", "5.3.4", "5.3.5", "5.4.1",
                  "5.4.2", "5.4.3", "5.4.4", "5.4.5", "5.4.6", "7.1.1", "7.1.2",
                  "7.1.3", "7.2.1", "7.2.2", "7.2.3", "7.2.4", "7.2.5", "7.2.6",
                  "7.2.7", "7.2.8", "7.2.9", "7.2.10", "7.2.11", "8.1.1", "8.1.2",
                  "8.2.1", "8.2.2", "8.4.1", "8.4.2", "8.5.1", "8.5.2", "9.1.1",
                  "9.1.2", "9.1.3", "9.2.1", "9.2.2", "9.2.3", "9.3.1", "9.3.2", 
                  "9.3.3", "9.3.4", "9.5.1")
  for (species in species_list){
    threats <- rl_threats(species, key = token)
    code1 <- as.numeric(threats$result$code)
    code2 <- na.omit(code1)
    code3 <- !(code2 %in% thirdlevel)
    threat_count[i] <- length(code3)
    i <- i + 1
  }
  df <- data.frame(species_list, threat_count, stringsAsFactors = FALSE)
  return(df)
}

hey <- sp_threat_count(full_se_data$new_species[1:2])
sp_all_genus <- function(species_list){
  # returns species list and genus of each species 
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
hey <- sp_all_genus(iucn_data$new_species[1:2])
sp_all_family <- function(species_list){
  #returns species list and family of each species 
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


######## Shortcut functions: Developed when it was taking too
######## long to communicate with the IUCN & I had to reach a 
######## deadline. Do not plan on incorporating these later on. 


short_cut <- function(species_list, desiredgenus){
  keepers <- vector(mode = "character", length = length(species_list))
  for (genus in desiredgenus){
    pos1 <- regexpr(genus, species_list)
    df <- data.frame(species_list, pos1, stringsAsFactors = FALSE)
    keep <- df[df$pos1 == 1,]
    vector1 <- keep$species_list
    keepers<- c(keepers, vector1)
  }
  return(keepers[keepers != ""])
}
longershortcut <- function(species_list, genus_list){
  newlist <- vector(mode = "character", length = length(species_list))
  for (genus in genus_list){
    test <- short_cut(species_list, genus)
    newlist <- c(newlist, test)
  }
  clean <- newlist[newlist != ""]
  return(clean)
}

########
######## Functions (2/2): Populating habitat table and threat table 
########

sp_habitats <- function(species_list){
  #Description: Takes a vector of species names (that are characters) and 
  #traverses through the vector. For each species, the habitat(s) are indentified.
  #The corresponding habitat code is then collected for each species. 
  #Parameter: species_list. 
  habitat_code <- vector(length = length(species_list), mode = "character")
  i <- 1 
  for (species in species_list){
    habitat <- rl_habitats(species, key = token)
    code <- habitat$result$code
    habitat_code[i] <- paste(code, collapse = " ") 
    i <- i + 1   
  }
  final_results <- data.frame(species_list, habitat_code)
  return(final_results)
} #df of species and habitat codes 
habitat_table <- function(species_list){
  #These are all of the habitats that are listed on the IUCN database 
  #Make the tables: 
  
  #Forest
  F1.1 <- rep(c(0),  each = length(species_list)) 
  F1.2 <- rep(c(0),  each = length(species_list))
  F1.3 <- rep(c(0),  each = length(species_list))
  F1.4 <- rep(c(0),  each = length(species_list))
  F1.5 <- rep(c(0),  each = length(species_list))
  F1.6 <- rep(c(0),  each = length(species_list))
  F1.7 <- rep(c(0),  each = length(species_list))
  F1.8 <- rep(c(0),  each = length(species_list))
  #Savanna
  Sa2.1 <- rep(c(0),  each = length(species_list)) 
  Sa2.2 <- rep(c(0),  each = length(species_list))
  #Shrubland 
  Sh3.1 <- rep(c(0),  each = length(species_list)) 
  Sh3.2 <- rep(c(0),  each = length(species_list))
  Sh3.3 <- rep(c(0),  each = length(species_list))
  Sh3.4 <- rep(c(0),  each = length(species_list))
  Sh3.5 <- rep(c(0),  each = length(species_list))
  Sh3.6 <- rep(c(0),  each = length(species_list))
  Sh3.7 <- rep(c(0),  each = length(species_list))
  Sh3.8 <- rep(c(0),  each = length(species_list))
  #Grassland
  G4.1 <- rep(c(0),  each = length(species_list))  
  G4.2 <- rep(c(0),  each = length(species_list))
  G4.3 <- rep(c(0),  each = length(species_list))
  G4.4 <- rep(c(0),  each = length(species_list))
  G4.5 <- rep(c(0),  each = length(species_list))
  G4.6 <- rep(c(0),  each = length(species_list))
  G4.7 <- rep(c(0),  each = length(species_list))
  #Wetlands
  W5.1 <- rep(c(0),  each = length(species_list))
  W5.2 <- rep(c(0),  each = length(species_list))
  W5.3 <- rep(c(0),  each = length(species_list))
  W5.4 <- rep(c(0),  each = length(species_list))
  W5.5 <- rep(c(0),  each = length(species_list))
  W5.6 <- rep(c(0),  each = length(species_list))
  W5.7 <- rep(c(0),  each = length(species_list))
  W5.8 <- rep(c(0),  each = length(species_list))
  W5.9 <- rep(c(0),  each = length(species_list))
  W5.10 <- rep(c(0),  each = length(species_list))
  W5.11 <- rep(c(0),  each = length(species_list))
  W5.12 <- rep(c(0),  each = length(species_list))
  W5.13 <- rep(c(0),  each = length(species_list))
  W5.14 <- rep(c(0),  each = length(species_list))
  W5.15 <- rep(c(0),  each = length(species_list))
  W5.16 <- rep(c(0),  each = length(species_list))
  W5.17 <- rep(c(0),  each = length(species_list))
  W5.18 <- rep(c(0),  each = length(species_list))
  #Rocky 
  R6 <- rep(c(0),  each = length(species_list))
  #Caves 
  C7.1 <- rep(c(0),  each = length(species_list))
  C7.2 <- rep(c(0),  each = length(species_list))
  #Desert 
  D8.1 <- rep(c(0),  each = length(species_list))
  D8.2 <- rep(c(0),  each = length(species_list))
  D8.3 <- rep(c(0),  each = length(species_list))
  #Marine Neritic 
  MN9.1 <- rep(c(0),  each = length(species_list))
  MN9.2 <- rep(c(0),  each = length(species_list))
  MN9.3 <- rep(c(0),  each = length(species_list))
  MN9.4 <- rep(c(0),  each = length(species_list))
  MN9.5 <- rep(c(0),  each = length(species_list))
  MN9.6 <- rep(c(0),  each = length(species_list))
  MN9.7 <- rep(c(0),  each = length(species_list))
  MN9.8 <- rep(c(0),  each = length(species_list))
  MN9.9 <- rep(c(0),  each = length(species_list))
  MN9.10 <- rep(c(0),  each = length(species_list))
  #Marine Oceanic 
  MO10.1 <- rep(c(0),  each = length(species_list))
  MO10.2 <- rep(c(0),  each = length(species_list))
  MO10.3 <- rep(c(0),  each = length(species_list))
  MO10.4 <- rep(c(0),  each = length(species_list))
  #Marine Benthic 
  M11.1 <- rep(c(0),  each = length(species_list))
  M11.2 <- rep(c(0),  each = length(species_list))
  M11.3 <- rep(c(0),  each = length(species_list))
  M11.4 <- rep(c(0),  each = length(species_list))
  M11.5 <- rep(c(0),  each = length(species_list))
  M11.6 <- rep(c(0),  each = length(species_list))
  #Marine Intertidal 
  MI12.1 <- rep(c(0),  each = length(species_list))
  MI12.2 <- rep(c(0),  each = length(species_list))
  MI12.3 <- rep(c(0),  each = length(species_list))
  MI12.4 <- rep(c(0),  each = length(species_list))
  MI12.5 <- rep(c(0),  each = length(species_list))
  MI12.6 <- rep(c(0),  each = length(species_list))
  MI12.7 <- rep(c(0),  each = length(species_list))
  #Marine Coastal 
  MC13.1 <- rep(c(0),  each = length(species_list))
  MC13.2 <- rep(c(0),  each = length(species_list))
  MC13.3 <- rep(c(0),  each = length(species_list))
  MC13.4 <- rep(c(0),  each = length(species_list))
  MC13.5 <- rep(c(0),  each = length(species_list))
  #Artifical/Terrestrial 
  AT14.1 <- rep(c(0),  each = length(species_list))
  AT14.2 <- rep(c(0),  each = length(species_list))
  AT14.3 <- rep(c(0),  each = length(species_list))
  AT14.4 <- rep(c(0),  each = length(species_list))
  AT14.5 <- rep(c(0),  each = length(species_list))
  AT14.6 <- rep(c(0),  each = length(species_list))
  #Artifical/Aquatic 
  AA15.1 <- rep(c(0),  each = length(species_list))
  AA15.2 <- rep(c(0),  each = length(species_list))
  AA15.3 <- rep(c(0),  each = length(species_list))
  AA15.4 <- rep(c(0),  each = length(species_list))
  AA15.5 <- rep(c(0),  each = length(species_list))
  AA15.6 <- rep(c(0),  each = length(species_list))
  AA15.7 <- rep(c(0),  each = length(species_list))
  AA15.8 <- rep(c(0),  each = length(species_list))
  AA15.9 <- rep(c(0),  each = length(species_list))
  AA15.10 <- rep(c(0),  each = length(species_list))
  AA15.11 <- rep(c(0),  each = length(species_list))
  AA15.12 <- rep(c(0),  each = length(species_list))
  AA15.13 <- rep(c(0),  each = length(species_list))
  #Introduced Vegetatino
  Veg16 <- rep(c(0),  each = length(species_list))
  #Other 
  O17 <- rep(c(0),  each = length(species_list))
  #Unknown
  U18 <- rep(c(0),  each = length(species_list))
  
  habitat_df <- data.frame(species_list, F1.1, F1.2, F1.3, F1.4, F1.5, F1.6, F1.7, F1.8,
                           Sa2.1, Sa2.2, Sh3.1, Sh3.2, Sh3.3, Sh3.4, Sh3.5, Sh3.6, 
                           Sh3.7, Sh3.8, G4.1, G4.2, G4.3, G4.4, G4.5, G4.6, 
                           G4.7, W5.1, W5.2, W5.3, W5.4, W5.5, W5.6, W5.7, 
                           W5.8, W5.9, W5.10, W5.11, W5.12, W5.13, W5.14, W5.15, 
                           W5.16, W5.17, W5.18, R6, C7.1, C7.2, D8.1, D8.2, D8.3,  
                           MN9.1, MN9.2, MN9.3, MN9.4, MN9.5, MN9.6, MN9.7, MN9.8,
                           MN9.9, MN9.10, MO10.1, MO10.2,MO10.3, MO10.4, M11.1, 
                           M11.2, M11.3, M11.4, M11.5, M11.6, MI12.1, MI12.2, MI12.3,
                           MI12.4, MI12.5, MI12.6, MI12.7, MC13.1, MC13.2, MC13.3, MC13.4,
                           MC13.5, AT14.1, AT14.2, AT14.3, AT14.4, AT14.5, AT14.6, 
                           AA15.1, AA15.2, AA15.3, AA15.4, AA15.5, AA15.6, AA15.7, 
                           AA15.8, AA15.9, AA15.10, AA15.11, AA15.12, AA15.13, 
                           Veg16, O17, U18)
  return(habitat_df)
} #Empty table of habitat 
pop_habitat_tables <- function(data){
  ### data is a dataframe with the species_list as one column and the habitat_code as the other
  ### aims to work directly with sp_habitats and habitat_tables 
  habitatdf <- habitat_table(data$species_list)
  i <- 1
  for (strings in data$habitat_code){
    many_strings <- strsplit(strings, split = " ")
    for (elem in many_strings){
      for(code in elem){
        #### Forest 
        if (code == "1.1"){
          habitatdf$F1.1[i] <- 1
        } else if (code == "1.2"){
          habitatdf$F1.2[i] <- 1
        } else if (code == "1.3"){
          habitatdf$F1.3[i] <- 1
        } else if (code == "1.4"){
          habitatdf$F1.4[i] <- 1
        } else if (code == "1.5"){
          habitatdf$F1.5[i] <- 1
        } else if (code == "1.6"){
          habitatdf$F1.6[i] <- 1
        } else if (code == "1.7"){
          habitatdf$F1.7[i] <- 1 
        } else if (code == "1.8"){
          habitatdf$F1.8[i] <- 1
        } else if (code == "5.1"){
          habitatdf$W5.1[i] <- 1
          #### Savanna 
        } else if (code == "2.1"){
          habitatdf$Sa2.1[i] <- 1
        } else if (code == "2.2"){
          habitatdf$Sa2.2[i] <- 1
          #### Shrubland 
        } else if (code == "3.1"){
          habitatdf$Sh3.1[i] <- 1
        } else if (code == "3.2"){
          habitatdf$Sh3.2[i] <- 1
        } else if (code == "3.3"){
          habitatdf$Sh3.3[i] <- 1
        } else if (code == "3.4"){
          habitatdf$Sh3.4[i] <- 1
        } else if (code == "3.5"){
          habitatdf$Sh3.5[i] <- 1
        } else if (code == "3.6"){
          habitatdf$Sh3.6[i] <- 1
        } else if (code == "3.7"){
          habitatdf$Sh3.7[i] <- 1
        } else if (code == "3.8"){
          habitatdf$Sh3.8[i] <- 1
          #### Grassland 
        } else if (code == "4.1"){
          habitatdf$G4.1[i] <- 1
        } else if (code == "4.2"){
          habitatdf$G4.2[i] <- 1
        } else if (code == "4.3"){
          habitatdf$G4.3[i] <- 1
        } else if (code == "4.4"){
          habitatdf$G4.4[i] <- 1
        } else if (code == "4.5"){
          habitatdf$G4.5[i] <- 1
        } else if (code == "4.6"){
          habitatdf$G4.6[i] <- 1
        } else if (code == "4.7"){
          habitatdf$G4.7[i] <- 1
          #### Wetland
        } else if (code == "5.1"){
          habitatdf$W5.1[i] <- 1
        } else if (code == "5.2"){
          habitatdf$W5.2[i] <- 1
        } else if (code == "5.3"){
          habitatdf$W5.3[i] <- 1
        } else if (code == "5.4"){
          habitatdf$W5.4[i] <- 1
        } else if (code == "5.5"){
          habitatdf$W5.5[i] <- 1
        } else if (code == "5.6"){
          habitatdf$W5.6[i] <- 1
        } else if (code == "5.7"){
          habitatdf$W5.7[i] <- 1
        } else if (code == "5.8"){
          habitatdf$W5.8[i] <- 1
        } else if (code == "5.9"){
          habitatdf$W5.9[i] <- 1
        } else if (code == "5.10"){
          habitatdf$W5.10[i] <- 1
        } else if (code == "5.11"){
          habitatdf$W5.11[i] <- 1
        } else if (code == "5.12"){
          habitatdf$W5.12[i] <- 1
        } else if (code == "5.13"){
          habitatdf$W5.13[i] <- 1
        } else if (code == "5.14"){
          habitatdf$W5.14[i] <- 1
        } else if (code == "5.15"){
          habitatdf$W5.15[i] <- 1
          #Rocky 
        } else if (code == "6"){
          habitatdf$R6[i] <- 1
          #### Caves 
        } else if (code == "7.1"){
          habitatdf$C7.1[i] <- 1
        } else if (code == "7.2"){
          habitatdf$C7.2[i] <- 1
          #### Desert
        } else if (code == "8.1"){
          habitatdf$D8.1[i] <- 1
        } else if (code == "8.2"){
          habitatdf$D8.2[i] <- 1
        } else if (code == "8.3"){
          habitatdf$D8.3[i] <- 1
          ### Marine Neritic 
        } else if (code == "9.1"){
          habitatdf$MN9.1[i] <- 1
        } else if (code == "9.2"){
          habitatdf$MN9.2[i] <- 1
        } else if (code == "9.3"){
          habitatdf$MN9.3[i] <- 1
        } else if (code == "9.4"){
          habitatdf$MN9.4[i] <- 1
        } else if (code == "9.5"){
          habitatdf$MN9.5[i] <- 1
        } else if (code == "9.6"){
          habitatdf$MN9.6[i] <- 1
        } else if (code == "9.7"){
          habitatdf$MN9.7[i] <- 1
        } else if (code == "9.8.1" | code == "9.8.2" | code == "9.8.3"|
                   code == "9.8.4" | code == "9.8.5"){
          habitatdf$MN9.8[i] <- 1
        } else if (code == "9.9"){
          habitatdf$MN9.9[i] <- 1
        } else if (code == "9.10"){
          habitatdf$MN9.10[i] <- 1
          #### Marine Oceanic 
        } else if (code == "10.1"){
          habitatdf$MO10.1[i] <- 1
        } else if (code == "10.2"){
          habitatdf$MO10.2[i] <- 1
        } else if (code == "10.3"){
          habitatdf$MO10.3[i] <- 1
        } else if (code == "10.4"){
          habitatdf$MO10.4[i] <- 1
          #### Marine Deep Benthic 
        } else if (code == "11.1" | code == "11.1.2" | code == "11.1.2" ){
          habitatdf$M11.1[i] <- 1
        } else if (code == "11.2"){
          habitatdf$M11.2[i] <- 1
        } else if (code == "11.3"){
          habitatdf$M11.3[i] <- 1
        } else if (code == "11.4"){
          habitatdf$M11.4[i] <- 1
        } else if (code == "11.5"){
          habitatdf$M11.5[i] <- 1
        } else if (code == "11.6"){
          habitatdf$M11.6[i] <- 1
          ### Marine Intertidal 
        } else if (code == "12.1"){
          habitatdf$MI12.1[i] <- 1
        } else if (code == "12.2"){
          habitatdf$MI12.2[i] <- 1
        } else if (code == "12.3"){
          habitatdf$MI12.3[i] <- 1
        } else if (code == "12.4"){
          habitatdf$MI12.4[i] <- 1
        } else if (code == "12.5"){
          habitatdf$MI12.5[i] <- 1
        } else if (code == "12.6"){
          habitatdf$MI12.6[i] <- 1
        } else if (code == "12.7"){
          habitatdf$MI12.7[i] <- 1
          #### Marine Coastal 
        } else if (code == "13.1"){
          habitatdf$MC13.1[i] <- 1
        } else if (code == "13.2"){
          habitatdf$MC13.2[i] <- 1
        } else if (code == "13.3"){
          habitatdf$MC13.3[i] <- 1
        } else if (code == "13.4"){
          habitatdf$MC13.4[i] <- 1
        } else if (code == "13.5"){
          habitatdf$MC13.5[i] <- 1
          #### Artificial/Terrestrial 
        } else if (code == "14.1"){
          habitatdf$AT14.1[i] <- 1
        } else if (code == "14.2"){
          habitatdf$AT14.2[i] <- 1
        } else if (code == "14.3"){
          habitatdf$AT14.3[i] <- 1
        } else if (code == "14.4"){
          habitatdf$AT14.4[i] <- 1
        } else if (code == "14.5"){
          habitatdf$AT14.5[i] <- 1
        } else if (code == "14.6"){
          habitatdf$AT14.6[i] <- 1
          #### Introduced Vegetication 
        } else if (code == "16"){
          habitatdf$Veg16[i] <- 1
          #### Other 
        } else if (code == "17"){
          habitatdf$O17[i] <- 1
          #### Unknown 
        } else if (code == "18"){
          habitatdf$U18[i] <- 1
          
        }
      }
    }
    i <- 1 + i 
  }
  return(habitatdf)
} #Takes DF from sp_habitat and populates empty habitat table

sp_threats <- function(species_list){
  threat_code <- vector(length = length(species_list),
                        mode = "character")
  i <- 1 
  for (species in species_list){
    threats <- rl_threats(species, key = token)
    code <- threats$result$code
    threat_code[i] <- paste(code, collapse = " ")
    i <- i + 1   
  }
  final_results <- data.frame(species_list, threat_code)
  return(final_results)
} #df of species and threat codes 
threat_table <- function(species_list){
  R1.1 <- rep(c(0),  each = length(species_list)) 
  R1.2 <- rep(c(0),  each = length(species_list))
  R1.3 <- rep(c(0),  each = length(species_list))
  
  #Agriculture and Aquaculture 
  A2.1 <- rep(c(0),  each = length(species_list))
  A2.1.1 <- rep(c(0),  each = length(species_list))
  A2.1.2 <- rep(c(0),  each = length(species_list))
  A2.1.3 <- rep(c(0),  each = length(species_list))
  A2.1.4 <- rep(c(0),  each = length(species_list))
  
  A2.2 <- rep(c(0),  each = length(species_list))
  A2.2.1 <- rep(c(0),  each = length(species_list))
  A2.2.2 <- rep(c(0),  each = length(species_list))
  A2.2.3 <- rep(c(0),  each = length(species_list))
  
  A2.3 <- rep(c(0),  each = length(species_list))
  A2.3.1 <- rep(c(0),  each = length(species_list))
  A2.3.2 <- rep(c(0),  each = length(species_list))
  A2.3.3 <- rep(c(0),  each = length(species_list))
  A2.3.4 <- rep(c(0),  each = length(species_list))
  
  A2.4 <- rep(c(0),  each = length(species_list))
  A2.4.1 <- rep(c(0),  each = length(species_list))
  A2.4.2 <- rep(c(0),  each = length(species_list))
  A2.4.3 <- rep(c(0),  each = length(species_list))
  
  #Energy Productoin and mining 
  E3.1 <- rep(c(0),  each = length(species_list)) 
  E3.2 <- rep(c(0),  each = length(species_list))
  E3.3 <- rep(c(0),  each = length(species_list)) 
  
  #Transportation and service corridots 
  T4.1 <- rep(c(0),  each = length(species_list))
  T4.2 <- rep(c(0),  each = length(species_list))
  T4.3 <- rep(c(0),  each = length(species_list))
  T4.4 <- rep(c(0),  each = length(species_list))
  
  #Biological Resource use 
  B5.1 <- rep(c(0),  each = length(species_list))
  B5.1.1 <- rep(c(0),  each = length(species_list))
  B5.1.2 <- rep(c(0),  each = length(species_list))
  B5.1.3 <- rep(c(0),  each = length(species_list))
  B5.1.4 <- rep(c(0),  each = length(species_list))
  
  B5.2 <- rep(c(0),  each = length(species_list))
  B5.2.1 <- rep(c(0),  each = length(species_list))
  B5.2.2 <- rep(c(0),  each = length(species_list))
  B5.2.3 <- rep(c(0),  each = length(species_list))
  B5.2.4 <- rep(c(0),  each = length(species_list))
  
  B5.3 <- rep(c(0),  each = length(species_list))
  B5.3.1 <- rep(c(0),  each = length(species_list))
  B5.3.2 <- rep(c(0),  each = length(species_list))
  B5.3.3 <- rep(c(0),  each = length(species_list))
  B5.3.4 <- rep(c(0),  each = length(species_list))
  B5.3.5 <- rep(c(0),  each = length(species_list))
  
  B5.4 <- rep(c(0),  each = length(species_list))
  B5.4.1 <- rep(c(0),  each = length(species_list))
  B5.4.2 <- rep(c(0),  each = length(species_list))
  B5.4.3 <- rep(c(0),  each = length(species_list))
  B5.4.4 <- rep(c(0),  each = length(species_list))
  B5.4.5 <- rep(c(0),  each = length(species_list))
  B5.4.6 <- rep(c(0),  each = length(species_list))
  
  #Human intrustions and disturbance 
  H6.1 <- rep(c(0),  each = length(species_list))  
  H6.2 <- rep(c(0),  each = length(species_list))
  H6.3 <- rep(c(0),  each = length(species_list))
  
  #Natural system modifications 
  N7.1 <- rep(c(0),  each = length(species_list))
  N7.1.1 <- rep(c(0),  each = length(species_list))
  N7.1.2 <- rep(c(0),  each = length(species_list))
  N7.1.3 <- rep(c(0),  each = length(species_list))
  
  N7.2 <- rep(c(0),  each = length(species_list))
  N7.2.11 <- rep(c(0),  each = length(species_list))
  N7.2.10 <- rep(c(0),  each = length(species_list))
  N7.2.1 <- rep(c(0),  each = length(species_list))
  N7.2.2 <- rep(c(0),  each = length(species_list))
  N7.2.3 <- rep(c(0),  each = length(species_list))
  N7.2.4 <- rep(c(0),  each = length(species_list))
  N7.2.5 <- rep(c(0),  each = length(species_list))
  N7.2.6 <- rep(c(0),  each = length(species_list))
  N7.2.7 <- rep(c(0),  each = length(species_list))
  N7.2.8 <- rep(c(0),  each = length(species_list))
  N7.2.9 <- rep(c(0),  each = length(species_list))
  
  N7.3 <- rep(c(0),  each = length(species_list))
  
  #Invasive and other problematic species, genes and disease 
  I8.1 <- rep(c(0),  each = length(species_list))
  I8.1.1 <- rep(c(0),  each = length(species_list))
  I8.1.2 <- rep(c(0),  each = length(species_list))
  
  I8.2 <- rep(c(0),  each = length(species_list))
  I8.2.1 <- rep(c(0),  each = length(species_list))
  I8.2.2 <- rep(c(0),  each = length(species_list))
  
  I8.3 <- rep(c(0),  each = length(species_list))
  
  I8.4 <- rep(c(0),  each = length(species_list))
  I8.4.1 <- rep(c(0),  each = length(species_list))
  I8.4.2 <- rep(c(0),  each = length(species_list))
  
  I8.5 <- rep(c(0),  each = length(species_list))
  I8.5.1 <- rep(c(0),  each = length(species_list))
  I8.5.2 <- rep(c(0),  each = length(species_list))
  
  I8.6 <- rep(c(0),  each = length(species_list))
  
  #Pollution 
  P9.1 <- rep(c(0),  each = length(species_list))
  P9.1.1 <- rep(c(0),  each = length(species_list))
  P9.1.2 <- rep(c(0),  each = length(species_list))
  P9.1.3 <- rep(c(0),  each = length(species_list))
  
  P9.2 <- rep(c(0),  each = length(species_list))
  P9.2.1 <- rep(c(0),  each = length(species_list))
  P9.2.2 <- rep(c(0),  each = length(species_list))
  P9.2.3 <- rep(c(0),  each = length(species_list))
  
  P9.3 <- rep(c(0),  each = length(species_list))
  P9.3.1 <- rep(c(0),  each = length(species_list))
  P9.3.2 <- rep(c(0),  each = length(species_list))
  P9.3.3 <- rep(c(0),  each = length(species_list))
  P9.3.4 <- rep(c(0),  each = length(species_list))
  
  P9.4 <- rep(c(0),  each = length(species_list))
  
  P9.5 <- rep(c(0),  each = length(species_list))
  P9.5.1 <- rep(c(0),  each = length(species_list))
  P9.5.2 <- rep(c(0),  each = length(species_list))
  P9.5.3 <- rep(c(0),  each = length(species_list))
  P9.5.4 <- rep(c(0),  each = length(species_list))
  
  P9.6 <- rep(c(0),  each = length(species_list))
  P9.6.1 <- rep(c(0),  each = length(species_list))
  P9.6.2 <- rep(c(0),  each = length(species_list))
  P9.6.3 <- rep(c(0),  each = length(species_list))
  P9.6.4 <- rep(c(0),  each = length(species_list))
  
  #Geological Events 
  G10.1 <- rep(c(0),  each = length(species_list))
  G10.2 <- rep(c(0),  each = length(species_list))
  G10.3 <- rep(c(0),  each = length(species_list))
  
  #Climate Change and severe weather 
  C11.1 <- rep(c(0),  each = length(species_list))
  C11.2 <- rep(c(0),  each = length(species_list))
  C11.3 <- rep(c(0),  each = length(species_list))
  C11.4 <- rep(c(0),  each = length(species_list))
  C11.5 <- rep(c(0),  each = length(species_list))
  
  #Other options 
  O12.1 <- rep(c(0),  each = length(species_list))
  
  threat_df <- data.frame(species_list, R1.1, R1.2, R1.3, A2.1, A2.1.1, A2.1.2, A2.1.3, A2.1.4,
                          A2.2, A2.2.1, A2.2.2, A2.2.3, A2.3, A2.3.1, A2.3.2, A2.3.3, A2.3.4, 
                          A2.4, A2.4.1, A2.4.2, A2.4.3, E3.1, E3.2, E3.3, T4.1, T4.2, T4.3, T4.4, 
                          B5.1, B5.1.1, B5.1.2, B5.1.3, B5.1.4, B5.2, B5.2.1, B5.2.2, B5.2.3, B5.2.4, 
                          B5.3, B5.3.1, B5.3.2, B5.3.3, B5.3.4, B5.3.5, B5.4, B5.4.1, B5.4.2, B5.4.3,
                          B5.4.4, B5.4.5, B5.4.6,
                          H6.1, H6.2, H6.3, N7.1, N7.1.1,
                          N7.1.2, N7.1.3, N7.2, N7.2.1, N7.2.2, N7.2.3, N7.2.4, N7.2.5, N7.2.6, N7.2.7,
                          N7.2.8, N7.2.9, N7.2.10, N7.2.11, N7.3, I8.1, I8.1.1, I8.1.2, I8.2, I8.2.1, 
                          I8.2.2, I8.3, I8.4, I8.4.1, I8.4.2, I8.5, I8.5.1, I8.5.2, I8.6, P9.1,P9.1.1,
                          P9.1.2, P9.1.3, P9.2,P9.2.1, P9.2.2, P9.2.3, P9.3, P9.3.1, P9.3.2, P9.3.3, 
                          P9.3.4, P9.4,P9.5, P9.5.1,P9.5.2, P9.5.3, P9.5.4, P9.6,P9.6.1, P9.6.2, P9.6.3, 
                          P9.6.4, G10.1, G10.2, G10.3, C11.1, C11.2, C11.3, C11.4, C11.5)
  return(threat_df)
} #Empty table of threats 
pop_threat_tables <- function(data){
  ### data is a dataframe with the species_list  
  ### as one column and the threat_code as the
  ### other aims to work directly with sp_threats 
  ###and threat_tables 
  threatdf <- threat_table(data$species_list)
  i <- 1
  for (strings in data$threat_code){
    many_strings <- strsplit(strings, split = " ")
    for (elem in many_strings){
      for(code in elem){
        #### Residential 
        if (code == "1.1"){
          threatdf$R1.1[i] <- 1
        } else if (code == "1.2"){
          threatdf$R1.2[i] <- 1
        } else if (code == "1.3"){
          threatdf$R1.3[i] <- 1
          #### Agriculture 
        } else if (code == "2.1"){
          threatdf$A2.1[i] <- 1
        } else if (code == "2.1.1"){
          threatdf$A2.1.1[i] <- 1
        } else if (code == "2.1.2"){
          threatdf$A2.1.2[i] <- 1
        } else if (code == "2.1.3"){
          threatdf$A2.1.3[i] <- 1
        } else if (code == "2.1.4"){
          threatdf$A2.1.4[i] <- 1
          
        } else if (code == "2.2"){
          threatdf$A2.2[i] <- 1
        } else if (code == "2.2.1"){
          threatdf$A2.2.1[i] <- 1
        } else if (code == "2.2.2"){
          threatdf$A2.2.2[i] <- 1
        } else if (code == "2.2.3"){
          threatdf$A2.2.3[i] <- 1
          
        } else if (code == "2.3"){
          threatdf$A2.3[i] <- 1
        } else if (code == "2.3.1"){
          threatdf$A2.3.1[i] <- 1
        } else if (code == "2.3.2"){
          threatdf$A2.3.2[i] <- 1
        } else if (code == "2.3.3"){
          threatdf$A2.3.3[i] <- 1
        } else if (code == "2.3.4"){
          threatdf$A2.3.4[i] <- 1
          
        } else if (code == "2.4"){
          threatdf$A2.4[i] <- 1
        } else if (code == "2.4.1"){
          threatdf$A2.4.1[i] <- 1
        } else if (code == "2.4.2"){
          threatdf$A2.4.2[i] <- 1
        } else if (code == "2.4.3"){
          threatdf$A2.4.3[i] <- 1
          #### Energy 
        } else if (code == "3.1"){
          threatdf$E3.1[i] <- 1
        } else if (code == "3.2"){
          threatdf$E3.2[i] <- 1
        } else if (code == "3.3"){
          threatdf$E3.3[i] <- 1
          
          #### Transportation and service corridors 
        } else if (code == "4.1"){
          threatdf$T4.1[i] <- 1
        } else if (code == "4.2"){
          threatdf$T4.2[i] <- 1
        } else if (code == "4.3"){
          threatdf$T4.3[i] <- 1
        } else if (code == "4.4"){
          threatdf$T4.4[i] <- 1
          #### Biological resource use 
        } else if (code == "5.1"){
          threatdf$B5.1[i] <- 1
        } else if (code == "5.1.1"){
          threatdf$B5.1.1[i] <- 1
        } else if (code == "5.1.2"){
          threatdf$B5.1.2[i] <- 1
        } else if (code == "5.1.3"){
          threatdf$B5.1.3[i] <- 1
        } else if (code == "5.1.4"){
          threatdf$B5.1.4[i] <- 1
          
        } else if (code == "5.2"){
          threatdf$B5.2[i] <- 1
        } else if (code == "5.2.1"){
          threatdf$B5.2.1[i] <- 1
        } else if (code == "5.2.2"){
          threatdf$B5.2.2[i] <- 1
        } else if (code == "5.2.3"){
          threatdf$B5.2.3[i] <- 1
        } else if (code == "5.2.4"){
          threatdf$B5.2.4[i] <- 1
          
        } else if (code == "5.3"){
          threatdf$B5.3[i] <- 1
        } else if (code == "5.3.1"){
          threatdf$B5.3.1[i] <- 1
        } else if (code == "5.3.2"){
          threatdf$B5.3.2[i] <- 1
        } else if (code == "5.3.3"){
          threatdf$B5.3.3[i] <- 1
        } else if (code == "5.3.4"){
          threatdf$B5.3.4[i] <- 1
        } else if (code == "5.3.5"){
          threatdf$B5.3.5[i] <- 1
          
        } else if (code == "5.4"){
          threatdf$B5.4[i] <- 1
        } else if (code == "5.4.1"){
          threatdf$B5.4.1[i] <- 1
        } else if (code == "5.4.2"){
          threatdf$B5.4.2[i] <- 1
        } else if (code == "5.4.3"){
          threatdf$B5.4.3[i] <- 1
        } else if (code == "5.4.4"){
          threatdf$B5.4.4[i] <- 1
        } else if (code == "5.4.5"){
          threatdf$B5.4.5[i] <- 1
        } else if (code == "5.4.6"){
          threatdf$B5.4.6[i] <- 1
          
          #### Human intrustions 
        } else if (code == "6.1"){
          threatdf$H6.1[i] <- 1
        } else if (code == "6.2"){
          threatdf$H6.2[i] <- 1
        } else if (code == "6.3"){
          threatdf$H6.3[i] <- 1
          
          #### Natural system modifications 
        } else if (code == "7.1"){
          threatdf$N7.1[i] <- 1
        } else if (code == "7.1.1"){
          threatdf$N7.1.1[i] <- 1
        } else if (code == "7.1.2"){
          threatdf$N7.1.2[i] <- 1
        } else if (code == "7.1.3"){
          threatdf$N7.1.3[i] <- 1
          
        } else if (code == "7.2"){
          threatdf$N7.2[i] <- 1
        } else if (code == "7.2.1"){
          threatdf$N7.2.1[i] <- 1
        } else if (code == "7.2.2"){
          threatdf$N7.2.2[i] <- 1
        } else if (code == "7.2.3"){
          threatdf$N7.2.3[i] <- 1
        } else if (code == "7.2.4"){
          threatdf$N7.2.4[i] <- 1
        } else if (code == "7.2.5"){
          threatdf$N7.2.5[i] <- 1
        } else if (code == "7.2.6"){
          threatdf$N7.2.6[i] <- 1
        } else if (code == "7.2.7"){
          threatdf$N7.2.7[i] <- 1
        } else if (code == "7.2.8"){
          threatdf$N7.2.8[i] <- 1
        } else if (code == "7.2.9"){
          threatdf$N7.2.9[i] <- 1
        } else if (code == "7.2.10"){
          threatdf$N7.2.10[i] <- 1
        } else if (code == "7.2.11"){
          threatdf$N7.2.11[i] <- 1
          
        } else if (code == "7.3"){
          threatdf$N7.3[i] <- 1
          
          #### Invasive species/disease/introduced genes 
        } else if (code == "8.1"){
          threatdf$I8.1[i] <- 1
        } else if (code == "8.1.1"){
          threatdf$I8.1.1[i] <- 1
        } else if (code == "8.1.2"){
          threatdf$I8.1.2[i] <- 1
          
        } else if (code == "8.2"){
          threatdf$I8.2[i] <- 1
        } else if (code == "8.2.1"){
          threatdf$I8.2.1[i] <- 1
        } else if (code == "8.2.2"){
          threatdf$I8.2.2[i] <- 1
          
          
        } else if (code == "8.3"){
          threatdf$I8.3[i] <- 1
          
        } else if (code == "8.4"){
          threatdf$I8.4[i] <- 1
        } else if (code == "8.4.1"){
          threatdf$I8.4.1[i] <- 1
        } else if (code == "8.4.2"){
          threatdf$I8.4.2[i] <- 1
          
        } else if (code == "8.5"){
          threatdf$I8.5[i] <- 1
        } else if (code == "8.5.1"){
          threatdf$I8.5.1[i] <- 1
        } else if (code == "8.5.2"){
          threatdf$I8.5.2[i] <- 1
          
        } else if (code == "8.6"){
          threatdf$I8.6[i] <- 1
          
          #### Pollution
        } else if (code == "9.1"){
          threatdf$P9.1[i] <- 1
        } else if (code == "9.1.1"){
          threatdf$P9.1.1[i] <- 1
        } else if (code == "9.1.2"){
          threatdf$P9.1.2[i] <- 1
        } else if (code == "9.1.3"){
          threatdf$P9.1.3[i] <- 1
          
          
        } else if (code == "9.2"){
          threatdf$P9.2[i] <- 1
        } else if (code == "9.2.1"){
          threatdf$P9.2.1[i] <- 1
        } else if (code == "9.2.2"){
          threatdf$P9.2.2[i] <- 1
        } else if (code == "9.2.3"){
          threatdf$P9.2.3[i] <- 1
          
        } else if (code == "9.3"){
          threatdf$P9.3[i] <- 1
        } else if (code == "9.3.1"){
          threatdf$P9.3.1[i] <- 1
        } else if (code == "9.3.2"){
          threatdf$P9.3.2[i] <- 1
        } else if (code == "9.3.3"){
          threatdf$P9.3.3[i] <- 1
        } else if (code == "9.3.4"){
          threatdf$P9.3.4[i] <- 1
          
        } else if (code == "9.4"){
          threatdf$P9.4[i] <- 1
          
        } else if (code == "9.5"){
          threatdf$P9.5[i] <- 1
        } else if (code == "9.5.1"){
          threatdf$P9.5.1[i] <- 1
        } else if (code == "9.5.2"){
          threatdf$P9.5.2[i] <- 1
        } else if (code == "9.5.3"){
          threatdf$P9.5.3[i] <- 1
        } else if (code == "9.5.4"){
          threatdf$P9.5.4[i] <- 1
          
        } else if (code == "9.6"){
          threatdf$P9.6[i] <- 1
        } else if (code == "9.6.1"){
          threatdf$P9.6.1[i] <- 1
        } else if (code == "9.6.2"){
          threatdf$P9.6.2[i] <- 1
        } else if (code == "9.6.3"){
          threatdf$P9.6.3[i] <- 1
        } else if (code == "9.6.4"){
          threatdf$P9.6.4[i] <- 1
          
          #### Geological Events
        } else if (code == "10.1"){
          threatdf$G10.1[i] <- 1
        } else if (code == "10.2"){
          threatdf$G10.2[i] <- 1
        } else if (code == "10.3"){
          threatdf$G10.3[i] <- 1
          
          #### Climate Change
        } else if (code == "11.1"){
          threatdf$C11.1[i] <- 1
        } else if (code == "11.2"){
          threatdf$C11.2[i] <- 1
        } else if (code == "11.3"){
          threatdf$C11.3[i] <- 1
        } else if (code == "11.4"){
          threatdf$C11.4[i] <- 1
        } else if (code == "11.5"){
          threatdf$C11.5[i] <- 1
        }
      }
    }
    i <- 1 + i 
  }
  return(threatdf)
} #takes DF from sp_threats and populates empty threat table
 
########
######## Get Data 
########

LC <- rl_sp_category("LC", key = token)
NT <- rl_sp_category("NT", key = token)
VU <- rl_sp_category("VU", key = token)
EN <- rl_sp_category("EN", key = token)
CR <- rl_sp_category("CR", key = token)
EW <- rl_sp_category("EW", key = token)
EX <- rl_sp_category("EX", key = token)

LCC <- LC
NTT <- NT
VUU <- VU
ENN <- EN
CRR <- CR
EWW <- EW
EXX <- EX


lc <- clean_data2(LCC)
nt <- clean_data2(NTT)
vu <- clean_data2(VUU)
en <- clean_data2(ENN)
cr <- clean_data2(CRR)
ew <- clean_data2(EWW)
ex <- clean_data2(EXX)


ew_frogs <- sp_class(ew) 
cr_frogs <- sp_class(cr)
ex_frogs <- sp_class(ex)
en_frogs <- sp_class(en)
# (for the rest of the categories,
# used shortcut and longershortcut functions (GetData1)) 

copyew_frogs <- ew_frogs
copyex_frogs <- ex_frogs 
copycr_frogs <- cr_frogs
copyen_frogs <- en_frogs 
copyvu_frogs <- vu_frogs
copynt_frogs <- nt_frogs 

write.table(copynt_frogs, "/Users/Alexandra_Gonzalez/Desktop/Herpetology/nt_frogs.txt", sep="\t")
write.table(copyex_frogs, "/Users/Alexandra_Gonzalez/Desktop/Herpetology/ex_frogs.txt", sep="\t")
write.table(copyew_frogs, "/Users/Alexandra_Gonzalez/Desktop/Herpetology/ew_frogs.txt", sep="\t")
write.table(copyen_frogs, "/Users/Alexandra_Gonzalez/Desktop/Herpetology/en_frogs.txt", sep="\t")
write.table(copyvu_frogs, "/Users/Alexandra_Gonzalez/Desktop/Herpetology/vu_frogs.txt", sep="\t")
write.table(copylc_frogs, "/Users/Alexandra_Gonzalez/Desktop/Herpetology/lc_frogs.txt", sep="\t")
write.table(copycr_frogs, "/Users/Alexandra_Gonzalez/Desktop/Herpetology/cr_frogs.txt", sep="\t")

cr_threat_count <- sp_threat_count(copycr_frogs)
en_threat_count <- sp_threat_count(copyen_frogs)
vu_threat_count <- sp_threat_count(copyvu_frogs)
ex_threat_count <- sp_threat_count(copyex_frogs)

cr_family <- sp_all_family(copycr_frogs)
en_family <- sp_all_family(copyen_frogs)
vu_family <- sp_all_family(copyvu_frogs)
ex_family <- sp_all_family(copyex_frogs)

########
########
######## Populating Threat/Habitat tables  
########
########


en_habitat <- sp_habitats(en_frogs)
cr_habitat <- sp_habitats(cr_frogs)
vu_habitat <- sp_habitats(vu_frogs)
nt_habitat <- sp_habitats(nt_frogs) 

cr_habitat_table <- pop_habitat_tables(cr_habitat)
en_habitat_table <- pop_habitat_tables(en_habitat)
vu_habitat_table <- pop_habitat_tables(vu_habitat)

cr_habitat_table$category <- "CR"
en_habitat_table$category <- "EN"
vu_habitat_table$category <- "VU"

frogs_danger <- rbind(cr_habitat_table, en_habitat_table, vu_habitat_table)
frogs_danger$category <- as.factor(frogs_danger$category)

frog_data <- select(frogs_danger, -category)
summary(frogs_danger)


#Filter out all of the columns where there are no frog habitats 
query1 <- select(frogs_danger, -F1.1, -F1.2, -Sh3.1, -Sh3.2, -Sh3.3, -G4.2, -W5.15, -W5.16, 
                 -W5.17, -W5.18, -D8.1, -MN9.1, -MN9.2, -MN9.3, -MN9.4, -MN9.5, -MN9.6, 
                 -MN9.7, -MN9.8, -MN9.9, -MO10.1, -MO10.2, -MO10.3,- MO10.4, -M11.1, -M11.2, -M11.3, 
                 -M11.4, -M11.5, -M11.6, -MI12.1, -MI12.2, -MI12.3, -MI12.4, -MI12.5, -MI12.6, 
                 -MI12.7, -MC13.1,- MC13.2, -MC13.3, -AA15.1, -AA15.2, -AA15.3, -AA15.4, -AA15.5,
                 -AA15.6, -AA15.7, -AA15.8, -AA15.9,- AA15.10, -AA15.11, -AA15.12,- AA15.13)
summary(query1)
hist(query1$F1.3)
colnames(query1)


query2 <- apply(query1, 2, count)

#name of frogs 
F1.2 <- sqldf("SELECT * FROM frogs_danger WHERE `F1.3` > 0")
F1.4 <- sqldf("SELECT * FROM frogs_danger WHERE `F1.4` > 0")
F1.5 <- sqldf("SELECT * FROM frogs_danger WHERE `F1.5` > 0")
F1.6 <- sqldf("SELECT * FROM frogs_danger WHERE `F1.6` > 0")
F1.7 <- sqldf("SELECT * FROM frogs_danger WHERE `F1.7` > 0")
F1.8 <- sqldf("SELECT * FROM frogs_danger WHERE `F1.8` > 0")


withoutnames <- select(query1, -species_list, -category)
cor(withoutnames)

#Correlation  
#(Script for creating heat maps taken from our textbook)
colnames(withoutnames)
Cordata <- cor(withoutnames)
Cordata

dataMelt <- melt(Cordata, varnames = c("x","y"), value.name = "Correlation")
dataMelt <- dataMelt [order(dataMelt$Correlation),]
dataMelt

#Correlation Heatmap ~~~
#with the habitat code 
plot3 <- ggplot(dataMelt, aes(x=x, y=y)) + geom_tile (aes(fill = Correlation)) + 
  scale_fill_gradient2(low = muted("red"), mid = "white", high = "blue")+
  labs(title = "Correlation Heat Map of Habitat Types") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  xlab("Habitat types") + 
  ylab("Habitat types")


forest_sub_swamp <- sqldf("SELECT species_list, category, `F1.8` FROM frogs_danger WHERE `F1.8` > 0")
forest_subantarctic <- sqldf("SELECT species_list, category, `F1.3` FROM frogs_danger WHERE `F1.3` > 0")
marine_nec_estuaries <- sqldf("SELECT species_list, category, `MN9.10` FROM frogs_danger WHERE `MN9.10` > 0")

plot1 <- ggplot(data = query1, aes(x = as.factor(F1.8), fill = category)) + geom_bar(position = "dodge")
plot2 <- ggplot(data = query1, aes(x = as.factor(F1.5), fill = category)) + geom_bar(position = "dodge")

# Try to do MCA 
#http://gastonsanchez.com/visually-enforced/how-to/2012/10/13/MCA-in-R/
#install.packages("FactoMineR")
# Code from ^ and changed for my variables

head(query1)
newtea <- select(query1, -species_list)
cats = apply(newtea, 2, function(x) nlevels(as.factor(x)))
mca1 <- MCA(newtea, graph = FALSE) #Doesnt WORK -- every column needs to be a factor

without_species <- select(query1, -species_list)
cats <- apply(without_species, 2, function(x) nlevels(as.factor(x)))
newdata <- apply(without_species, 2, factor)
newdata <- as.data.frame(newdata)
mca1 <- MCA(newdata, graph = FALSE)

#data frame with variable coordinates 
mca1_vars_df <- data.frame(mca1$var$coord, Variable = rep(names(cats), cats))

#data frame with observation coordinates 
mca1_obs_df <- data.frame(mca1$ind$coord)

#plot of variable categories
plot3.9 <- ggplot(data=mca1_vars_df, 
                  aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA plot of Habitat Types")

#logistic 
fit1 <- glm(category ~., data = newdata, family = "binomial")
summary(fit1)



cr_threats<- sp_threats(cr_frogs)
head(cr_threats)
cr_threat_table <- pop_threat_tables(cr_threats)

en_threats <- read.table(file = "en_threats.txt", stringsAsFactors = FALSE)
en_threats<- sp_threats(en_frogs)
head(en_threats)
en_threat_table <- pop_threat_tables(en_threats)


vu_threats <- read.table(file = "vu_threats.txt", stringsAsFactors = FALSE)
head(vu_threats) 
vu_threat_table <- pop_threat_tables(vu_threats)


vu_threat_table$category <- "VU"
en_threat_table$category <- "EN"
cr_threat_table$category <- "CR"

allthreats <- rbind(cr_threat_table, en_threat_table, vu_threat_table)
summary(allthreats)
withoutnames2 <- select(allthreats, -species_list, -category)

#Remove the threats that do not effect frogs 
query_threat<- select(allthreats, -A2.4.1, -B5.1.4, -B5.2.1, -B5.2.3, -B5.4.5,
                      -N7.2.2, -I8.3, -I8.5.2, -I8.6, -P9.6, -P9.6.1, -P9.6.2,
                      -P9.6.3, -P9.6.4, -G10.2)
#Take out all of the descriptive two point decimal system columns
x <- select(query_threat, species_list , R1.1,R1.2, R1.3, A2.1, A2.2,  A2.3, A2.4,  E3.1, E3.2, E3.3,T4.1,
            T4.2, T4.3, T4.4, B5.1 , B5.2, B5.3 , B5.4, H6.1, H6.2, H6.3, N7.1, N7.2, N7.3, I8.1, I8.2,      
            I8.4 , I8.5 ,P9.1, P9.2 , P9.3, P9.4, P9.5, G10.1, G10.3, C11.1, C11.2, C11.3,C11.4, C11.5,category)   

querythreatVU <- sqldf("SELECT * FROM x WHERE category == 'VU' ")
querythreatEN <- sqldf("SELECT * FROM x WHERE category == 'EN' ")
querythreatCR <- sqldf("SELECT * FROM x WHERE category == 'CR' ")

#Take out species list and category to get column sums and make plot
querythreatVU.1 <- select(querythreatVU, -species_list, -category)
name <- colnames(querythreatVU.1)
sum <- colSums(querythreatVU.1)
df <- data.frame(name, sum)
name <- df$name
sum <- df$sum

names2 <- c("Res.Housing", "Res.Commerical", "Res.Tourism", "Agr.crops", "Agr.Wood", 
            "Agr.livestock", "Agr.Marine", "Energy.Oil", "Energy.Mining", "Energy.Renewable",
            "Trans.Roads", "Trans.Utility", "Trans.Shipping", "Trans.Flight", "Bio.Hunt", "Bio.Gather",
            "Bio.Logging", "Bio.Fishing", "Human.Rec", "Human.War", "Human.Work", "Natural.Fire",
            "Natural.Dams", "Natural.Other", "Invasive.species", "Problematic.native", "Problematic.species",
            "Viral.prion", "Pollution.water", "Pollution.Industrial", "Pollution.Agr","Pollution.Garbage", "Pollution.Air",
            "Volacnoes", "Avalanche", "Clim.Habitat", "Clim.droughts", "Clim.Temp", "Clim.Storms",
            "Clim.Other")
df_vu2 <- data.frame(names2, sum)
df_vu <- data.frame(name, sum)

a_vu <- ggplot(data = df_vu2, aes(x = names2, y = sum, fill = names2)) + geom_bar(stat = "identity") + 
  labs(title = "Threats Affecting Vulnerable Amphibians") + theme_minimal() +
  xlab("Threat Type") + ylab("Number of Species") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

querythreatEN.1 <- select(querythreatEN, -species_list, -category)
name <- colnames(querythreatEN.1)
sum <- colSums(querythreatEN.1)
df <- data.frame(name, sum)
name <- df$name
sum <- df$sum
names2 <- c("Res.Housing", "Res.Commerical", "Res.Tourism", "Agr.crops", "Agr.Wood", 
            "Agr.livestock", "Agr.Marine", "Energy.Oil", "Energy.Mining", "Energy.Renewable",
            "Trans.Roads", "Trans.Utility", "Trans.Shipping", "Trans.Flight", "Bio.Hunt", "Bio.Gather",
            "Bio.Logging", "Bio.Fishing", "Human.Rec", "Human.War", "Human.Work", "Natural.Fire",
            "Natural.Dams", "Natural.Other", "Invasive.species", "Problematic.native", "Problematic.species",
            "Viral.prion", "Pollution.water", "Pollution.Industrial", "Pollution.Agr","Pollution.Garbage", "Pollution.Air",
            "Volacnoes", "Avalanche", "Clim.Habitat", "Clim.droughts", "Clim.Temp", "Clim.Storms",
            "Clim.Other")
df_en2 <- data.frame(names2, sum)

b_en <- ggplot(data = df_en2, aes(x = names2, y = sum, fill = names2)) + geom_bar(stat = "identity") + 
  labs(title = "Threats Affecting Endangered Amphibians") + theme_minimal() +
  xlab("Threat Type") + ylab("Number of Species") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

querythreatCR.1 <- select(querythreatCR, -species_list, -category)
name <- colnames(querythreatCR.1)
sum <- colSums(querythreatCR.1)
df <- data.frame(name, sum)
name <- df$name
sum <- df$sum
names2 <- c("Res.Housing", "Res.Commerical", "Res.Tourism", "Agr.crops", "Agr.Wood", 
            "Agr.livestock", "Agr.Marine", "Energy.Oil", "Energy.Mining", "Energy.Renewable",
            "Trans.Roads", "Trans.Utility", "Trans.Shipping", "Trans.Flight", "Bio.Hunt", "Bio.Gather",
            "Bio.Logging", "Bio.Fishing", "Human.Rec", "Human.War", "Human.Work", "Natural.Fire",
            "Natural.Dams", "Natural.Other", "Invasive.species", "Problematic.native", "Problematic.species",
            "Viral.prion", "Pollution.water", "Pollution.Industrial", "Pollution.Agr","Pollution.Garbage", "Pollution.Air",
            "Volacnoes", "Avalanche", "Clim.Habitat", "Clim.droughts", "Clim.Temp", "Clim.Storms",
            "Clim.Other")
df_cr2 <- data.frame(names2, sum)


c_cr <- ggplot(data = df_cr2, aes(x = names2, y = sum, fill = names2)) + geom_bar(stat = "identity") + 
  labs(title = "Threats Affecting Critically Endangered Amphibians") + theme_minimal() +
  xlab("Threat Type") + ylab("Number of Species") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 



rl_habitats("Theloderma asperum", key = token)
