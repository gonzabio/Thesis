#Data Analysis 
#Last updated: Feb 6, 2018 

#Purpose: writing code for final thesis project. 
#Bring suggested data analysis to Prof. Sun on Friday 
#Redownload the newest IUCN data (because it was recently updated)


library("profvis") 
library("rredlist")
library(RSQLite)
library(sqldf)
library(dplyr)
require(ggplot2)
require(reshape2)
require(scales)
token <- "cf51b7524f618f5b23220e687b70c01d0d240cc82d179cf2c5b08b70fdfb83d4" 

####


#1 Get data by category using rl_sp_category 
LC <- rl_sp_category("LC", key = token)
NT <- rl_sp_category("NT", key = token)
VU <- rl_sp_category("VU", key = token)
EN <- rl_sp_category("EN", key = token)
CR <- rl_sp_category("CR", key = token)
EW <- rl_sp_category("EW", key = token)
EX <- rl_sp_category("EX", key = token)
DD <- rl_sp_category("DD", key = token)

#2 Use a function, species_only, to pull out only only species 
species_only <- function(organism_list){
  #Parameters: 
  #organism_list: class = list. take the output from rl_sp_category 
                  #and incorporate into this function. 
  #Intent: takes a list of taxonid, scientific name, subspecies, subpopulation 
                  #and returns ONLY a list of species 
  df<- as.data.frame(organism_list)
  df[is.na(df)] <- 0 #setting everything that is NA = Zero 
  only_species <- subset(df, (df$result.subspecies == 0) & 
                           (df$result.rank == 0) & (df$result.subpopulation == 0))
                  #Q: Is it necessary to filter out the subspecies,
                      #ranks and subpopulation like this?
                  #Should it just be pulling out the scientific_name? 
                  #A: One reason to filter out the subspecies, ranks, subpopulations 
                      #is a large majority of them dont have threats associated with them
                      #Tried Veratrum mengtzeanum and Veratrum mengtzeanum ssp. mengtzeanum
  only_species <- only_species$result.scientific_name
  return(only_species)
}

        #Species only data: 
LC_species <- species_only(LC)
NT_species <- species_only(NT)
VU_species <- species_only(VU)
EN_species <- species_only(EN)
CR_species <- species_only(CR)
EW_species <- species_only(EW)
EX_species <- species_only(EX)
DD_species <- species_only(DD)


#3 Created function to pull out subspecies that aren't included in the broader list 
subspecies <- function(organism_list){
  #Parameters: 
  #organism_list: class = list. take the output from rl_sp_category 
  #and incorporate into this function. 
  #Intent: dataframe of subspecies (this shows which organisms are filtered 
            #out through species_only)
  df<- as.data.frame(organism_list)
  df[is.na(df)] <- 0 #setting everything that is NA = Zero 
  subspecies <- subset(df, (df$result.subspecies != 0))
  #rank <- subset(df, df$result.rank !=0)
  #subpopulaiton <- subset(df, df$result.rank !=0)
  subspecies <- subspecies$result.scientific_name
  return(subspecies)
}

        #Subspecies data: 
LCsubspecies <- subspecies(LC)
NTsubspecies <- subspecies(NT)
VUsubspecies <- subspecies(VU)
ENsubspecies <- subspecies(EN)
CRsubspecies <- subspecies(CR)
EWsubspecies <- subspecies(EW)
EXsubspecies <- subspecies(EX)
DDsubspecies <- subspecies(DD)

#4 copying over sp_class function, pulling out class = amphibai 
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


#4 cont. created sp_class2 - this is the same as sp_class,
  #however, this one lets you put in tax_class as the parameter 
  #which opens it up to other taxonomy classes (NOT SURE IF THIS WORKS)

sp_class2 <- function (species_list, tax_class){
  #Des.: Traverses through a list, and pulls out Class = AMPHIBIA
  #Input: species_list is any list of species w/ and w/o amphibians 
        #tax_class - in quotes the class you're looking for 
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
    }else if (tax == tax_class){
      amphibian_list[i] <- species
    }
    i <- i+1
  }
  amph <- amphibian_list[amphibian_list != ""]
  return(amph)
}  


#5 Get amphibian data - need to start making this run 
# 
# LC_amphibians2 <- sp_class(LC_species[1:1000])
# LC_amphibians3 <- sp_class(LC_species[1001:2000])
# LC_amphibians4 <- sp_class(LC_species[2001:2010])
# LC_amphibains5 <- sp_class(LC_species[2011:2030])
# LC_amphibians6 <- sp_class(LC_species[2031:2100])
# LC_amphibians7 <- sp_class(LC_species[2101:2180])
# LC_amphibians8 <- sp_class(LC_species[2181:2250])
# LC_amphibians9 <- sp_class(LC_species[2251:2400])
# LC_amphibains10 <- sp_class(LC_species[2401:2700])
# LC_amphibians11 <- sp_class(LC_species[2701:3100])
# LC_amphibains12 <- sp_class(LC_species[3101:3300])
# LC_amphibains13 <- sp_class(LC_species[3301:3600])
# LC_amphibians14 <- sp_class(LC_species[3601:3800])
# LC_amphibians15 <- sp_class(LC_species[4000:4300])
# LC_amphibains16 <- sp_class(LC_species[4301:4600])
# LC_amphibians17 <- sp_class(LC_species[4601:4900])
# LC_amphibians18 <- sp_class(LC_species[4901:5200])
# LL_amphibians19 <- sp_class(LC_species[5201:5500])
# LC_amphibians20 <- sp_class(LC_species[5501:6000])
# LC_amphibians21 <- sp_class(LC_species[6001:6500])
# LC_amphibians22 <- sp_class(LC_species[6501:7000])
# LC_amphibains23 <- sp_class(LC_species[7001:7500])
# LC_amphibians24 <- sp_class(LC_species[7501:8200])
# LC_amphibians25 <- sp_class(LC_species[8201:9000])
# LC_amphibians26 <- sp_class(LC_species[9001:9800])
# ---> FINISH #LC_amphibians27 <- sp_class(LC_species[9801:1700])




#Error: lexical error: invalid char in json text.
#<html lang="en"> <head> <!-- WI
#(right here) ------^ 

    #Create on a function that will traverse through old list of LC before filtered - 
    #pick out what is the same...and then show what is different
##LC2017 <- LCC

getwd()
setwd("/Users/Alexandra_Gonzalez/Downloads")
iucn_data <- read.csv("export-89207.csv", header = TRUE)
head(iucn_data)

setwd("/Users/Alexandra_Gonzalez/Downloads/AmphiBIO_v1")
getwd()
amphiBIO <- read.csv("AmphiBIO_v1.csv", header = TRUE)
head(amphiBIO)


