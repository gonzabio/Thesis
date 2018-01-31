#Data Analysis 

#Purpose: write code for FINAL thing. Need to clean up the data to be able to do 
#data analysis. See what type of data analysis I can do 


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

