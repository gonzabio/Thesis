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

#2 Use a function, species_only, to pull out only only species 

species_only <- function(organism_list){
  #organism list: class = list. take the output from rl_sp_category 
                  #and incorporate into this function. 
  #Intent: takes a list of taxonid, scientific name, subspecies, subpopulation 
                  #and returns ONLY a list of species 
  df<- as.data.frame(organism_list)
  df[is.na(df)] <- 0
  only_species <- subset(df, (df$result.subspecies == 0) & 
                           (df$result.rank == 0) & (df$result.subpopulation == 0))
                  #Q: Is it necessary to filter out the subspecies,
                      #ranks and subpopulation like this?
                  #Should it just be pulling out the scientific_name? 
  only_species <- only_species$result.scientific_name
  return(only_species)
}

