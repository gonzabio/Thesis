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








#######Up until this point, ^^^ trying to download data using 2 second method 
####### SLOW AS HELL 
####### downloaded/imported data straight from IUCN and amphiBIO and instead am going to use
####### the functions created to include threat analyses to make a comprehensive view 
            #IDEA: consider making a function that automatically downloads IUCN and amphiBIO 
            #data and joins it with the threat data that I am working on accumulating 
            #so researchers can have table with combined info off the jump 
#IUCN
getwd()
setwd("/Users/Alexandra_Gonzalez/Downloads")
iucn_data <- read.csv("export-89207.csv", header = TRUE)
head(iucn_data)

#AmphiBIO 
  #https://www.nature.com/articles/sdata2017123 (scroll down to Data Records)
setwd("/Users/Alexandra_Gonzalez/Downloads/AmphiBIO_v1")
getwd()
amphiBIO <- read.csv("AmphiBIO_v1.csv", header = TRUE)
head(amphiBIO)
head(amphiBIO)
head(iucn_data)
amphiBIO <- select(amphiBIO, -Order, -Family, -Genus)
amphiBIO$a_Species <- amphiBIO$Species
amphiBIO <- select(amphiBIO, -Species)


class(amphiBIO$a_Species)
class(iucn_data$Species)

#IUCN data seperates species and genus. AmphibBIO has them together. Code to create
#a new column to make species in IUCN_Data = Genus + species 
new_species <- data.frame(A = iucn_data$Genus, B = iucn_data$Species)
iucn_data$new_species <- paste(new_species$A, new_species$B, sep=" ")
class(iucn_data$new_species)
iucn_data$new_species <- as.factor(iucn_data$new_species)

#Joins both datasets 
library(sqldf)
combined_df <- sqldf("SELECT * FROM iucn_data INNER JOIN amphiBIO ON amphiBIO.a_Species = iucn_data.new_species")
summary(combined_df) 
  #Joining IUCN and AmphiBIO data together 

Missing <- setdiff(iucn_data$new_species, amphiBIO$a_Species)
  #Finding inconsistencies between them. ^^ These 700 + are missing from running total 
  #because they don't match 

ifelse(iucn_data$new_species == test$new_species, row.names(iucn_data),0 )
          #tomorrow/ or next week - come up with a solid way of knowing which
          #info came from where (i.e. of the missing amphibains that aren't in BOTH
          #IUCN and AmphiBIO...which database is lacking???)




#Next Steps: 
  #Clean data (Make columns nice, rename columns, add functions)
  #logistic and linear regressions (?)
  #Add threats

#Practice linear regression 
      #LM on body size 
linearmod <- lm(combined_df$Body_mass_g ~ combined_df$Offspring_size_max_mm, data = combind_df)
summary(linearmod)


lm1 <- lm(Body_size_mm ~ Offspring_size_min_mm + 
                Reproductive_output_y + Litter_size_max_n, data = combined_df)
summary(lm1)
      #Results: R  = 0.069. 
      #Lots of data is being delted because of missingness? 

lm2 <- lm(combined_df$Body_size_mm ~ combined_df$Family + combined_df$Genus + combined_df$Red.List.status +
          combined_df$Population.trend)
summary(lm2)
plot(lm2)

lm3 <- lm(combined_df$Body_size_mm ~ combined_df$Population.trend + 
            combined_df$Red.List.status + combined_df$Genus)
summary(lm3)

lm4 <- lm(combined_df$Body_size_mm ~ combined_df$Genus)
summary(lm4)
      #Results: 
      #Genus has a relationship with body_size (unsurprisingly) 
lm5 <- glm(combined_df$Population.trend ~ combined_df$Body_mass_g + combined_df$Body_size_mm)

################

#GET THREAT DATA 

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


iucn_threats1 <- sp_threats(combined_df$new_species[1:1000])
iucn_threats2 <- sp_threats(combined_df$new_species[1001:2000])
iucn_threats3 <- sp_threats(combined_df$new_species[2001:3000])
iucn_threats4 <- sp_threats(combined_df$new_species[3001:4000])
iucn_threats5 <- sp_threats(combined_df$new_species[4001:5000])
iucn_threats6 <- sp_threats(combined_df$new_species[5001:5847])
iucn_threats7 <- sp_threats(combined_df$new_species[5848:5857])

threat_code <- rbind(iucn_threats1, iucn_threats2, iucn_threats3, iucn_threats4,
                 iucn_threats5, iucn_threats6, iucn_threats7)

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

threat_data <- pop_threat_tables(threat_code)
summary(threat_data)

threat_data2 <- select(threat_data, -A2.4.1, -B5.1.4, -B5.2.1, -B5.2.3, -B5.4.2, -B5.4.5,
                     -N7.2.2, -I8.3, -P9.6, -P9.6.1, -P9.6.2,
                      -P9.6.3, -P9.6.4, -G10.2)

summary(threat_data2)

library(dplyr)

#Change to factor 
combined_df$Phylum <- as.factor(combined_df$Phylum)
combined_df$Class <- as.factor(combined_df$Class)
combined_df$Order <- as.factor(combined_df$Order)
combined_df$Family <- as.factor(combined_df$Family)
combined_df$Genus <- as.factor(combined_df$Genus)
combined_df$Infraspecific.rank <- as.factor(combined_df$Infraspecific.rank)


test <- full_join(combined_df, threat_data2)
