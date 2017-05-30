#Making Graphs for Poster
#Alexandra Gonzalez 


library("profvis") #profvis(sp_taxonomy(CR_names_10)) #how you check how long a function is 
library("rredlist")
library("dplyr")
token <- "cf51b7524f618f5b23220e687b70c01d0d240cc82d179cf2c5b08b70fdfb83d4" 

cr_threat_count <- sp_threat_count(copycr_frogs)
cr_family <- sp_all_family(copycr_frogs)
rl_habitats("Adenomus dasi", key = token)


vu_threat_count <- sp_threat_count(copyvu_frogs)
vu_family <- sp_all_family(copyvu_frogs)

en_threat_count$threat_count <- as.numeric(en_threat_count$threat_count)
en_family$family_list <- as.factor(en_family$family_list)

#Threat type: 

#1 Residential and Commerical Development   #1_res_development
#2 Agriculture and Aquaculture              #2_agriculture
#3 Energy production and Mining             #3_energy 
#4 Transportation * service corridors       #4_transport
#5 Biological Resource use                  #5_bio_resource 
#6 Human intrusions and disturbance         #6_human_intrusions
#7 Natural System Modifications             #7_natural_sys
#8 Invasion and other problematic speices, genes and disease    #8_invasion
#9 Pollution                                #9_pollution
#10 geological events                       #10_geo
#11 climate changes and severe weather      #11_climate
#12 other options                           #12_other


threat_list <- function(species_list){
  for (species in species_list){
    result <- rl_threats(species, key = token)
    codelist <- result$result$code
    for (num in codelist){
      if (num == "2.3"){
        print (species)
      }
    }
  }
}

test <- c("Alsodes barrioi", "Lithobates chiricahuensis")  
threat_list(test)

all_threats <- c("1_res_development", "2_agriculture", "3_energy", "4_transport",
                "5_bio_resource", "6_human_intrusions", "7_natural_sys", "8_invasion",
                "9_pollution","10_geo", "11_climate", "12_other")

cr_data <- cr_family
head(cr_data)
cr_data$category <- "CR"
cr_data$threat_count <- cr_threat_count$threat_count

combind_df$family_list <- as.factor(combind_df$family_list)

vu_data <- vu_family
vu_data$threat_count <- vu_threat_count$threat_count
head(vu_data)
vu_data$category <- "VU"


en_data <- en_family
en_data$threat_count <- en_threat_count$threat_count
en_data$category <- "EN"


combined_df <- rbind(cr_data, vu_data, en_data)
head(en_data)
tail(combind_df)

combind_df$family_list <- as.character(combined_df$family_list)
en_data$family_list <- as.character(en_data$family_list)


new_df <- rbind(combined_df, en_data)
summary(combind_df)
summary(en_data)

combind_df$threat_count <- as.numeric(combined_df$threat_count)
combind_df$category <- as.factor(combined_df$category)

barplot(combind_df$category, combind_df$threat_count)



vu_data
en_data$threat_count <- as.character(en_data$threat_count)
cr_data


#########################################################

lc_data_summary <- lc_threats
lc_data_summary$family <- "na"
lc_data_summary$category <- "LC"
head(lc_data_summary)
lc_data_summary$species_list <- as.character(lc_data_summary$species_list)
lc_data_summary$category <- as.factor(lc_data_summary$category)
lc_data_summary <- select(lc_data_summary, -family)


newdf3 <- rbind(newdf2, lc_data_summary, nt_data)
summary(newdf3)
newdf3$threat_count <- as.numeric(newdf3$threat_count)
library(dplyr)
newdf2 <- select(newdf, - family_list)

nt_threat <- read.table(file = "nt_threat_count.txt")
nt_data <- nt_threat
nt_data$category <- "NT"
nt_data$species_list <- as.character(nt_data$species_list)
nt_data$category <- as.factor(nt_data$category)


newdf$threat_count <- as.numeric(newdf$threat_count)
newdf$category <- as.factor(newdf$category)
newdf$family_list <- as.factor(newdf$family_list)

plot(newdf$family_list,newdf$threat_count)
library(ggplot2)

newdf3$category4 <- factor(newdf3$category, levels = newdf3$category[order(newdf3$threat_count)])

df$threat_count <- as.numeric(df$threat_count)

#CARD NUMBER OF THREATS BY RED LIST CATEGORY 
g <- ggplot(data = df,(aes (x = category, y = threat_count, fill = category))) 
g + geom_boxplot() + labs(title = "Number of Threats by Red List Category") +
  scale_fill_manual(values = c("green3", "olivedrab2", "yellow","orangered", "red3" ), 
                    name = c(""), 
                    labels = c("Least Concern", "Near Threatened", "Vulnerable", 
                               "Endangered", "Critically Endangered")) +
  xlab("Red List Cateogry") + ylab("Number of Threats") +
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20)) +
  theme(text = element_text(size = 23)) + 
  theme_minimal(base_size = 20)







#CARD DENSITY DISTRIBUTION NUMBER OF THREATS 
h <- ggplot(df, aes(x = threat_count, colour = category)) + geom_density(size = 0.75) +
  scale_x_continuous(name = "Number of Threats") + 
  scale_y_continuous(name = "Density") + 
  scale_color_manual(values = c("green3", "olivedrab2", "yellow","orangered", "red3" ), 
                     name = c(""), 
                     labels = c("Least Concern", "Near Threatened", "Vulnerable", 
                                "Endangered", "Critically Endangered")) + 
  theme_minimal(base_size = 20) + 
  labs(title = "Density Distribution of the Number of Threats")




#PIE CHART with the breakdown of everything 
summary(df)
#LC: 2485
#NT: 399
#VU: 670 
#EN: 852
#CR: 546

x <- aov(df$threat_count ~ df$category)
summary(x)
TukeyHSD(x)

h + scale_fill_manual(values = c("green3", "olivedrab2", "yellow","orangered", "red3" ), 
                    name = c(""), 
                    labels = c("Least Concern", "Near Threatened", "Vulnerable", 
                               "Endangered", "Critically Endangered")) 
  
c1 <- ggplot(data = liverdata2, aes(x = sgpt, fill = drink_groups))
c1 + geom_histogram(binwidth = 15) + facet_grid(. ~ drink_groups) + labs(title = "SGPT by Drink Groups")



lc_data2 <- lc_data_summary
nt_data2 <- nt_data
vu_data2 <- select(vu_data, - family_list)
en_data2 <- select(en_data, - family_list)
cr_data2 <- select(cr_data, - family_list)

df <- rbind(lc_data2, nt_data2, vu_data2, en_data2, cr_data2)



vu_data$family_list <- as.factor(vu_data$family_list)
vu_data$threat_count <- as.numeric(vu_data$threat_count)

en_data$family_list <- as.factor(en_data$family_list)
en_data$threat_count <- as.numeric(en_data$threat_count)

cr_data$family_list <- as.factor(cr_data$family_list)
cr_data$threat_count <- as.numeric(cr_data$threat_count)


#no hangup command --- doesnt matter if its asleep  (run it in the background)

copyew_frogs <- ew_frogs
copyex_frogs <- ex_frogs 
copycr_frogs <- cr_frogs
copyen_frogs <- en_frogs 
copyvu_frogs <- vu_frogs
copynt_frogs <- nt_frogs 



head(df)
library(ggplot2)
bp <- ggplot(data = df, aes(x = group, y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity") + 
  labs(title = "Threatened Species by Red List Category") +
  xlab("Red List Category") + 
  ylab("Number of Species") 



pie <- bp + coord_polar("y")



rl_habitats("Theloderma asperum", key = token)

rl_threats




