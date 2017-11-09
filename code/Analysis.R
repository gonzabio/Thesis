#Data analysis 
#May 29, 2017 

#FIGURE OUT WTF IS HAPPENING HERE 

token <- "cf51b7524f618f5b23220e687b70c01d0d240cc82d179cf2c5b08b70fdfb83d4" 
library(rredlist)
library(RSQLite)
library(sqldf)
library(dplyr)
library(ggplot2)
####HABITAT 

#From Previous script (Already saved on local desktop): 
#(Demonstrating how I get habitat/habitat_table data )
en_habitat <- sp_habitats(en_frogs)
cr_habitat <- sp_habitats(cr_frogs)
vu_habitat <- sp_habitats(vu_frogs)
nt_habitat <- sp_habitats(nt_frogs) 
lc_habitat <- sp_habitats(lc_frogs)

cr_habitat_table <- pop_habitat_tables(cr_habitat)
en_habitat_table <- pop_habitat_tables(en_habitat)
vu_habitat_table <- pop_habitat_tables(vu_habitat)
nt_habitat_table <- pop_habitat_tables(nt_habitat)
lc_habitat_table <- pop_habitat_tables(lc_habitat)

all <- rbind(cr_habitat_table, en_habitat_table, vu_habitat_table, nt_habitat_table, lc_habitat_table)
summary(all)
all$category <- as.factor(all$category)


reduced_table  <- select(all,    -W5.16, 
                 -W5.17, -W5.18,  -MN9.1, -MN9.2, -MN9.3, -MN9.4, -MN9.5, -MN9.6, 
                 -MN9.7, -MN9.8, -MN9.9, -MO10.1, - MO10.4, -M11.1, -M11.2, -M11.3, 
                 -M11.4, -M11.5, -M11.6, -MI12.1, -MI12.2, -MI12.3, -MI12.4,  -MI12.6, 
                 -MI12.7, -MC13.1,- MC13.2,  -AA15.1, -AA15.2, -AA15.3, -AA15.4, -AA15.5,
                 -AA15.6, -AA15.7, -AA15.8, -AA15.9,- AA15.10, -AA15.11, -AA15.12,- AA15.13)

#Endangered/Critically Engagered/Vulnerable DO NOT have amphibians in the following locations that LC/NT do: 
#In other words, once I added LC/NT there were amphibians listed here 
# -F1.1, -F1.2,
# -Sh3.1, -Sh3.2, -Sh3.3
# -G4.2,
# -W5.15, 
# -D8.1,
#  -MO10.2, -MO10.3,
# -MI12.5,
# -MC13.3,

summary(reduced_table)

####### THREATS 
### Getting THREAT DATA 

cr_threat_table <- pop_threat_tables(cr_threats)
en_threat_table <- pop_threat_tables(en_threats)
vu_threat_table <- pop_threat_tables(vu_threats)
nt_threat_table <- pop_threat_tables(nt_threats)
lc_threat_table <- pop_threat_tables(lc_threats)

# write.table(cr_threat_table, "/Users/Alexandra_Gonzalez/Desktop/Herpetology/cr_threat_table.txt", sep="\t")
# write.table(en_threat_table, "/Users/Alexandra_Gonzalez/Desktop/Herpetology/en_threat_table.txt", sep="\t")
# write.table(vu_threat_table, "/Users/Alexandra_Gonzalez/Desktop/Herpetology/vu_threat_table.txt", sep="\t")
# write.table(nt_threat_table, "/Users/Alexandra_Gonzalez/Desktop/Herpetology/nt_threat_table.txt", sep="\t")
# write.table(lc_threat_table, "/Users/Alexandra_Gonzalez/Desktop/Herpetology/lc_threat_table.txt", sep="\t")





cr_threat_table$category <- "CR"
en_threat_table$category <- "EN"
vu_threat_table$category <- "VU"
nt_threat_table$category <- "NT"
lc_threat_table$category <- "LC"

allthreats2 <- rbind(cr_threat_table, en_threat_table, vu_threat_table, nt_threat_table, lc_threat_table)
allthreats2$category <- as.factor(allthreats2$category)
summary(allthreats2)



#Remove the threats that do not effect frogs 
query_threat<- select(allthreats, -A2.4.1, -B5.1.4, -B5.2.1, -B5.2.3, -B5.4.5,
                      -N7.2.2, -I8.3, -I8.5.2, -I8.6, -P9.6, -P9.6.1, -P9.6.2,
                      -P9.6.3, -P9.6.4, -G10.2) 
                      #these threats are for endagered, vulnerable, and critically endangered
                      #do not include nt or lc 
#Differnece: -B5.1.4

query_threat2 <- select(allthreats2, -A2.4.1, -B5.2.1, -B5.2.3, -B5.4.5, -N7.2.2, 
                        -I8.3, -I8.6, -P9.6, -P9.6.1, -P9.6.2,-P9.6.3, -P9.6.4,
                        -G10.2)
#Conclusion: Threats between all categories and only the endangered ones are very similar. 
            #There really isnt a differnce between them. 

#Analysis by habitat type
forest <- sqldf("SELECT species_list, category,`F1.1`, `F1.2`,`F1.3`,`F1.4`,`F1.5`, `F1.6`, `F1.7`, `F1.8` FROM 
                reduced_table WHERE `F1.1` > 0 OR `F1.2` > 0
                OR `F1.3` > 0 OR `F1.4` > 0
                OR `F1.5` > 0 OR `F1.6` > 0
                OR `F1.7` > 0 OR `F1.8` > 0")

savanna <- sqldf("SELECT species_list, category,`Sa2.1`, `Sa2.2` FROM 
                reduced_table WHERE `Sa2.1`>0 OR `Sa2.2`>0")

shrubland <- sqldf("SELECT species_list, category,`Sh3.1`, `Sh3.2`, `Sh3.3`,
                   `Sh3.4`, `Sh3.5`, `Sh3.6`, `Sh3.7`, `Sh3.8` FROM reduced_table 
                   WHERE `Sh3.1` > 0 OR `Sh3.2`>0 
                   OR `Sh3.3` > 0 OR `Sh3.4`>0
                   OR `Sh3.5` > 0 OR `Sh3.6`>0
                   OR `Sh3.7` > 0 OR `Sh3.8`>0")
nonames <- select(reduced_table, - species_list, -category)
sums <- colSums(nonames)

##### STILL NEED TO ADD THE REST OF EM ^^^ 

#Plot the amount of threats in each one 
library(ggplot2)
forest2 <- select(forest, -category, -species_list)
x <- colSums(forest2)
x <- as.data.frame(x)

names <- colnames(forest2)
sum <- colSums(forest2)

df <- data.frame(names, sum)
names <- df$names
sums <- df$sum
df <- data.frame(names, sums)

plot <- ggplot(data = df, aes(x = names, y = sums)) + geom_bar(stat = "identity")


colnames(forest)
forest$total <- forest$F1.1 + forest$F1.2 + forest$F1.3 + forest$F1.4 + 
                forest$F1.5 + forest$F1.6 + forest$F1.7 + forest$F1.8


forest_2 <- sqldf("SELECT * FROM forest WHERE total > 1 ORDER BY category")

############### ~ to find insights 
query1 <- sqldf("SELECT * FROM query_threat2 WHERE `R1.1`>0 AND `R1.2`>0 AND `R1.3` >0")
rl_threats(name = "Ambystoma granulosum", key = token)

query_threat2




####FROM PREVIOUS SCRIPT 

#Remove the threats that do not effect frogs 
query_threat<- select(allthreats2, -A2.4.1, -B5.1.4, -B5.2.1, -B5.2.3, -B5.4.5,
                      -N7.2.2, -I8.3, -I8.5.2, -I8.6, -P9.6, -P9.6.1, -P9.6.2,
                      -P9.6.3, -P9.6.4, -G10.2)

#Take out all of the descriptive two point decimal system columns
x <- select(query_threat, species_list , R1.1,R1.2, R1.3, A2.1, A2.2,  A2.3, A2.4,  E3.1, E3.2, E3.3,T4.1,
            T4.2, T4.3, T4.4, B5.1 , B5.2, B5.3 , B5.4, H6.1, H6.2, H6.3, N7.1, N7.2, N7.3, I8.1, I8.2,      
            I8.4 , I8.5 ,P9.1, P9.2 , P9.3, P9.4, P9.5, G10.1, G10.3, C11.1, C11.2, C11.3,C11.4, C11.5,category)   

querythreatVU <- sqldf("SELECT * FROM x WHERE category == 'VU' ")
querythreatEN <- sqldf("SELECT * FROM x WHERE category == 'EN' ")

querythreatVU2 <- sqldf("SELECT * FROM query_threat WHERE category == 'VU' ")
quertthreatVU2 <- select(querythreatVU2, -category, -species_list)
colSums(quertthreatVU2)

querythreatEN2 <- sqldf("SELECT * FROM query_threat WHERE category == 'EN' ")
quertthreatEN2 <- select(querythreatEN2, -category, -species_list)
colSums(quertthreatEN2)

querythreatCREN2 <- sqldf("SELECT * FROM query_threat WHERE category == 'CR' ")
quertthreatCREN2 <- select(querythreatCREN2, -category, -species_list)
colSums(quertthreatCREN2)

querythreatCR <- sqldf("SELECT * FROM x WHERE category == 'CR' ")
querythreatNT <- sqldf("SELECT * FROM x WHERE category == 'NT' ")
querythreatLC <- sqldf("SELECT * FROM x WHERE category == 'LC' ")



querythreatLC2 <- sqldf("SELECT * FROM query_threat WHERE category == 'LC' ")
quertthreatLC2 <- select(querythreatLC2, -category, -species_list)
colSums(quertthreatLC2)




#Take out species list and category to get column sums and make plot

#VU plot 
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
  labs(title = "Threats Affecting 'Vulnerable' Amphibians") + theme_minimal() +
  xlab("Threat Type") + ylab("Number of Species") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
a_vu <- a_vu + guides(fill = guide_legend(title = "Threat Type"))


#EN Plot
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
  labs(title = "Threats Affecting 'Endangered' Amphibians") + theme_minimal() +
  xlab("Threat Type") + ylab("Number of Species") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
b_en <- b_en + guides(fill = guide_legend(title = "Threat Type"))

#CR Plot
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
  labs(title = "Threats Affecting 'Critically Endangered' Amphibians") + theme_minimal() +
  xlab("Threat Type") + ylab("Number of Species") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
c_cr <- c_cr + guides(fill = guide_legend(title = "Threat Type"))

#NT Plot 
querythreatNT.1 <- select(querythreatNT, -species_list, -category)
name <- colnames(querythreatNT.1)
sum <- colSums(querythreatNT.1)
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
df_nt2 <- data.frame(names2, sum)


d_nt <- ggplot(data = df_nt2, aes(x = names2, y = sum, fill = names2)) + geom_bar(stat = "identity") + 
  labs(title = "Threats Affecting 'Near Threatened' Amphibians") + theme_minimal() +
  xlab("Threat Type") + ylab("Number of Species") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
d_nt <- d_nt + guides(fill = guide_legend(title = "Threat Type"))

#LC Plot 
querythreatLC.1 <- select(querythreatLC, -species_list, -category)
name <- colnames(querythreatLC.1)
sum <- colSums(querythreatLC.1)
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
df_lc2 <- data.frame(names2, sum)

#Threats affecting least concern amphibains (bar graph - not good)
e_lc <- ggplot(data = df_lc2, aes(x = names2, y = sum, fill = names2)) + geom_bar(stat = "identity") + 
  labs(title = "Threats Affecting 'Least Concern' Amphibians") + theme_minimal() +
  xlab("Threat Type") + ylab("Number of Species") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
e_lc <- e_lc + guides(fill = guide_legend(title = "Threat Type"))










