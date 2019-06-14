#AG: MAY 2 
#AMPHBIO + IUCN TOGETHER 



#Download general IUCN data 
iucn_data <- read.csv("export-89505.csv", header = TRUE)
  new_species <- data.frame(A = iucn_data$Genus, B = iucn_data$Species)
  iucn_data$new_species <- paste(new_species$A, new_species$B, sep=" ")
  #differences in IUCN data: 
  #subsetting <- setdiff(iucn_data$new_species, SE_Asia_Amphibians$species_list)
  # match(subsetting, iucn_data$new_species)
  # subsetting <- as.data.frame(subsetting)


library(dplyr)
library(sqldf)
library(rredlist)

#############
no_data_deficient <- filter(iucn_data, iucn_data$Red.List.status != "DD" & iucn_data$Red.List.status != "EW" & iucn_data$Red.List.status != "EX")
  #remove all DD, EX, EW because logisitc regression only needs EN, CR, LC, NT, VU

se_data1 <- threat_details(no_data_deficient$new_species[1:100])
se_data2 <- threat_details(no_data_deficient$new_species[101:200])
se_data3 <- threat_details(no_data_deficient$new_species[201:300])
se_data4 <- threat_details(no_data_deficient$new_species[301:400])
se_data5 <- threat_details(no_data_deficient$new_species[401:500])
se_data6 <- threat_details(no_data_deficient$new_species[501:600])
se_data7 <- threat_details(no_data_deficient$new_species[601:700])
se_data8 <- threat_details(no_data_deficient$new_species[701:800])
se_data8 
head(se_data_all)
se_data_all <- rbind(se_data1, se_data2, se_data3, se_data4, se_data5,
                     se_data6, se_data7, se_data8)


se_all_codes <- threat_code(se_data_all)
head(se_all_codes)
se_all_tables <- threat_tables(se_all_codes)
            #collected threat data for ALL IUCN species except DD, EX, EW 
            #AmphiBIO data has not been matched at this point 
head(se_all_tables)
setwd("/Users/Alexandra_Gonzalez/Downloads/AmphiBIO_v1")
amphiBIO <- read.csv("AmphiBIO_v1.csv", header = TRUE)
            #6750 rows - need to filter out only the amphibians in south east asia (aka se_all_tables) 
            #se_all_tables contains SE Asian amphibain threats
head(amphiBIO)
amph_threats <- sqldf("SELECT * FROM se_all_tables INNER JOIN amphiBIO ON amphiBIO.Species = se_all_tables.species_list")
            #se_all_tables went from 777 species to 645 species. This is where there is a match between
            #se_all_tables and amphiBIO. amph_threats doesnt have Red List Status data. Need to join original
            #IUCN general info (like population status, year assessed, etc.) to this dataframe. 

a_thre_iucn <- sqldf("SELECT * FROM amph_threats INNER JOIN iucn_data ON iucn_data.new_species = amph_threats.species_list")
            #645 rows...the same as amph_threats

#Remove zero variance, high correlations 
a_subset <- a_thre_iucn[,1:45]
head(a_subset)
summary(a_subset)
remove_ids <- apply(a_subset, 2 , function(x) sd(x, na.rm = TRUE)==0 ) 
      #everything with TRUE has a standard deviation of 0 
remove_ids[1] <- FALSE
no_ids <- a_subset[, ! remove_ids]
summary(no_ids)

library(corrplot)
library(psych)
library(caret)

remov_species <- no_ids[,2:33]
corr_matrix <- tetrachoric(remov_species)
cmatrix <- corr_matrix$rho
#co1a <- cor(cmat, use = "pairwise.complete.obs")#compute the correlation matrix
cor2a <- cor(remov_species, use = "pairwise.complete.obs") #compare to pearson

corrplot(cmatrix, order = "hclust")
corrplot(cor2a, order = "hclust")

highlyCor2 <- findCorrelation(cmatrix, cutoff=0.70, verbose = FALSE ,exact=T)
threats_uncor2 <- cmatrix[,-c(highlyCor2)]
n <- ncol(threats_uncor2)

  #plotting uncorrelated threats 
library(corrplot)
co1b <- cor(threats_uncor2, use = "pairwise.complete.obs")#compute the correlation matrix
co1b
corrplot(co1b, order = "hclust")

vec2 <- colnames(threats_uncor2)
library(dplyr)
output2 <- no_ids[, vec2]
#ncol(output2) = 25
#nrow(output2) # 645
        #when amphiBIO data is joined, two additional columns are dropped 


#Add IUCN general data and amphiBIO data all together 
output2$species_name <- a_thre_iucn$species_list
se_threats_uncor <- output2

gen_threats <- sqldf("SELECT * FROM output2 INNER JOIN no_data_deficient 
                   ON output2.species_name = no_data_deficient.new_species")
summary(amph_thr_iucn)
amph_thr_iucn <- sqldf("SELECT * FROM gen_threats INNER JOIN amphiBIO ON amphiBIO.Species = gen_threats.species_name")
      #all data (IUCN gen info, AmphiBIO and threats are together)

amph_thr_iucn$threatened <- ifelse(amph_thr_iucn$Red.List.status == "LC" | amph_thr_iucn$Red.List.status == "NT", "Not Threatened", "Threatened")
amph_thr_iucn$threatened <- as.factor(amph_thr_iucn$threatened)

write.csv(amph_thr_iucn, file = "amph_thr_iucn.csv") #amphibio_threats_general,iucn data 




#Stepwise Regression
library(MASS)
amph1 <- glm(amph_thr_iucn$threatened ~ amph_thr_iucn$R1.2 + amph_thr_iucn$R1.3+ amph_thr_iucn$A2.1 + amph_thr_iucn$A2.2 + amph_thr_iucn$A2.3 +
               amph_thr_iucn$A2.4 + amph_thr_iucn$E3.2 +  amph_thr_iucn$T4.1 + 
               amph_thr_iucn$T4.2 + amph_thr_iucn$B5.1 + amph_thr_iucn$B5.2 + amph_thr_iucn$B5.3 + 
               amph_thr_iucn$B5.4 + amph_thr_iucn$H6.1 +  amph_thr_iucn$N7.1 + 
               amph_thr_iucn$N7.2 +  amph_thr_iucn$N7.3 + amph_thr_iucn$I8.1 + 
               amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2 + amph_thr_iucn$P9.3 + amph_thr_iucn$P9.4 + amph_thr_iucn$G10.1 +
               amph_thr_iucn$C11.2 + amph_thr_iucn$C11.4, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(amph1) #AIC: 784.96

step2 <- stepAIC(amph1, direction="both")
summary(step2) #AIC: 765.69

library(pscl) # Pseudo R^2 to evaluate models
pR2(step2)

#hand tuning regression model
step2_minusB5_2 <- glm(formula = amph_thr_iucn$threatened ~ amph_thr_iucn$R1.2 + 
      amph_thr_iucn$A2.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$B5.1 + 
      amph_thr_iucn$H6.1 + amph_thr_iucn$N7.2 + 
      amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2 + 
      amph_thr_iucn$C11.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(step2_minusB5_2) #AIC: 766.24

step2_minusB5_2_R1_2 <- glm(formula = amph_thr_iucn$threatened ~ 
                         amph_thr_iucn$A2.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$B5.1 + 
                         amph_thr_iucn$H6.1 + amph_thr_iucn$N7.2 + 
                         amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2 + 
                         amph_thr_iucn$C11.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(step2_minusB5_2_R1_2) #AIC: 767

###Final: 
step2_minusB5_2_R1_2_N72 <- glm(formula = amph_thr_iucn$threatened ~ 
                              amph_thr_iucn$A2.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$B5.1 + 
                              amph_thr_iucn$H6.1 + 
                              amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2 + 
                              amph_thr_iucn$C11.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(step2_minusB5_2_R1_2_N72) #AIC: 767.54


exp(cbind(OR = coef(step2_minusB5_2_R1_2_N72), confint(step2_minusB5_2_R1_2_N72)))






#regression interactions 
amp_int1 <- glm(formula = amph_thr_iucn$threatened ~ 
                                  amph_thr_iucn$A2.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$B5.1 + 
                                  amph_thr_iucn$H6.1 + 
                                  amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1:amph_thr_iucn$P9.2 + 
                                  amph_thr_iucn$C11.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(amp_int1) #AIC: 781

amp_int2 <- glm(formula = amph_thr_iucn$threatened ~ 
                  amph_thr_iucn$A2.1:amph_thr_iucn$B5.1 + 
                  amph_thr_iucn$H6.1 + amph_thr_iucn$E3.2 +
                  amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2 + 
                  amph_thr_iucn$C11.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(amp_int2) #AIC: 810, A2.1:B5.1 interaction is signifant 

amp_int3 <- glm(formula = amph_thr_iucn$threatened ~ 
                  amph_thr_iucn$A2.1:amph_thr_iucn$B5.1 + 
                  amph_thr_iucn$H6.1 + amph_thr_iucn$E3.2 +
                  amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2 + 
                  amph_thr_iucn$C11.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(amp_int3) #AIC: 813, A2.1:B5.1 interaction is signifant, minus 18.1 


#AMPHBIO data that will be used: 
#Habitat
#body_size
#reproductive output_y
#breeding strategy 

summary(amph_thr_iucn)
amph_thr_iucn$Order <- as.factor(amph_thr_iucn$Order)


amph_thr_iucn$Body_size_mm <- ifelse(is.na(amph_thr_iucn$Body_size_mm == TRUE), mean(amph_thr_iucn$Body_size_mm, na.rm = TRUE), amph_thr_iucn$Body_size_mm)
amph_thr_iucn$Reproductive_output_y <- ifelse(is.na(amph_thr_iucn$Reproductive_output_y == TRUE), mean(amph_thr_iucn$Reproductive_output_y, na.rm = TRUE), amph_thr_iucn$Reproductive_output_y)
amph_thr_iucn$Age_at_maturity_min_y <- ifelse(is.na(amph_thr_iucn$Age_at_maturity_min_y == TRUE), mean(amph_thr_iucn$Age_at_maturity_min_y, na.rm = TRUE), amph_thr_iucn$Age_at_maturity_min_y)




summary(amph_thr_iucn)

st_amph1 <- glm(formula = amph_thr_iucn$threatened ~ 
                                  amph_thr_iucn$A2.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$B5.1 + 
                                  amph_thr_iucn$H6.1 + amph_thr_iucn$Body_size_mm +
                                  amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2 + 
                                  amph_thr_iucn$C11.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph1) #AIC: 759.41 - no interactins 

st_amph2 <- glm(formula = amph_thr_iucn$threatened ~ 
                  amph_thr_iucn$A2.1 + amph_thr_iucn$E3.2 + 
                  amph_thr_iucn$H6.1 + amph_thr_iucn$Body_size_mm:amph_thr_iucn$B5.1 +  
                  amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2 + 
                  amph_thr_iucn$C11.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph2) #AIC: 775.17

st_amph3 <- glm(formula = amph_thr_iucn$threatened ~ 
                  amph_thr_iucn$A2.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$Reproductive_output_y + 
                  amph_thr_iucn$H6.1 + amph_thr_iucn$Body_size_mm:amph_thr_iucn$B5.1 +  
                  amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2 + 
                  amph_thr_iucn$C11.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph3) #AIC: 776, reproductive output is not significant 

st_amph4 <- glm(formula = amph_thr_iucn$threatened ~ 
                  amph_thr_iucn$A2.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$Reproductive_output_y:amph_thr_iucn$C11.2 + 
                  amph_thr_iucn$H6.1 + amph_thr_iucn$Body_size_mm:amph_thr_iucn$B5.1 +  
                  amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph4) #AIC: 775.11, reproductive output and climate change is significant 
                  #from lit knownledge where climate change droughts can affect fecundity 

st_amph5 <- glm(formula = amph_thr_iucn$threatened ~ 
                  amph_thr_iucn$A2.1 + amph_thr_iucn$H6.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$Reproductive_output_y:amph_thr_iucn$C11.2 + 
                  amph_thr_iucn$Body_size_mm:amph_thr_iucn$B5.1 +  
                  amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph5) #AIC: 775.11, ALL VALUES SIGNIFCANT

st_amph6 <- glm(formula = amph_thr_iucn$threatened ~ amph_thr_iucn$Family +
                  amph_thr_iucn$A2.1 + amph_thr_iucn$H6.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$Reproductive_output_y:amph_thr_iucn$C11.2 + 
                  amph_thr_iucn$Body_size_mm:amph_thr_iucn$B5.1 +  
                  amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph6) #AIC: 737 - added family, AIC went down but NONE of the family options are significant 


st_amph7 <- glm(formula = amph_thr_iucn$threatened ~ 
                  amph_thr_iucn$A2.1 + amph_thr_iucn$H6.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$Reproductive_output_y:amph_thr_iucn$C11.2 + 
                  amph_thr_iucn$Body_size_mm*amph_thr_iucn$B5.1 +  
                  amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1*amph_thr_iucn$P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph7) #AIC 747 not everything is sig.

st_amph8 <- glm(formula = amph_thr_iucn$threatened ~ 
                  amph_thr_iucn$A2.1 + amph_thr_iucn$H6.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$Reproductive_output_y*amph_thr_iucn$C11.2 + 
                  amph_thr_iucn$Body_size_mm*amph_thr_iucn$B5.1 +  
                  amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1*amph_thr_iucn$P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph8) #AIC: 747

st_amph9 <- glm(formula = amph_thr_iucn$threatened ~ 
                  amph_thr_iucn$A2.1 + amph_thr_iucn$H6.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$Reproductive_output_y*amph_thr_iucn$C11.2 + 
                  amph_thr_iucn$Body_size_mm*amph_thr_iucn$B5.1 +  
                  amph_thr_iucn$I8.1, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph9) #AIC: 764

st_amph10 <- glm(formula = amph_thr_iucn$threatened ~ 
                  amph_thr_iucn$A2.1 + amph_thr_iucn$H6.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$Reproductive_output_y*amph_thr_iucn$C11.2 + 
                  amph_thr_iucn$Body_size_mm*amph_thr_iucn$B5.1 +  amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2 +
                  amph_thr_iucn$I8.1, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph10) #AIC: 753

st_amph11 <- glm(formula = amph_thr_iucn$threatened ~ 
                   amph_thr_iucn$A2.1 + amph_thr_iucn$H6.1 + amph_thr_iucn$E3.2 +  
                   amph_thr_iucn$Body_size_mm*amph_thr_iucn$B5.1 +  amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2 +
                   amph_thr_iucn$I8.1, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph11) #AIC: 763 (removed climate change and reproductive output)


st_amph8 <- glm(formula = amph_thr_iucn$threatened ~ 
                  amph_thr_iucn$A2.1 + amph_thr_iucn$H6.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$Reproductive_output_y*amph_thr_iucn$C11.2 + 
                  amph_thr_iucn$Body_size_mm*amph_thr_iucn$B5.1 +  
                  amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1*amph_thr_iucn$P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph8) #AIC: 747 - but lots of the values are not signifcant 

st_amph8_noH61 <- glm(formula = amph_thr_iucn$threatened ~ 
                  amph_thr_iucn$A2.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$Reproductive_output_y*amph_thr_iucn$C11.2 + 
                  amph_thr_iucn$Body_size_mm*amph_thr_iucn$B5.1 +  
                  amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1*amph_thr_iucn$P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph8_noH61) #AIC: 793 (NEED H6.1 )

st_amph8_noP92 <- glm(formula = amph_thr_iucn$threatened ~ 
                  amph_thr_iucn$A2.1 + amph_thr_iucn$H6.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$Reproductive_output_y*amph_thr_iucn$C11.2 + 
                  amph_thr_iucn$Body_size_mm*amph_thr_iucn$B5.1 +  
                  amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph8_noP92) #AIC: 759.81

st_amph8_noP92_noC11.2 <- glm(formula = amph_thr_iucn$threatened ~ 
                        amph_thr_iucn$A2.1 + amph_thr_iucn$H6.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$Reproductive_output_y + 
                        amph_thr_iucn$Body_size_mm*amph_thr_iucn$B5.1 +  
                        amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph8_noP92_noC11.2) #AIC: 770


st_amph8_and_Order <- glm(formula = amph_thr_iucn$threatened ~ amph_thr_iucn$Order + 
                  amph_thr_iucn$A2.1 + amph_thr_iucn$H6.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$Reproductive_output_y*amph_thr_iucn$C11.2 + 
                  amph_thr_iucn$Body_size_mm*amph_thr_iucn$B5.1 +  
                  amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1*amph_thr_iucn$P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph8_and_Order) #AIC: 739 _ values are no longer signicant 


st_amph8_and_Order_minusH61 <- glm(formula = amph_thr_iucn$threatened ~ amph_thr_iucn$Order + 
                            amph_thr_iucn$A2.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$Reproductive_output_y*amph_thr_iucn$C11.2 + 
                            amph_thr_iucn$Body_size_mm*amph_thr_iucn$B5.1 +  
                            amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1*amph_thr_iucn$P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph8_and_Order_minusH61) #AIC: 786.87, need H6.1 


st_amph8_intr <- glm(formula = amph_thr_iucn$threatened ~ amph_thr_iucn$Order + 
                            amph_thr_iucn$A2.1 + amph_thr_iucn$H6.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$Reproductive_output_y*amph_thr_iucn$C11.2 + 
                            amph_thr_iucn$Body_size_mm:amph_thr_iucn$B5.1 +  
                            amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1:amph_thr_iucn$P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph8_intr) #AIC: 739 <- not all values significant 
pR2(st_amph8_intr)


st_amph1_body_int_ageofMatchurtoy <- glm(formula = amph_thr_iucn$threatened ~ amph_thr_iucn$Age_at_maturity_min_y + 
                                           amph_thr_iucn$A2.1 + amph_thr_iucn$E3.2 +  
                                           amph_thr_iucn$H6.1 + amph_thr_iucn$Body_size_mm:amph_thr_iucn$B5.1 +
                                           amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2 + 
                                           amph_thr_iucn$C11.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph1_body_int_ageofMatchurtoy) #AIC: 777, not signicant 

###
st_amph1_body_int <- glm(formula = amph_thr_iucn$threatened ~ 
                  amph_thr_iucn$A2.1 + amph_thr_iucn$E3.2 +  
                  amph_thr_iucn$H6.1 + amph_thr_iucn$Body_size_mm:amph_thr_iucn$B5.1 +
                  amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2 + 
                  amph_thr_iucn$C11.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph1_body_int) #AIC: 775 body weight and B5.1 interact. everything sig, but AIC increased 

library(pscl) # Pseudo R^2 to evaluate models
pR2(st_amph1_body_int)



exp(cbind(OR = coef(step2_minusB5_2_R1_2_N72), confint(step2_minusB5_2_R1_2_N72)))

#cool dendogram 

data_dist2 <- 1 - cmatrix
#clustering works on distance instead of similarity
distance2 <- as.dist(data_dist2)

hc2 <- hclust(distance2, method = "average")
my_palette <- colorRampPalette(c("lightyellow", "red", "green"))(n = 299)
clusterCut2 <- cutree(hc2,6) 
table(clusterCut2)

plot2 <-plot(hc2, main="Dissimilarity = 1 - Correlation", xlab="")

#####
st_amph5a <- glm(formula = amph_thr_iucn$threatened ~ 
                  amph_thr_iucn$A2.1 + amph_thr_iucn$H6.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$C11.2 + 
                  amph_thr_iucn$Body_size_mm:amph_thr_iucn$B5.1 +  
                  amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph5a) #AIC: 561
pR2(st_amph5a)



st_amph5b <- glm(formula = amph_thr_iucn$threatened ~ 
                   amph_thr_iucn$A2.1 + amph_thr_iucn$H6.1 + amph_thr_iucn$E3.2 + amph_thr_iucn$C11.2 + 
                   amph_thr_iucn$Body_size_mm:amph_thr_iucn$B5.1 +  
                   amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph5b) #AIC: 563
pR2(st_amph5b)


st_amph5c <- glm(formula = amph_thr_iucn$threatened ~ 
                   amph_thr_iucn$A2.1 + amph_thr_iucn$H6.1 + amph_thr_iucn$C11.2 + 
                   amph_thr_iucn$Body_size_mm:amph_thr_iucn$B5.1 +  
                   amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph5c) #AIC: 563
pR2(st_amph5c)

#######################
st_amph5d <- glm(formula = amph_thr_iucn$threatened ~ 
                   amph_thr_iucn$A2.1 + amph_thr_iucn$H6.1 + 
                   amph_thr_iucn$Body_size_mm:amph_thr_iucn$B5.1 +  
                   amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph5d) #AIC: 563
pR2(st_amph5d)
########################

exp(cbind(OR = coef(st_amph5d), confint(st_amph5d)))





##
st_amph1_body_intA <- glm(formula = amph_thr_iucn$threatened ~ 
                           amph_thr_iucn$A2.1 + amph_thr_iucn$E3.2 +  
                           amph_thr_iucn$H6.1 + amph_thr_iucn$Body_size_mm:amph_thr_iucn$B5.1 +
                           amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2 + 
                           amph_thr_iucn$C11.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph1_body_intA) #AIC: 561
pR2(st_amph1_body_intA)


st_amph1_body_intB <- glm(formula = amph_thr_iucn$threatened ~ 
                            amph_thr_iucn$A2.1 + amph_thr_iucn$E3.2 +  
                            amph_thr_iucn$H6.1 + amph_thr_iucn$Body_size_mm:amph_thr_iucn$B5.1 +
                            amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2 + 
                            amph_thr_iucn$C11.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph1_body_intB) #AIC: 562
pR2(st_amph1_body_intB)

st_amph1_body_intC <- glm(formula = amph_thr_iucn$threatened ~ 
                            amph_thr_iucn$A2.1 + 
                            amph_thr_iucn$H6.1 + amph_thr_iucn$Body_size_mm:amph_thr_iucn$B5.1 +
                            amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2 + 
                            amph_thr_iucn$C11.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph1_body_intC) #AIC: 562
pR2(st_amph1_body_intC)

st_amph1_body_intD <- glm(formula = amph_thr_iucn$threatened ~ 
                            amph_thr_iucn$A2.1 + 
                            amph_thr_iucn$H6.1 + amph_thr_iucn$Body_size_mm:amph_thr_iucn$B5.1 +
                            amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph1_body_intD) #AIC: 562
pR2(st_amph1_body_intD)

##
st_amph8_intrA <- glm(formula = amph_thr_iucn$threatened ~ amph_thr_iucn$Order + 
                       amph_thr_iucn$A2.1 + amph_thr_iucn$H6.1 + amph_thr_iucn$E3.2 + 
                       amph_thr_iucn$Body_size_mm:amph_thr_iucn$B5.1 +  
                       amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1:amph_thr_iucn$P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph8_intrA) #AIC: 570
pR2(st_amph8_intrA)

st_amph8_intrB <- glm(formula = amph_thr_iucn$threatened ~ amph_thr_iucn$Order + 
                        amph_thr_iucn$A2.1 + amph_thr_iucn$H6.1 + amph_thr_iucn$E3.2 + 
                        amph_thr_iucn$Body_size_mm:amph_thr_iucn$B5.1 +  
                        amph_thr_iucn$I8.1 + amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph8_intrB) #AIC: 561
pR2(st_amph8_intrB)


st_amph8_intrC <- glm(formula = amph_thr_iucn$threatened ~ amph_thr_iucn$Order + 
                        amph_thr_iucn$A2.1 + amph_thr_iucn$H6.1 + amph_thr_iucn$E3.2 + 
                        amph_thr_iucn$Body_size_mm:amph_thr_iucn$B5.1 +  
                        amph_thr_iucn$P9.1 + amph_thr_iucn$P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph8_intrC) #AIC: 564
pR2(st_amph8_intrC)


st_amph9_intrC <- glm(formula = threatened ~ Order + 
                        A2.1 + H6.1 + E3.2 + 
                        Body_size_mm + B5.1 +  
                        P9.1 + P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph9_intrC)  #when body size and hunting are together, body size is significant and hunting isnt
pR2(st_amph9_intrC)


st_amph10_intrC <- glm(formula = threatened ~ Order + 
                        A2.1 + H6.1 + E3.2 +  B5.1 +  
                        P9.1 + P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph10_intrC) #when body size is removed, hunting is significant 
pR2(st_amph10_intrC)

#body size might be significant along with ag (two important variables that have seperate effects and can see )
st_amph11_intrC <- glm(formula = threatened ~ Order + 
                        A2.1 + H6.1 + E3.2 + 
                        Body_size_mm +   
                        P9.1 + P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph11_intrC) 
pR2(st_amph11_intrC)


st_amph12_intrC <- glm(formula = threatened ~ Order + 
                         A2.1 + H6.1 + 
                         Body_size_mm +   
                         P9.1 + P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(st_amph12_intrC) 
pR2(st_amph12_intrC)

## body size and bodysize:hunting 

hunt_body <- glm(formula = threatened ~ Order + 
                        A2.1 + H6.1 + E3.2 + 
                        Body_size_mm + Body_size_mm:B5.1 + B5.1 +
                        P9.1 + P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(hunt_body)  
pR2(hunt_body)




##########################################################################################
######
###### Work completed with Patricia on June 14, 2019 
######
######


hunt_body2 <- glm(formula = threatened ~ Order + 
                   A2.1 + H6.1 + 
                   Body_size_mm + Body_size_mm:B5.1 + B5.1 +
                   P9.1 + P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
summary(hunt_body2)  #best one so far 
pR2(hunt_body2)

#Hold variable constant 
# ORDER = ANURA (because there are so many - and i'm interested in that) 
# make more sense as 0 or 1 - whether u have it or not 
# makes more sense when they are inbalanced too (more men than women, set at men)

summary(amph_thr_iucn$A2.1) #Annual and perennial non-timber crops 
    #Set at 1 (more 1s than 0)
summary(amph_thr_iucn$H6.1) #Recreational activities 
    #Set as 0 (mean is 0.0325)
summary(amph_thr_iucn$B5.1) #Hunting and trapping 
    ## TRY VARYING 
    #Set as 0, for the other vary (make it the colored variable in the graph -heart)
summary(amph_thr_iucn$P9.1) #Pollution - domestic and urban waste water 
    #set as 0, try varying
summary(amph_thr_iucn$P9.2) #Pollution - indsutrial and military effluents 
    #set as 0 
#ORDER = ANURA 
# Try varying hunting and trapping FIRST, then try different pollutions (P's), then try H6.1,
      #try Agriculture after 
# Try all of them 

#Hold body_size and hunting at average (the interaction at average) 
    #body size average for a hunted region
    #mean(body size * threat of hunting)


    








