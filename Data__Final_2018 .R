
full_se_data
master_copy <- full_se_data 

head(master_copy)
tail(master_copy)

library(sqldf)
library(ggplot2)
library(rredlist)

#Goal:
  #I want to see if there is any relationship between threats and red list status. 
  #what are the trends in threats? 

#Exploratory Plots 
ggplot(master_copy, aes(x = R1.1)) + geom_histogram(binwidth=.5)
ggplot(master_copy, aes(x = R1.2)) + geom_histogram(binwidth=.5)
ggplot(master_copy, aes(x = R1.3)) + geom_histogram(binwidth=.5)
ggplot(master_copy, aes(x = A2.1)) + geom_histogram(binwidth=.5)
ggplot(master_copy, aes(x = A2.2)) + geom_histogram(binwidth=.5)
ggplot(master_copy, aes(x = A2.3)) + geom_histogram(binwidth=.5)
ggplot(master_copy, aes(x = A2.4)) + geom_histogram(binwidth=.5)


amph_data <- sqldf("SELECT Fos, Ter, Aqu, Arb, Leaves, Flowers, Seeds, Fruits, Arthro, Vert,
                   Diu, Noc, Crepu, Wet_warm, Wet_cold, Dry_Warm, Dry_cold, Body_mass_g, 
                   Age_at_maturity_min_y, Age_at_maturity_max_y, Body_size_mm, Size_at_maturity_min_mm,
                   Size_at_maturity_max_mm, Longevity_max_y, Litter_size_min_n, Litter_size_max_n,
                   Reproductive_output_y, Offspring_size_min_mm, Offspring_size_max_mm, Dir, Lar,
                   Viv, OBS, species_list FROM master_copy")

no_dd <- master_copy[master_copy$Red.List.status != "DD",]
no_dd$threatened <- ifelse(no_dd$Red.List.status == "NT" | no_dd$Red.List.status == "LC",
                                 "not_threatened", "threatened")




    #Remove DD because we are not sure what the status is. Next step would be to include 
    #DD data because they are most likely threatened. See if the data changes 

as.factor(no_dd$R1.1)
as.factor(no_dd$R1.2)
no_dd$R1.3<- as.factor(no_dd$R1.3)
no_dd$threatened <- as.factor(no_dd$threatened)

lm1 <- glm(no_dd$threatened ~ no_dd$R1.1 + no_dd$R1.2 + no_dd$R1.3 +
           no_dd$A2.1 + no_dd$A2.2 + no_dd$A2.3 + no_dd$A2.4 + no_dd$E3.1 +
           no_dd$E3.2 + no_dd$E3.3 + no_dd$T4.1 + no_dd$T4.2 + no_dd$T4.3 +
           no_dd$T4.4 + no_dd$B5.1 + no_dd$B5.2 + no_dd$B5.3 + no_dd$B5.4 +
           no_dd$H6.1 + no_dd$H6.2 + no_dd$H6.2 + no_dd$H6.3 + no_dd$N7.1 +
           no_dd$N7.2 + no_dd$N7.3 + no_dd$I8.1 + no_dd$I8.2 + no_dd$I8.4 + 
           no_dd$I8.5 + no_dd$I8.6 + no_dd$P9.1 + no_dd$P9.2 + no_dd$P9.3 + 
           no_dd$P9.4 + no_dd$P9.5 + no_dd$G10.1 + no_dd$G10.3 + no_dd$C11.1 +
           no_dd$C11.2 + no_dd$C11.3 + no_dd$C11.4 + no_dd$C11.5, data = no_dd, 
           family = binomial(link = "logit"))
summary(lm1)

no_dd$R1.1 <- as.factor(no_dd$R1.1)
no_dd$R1.2 <- as.factor(no_dd$R1.2)
no_dd$R1.3 <- as.factor(no_dd$R1.3)
no_dd$A2.1 <- as.factor(no_dd$A2.1)
no_dd$A2.2 <- as.factor(no_dd$A2.2)
no_dd$A2.3 <- as.factor(no_dd$A2.3)
no_dd$A2.4 <- as.factor(no_dd$A2.4)
no_dd$E3.1 <- as.factor(no_dd$E3.1)
no_dd$E3.2 <- as.factor(no_dd$E3.2)
no_dd$E3.3 <- as.factor(no_dd$E3.3)


lm2 <- glm(no_dd$threatened ~ no_dd$R1.1 + no_dd$R1.2 + no_dd$R1.3 +
             no_dd$A2.1 + no_dd$A2.2 + no_dd$A2.3 + no_dd$A2.4 + 
             no_dd$E3.2 + no_dd$E3.3 + no_dd$T4.1 + no_dd$T4.2 + 
             no_dd$B5.1 + no_dd$B5.2 + no_dd$B5.3 + no_dd$B5.4 +
             no_dd$H6.1 + no_dd$H6.2 + no_dd$H6.2 + no_dd$H6.3 + no_dd$N7.1 +
             no_dd$N7.2 + no_dd$N7.3 + no_dd$I8.1 + no_dd$I8.2 + no_dd$I8.4 + 
             no_dd$P9.1 + no_dd$P9.2 + no_dd$P9.3 + 
             no_dd$P9.4 + no_dd$G10.1 + no_dd$C11.2 + no_dd$C11.3 + no_dd$C11.4, data = no_dd, 
           family = binomial(link = "logit"))
#Warning message:
#glm.fit: fitted probabilities numerically 0 or 1 occurred 
summary(lm2)


lm1 <- glm(no_dd$threatened ~ no_dd$R1.1 + no_dd$R1.2 + no_dd$R1.3 +
             no_dd$A2.1 + no_dd$A2.2 + no_dd$A2.3 + no_dd$A2.4 + no_dd$E3.1 +
             no_dd$E3.2 + no_dd$E3.3 + no_dd$T4.1 + no_dd$T4.2 + no_dd$T4.3 +
             no_dd$T4.4 + no_dd$B5.1 + no_dd$B5.2 + no_dd$B5.3 + no_dd$B5.4 +
             no_dd$H6.1 + no_dd$H6.2 + no_dd$H6.2 + no_dd$H6.3 + no_dd$N7.1 +
             no_dd$N7.2 + no_dd$N7.3 + no_dd$I8.1 + no_dd$I8.2 + no_dd$I8.4 + 
             no_dd$I8.5 + no_dd$I8.6 + no_dd$P9.1 + no_dd$P9.2 + no_dd$P9.3 + 
             no_dd$P9.4 + no_dd$P9.5 + no_dd$G10.1 + no_dd$G10.3 + no_dd$C11.1 +
             no_dd$C11.2 + no_dd$C11.3 + no_dd$C11.4 + no_dd$C11.5, data = no_dd, 
           family = binomial(link = "logit"))
summary(lm1)

lm3 <- glm(no_dd$threatened ~ no_dd$R1.1 + no_dd$R1.2 + no_dd$R1.3 +
             no_dd$A2.1 + no_dd$A2.2 + no_dd$A2.3 + no_dd$A2.4 + 
             no_dd$E3.2 + no_dd$E3.3 + no_dd$T4.1 + no_dd$T4.2 + no_dd$T4.3 +
             no_dd$T4.4 + no_dd$B5.1 + no_dd$B5.2 + no_dd$B5.3 + no_dd$B5.4 +
             no_dd$H6.1 + no_dd$H6.2 + no_dd$H6.2 + no_dd$H6.3 + no_dd$N7.1 +
             no_dd$N7.2 + no_dd$N7.3 + no_dd$I8.1 + no_dd$I8.2 + no_dd$I8.4 + 
             no_dd$I8.5 + no_dd$I8.6 + no_dd$P9.1 + no_dd$P9.2 + no_dd$P9.3 + 
             no_dd$P9.4 + no_dd$P9.5 + no_dd$G10.1 + no_dd$G10.3 + no_dd$C11.1 +
             no_dd$C11.2 + no_dd$C11.3 + no_dd$C11.4 + no_dd$C11.5, data = no_dd, 
           family = binomial(link = "logit"))
summary(lm3)
#Warning message:
#  glm.fit: fitted probabilities numerically 0 or 1 occurred 


      #Online Answer said: 
      #https://www.researchgate.net/post/Help_with_Logistic_Regression_In_rglmfit_fitted_probabilities_numerically_0_or_1_occurred_glmfit_algorithm_did_not_converge
      #Renaud Lancelot
      # Cirad - La recherche agronomique pour le développement
      # It is always safer to code factors as such but it won't 
      # change anything in this case (because you have defined 2 
      # categories coded as 0 an 1). Moreover it's a common practice 
      # to code the response as 0/1, and even better to use probabilities 
      # or success/failure (as a matrix for the latter) when possible.
      # In short, I think you are confronted to an over-fitting problem:
      # too many variables in your model, leading to a perfect separation 
      # of cases. The consequences is that model likelihood
      # is not defined, and thus you can't get the model to converge.
      # I would recommend to do a preliminary exploratory data analysis to 
      # exclude correlated variables.Moreover, in this case, I 
      # would progressively build the model, starting with a very 
      # simple model and adding variables one by one.


#Overfitting the model - means that it's a perfect seperation because 
#I have more than enough variables

#Do exploratory analysis to see what variables are correlated, remove them 
#Remove columns where it is 0 


#Remove all columns where everything = 0, no species have any threats 
master_copy$E3.1 <- NULL
master_copy$T4.3 <- NULL
master_copy$T4.4 <- NULL
master_copy$I8.5 <- NULL
master_copy$I8.6 <- NULL
master_copy$P9.5 <- NULL
master_copy$G10.3 <- NULL
master_copy$C11.1 <- NULL
master_copy$C11.5 <- NULL
head(master_copy)


#Select only threats from master_copy 
threats <- master_copy[,60:152]
colnames(threats)
  
#Remove more specific threat distinctions (there are duplicates in A.2 and A.2.1) 
  #throws off correlation 
threats$A2.1.1 <- NULL
threats$A2.1.2 <- NULL 
threats$A2.1.3 <- NULL 
threats$A2.1.4 <- NULL 
threats$A2.2.1 <- NULL 
threats$A2.2.2 <- NULL 
threats$A2.2.3 <- NULL 
threats$A2.3.1 <- NULL 
threats$A2.3.2 <- NULL 
threats$A2.3.3 <- NULL 
threats$A2.3.4 <- NULL 
threats$A2.4.2 <- NULL 
threats$A2.4.3 <- NULL 
threats$B5.1.1 <- NULL 
threats$B5.1.2 <- NULL 
threats$B5.1.3 <- NULL 
threats$B5.2.2 <- NULL 
threats$B5.2.4 <- NULL 
threats$B5.3.1 <- NULL 
threats$B5.3.2 <- NULL 
threats$B5.3.3 <- NULL 
threats$B5.3.4 <- NULL 
threats$B5.3.5 <- NULL 
threats$B5.4.1 <- NULL 
threats$B5.4.3 <- NULL 
threats$B5.4.4 <- NULL 
threats$B5.4.6 <- NULL 
threats$N7.1.1 <- NULL 
threats$N7.1.2 <- NULL 
threats$N7.1.3 <- NULL 
threats$N7.2.1 <- NULL
threats$N7.2.10 <- NULL
threats$N7.2.11 <- NULL
threats$N7.2.3 <- NULL
threats$N7.2.4 <- NULL
threats$N7.2.5 <- NULL
threats$N7.2.6 <- NULL
threats$N7.2.7 <- NULL
threats$N7.2.8 <- NULL
threats$N7.2.9 <- NULL
threats$I8.1.1 <- NULL
threats$I8.1.2 <- NULL
threats$I8.2.1 <- NULL
threats$I8.2.2 <- NULL
threats$I8.4.1 <- NULL
threats$I8.4.2 <- NULL
threats$I8.5.1 <- NULL
threats$I8.5.2 <- NULL
threats$P9.1.1 <- NULL
threats$P9.1.2 <- NULL
threats$P9.1.3 <- NULL
threats$P9.2.1 <- NULL
threats$P9.2.2 <- NULL
threats$P9.2.3 <- NULL
threats$P9.3.1 <- NULL
threats$P9.3.2 <- NULL
threats$P9.3.3 <- NULL
threats$P9.3.4 <- NULL
threats$P9.5.1 <- NULL
threats$P9.5.2 <- NULL
threats$P9.5.3 <- NULL
threats$P9.5.4 <- NULL

#Rename threat columns to useful values 
library(plyr)
threats <- rename(threats, c("R1.1"="Res_Housing", "R1.2"="Res_Commercial",
                  "R1.3" = "Res_Tourism", "A2.1" = "Ag_non-timber_crops", 
                  "A2.2" = "Ag_wood", "A2.3" = "Ag_livestock", "A2.4" =" Ag_aquaculture", 
                  "E3.2" = "Energy_mining", "E3.3" = "Energy_renewable", 
                  "T4.1" = "Trans_rail", "T4.2" = "Trans_utility", "B5.1" = "Bio_hunting",
                  "B5.2" = "Bio_gathering_plants", "B5.3" = "Bio_logging", "B5.4" = "Bio_fishing",
                  "H6.1" = "Human_rec", "H6.2" = "Human_war", "H6.3" = "Human_work", 
                  "N7.1" = "Natural_fire", "N7.2" = "Natural_dam", "N7.3" = "Natural_other", 
                  "I8.1" = "Invasive_non-native", "I8.2" = "Invasive_problematic", "I8.4" = "Invasive_unknown",
                  "P9.1" = "Pollution_water", "P9.2" = "Pollution_industrial", "P9.3" = "Pollution_ag", 
                  "P9.4" = "Pollution_garbage", "G10.1" = "Geo_volcanoe", "C11.2" = "Clim_drought", 
                  "C11.3" = "Clim_temp"))

#Create correlation matrix of all threats 
cor(threats)
require(scales)
require(reshape2)
econcor <- cor(threats)
econmelt <- melt(econcor, varnames = c("x","y"), value.name = "Correlation" )
econmelt <- econmelt[order(econmelt$Correlation),] 
cor_threats <- ggplot(econmelt, aes(x=x, y=y)) + 
  geom_tile(aes(fill = Correlation)) + 
  scale_fill_gradient2(low = muted("red"), mid="white", 
                      high = "steelblue",
                      guide = guide_colorbar(ticks=FALSE, barheight = 10),
                      limits = c(-1,1)) + theme_minimal() + labs(x=NULL, y=NULL) +
                      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                      ggtitle("Southeast Amphibian Threat Correlations, IUCN Red List") 

#Dive deep into SE Threats where the mean of the threat is .1> . I.E. where there are 
#many records of frogs having that threat 

#Residential Threats 
library(plyr)

res <- master_copy[,60:62]
res <-  rename(res, c("R1.1" = "Housing & urban areas", "R1.2" = "Commercial & industrial areas","R1.3" = "Tourism & recreation areas"))

res_cor <- cor(res)
resmelt <- melt(res_cor , varnames = c("x","y"), value.name = "Correlation" )
resmelt <- resmelt[order(resmelt$Correlation),] 
res_threats <- ggplot(resmelt, aes(x=x, y=y)) + 
  geom_tile(aes(fill = Correlation)) + 
  scale_fill_gradient2(low = muted("red"), mid="white", 
                       high = "steelblue",
                       guide = guide_colorbar(ticks=FALSE, barheight = 10),
                       limits = c(-1,1)) + theme_minimal() + labs(x=NULL, y=NULL) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  ggtitle("Residential & commercial development threats") 

#Agriculture Threats 
ag <- master_copy[,63:79]
ag$A2.1 <- NULL
ag$A2.2 <- NULL
ag$A2.3 <- NULL
ag$A2.4 <- NULL

ag <- rename(ag, c("A2.1.1" = "Non-timber crops, Shifting ag", 
                   "A2.1.2" = "Non-timber crops, Small-holder farming",
                   "A2.1.3" = "Non-timber crops, Agro-industry farming",
                   "A2.1.4" = "Non-timber crops, Scale unknown",
                   "A2.2.1" = "Wood plantation, Small-holder plantation", 
                   "A2.2.2" = "Wood plantation, Agro-industry plantation", 
                   "A2.2.3" = "Wood plantation, Scale unknown",
                   "A2.3.1" = "Livestock farming, Nomadic",
                   "A2.3.2" = "Livestock farming, Small-holder", 
                   "A2.3.3" = "Livestock farming, Agro-industry",
                   "A2.3.4" = "Livestock farming, Scale unknown", 
                   "A2.4.2" = "Aquaculture, Industrial",
                   "A2.4.3" = "Aquaculture, Scale Unknown"))
ag_cor <- cor(ag)
agmelt <- melt(ag_cor , varnames = c("x","y"), value.name = "Correlation" )
agmelt <- agmelt[order(agmelt$Correlation),] 
ag_threats <- ggplot(agmelt, aes(x=x, y=y)) + 
  geom_tile(aes(fill = Correlation)) + 
  scale_fill_gradient2(low = muted("red"), mid="white", 
                       high = "steelblue",
                       guide = guide_colorbar(ticks=FALSE, barheight = 10),
                       limits = c(-1,1)) + theme_minimal() + labs(x=NULL, y=NULL) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Agriculture & aquaculture threats") 



#Bio 
bio <- master_copy[,84:101]
testeorgaeoirjg <- glm(bio$B5.1 ~ ., data = bio, family = binomial(link = "logit"))

bio$B5.1 <- NULL
bio$B5.2 <- NULL
bio$B5.3 <- NULL
bio$B5.4 <- NULL

colnames(bio)
bio <- rename(bio, c("B5.1.1" = "Hunting and trapping, intentional", "B5.1.2" = "Hunting and trapping, Unintentional",
                     "B5.1.3" = "Hunting and trapping, persecution/control", "B5.2.2" = "Gathering terrestrial plants, Unintentional",
                     "B5.2.4" = "Gathering terrestrial plants, motivation unknown", "B5.3.1" = "Logging & wood harvesting, intentional, small",
                     "B5.3.2" = "Logging & wood harvesting, intentional, large", "B5.3.3" = "Logging & wood harvesting, unintentional, small",
                     "B5.3.4" = "Logging & wood harvesting, unintentional, large", "B5.3.5" = "Logging & wood harvesting, motivation unknown",
                     "B5.4.1" = "Harvesting aquatic, intentional, small", "B5.4.3" = "Harvesting aquatic, unintentional, small", 
                     "B5.4.4" = "Harvesting aquaric, unintentional, large", "B5.4.6" = "Harvesting aquatic, motivation unknown"))


summary(no_dd$b5.4.4)

bio_cor <- cor(bio)
biomelt <- melt(bio_cor , varnames = c("x","y"), value.name = "Correlation" )
biomelt <- biomelt[order(biomelt$Correlation),] 
bio_threats <- ggplot(biomelt, aes(x=x, y=y)) + 
  geom_tile(aes(fill = Correlation)) + 
  scale_fill_gradient2(low = muted("red"), mid="white", 
                       high = "steelblue",
                       guide = guide_colorbar(ticks=FALSE, barheight = 10),
                       limits = c(-1,1)) + theme_minimal() + labs(x=NULL, y=NULL) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("BIO threats") 
        #Missed some that have 0 <- go back and deelete those that hvae zeo


        #This one is being weird - getting a lot of standard deviation = 0 
no_dd$E3.1 <- NULL
no_dd$T4.3<- NULL
no_dd$T4.4 <- NULL
no_dd$I8.5 <- NULL
no_dd$I8.6 <- NULL
no_dd$P9.5 <- NULL
no_dd$G10.3 <- NULL
no_dd$C11.1 <- NULL
no_dd$C11.5 <- NULL
head(no_dd)


lm4 <- glm(no_dd$threatened ~ no_dd$R1.1, data = bio, family = binomial(link = "logit"))
summary(lm4) #AIC = 911.53

lm5 <- glm(no_dd$threatened ~ no_dd$R1.1 + no_dd$R1.2, data = bio, family = binomial(link = "logit"))
summary(lm5) #AIC = 909.5

lm6 <- glm(no_dd$threatened ~ no_dd$R1.1 + no_dd$R1.2 + no_dd$R1.3, data = bio, family = binomial(link = "logit"))
summary(lm6) #AIC = 909.67

lm7 <- glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1, data = bio, family = binomial(link = "logit"))
summary(lm7) #AIC = 892.89 

lm8 <- glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.1 + no_dd$A2.1.2, data = bio, family = binomial(link = "logit"))
summary(lm8) #AIC = 904.51

lm9 <- glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.2 + no_dd$A2.1.3, data = bio, family = binomial(link = "logit"))
summary(lm9) #AIC = 892.65 

lm10 <- glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.2 + no_dd$A2.1.3 + no_dd$A2.1.4, data = bio, family = binomial(link = "logit"))
summary(lm10) #AIC = 893.31

lm11 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.2 + no_dd$A2.1.3 + no_dd$A2.1.4
             + no_dd$A2.2.1, data = bio, family = binomial(link = "logit"))
summary(lm11) #AIC = 893

lm12 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$A2.2.2, data = bio, family = binomial(link = "logit"))
summary(lm12) #AIC = 892.93

lm13 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$A2.2.3, data = bio, family = binomial(link = "logit"))
summary(lm13) #AIC = 891.15

lm14 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$A2.3.1, data = bio, family = binomial(link = "logit"))
summary(lm14) #AIC = 894.54

lm15 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$A2.3.2, data = bio, family = binomial(link = "logit"))
summary(lm15)

lm16 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$A2.3.3, data = bio, family = binomial(link = "logit"))
summary(lm16)

lm17 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$A2.3.4, data = bio, family = binomial(link = "logit"))
summary(lm17) #AIC = 894.54

lm18 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$A2.4.2, data = bio, family = binomial(link = "logit"))
summary(lm18)

lm19 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$A2.4.3, data = bio, family = binomial(link = "logit"))
summary(lm19)

lm20 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$A2.4, data = bio, family = binomial(link = "logit"))
summary(lm20) #AIC = 894.54

lm21 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3, data = bio, family = binomial(link = "logit"))
summary(lm21)

lm22 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.1, data = bio, family = binomial(link = "logit"))
summary(lm22) #AIC = 892.56 

lm23 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2, data = bio, family = binomial(link = "logit"))
summary(lm23)

lm24 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$E3.3, data = bio, family = binomial(link = "logit"))
summary(lm24)

lm25 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.3, data = bio, family = binomial(link = "logit"))
summary(lm25) #AIC = 893.64

lm26 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$T4.1, data = bio, family = binomial(link = "logit"))
summary(lm26)

lm27 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$T4.2, data = bio, family = binomial(link = "logit"))
summary(lm27) #AIC = 879.54

lm28 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.1, data = bio, family = binomial(link = "logit"))
summary(lm28)

lm29 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.1.1, data = bio, family = binomial(link = "logit"))
summary(lm29) #AIC = 863.97

lm30 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.1.2, data = bio, family = binomial(link = "logit"))
summary(lm30)

lm31 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1, data = bio, family = binomial(link = "logit"))
summary(lm31)

lm32 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$B5.3, data = bio, family = binomial(link = "logit"))
summary(lm32) #AIC = 861.38

lm33 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$B5.4, data = bio, family = binomial(link = "logit"))
summary(lm33) #AIC = 861.07

lm34 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1, data = bio, family = binomial(link = "logit"))
summary(lm34) #AIC = 832.28

lm35 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$H6.2, data = bio, family = binomial(link = "logit"))
summary(lm35)

lm36 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$H6.3, data = bio, family = binomial(link = "logit"))
summary(lm36) #AIC = 836.25

#Third Second Best?
lm37 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$N7.2, data = bio, family = binomial(link = "logit"))
summary(lm37) #AIC = 832.5 

lm38 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$N7.2 + no_dd$N7.2.3, data = bio, family = binomial(link = "logit"))
summary(lm38) #AIC = 834

lm39 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$N7.2 + no_dd$N7.3, data = bio, family = binomial(link = "logit"))
summary(lm39) #AIC = 832.3

#Second Best One?
lm40 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$N7.2 + no_dd$I8.1, data = bio, family = binomial(link = "logit"))
summary(lm40) #AIC = 830.49 

lm41 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$N7.2 + no_dd$I8.1 + no_dd$I8.2, data = bio, family = binomial(link = "logit"))
summary(lm41) #AIC = 830.49 

lm42 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$N7.2 + no_dd$I8.1 + no_dd$I8.4, data = bio, family = binomial(link = "logit"))
summary(lm42) #830.45, but not all values sig 

lm43 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$N7.2 + no_dd$I8.1 + no_dd$I8.5.1, data = bio, family = binomial(link = "logit"))
summary(lm43) #830.45, but not all values sig 

lm44 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$N7.2 + no_dd$I8.1 + no_dd$I8.5.2, data = bio, family = binomial(link = "logit"))
summary(lm44) #NA  

#Best One? 
lm45 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$N7.2 + no_dd$I8.1 + no_dd$P9.1, data = bio, family = binomial(link = "logit"))
summary(lm45) #AIC = 826.89

#???
lm46 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$P9.1, data = bio, family = binomial(link = "logit"))
summary(lm46) #AIC = 829.97 - two variables deleted (?)

lm47 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$N7.2 + no_dd$I8.1 + no_dd$P9.1 + no_dd$P9.2, data = bio, family = binomial(link = "logit"))
summary(lm47) #AIC = 825.02 

#Best one? 
lm48 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$I8.1 + no_dd$P9.1 + no_dd$P9.2, data = bio, family = binomial(link = "logit"))
summary(lm48) #AIC = 825.7, deleted a variable


lm49 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$I8.1 + no_dd$P9.1 + no_dd$P9.2 + no_dd$P9.3, data = bio, family = binomial(link = "logit"))
summary(lm49)  

lm50 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$I8.1 + no_dd$P9.1 + no_dd$P9.2 + no_dd$P9.4, data = bio, family = binomial(link = "logit"))
summary(lm50)  

lm51 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$I8.1 + no_dd$P9.1 + no_dd$P9.2 + no_dd$P9.5.3, data = bio, family = binomial(link = "logit"))
summary(lm51)  

lm52 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$I8.1 + no_dd$P9.1 + no_dd$P9.2 + no_dd$G10.1, data = bio, family = binomial(link = "logit"))
summary(lm52) #AIC = 827.69 

#Best one? 
lm53 <-  glm(no_dd$threatened ~ no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$I8.1 + no_dd$P9.1 + no_dd$P9.2 + no_dd$C11.2, data = bio, family = binomial(link = "logit"))
summary(lm53) #AIC = 813.91,deleted R1.2 (NO LONGER SIG) .... 
              #with R1.2 and addign this new variable climate (drought)... AIC = 813.42 and R1.2 is no longer significant
              #Removing R1.2 and keeping that C11.2... AIC = 813.91 - not sure if I should keep or delete 

lm54 <-  glm(no_dd$threatened ~ no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$I8.1 + no_dd$P9.1 + no_dd$P9.2 + no_dd$C11.2 + no_dd$C11.3, data = bio, family = binomial(link = "logit"))
summary(lm54)  

lm55 <-  glm(no_dd$threatened ~ no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$I8.1 + no_dd$P9.1 + no_dd$P9.2 + no_dd$C11.2 + no_dd$C11.4, data = bio, family = binomial(link = "logit"))
summary(lm55) #AIC = 813.91,deleted R1.2 (NO LONGER SIG) .... 




#Contenders 
#1 
lm53 <-  glm(no_dd$threatened ~ no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$I8.1 + no_dd$P9.1 + no_dd$P9.2 + no_dd$C11.2, data = no_dd, family = binomial(link = "logit"))
summary(lm53) #AIC = 813.91,deleted R1.2 (NO LONGER SIG) .... 
#with R1.2 and addign this new variable climate (drought)... AIC = 813.42 and R1.2 is no longer significant
#Removing R1.2 and keeping that C11.2... AIC = 813.91 - not sure if I should keep or delete 

summary(no_dd$threatened)
summary(no_dd$E3.2)


#Lets try using some of the AmphiBIO data as well 

#Habitat, breading strategy and body_size_mm percent of completeness
#Start with habitat 


      #going to make a copy of no_dd because I am going to change the ones that are NA to 0 
amph_test <- no_dd

amph_test$Fos <- ifelse(is.na(amph_test$Fos) == TRUE, 0, 1) 
amph_test$Ter <- ifelse(is.na(amph_test$Ter) == TRUE, 0, 1) 
amph_test$Aqu <- ifelse(is.na(amph_test$Aqu) == TRUE, 0, 1)
amph_test$Arb <- ifelse(is.na(amph_test$Arb) == TRUE, 0, 1)

a1 <- glm(amph_test$threatened ~ amph_test$Fos + amph_test$Ter + amph_test$Aqu +  no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$I8.1 + no_dd$P9.1 + no_dd$P9.2 + no_dd$C11.2, data = amph_test, family = binomial(link = "logit"))
summary(a1) #AIC = 737.76  (threats + habitats)

a2 <- glm(amph_test$threatened ~ amph_test$Fos + amph_test$Ter + amph_test$Aqu +amph_test$Body_size_mm  +  no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$I8.1 + no_dd$P9.1 + no_dd$P9.2 + no_dd$C11.2, data = amph_test, family = binomial(link = "logit"))
summary(a2) 

a3 <- glm(amph_test$threatened ~ amph_test$Ter + amph_test$Aqu +amph_test$Body_size_mm  +  no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$I8.1 + no_dd$P9.1 + no_dd$P9.2 + no_dd$C11.2, data = amph_test, family = binomial(link = "logit"))
summary(a3) 


a4 <- glm(amph_test$threatened ~ amph_test$Ter +amph_test$Body_size_mm  +  no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$I8.1 + no_dd$P9.1 + no_dd$P9.2 + no_dd$C11.2, data = amph_test, family = binomial(link = "logit"))
summary(a4) #AIC = 556.02


a5 <- glm(amph_test$threatened ~ amph_test$Ter +amph_test$Body_size_mm  +  no_dd$A2.1.3 + no_dd$B5.1 + no_dd$H6.1 + no_dd$I8.1 + no_dd$P9.1 + no_dd$P9.2 + no_dd$C11.2, data = amph_test, family = binomial(link = "logit"))
summary(a5) #AIC = 555.91


a6 <- glm(amph_test$threatened ~ amph_test$Ter +amph_test$Body_size_mm  + no_dd$A2.1.3 + no_dd$H6.1 + no_dd$I8.1 + no_dd$P9.1 + no_dd$P9.2 + no_dd$C11.2, data = amph_test, family = binomial(link = "logit"))
summary(a6) #AIC = 554.92

a7 <- glm(amph_test$threatened ~ amph_test$Ter +amph_test$Body_size_mm  + no_dd$A2.1.3 + no_dd$H6.1 + no_dd$P9.1 + no_dd$P9.2 + no_dd$C11.2, data = amph_test, family = binomial(link = "logit"))
summary(a7) #AIC = 555.77, this has body_Size_mm  (threats + body_size_mm + habitat)

getwd()
write.csv(no_dd,"/Users/Alexandra_Gonzalez/Desktop/NO_DD.csv")
#breeding strategy: dir, larv, viv

a8 <- glm(amph_test$threatened ~ amph_test$Lar + amph_test$Viv, data = amph_test, family = binomial(link = "logit"))
summary(a8)
summary(amph_test$Lar)

summary(no_dd)


#################################################
############DATA ANALYSIS APRIL 14, 2018 ########


###Functions 
threat_details <- function(species_list){
  datalist <- list()
  i <- 1 
  for (species in species_list){
    threats <- rl_threats(species, key = token)
    df1 <- threats$result
    df1$species_name <- species 
    datalist[[i]] <- df1
    i <- i + 1 
  }
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
  library(dplyr)
  big_data <- dplyr::bind_rows(datalist)
  big_data <- as.data.frame(big_data)
  final <- filter(big_data, !(code %in% thirdlevel))
  reordered <- final[,c("code","title","timing","scope","severity","score","invasive","species_name")]
  return(reordered)
}
threat_code <- function(threat_details_output){
  ### data is a dataframe with the species_list  
  ### as one column and the threat_code as the
  ### other aims to work directly with sp_threats 
  ###and threat_tables 
  species_list <- names(table(threat_details_output$species_name)) 
  threat_code <- vector(mode = "character", length = length(species_list))
  i <- 1
  library(dplyr)
  for (species in species_list){
    subset_species <- filter(threat_details_output, threat_details_output$species_name == species)
    code <- names(table(subset_species$code))
    threat_code[i] <- paste(code, collapse = " ")
    i <- 1 + i 
  }
  threatdf <- data.frame(species_list, threat_code)
  return(threatdf)
}
no_third_level <- function(species_list){
  R1.1 <- rep(c(0),  each = length(species_list)) 
  R1.2 <- rep(c(0),  each = length(species_list))
  R1.3 <- rep(c(0),  each = length(species_list))
  
  #Agriculture and Aquaculture 
  A2.1 <- rep(c(0),  each = length(species_list))
  A2.2 <- rep(c(0),  each = length(species_list))
  A2.3 <- rep(c(0),  each = length(species_list))
  A2.4 <- rep(c(0),  each = length(species_list))
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
  B5.2 <- rep(c(0),  each = length(species_list))
  
  B5.3 <- rep(c(0),  each = length(species_list))
  
  B5.4 <- rep(c(0),  each = length(species_list))
  #Human intrustions and disturbance 
  H6.1 <- rep(c(0),  each = length(species_list))  
  H6.2 <- rep(c(0),  each = length(species_list))
  H6.3 <- rep(c(0),  each = length(species_list))
  
  #Natural system modifications 
  N7.1 <- rep(c(0),  each = length(species_list))
  
  N7.2 <- rep(c(0),  each = length(species_list))
  N7.3 <- rep(c(0),  each = length(species_list))
  
  #Invasive and other problematic species, genes and disease 
  I8.1 <- rep(c(0),  each = length(species_list))
  
  I8.2 <- rep(c(0),  each = length(species_list))
  I8.3 <- rep(c(0),  each = length(species_list))
  
  I8.4 <- rep(c(0),  each = length(species_list))
  
  I8.5 <- rep(c(0),  each = length(species_list))
  
  I8.6 <- rep(c(0),  each = length(species_list))
  
  #Pollution 
  P9.1 <- rep(c(0),  each = length(species_list))
  
  P9.2 <- rep(c(0),  each = length(species_list))
  P9.3 <- rep(c(0),  each = length(species_list))
  
  P9.4 <- rep(c(0),  each = length(species_list))
  
  P9.5 <- rep(c(0),  each = length(species_list))
  
  P9.6 <- rep(c(0),  each = length(species_list))
  
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
  
  threat_df <- data.frame(species_list, R1.1, R1.2, R1.3, A2.1, A2.2, A2.3, A2.4, E3.1, E3.2,
                          E3.3, T4.1, T4.2, T4.3, T4.4, B5.1, B5.2,B5.3, B5.4,
                          H6.1, H6.2, H6.3, N7.1,N7.2,N7.3, I8.1,I8.2,I8.3, I8.4, I8.5,I8.6, P9.1,
                          P9.2, P9.3,P9.4,P9.5, P9.6, G10.1, G10.2, G10.3, C11.1, C11.2, C11.3, C11.4, C11.5)
  return(threat_df)
} #Empty table of threats 
threat_tables <- function(data){
  ### data is a dataframe with the species_list  
  ### as one column and the threat_code as the
  ### other aims to work directly with sp_threats 
  ###and threat_tables 
  threatdf <- no_third_level(data$species_list)
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

####Data 

#This is data that has been reduced because of AmphiBIO Connection
class(full_se_data$species_list[1:5])
set1 <- threat_details(full_se_data$species_list[1:5])
set2 <- threat_details(full_se_data$species_list[6:10])
set3 <- threat_details(full_se_data$species_list[11:14])
set4 <- threat_details(full_se_data$species_list[15:30])
set5 <- threat_details(full_se_data$species_list[31:70])
set6 <- threat_details(full_se_data$species_list[71:140])
set7 <- threat_details(full_se_data$species_list[141:200])
set8 <- threat_details(full_se_data$species_list[201:300])
set9 <- threat_details(full_se_data$species_list[301:400])
set10 <- threat_details(full_se_data$species_list[401:500])
set11 <- threat_details(full_se_data$species_list[501:600])
set12 <- threat_details(full_se_data$species_list[601:700])
set13 <- threat_details(full_se_data$species_list[701:800])
set14 <- threat_details(full_se_data$species_list[801:900])
set15 <- threat_details(full_se_data$species_list[901:931])
        #Only data that matches with AmphiBIO 
threat_specifics <- rbind(set1,set2, set3, set4, set5, set6,
                           set7, set8, set9, set10, set11, set12,
                           set13, set14, set15)

#full_se_data = Where AmphiBIO data and IUCN data did not match, filtered out 
#when only analysing the threats from iucn, I dont necessarily care about
#where they match or dont match
wu <- setdiff(se_asian_amph$new_species, full_se_data$new_species)
set16 <- threat_details(wu[1:150])
set17 <- threat_details(wu[151:182])

details <- rbind(set1,set2, set3, set4, set5, set6,
              set7, set8, set9, set10, set11, set12,
              set13, set14, set15, set16, set17)
summary(details)

code_name <- threat_code(details)
finaldata <- threat_tables(code_name)
summary((finaldata$R1.1))



rl_threats("Amolops cucae", key = token)
############################################


#Start filtering the data 
library(sqldf)
library(dplyr)
library(ggplot2)



    #copy details, then add se_asian_amph data to it
copy <- details
summary(copy)
together <- merge(copy, se_asian_amph, by = "species_name")
summary(together)


together #has iucn taxon info and iucn threat info
summary(together)

ALLDATA  #COPY OF TOGETHER. IM GOING TO REMOVE DD FROM TOGETHER AND REDO ANALYSIS 

count(ALLDATA$title)







together <- together[!(together$Red.List.status == "DD"),]
summary(together$Red.List.status)


together$code <- as.factor(together$code)
together$title <- as.factor(together$title)
together$timing <- as.factor(together$timing)
together$scope <- as.factor(together$scope)
together$severity <- as.factor(together$severity)
together$score <- as.factor(together$score)
together$invasive <- as.factor(together$invasive)
together$threatened <- ifelse(together$Red.List.status == "LC" | together$Red.List.status == "NT", "Not Threatened", "Threatened")
together$threatened <- as.factor(together$threatened)


#Graphs by Order 
caudata <- filter(together, Order == "CAUDATA")
gymnophiona <- filter(together, Order == "GYMNOPHIONA")
anura <- filter(together, Order == "ANURA")

card1 <- ggplot(together, mapping = aes(x = Population.trend, fill = Red.List.status)) + 
  geom_bar(stat = "count")

#annual_crops 
annual_crops <- filter(together, code == 2.1)
ag_redlist <- summary(annual_crops$Red.List.status)
ag_redlist <- as.data.frame(count(annual_crops, Red.List.status))


#logging and wood harvesting 
logging_wood <- filter(together, code == 5.3)
log_redlist <- as.data.frame(count(logging_wood, Red.List.status))

#do threatened vs not_threatened .... and then compare the titles of the threats to see
  #if there is any real difference 



##############################
###correlation
#############################
ag <- filter(finaldata, A2.1 == 1)
summary(ag)
final_num <- finaldata
final_num <- apply(final_num, 2, as.numeric)
threat_cor <- cor(final_num)
final_num$I8.3 <- NULL

plot(threat_cor)
library(reshape2)
melted_threats <- melt(threat_cor)
head(melted_threats)
        #testing the signifcance of the correlation: (need to see if 
        #pearsons test is the best for this - is this data paired?)
cor.test(final_num$C11.5, final_num$C11.1)
cor.test(final_num$R1.1, final_num$R1.2)


library(ggplot2)
cardib3 <- ggplot(data = melted_threats, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, 
                                   size = 9, hjust = 1))+
  coord_fixed()
#############################
#MAKE GRAPHS SO ISH CAN POP OFF
############################



#Figure 1 
bodak1 <- ggplot(data = together, aes (x = code, fill = threatened)) +
            geom_bar(stat = "count", position = "dodge") + 
            xlab(label = "Red List Threat Code") + ylab(label = "Species Count") + 
            #Removes the grid lines in the back: 
            theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
            theme(axis.text.x = element_text(angle = 60, vjust = 1, size = 9, hjust = 1)) +
            guides(fill=guide_legend(title=NULL)) +
            scale_fill_manual(values = c("dark grey", "red")) + 
            #Reorders the x acis to be in the correct order
            scale_x_discrete(limits = c('1.1', '1.2', '1.3', '2.1', '2.2', '2.3', 
                                        '2.4', '3.2', '3.3', '4.1', '4.2', '5.1', 
                                        '5.2', '5.3', '5.4', '6.1', '6.2', '6.3', 
                                        '7.1', '7.2', '7.3', '8.1', '8.2', '8.4', 
                                        '9.1', '9.2', '9.3', '9.4', '10.1', '11.1',
                                        '11.2', '11.3', '11.4', '12.1', NA))

bodak1

bodak2 <- ggplot(data = together, aes( x = timing)) + geom_bar(stat = "count") +
            theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
            theme(axis.text.x = element_text(angle = 30, vjust = 1, size = 9, hjust = 1)) +
            guides(fill=guide_legend(title=NULL)) +
            xlab(label = "Red List Threat Timing") + ylab(label = "Count") 
bodak2

bodak3 <- ggplot(data = together, aes( x = scope)) + geom_bar(stat = "count") +
              theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
              theme(axis.text.x = element_text(angle = 30, vjust = 1, size = 9, hjust = 1)) +
              guides(fill=guide_legend(title=NULL)) +
              xlab(label = "Threat Scope") + ylab(label = "Count") 
bodak3

bodak4 <- ggplot(data = together, aes( x = severity)) + geom_bar(stat = "count") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  xlab(label = "Threat Severity") + ylab(label = "Count") 
bodak4


bodak5 <- ggplot(data = together, aes( x = score)) + geom_bar(stat = "count") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  xlab(label = "Threat Score") + ylab(label = "Count") 
bodak5






bodak6 <- ggplot(data = together, aes( x = score, fill = threatened)) + geom_bar(stat = "count", position = "dodge") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  xlab(label = "Threat Score") + ylab(label = "Count") 
bodak6

bodak7 <- ggplot(data = together, aes( x = Family, fill = threatened)) + geom_bar(stat = "count", position = "dodge") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  xlab(label = "Family") + ylab(label = "Count") 
bodak7


bodak8 <- ggplot(data = together, aes( x = Family, fill = Red.List.status)) + geom_bar(stat = "count", position = "dodge") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  xlab(label = "Family") + ylab(label = "Count") 
bodak8


bodak9 <- ggplot(data = together, aes( x = Year.assessed)) + geom_bar(stat = "count", position = "dodge") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  xlab(label = "Year Assessed") + ylab(label = "Count") 
bodak9



#####
KOD <- ggplot(data = together, aes( x = Red.List.status, fill = Red.List.status)) + geom_bar(stat = "count") + coord_polar() + facet_wrap(~Year.assessed)
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  xlab(label = "Threat Score") + ylab(label = "Count") 
bodak5

ggplot(data = together) + 
  geom_count(mapping = aes(x = score, y = timing, size = prop))

#Rhacophoridae: Select only data from each family 
      #some of thesee graphs are off because they are duplicates
      #i.e. same species but many rows because there are
      #lots of threats.
      #need to eliminate duplicates and then take these into consideration
      #right now, the number is off 
rhacophoridae <- together %>% filter(together$Family == "RHACOPHORIDAE")
rhac1 <- ggplot(data = rhacophoridae, aes( x = title)) + geom_bar(stat = "count", position = "dodge") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  xlab(label = "Threat Type") + ylab(label = "Count") 
rhac1 

rhac2 <- ggplot(data = rhacophoridae, aes( x = timing)) + geom_bar(stat = "count", position = "dodge") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  xlab(label = "Threat Timing") + ylab(label = "Count") 
rhac2

rhac3 <- ggplot(data = rhacophoridae, aes( x = severity)) + geom_bar(stat = "count", position = "dodge") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  xlab(label = "Threat Timing") + ylab(label = "Count") 
rhac3

rhac4 <- ggplot(data = rhacophoridae, aes( x = Red.List.status)) + geom_bar(stat = "count", position = "dodge") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  xlab(label = "Red List Status") + ylab(label = "Count") 
rhac4

#Ranidae 
ranidae <- together %>% filter(together$Family == "RANIDAE")
ran1 <- ggplot(data = together, aes( x = title)) + geom_bar(stat = "count", position = "dodge") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  xlab(label = "Threat Type") + ylab(label = "Count") 
ran1 


###^ graphs explaining the overview of the data 






library(rredlist)
rl_threats("Mantella cowanii", key = token)

#################
##CHI SQUARE TEST
#################

yes_threatened <- sqldf("SELECT * FROM together WHERE threatened == 'Threatened' ")
no_threatened <- sqldf ("SELECT * FROM together WHERE threatened != 'Threatened' ")



yes_threatened$title <- as.factor(yes_threatened$title)
no_threatened$title <- as.factor(no_threatened$title)
summary(yes_threatened$title)

summary(yes_threatened$title)
#Want to see if there is a significant difference in the number of threats between 
#threatened and not threatened population 

          #Top 5 threats included (chisquare does not include NA)
library(dplyr)
s1 <- sqldf("SELECT title, threatened, timing FROM together")
#1.1, 2.1, 5.3, 9.3, NA 
s1$timing <- as.factor(s1$timing)
summary(s1$timing)
n <- high_threats$title

s2 <- filter(s1, title %in% n)
#s2 <- s1 %>% filter(s1$title == "Annual & perennial non-timber crops" |s1$title == "Logging & wood harvesting")
      #chi square null hypothesis: the rows and columns are independent 
      #chi square alt. hypothesis: the rowas and columns are dependent 
rs1 <- chisq.test(s2$threatened, s2$title, correct = FALSE)


low_threats <- count(together, title)
low_threats <- as.data.frame(low_threats)
low_threats <- order(low_threats, decreasing = TRUE)

high_threats <- filter(low_threats, low_threats$n >= 10)

      #the p value is less than 0.05, therefore we can reject the null hypothesis 
      #and accept the alternate hypothesis. 


