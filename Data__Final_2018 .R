
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





