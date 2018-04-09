#Send to Dr. Francis-Lyon 

no_dd <- read.csv("no_dd.csv")
#Data description 
      #Columns 1:24 - IUCN General Data 
      #Columns 24:59 - AmphiBIO Data 
      #Columns 60:end - IUCN Threat data ~ collected by code I made (see preview on lines 10-198)

#Preview: Functions to get threat data 
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


#Logisitc Regressions 

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

lm37 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$N7.2, data = bio, family = binomial(link = "logit"))
summary(lm37) #AIC = 832.5 

lm38 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$N7.2 + no_dd$N7.2.3, data = bio, family = binomial(link = "logit"))
summary(lm38) #AIC = 834

lm39 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$N7.2 + no_dd$N7.3, data = bio, family = binomial(link = "logit"))
summary(lm39) #AIC = 832.3

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

lm45 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$N7.2 + no_dd$I8.1 + no_dd$P9.1, data = bio, family = binomial(link = "logit"))
summary(lm45) #AIC = 826.89

lm46 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$P9.1, data = bio, family = binomial(link = "logit"))
summary(lm46) #AIC = 829.97 - two variables deleted (?)

lm47 <-  glm(no_dd$threatened ~ no_dd$R1.2 + no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$N7.2 + no_dd$I8.1 + no_dd$P9.1 + no_dd$P9.2, data = bio, family = binomial(link = "logit"))
summary(lm47) #AIC = 825.02 

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

###################
###################
#Best one
lm53 <-  glm(no_dd$threatened ~ no_dd$A2.1.3 + no_dd$E3.2 + no_dd$B5.2 + no_dd$B5.1 + no_dd$H6.1 + no_dd$I8.1 + no_dd$P9.1 + no_dd$P9.2 + no_dd$C11.2, data = bio, family = binomial(link = "logit"))
summary(lm53) #AIC = 813.91,deleted R1.2 (NO LONGER SIG) .... 
                #with R1.2 and addign this new variable climate (drought)... AIC = 813.42 and R1.2 is no longer significant
                #Removing R1.2 and keeping that C11.2... AIC = 813.91 - not sure if I should keep or delete 

##################
#################


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

################
################
a7 <- glm(amph_test$threatened ~ amph_test$Ter +amph_test$Body_size_mm  + no_dd$A2.1.3 + no_dd$H6.1 + no_dd$P9.1 + no_dd$P9.2 + no_dd$C11.2, data = amph_test, family = binomial(link = "logit"))
summary(a7) #AIC = 555.77, this has body_Size_mm  (threats + body_size_mm + habitat)

###############
###############