######################
#Trying to find a better McFadden = 0.435 
#See Southeast Asia Data2.R script as well 
######################


#Here's the one to beat from Meeting w/ Patricia: 
#Mcfaden = 0.4357484
hunt_body2 <- glm(formula = threatened ~ Order + 
                    A2.1 + H6.1 + 
                    Body_size_mm + Body_size_mm:B5.1 + B5.1 +
                    P9.1 + P9.2, family = binomial(link = "logit"), data = amph_thr_iucn)
pR2(hunt_body2) 


### START - LOGISTIC REGRESSION W/ THREATS ONLY (NO ECOLOGICAL DATA) 
june1 <- glm(amph_thr_iucn$threatened ~ Order + R1.2 + R1.3 + A2.1 + A2.2 + A2.3 +
               A2.4 + E3.2 +  T4.1 + 
               T4.2 + B5.1 + B5.2 + B5.3 + 
               B5.4 + H6.1 +  N7.1 + 
               N7.2 +  N7.3 + I8.1 + 
               P9.1 + P9.2 + P9.3 + P9.4 + G10.1 + 
               C11.2 + C11.4, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(june1) 
pR2(june1) #McFaden = 0.17 

      #Removing insignificant variables one-by-one based on lowest estimate first 
june2 <- glm(amph_thr_iucn$threatened ~ Order + R1.2 + A2.1 + A2.2 + A2.3 +
               A2.4 + E3.2 +  T4.1 + 
               T4.2 + B5.1 + B5.2 + B5.3 + 
               B5.4 + H6.1 +  N7.1 + 
               N7.2 +  N7.3 + I8.1 + 
               P9.1 + P9.2 + P9.3 + P9.4 + G10.1 +
               C11.2 + C11.4, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(june2) 
pR2(june2) #McFaden = 0.17

june3 <- glm(amph_thr_iucn$threatened ~ Order + R1.2 + A2.1 + A2.2 +
               A2.4 + E3.2 +  T4.1 + 
               T4.2 + B5.1 + B5.2 + B5.3 + 
               B5.4 + H6.1 +  N7.1 + 
               N7.2 +  N7.3 + I8.1 + 
               P9.1 + P9.2 + P9.3 + P9.4 + G10.1 +
               C11.2 + C11.4, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(june3) 
pR2(june3) #McFaden = 0.1704071, Removed A2.3 

june4 <- glm(amph_thr_iucn$threatened ~ Order + R1.2 + A2.1 + A2.2 +
               E3.2 +  T4.1 + 
               T4.2 + B5.1 + B5.2 + B5.3 + 
               B5.4 + H6.1 +  N7.1 + 
               N7.2 +  N7.3 + I8.1 + 
               P9.1 + P9.2 + P9.3 + P9.4 + G10.1 +
               C11.2 + C11.4, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(june4) 
pR2(june4) #McFaden = 0.1704066, Removed A2.4 

june5 <- glm(amph_thr_iucn$threatened ~ Order + R1.2 + A2.1 + A2.2 +
               E3.2 + 
               T4.2 + B5.1 + B5.2 + B5.3 + 
               B5.4 + H6.1 +  N7.1 + 
               N7.2 +  N7.3 + I8.1 + 
               P9.1 + P9.2 + P9.3 + P9.4 + G10.1 +
               C11.2 + C11.4, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(june5) 
pR2(june5) #McFaden = 0.1703809, Removed T4.1  

june6 <- glm(amph_thr_iucn$threatened ~ Order + R1.2 + A2.1 + A2.2 +
               E3.2 + 
               T4.2 + B5.1 + B5.2 + B5.3 + 
               B5.4 + H6.1 +  N7.1 + 
               N7.2 +  N7.3 + I8.1 + 
               P9.1 + P9.2 + P9.4 + G10.1 +
               C11.2 + C11.4, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(june6) 
pR2(june6) #McFaden = 0.1703652, Removed P9.3  

june7 <- glm(amph_thr_iucn$threatened ~ Order + R1.2 + A2.1 + A2.2 +
               E3.2 + 
               T4.2 + B5.1 + B5.2 + 
               B5.4 + H6.1 +  N7.1 + 
               N7.2 +  N7.3 + I8.1 + 
               P9.1 + P9.2 + P9.4 + G10.1 +
               C11.2 + C11.4, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(june7) 
pR2(june7) #McFaden = 0.1690688, Removed B5.3  


june8 <- glm(amph_thr_iucn$threatened ~ Order + R1.2 + A2.1 + A2.2 +
               E3.2 + 
               T4.2 + B5.1 + B5.2 + 
               B5.4 + H6.1 + 
               N7.2 +  N7.3 + I8.1 + 
               P9.1 + P9.2 + P9.4 + G10.1 +
               C11.2 + C11.4, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(june8) 
pR2(june8) #McFaden = 0.1681172, Removed N7.1  

june9 <- glm(amph_thr_iucn$threatened ~ Order + R1.2 + A2.1 + A2.2 +
               E3.2 + 
               T4.2 + B5.1 + B5.2 + 
               B5.4 + H6.1 + 
               N7.2 + I8.1 + 
               P9.1 + P9.2 + P9.4 + G10.1 +
               C11.2 + C11.4, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(june9) 
pR2(june9) #McFaden = 0.1674999, Removed N7.3  

june10 <- glm(amph_thr_iucn$threatened ~ Order + R1.2 + A2.1 + A2.2 +
               E3.2 + 
               T4.2 + B5.1 + B5.2 + 
               B5.4 + H6.1 + 
               N7.2 + I8.1 + 
               P9.1 + P9.2 + P9.4 + G10.1 +
               C11.2, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(june10) 
pR2(june10) #McFaden = 0.1657746, Removed C11.4  

june11 <- glm(amph_thr_iucn$threatened ~ Order + R1.2 + A2.1 + A2.2 +
                E3.2 + 
                T4.2 + B5.1 + B5.2 + 
                B5.4 + H6.1 + 
                N7.2 + I8.1 + 
                P9.1 + P9.2 + P9.4 +
                C11.2, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(june11) 
pR2(june11) #McFaden = 0.1657126, Removed G10.1  

june12 <- glm(amph_thr_iucn$threatened ~ Order + R1.2 + A2.1 + A2.2 +
                E3.2 + 
                T4.2 + B5.1 + B5.2 + 
                H6.1 + 
                N7.2 + I8.1 + 
                P9.1 + P9.2 + P9.4 +
                C11.2, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(june12) 
pR2(june12) #McFaden = 0.1644943, Removed B5.4  

june13 <- glm(amph_thr_iucn$threatened ~ Order + R1.2 + A2.1 + A2.2 +
                E3.2 + 
                T4.2 + B5.1 + B5.2 + 
                H6.1 + 
                N7.2 + I8.1 + 
                P9.1 + P9.2 +
                C11.2, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(june13) 
pR2(june13) #McFaden = 0.1619884, Removed P9.4  

june14 <- glm(amph_thr_iucn$threatened ~ Order + R1.2 + A2.1 + A2.2 +
                E3.2 + 
                B5.1 + B5.2 + 
                H6.1 + 
                N7.2 + I8.1 + 
                P9.1 + P9.2 +
                C11.2, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(june14) 
pR2(june14) #McFaden = 0.1619295, Removed T4.2  

june15 <- glm(amph_thr_iucn$threatened ~ Order + R1.2 + A2.1 +
                E3.2 + 
                B5.1 + B5.2 + 
                H6.1 + 
                N7.2 + I8.1 + 
                P9.1 + P9.2 +
                C11.2, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(june15) 
pR2(june15) #McFaden = 0.1598814, Removed A2.2  

june16 <- glm(amph_thr_iucn$threatened ~ Order + R1.2 + A2.1 +
                E3.2 + 
                B5.1 + 
                H6.1 + 
                N7.2 + I8.1 + 
                P9.1 + P9.2 +
                C11.2, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(june16) 
pR2(june16) #McFaden = 0.1569767, Removed B5.2  

june17 <- glm(amph_thr_iucn$threatened ~ Order + R1.2 + A2.1 +
                E3.2 + 
                B5.1 + 
                H6.1 + 
                I8.1 + 
                P9.1 + P9.2 +
                C11.2, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(june17) 
pR2(june17) #McFaden = 0.1539489, Removed N7.2  

june18 <- glm(amph_thr_iucn$threatened ~ Order + A2.1 +
                E3.2 + 
                B5.1 + 
                H6.1 + 
                I8.1 + 
                P9.1 + P9.2 +
                C11.2, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(june18) 
pR2(june18) #McFaden = 0.1509784, Removed R1.2 


### START 2 - Ecological data 
library(pscl) 

  #amph_thr_iucn$Ter[is.na(amph_thr_iucn$Ter)] <- 0
  #amph_thr_iucn$Aqu[is.na(amph_thr_iucn$Aqu)] <- 0
  #amph_thr_iucn$Arb[is.na(amph_thr_iucn$Arb)] <- 0

  ####Habitat Only: (FOS, TER, AQU, ARB)
nev1 <- glm(amph_thr_iucn$threatened ~ Fos + Ter + Aqu + Arb, 
            family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(nev1) 
pR2(nev1) #McFaden = 0.04, Habitats only 

  ####Body Size Only: (Body_size_mm)
nev2 <- glm(amph_thr_iucn$threatened ~ Body_size_mm, 
            family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(nev2) 
pR2(nev2) #McFaden = 0.33 

  ####Reproductive Output: (Reproductive_output_y)
nev3 <- glm(amph_thr_iucn$threatened ~ Reproductive_output_y, 
            family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(nev3) 
pR2(nev3) #McFaden = 0.28 

nev4 <- glm(amph_thr_iucn$threatened ~ Dir + Lar + Viv, 
            family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(nev4) 
pR2(nev4) #McFaden = 0.261 

#AmphiBIO Database: https://www.nature.com/articles/sdata2017123
  #Percent of Completeness - 
    #Habitat ~ 80%
    #Body size mm ~ 75% 
    #Reproductive output (per year) ~ 60%
    #Breeding strategy ~80% (Dir, Lar, Viv)

    #All ecological data w/ more than 60% completeness 
nev5 <- glm(amph_thr_iucn$threatened ~ Dir + Lar + Viv + Reproductive_output_y +
            Fos + Ter + Aqu + Arb + Body_size_mm, 
            family = binomial(link = "logit"), data = amph_thr_iucn)
summary(nev5) 
pR2(nev5) #McFaden = 0.480  

nev6 <- glm(amph_thr_iucn$threatened ~ Dir + Lar + Viv + Reproductive_output_y +
              Body_size_mm, 
            family = binomial(link = "logit"), data = amph_thr_iucn)
summary(nev6) 
pR2(nev6) #McFaden = 0.477, Removed habitat   

nev7 <- glm(amph_thr_iucn$threatened ~ Dir + Lar + Viv +
              Body_size_mm, 
            family = binomial(link = "logit"), data = amph_thr_iucn)
summary(nev7) 
pR2(nev7) #McFaden = 0.444, Removed Reproductive outpout 

nev8 <- glm(amph_thr_iucn$threatened ~ Body_size_mm, 
            family = binomial(link = "logit"), data = amph_thr_iucn)
summary(nev8) 
pR2(nev8) #McFaden = 0.33, Removed Breeding strategy (Dir, Lar, Viv = NA) 


summary(amph_thr_iucn$Lar)
summary(amph_thr_iucn$Viv)
summary(amph_thr_iucn$Dir)


nev7_ <- glm(amph_thr_iucn$threatened ~ Dir + 
              Body_size_mm, 
            family = binomial(link = "logit"), data = amph_thr_iucn)
summary(nev7_) 
pR2(nev7_) #McFaden = 0.444, Removed Reproductive outpout 


#Get the same McFaden value when either Dir and Lar are used 
nev9 <- glm(amph_thr_iucn$threatened ~ Lar +
              Body_size_mm, 
            family = binomial(link = "logit"), data = amph_thr_iucn)
summary(nev9) 
pR2(nev9) #McFaden = 0.4446, Removed Reproductive outpout 







###COMBINING THREAT AND ECOLOGICAL DATA  (No interactions)

combo1 <- glm(amph_thr_iucn$threatened ~ Order + Lar +
              Body_size_mm + A2.1 + E3.2 + B5.1 + H6.1 + 
              I8.1 + P9.1 + P9.2 +
              C11.2, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(combo1) 
pR2(combo1) #McFaden = 0.53

combo2 <- glm(amph_thr_iucn$threatened ~ Order + Lar +
                Body_size_mm + A2.1 + B5.1 + H6.1 + 
                I8.1 + P9.1 + P9.2 +
                C11.2, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(combo2) 
pR2(combo2) #McFaden = 0.53, Removed E3.2  

combo3 <- glm(amph_thr_iucn$threatened ~ Order + Lar +
                Body_size_mm + A2.1 + B5.1 + H6.1 + 
                 P9.1 + P9.2 +
                C11.2, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(combo3) 
pR2(combo3) #McFaden = 0.527, Removed E3.2, I8.1  


###############             ##############
############### Best Model  ##############
###############             ##############
combo4 <- glm(amph_thr_iucn$threatened ~ Order + Lar +
                Body_size_mm + A2.1 + H6.1 + 
                P9.1 + P9.2 +
                C11.2, family = binomial(link = "logit"), data = amph_thr_iucn) 
summary(combo4) 
pR2(combo4) #McFaden = 0.53, Removed E3.2, I8.1, B5.1


#Hold variable constant 
# ORDER = ANURA (because there are so many - and i'm interested in that) 
# make more sense as 0 or 1 - whether u have it or not 
# makes more sense when they are inbalanced too (more men than women, set at men)
  
#same as last model: 
    summary(amph_thr_iucn$A2.1) #Annual and perennial non-timber crops 
    #Set at 1 (more 1s than 0)
    summary(amph_thr_iucn$H6.1) #Recreational activities 
    #Set as 0 (mean is 0.0325)
    summary(amph_thr_iucn$P9.1) #Pollution - domestic and urban waste water 
    #set as 0, try varying
    summary(amph_thr_iucn$P9.2) #Pollution - indsutrial and military effluents 
    #set as 0 
    #ORDER = ANURA 
    # Try varying hunting and trapping FIRST, then try different pollutions (P's), 
        #then try H6.1,
    #try Agriculture after 
    # Try all of them 
    
 #new:    
    summary(amph_thr_iucn$Lar) #Larval - type of breeding strategy 
    #Mean at 0.8302, hold at 1 


  #from previous model, not using this:     
    summary(amph_thr_iucn$B5.1) #Hunting and trapping 
    ## TRY VARYING 
    #Set as 0, for the other vary (make it the colored variable in the graph -heart)

    
#Consider combining the two pollutions P9.1, P9.2 (make it into one boolean variable, pollution or no)

    # Produce grid to predict for threatened or not threatened 
          #for frogs (ANURA) with Larval breeding strategy,  
          #are threatened by annual and perennial non-timber crops (A2.1),
          #are not threatened by recreational activities (H6.1), 
          #are not threatened by pollution through domestic waste water (P9.1),
          #are not threatened by pollution through industrial and military effluents 
    # so hold A2.1 == 1, , P9.1 == 0, P9.2 == 0, Lar == 1,
    #VARY H6.1                        
    frogdf <- with(amph_thr_iucn,
                   data.frame(Body_size_mm = rep(seq(from = 13, to = 591, length.out = 250), 2), 
                        A2.1=1, P9.1=0, P9.2=0, Lar =1, 
                        H6.1 = factor(rep(0:1, each = 250)))) #making grid, 
                                                            #only vary is H6.1 (between)

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

