######################
#Trying to find a better McFadden = 0.435 
#See Southeast Asia Data2.R script as well 
######################


#Here's the one to beat 
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


### START 2 - Logistic regression with threats AND ecological data 

