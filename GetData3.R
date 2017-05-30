#Alexandra Gonzalez 
#Get data 3 

library("profvis") #profvis(sp_taxonomy(CR_names_10)) #how you check how long a function is 
library("rredlist")
token <- "cf51b7524f618f5b23220e687b70c01d0d240cc82d179cf2c5b08b70fdfb83d4" 

#Threat Count 
cr_threat_count <- sp_threat_count(copycr_frogs)
en_threat_count <- sp_threat_count(copyen_frogs)
vu_threat_count <- sp_threat_count(copyvu_frogs)
ex_threat_count <- sp_threat_count(copyex_frogs)


#Family 
cr_family <- sp_all_family(copycr_frogs)
en_family <- sp_all_family(copyen_frogs)
vu_family <- sp_all_family(copyvu_frogs)
ex_family <- sp_all_family(copyex_frogs)

x <- rl_habitats("Theloderma asperum", key = token)
lc <- clean_data2(LCC)
nt <- clean_data2(NTT) #done 

write.table(en_family, "/Users/Alexandra_Gonzalez/Desktop/Herpetology/en_family.txt", sep="\t")

new <- cr_threat_count
new$threat_count <- as.factor(new$threat_count)

plot(new$threat_count)

new2 <- en_threat_count
new2$threat_count <- as.factor(new2$threat_count)

plot(new2$threat_count)

lc_frogs
