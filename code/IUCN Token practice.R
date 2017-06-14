#install.packages("rredlist")
library("rredlist")

token <- "cf51b7524f618f5b23220e687b70c01d0d240cc82d179cf2c5b08b70fdfb83d4" 


#IUCN API   
asperum <- rl_search('Theloderma asperum', key = token) #leaves a data frame
asperum$result
asperum2 <- rl_search('Theloderma asperum', key = token, parse = FALSE) #faster... just a list, still a high level api 
threat_level <- asperum2$result[[1]]$category




#High level API: do the HTTP request and parse data to a data frame. 
#Low Level API: only does HTTP request and gives back JSON w/o doing more parsing.
#low level functions DO have an underscore at the end of them ... rl_search_ 


#GET COUNTRIES 
rl_countries(key = token, parse = TRUE) 
      #There are 251 countries. 


#GET SPECIES habitats by taxon name, IUCN id, and region
practice <- rl_habitats('Fratercula arctica', key = token)
rl_habitats('Fratercula arctica', region = 'europe', key = token)


rl_sp_count(key = token, parse = TRUE)





#Get species by category 
all_vulnerable_species <- rl_sp_category('VU', key = token, parse = TRUE)
all_vulnerable_species$result[[2]] #this leaves out #1 






