#Alexandra Gonzalez 
#Habitat Aquisition 


library("profvis") #profvis(sp_taxonomy(CR_names_10)) #how you check how long a function is 
library("rredlist")
token <- "cf51b7524f618f5b23220e687b70c01d0d240cc82d179cf2c5b08b70fdfb83d4" 


rl_habitats("Melanophryniscus tumifrons", key = token)
  #what if i use lapply because its already in a list format 
lapply()
