#Alexandra Gonzalez
#March 29, 2017 


library("profvis") #profvis(sp_taxonomy(CR_names_10)) #how you check how long a function is 
library("rredlist")
token <- "cf51b7524f618f5b23220e687b70c01d0d240cc82d179cf2c5b08b70fdfb83d4" 

#####Functions: 
#(1)Data Prepocessing 

clean_data2 <- function(species_list){
  df<- as.data.frame(species_list)
  df[is.na(df)] <- 0
  only_species <- subset(df, (df$result.subspecies == 0) &
                  (df$result.rank == 0) & 
                  (df$result.subpopulation == 0))
  only_species <- only_species$result.scientific_name
  return(only_species)
}

#(2)Specify taxonomy, get all species of listed taxonomy 
sp_class <- function (species_list){
  #Des.: Traverses through a list, and pulls out Class = AMPHIBIA
  #Input: species_list is any list of species w/ and w/o amphibians 
  #Output: Vector of amphibians 
  #Consider Adding Parameters: token, desired class (that way it can be any)
  amphibian_list <- vector(length = length(species_list), 
                           mode = "character")  
  i <- 1 
  for (species in species_list){
    general_info <- rl_search(species, key = token)
    Sys.sleep(2) 
    tax <- general_info$result$class
    if (tax == "AMPHIBIA"){
      amphibian_list[i] <- species
    }
    i <- i+1
  }
  return(amphibian_list[amphibian_list != ""])
}  
sp_family <- function (species_list, desired.family){
  #Des.: Traverses through a list, and pulls out selected taxa 
  #Input: species_list is any list of species w/ and w/o amphibians
  #Input: desired.family = name of family you'd like to pull species names
  #Output: Vector of amphibian species of desired family  
  #Consider Adding Parameters: token 
  
  amphibian_list <- vector(length = length(species_list), 
                           mode = "character")  
  i <- 1 
  for (species in species_list){
    general_info <- rl_search(species, key = token)
    Sys.sleep(2)
    family <- general_info$result$family
    if (family == desired.family){
      amphibian_list[i] <- species
    }
    i <- i+1
  }
  return(amphibian_list[amphibian_list != ""])
}  
sp_genus <- function(species_list, desired.genus){
  amphibian_list <- vector(length = length(species_list), mode = "character")
  i <- 1
  for (species in species_list){
    general_info <- rl_search(species, key = token)
    Sys.sleep(2)
    genus <- general_info$result$genus
    if (genus == desired.genus){
      amphibian_list[i] <- species
    }
    i <- i + 1
  }
  return(amphibian_list[amphibian_list != ""])
}
sp_threat_count <- function(species_list){
  threat_count <- vector(length = length(species_list), mode = "character")
  i <- 1
  for (species in species_list){
    threats <- rl_threats(species, key = token)
    Sys.sleep(2)
    code <- as.numeric(threats$result$code)
    code <- na.omit(code)
    threat_count[i] <- length(code)
    i <- i + 1
  }
  df <- data.frame(species_list, threat_count)
  return(df)
}

#(2)Specify species, get the genus 
sp_all_genus <- function(species_list){
  genus_list <- vector(length = length(species_list), mode = "character")
  i <- 1 
  for (species in species_list){
    general_info <- rl_search(species, key = token)
    Sys.sleep(2)
    genus_list[i] <- general_info$result$genus
    i <- i + 1 
  }
  spgenus <- data.frame(species_list, genus_list)
  return(spgenus)
}
sp_all_family <- function(species_list){
  family_list <- vector(length = length(species_list), mode = "character")
  i <- 1 
  for (species in species_list){
    general_info <- rl_search(species, key = token)
    family_list[i] <- general_info$result$family
    i <- i + 1 
  }
  spfamily <- data.frame(species_list, family_list, stringsAsFactors = FALSE)
  return(spfamily)
}



#Get Data: 
LC <- rl_sp_category("LC", key = token)
NT <- rl_sp_category("NT", key = token)
VU <- rl_sp_category("VU", key = token)
EN <- rl_sp_category("EN", key = token)
CR <- rl_sp_category("CR", key = token)
EW <- rl_sp_category("EW", key = token)
EX <- rl_sp_category("EX", key = token)

#COPY DATA
LCC <- LC
NTT <- NT
VUU <- VU
ENN <- EN
CRR <- CR
EWW <- EW
EXX <- EX



#Clean data - only want species, no subspecies or subpopulation or var
lc <- clean_data2(LCC)
nt <- clean_data2(NTT)
vu <- clean_data2(VUU)
en <- clean_data2(ENN)
cr <- clean_data2(CRR)
ew <- clean_data2(EWW)
ex <- clean_data2(EXX)




####BETTER WAY 
####Need to edit this ~~~~~~~~~ 
#sp_threat_count <- function(species_list){
  threat_count <- vector(length = length(species_list), mode = "character")
  i <- 1
  for (species in species_list){
    threats <- rl_threats(species, key = token)
    code <- (threats$result$code)
    #code <- na.omit(code)
    pos <- regexpr('\.', code)
    sub1 <- substr(code, pos, 1000)
    pos <- regexpr('\.', sub1)
    ifelse(pos > 1, pos == NA, pos) 
    threat_count[i] <- length(code)
    i <- i + 1
  }
  df <- data.frame(species_list, threat_count)
  return(df)
}
#pos = regexpr('pattern', x)
#substr(string, pos, 10000)
#do pos again <-  so if its not there it is -1 
#pos = regexpr('pattern', x)
#substr()

#last - put a supepr high value 
#substr ("abcd", 2,1000)

#grep  #its a unix command . 
#pos = regex

#get the pos using the regular expression --- pos = regexpr('pattern)
#form pos get the substring and then look for it again 


####In progress 
sp_threat_count(CR_names110)


# ##2 in progress!!!!!
sp_only <- function(category){
  only <-vector(length =11500, mode = "character")
  specieslist <- rl_sp_category(category, key = token)
  i <- 1
  for(species in specieslist){
    if (is.na(specieslist$result$subspecies) == TRUE){
      only[i] <- species  
      i <- i + 1
    }
    
  }
  return(only)
}

pos <- regexpr("var.", CR_names_100)
df <- data.frame(CR_names_100, pos)
if (df$pos != -1){
  rm
}
df2 <- subset(df, df$pos == -1)
length(df2$CR_names_100)

pos2 <- regexpr("ssp.", df2$CR_names_100)
df <- data.frame(df2$CR_names_100, pos2)
df <- subset(df, df$pos2 == -1)

pos3 <- regexpr("subpopulation", df$df2.CR_names_100)
df <- data.frame(df$)



pos1 <- regexpr("var.", CR_names_100)
pos2 <- regexpr("ssp.", CR_names_100)
pos3 <- regexpr("subpopulation", CR_names_100)

df <- data.frame(CR_names_100, pos1, pos2, pos3)
df <- subset(df, df$pos1 == -1)
df <- subset(df, df$pos2 == -1)
df <- subset(df, df$pos3 == -1)




pos1 <- regexpr(("Alsodes","Eupsophus"), vu)
df <- data.frame(vu, pos1)
keep <- df[df$pos1 == 1,]

new <- rbind(keep, keep2)
length(new$vu)

pos1 <- regexpr("Eupsophus", vu)
df2 <- data.frame(vu, pos1)
keep2 <- df2[df2$pos1 == 1,]

pos3 <- regexpr("Alytes", vu)
df3 <- data.frame(vu, pos3)
keep3 <- df3[df3$pos3 == 1,]
keep3$vu


#accumulator function
#where you input all of the desired genus and species list
#loops through desired genus and does the regexpr thing 


short_cut <- function(species_list, desiredgenus){
  keepers <- vector(mode = "character", length = length(species_list))
  for (genus in desiredgenus){
    pos1 <- regexpr(genus, species_list)
    df <- data.frame(species_list, pos1, stringsAsFactors = FALSE)
    keep <- df[df$pos1 == 1,]
    vector1 <- keep$species_list
    keepers<- c(keepers, vector1)
  }
  return(keepers[keepers != ""])
}



#ALL FOR VULNERABLE LIST ~ 670 
newlist <- vector(length = length(vu), mode = "character")
#anura 
alsodidae <- c("Alsodes", "Eupsophus")
alytidae <- c("Alytes")
aromobatidae <- c("Allobates", "Anomaloglossus", "Mannophryne")
arthroleptidae <- c("Arthroleptis", "Astylosternus", "Cardioglossa", "Leptodactylodon", "Leptopelis")
batrachylidae <- c("Atelognathus", "Batrachyla")
bombinatoridae <- c("Barbourula", "Bombina")
brevicipitidae <- c("Breviceps")
bufonidae <- c("Anaxyrus", "Ansonia", "Atelopus", "Bufo",
               "Capensibufo", "Duttaphrynus", "Ghatophryne",
               "Incilius", "Melanophryniscus", "Mertensophryne", "Metaphryniscus",
               "Nannophryne", "Oreophrynella", "Osornophryne",
               "Pelophryne", "Peltophryne", "Rhinella", "Sclerophrys", "Wolterstorffina")
calyptocephalellidae <- c("Calyptocephalella","Telmatobufo")
centrolenidae <- c("Celsiella", "Centrolene", "Chimerella", "Cochranella", 
                   "Espadarana", "Hyalinobatrachium", "Ikakogi", "Nymphargus",
                   "Rulyrana", "Vitreorana")
ceratobatrachidae <- c("Alcalus", "Cornufer", "Palmatorappia", "Platymantis")
ceratophryidae <- c("Ceratophrys")
ceuthomantidae <- c("Ceuthomantis")
conrauidae <- c("Conraua")
craugastoridae <- c("Craugastor", "Euparkerella", "Hypodactylus", "Lynchius",
                    "Phrynopus", "Pristimantis", "Psychrophrynella", "Strabomantis",
                    "Tachiramantis","Yunganastes")
cycloramphidae <- c("Cycloramphus", "Rhinoderma", "Thoropa")
dendrobatidae <- c("Ameerega", "Andinobates", "Hyloxalus", "Oophaga", "Ranitomeya")
dicroglossidae <- c("Ingerana", "Limnonectes", "Nannophrys","Nanorana", "Occidozyga", "Quasipaa")
eleutherodactylidae <- c("Adelophryne","Diasporus", "Eleutherodactylus")
hemiphractidae <- c("Cryptobatrachus", "Gastrotheca", "Stefania")
hemisotidae <- c("Hemisus")
hylidae <- c("Bokermannohyla", "Charadrahyla", "Dendropsophus", "Duellmanohyla",
             "Ecnomiohyla", "Exerodonta", "Hyla walkeri", "Hyloscirtus", "Hypsiboas",
             "Litoria", "Nyctimystes","Osteopilus","Tepuihyla","Tlalocohyla")
hyperoliidae <- c("Afrixalus", "Callixalus", "Hyperolius", "Kassina", "Morerella",
                  "Paracassina")
leiopelmatidae <- c("Leiopelma")
leptodactylidae <- c("Leptodactylus", "Physalaemus")
limnodynastidae <- c("Heleioporus")
mantellidae <- c("Aglyptodactylus", "Boehmantis", "Boophis", "Gephyromantis",
                 "Guibemantis", "Mantella", "Mantidactylus", "Spinomantis")
megaphryidae <- c("Borneophrys","Brachytarsophrys","Leptobrachella",
                  "Leptobrachium","Leptolalax","Megophrys", "Ophryophryne",
                  "Oreolalax","Scutiger")
micrixalidae <- c("Micrixalus")
microhylidae <- c("Anodonthyla", "Austrochaperina", "Cophixalus", "Cophyla", "Copiula",
                  "Ctenophryne", "Dasypops", "Gastrophrynoides", "Hypopachus","Kalophrynus minusculus",
                  "Kalophrynus intermedius", "Kalophrynus punctatus",
                  "Kaloula", "Microhyla", "Oreophryne anulata", "Oreophryne celebensis", "Oreophryne variabilis",
                  "Plethodontohyla", "Rhombophryne",
                  "Scaphiophryne", "Uperodon")
myobatrachidae <- c("Crinia", "Geocrinia", "Mixophyes", "Pseudophryne", "Spicospina")
nyctibatrachidae <- c("Nyctibatrachus")
odontophrynidae <- c("Odontophrynus")
phrynobatrachidae <- c("Phrynobatrachus")
phyllomedusidae <- c("Agalychnis")
pyxicephalidae <- c("Anhydrophryne", "Strongylopus")
ranidae <- c("Amnirana", "Amolops", "Huia", "Indosylvirana", "Lithobates", "Meristogenys",
             "Odorrana", "Papurana", "Pelophylax", "Pseudorana", "Pterorana", "Rana",
             "Sanguirana", "Sylvirana")
ranixalidae <- c("Indirana")

rhacophoridae <- c("Buergeria", "Chiromantis", "Gracixalus", "Philautus", "Pseudophilautus",
                   "Raorchestes", "Rhacophorus", "Theloderma")

telmatobiidae <- c("Telmatobius")
#caudata
ambystomatidae <- c("Ambystoma")
hynobiidae <- c("Batrachuperus", "Hynobius", "Liua", "Pachyhynobius", "Pseudohynobius")
plethodontidae <- c("Aquiloeurycea", "Batrachoseps","Bolitoglossa", "Chiropterotriton",
                    "Dendrotriton", "Eurycea", "Gyrinophilus", "Hydromantes","Isthmura",
                    "Nototriton", "Oedipina","Pseudoeurycea","Speleomantes",
                    "Thorius","Plethodon amplus", "Plethodon asupak","Plethodon cheoah",
                    "Plethodon fourchensis","Plethodon hubrichti",  "Plethodon meridianus",
                    "Plethodon petraeus","Plethodon shenandoah","Plethodon shermani",
                    "Plethodon sherando")
proteidae <- c("Proteus")
rhyacotritonidae <- c("Rhyacotriton")
salamandridae <- c("Chioglossa","Lyciasalamandra","Mertensiella","Neurergus","Paramesotriton",
                   "Pleurodeles","Salamandra","Tylototriton")

#Gymnophiona
dermophiidae <- c("Dermophis")
herpelidae <- c("Boulengerula")
ichthyophiidae <- c("Ichthyophis")


vu_genus_list <- c(alsodidae,alytidae,aromobatidae, arthroleptidae, batrachylidae,
                bombinatoridae, brevicipitidae, bufonidae, calyptocephalellidae,
                centrolenidae, ceratobatrachidae, ceratophryidae, 
                ceuthomantidae, conrauidae, craugastoridae, cycloramphidae,
                dendrobatidae, dicroglossidae, eleutherodactylidae, hemiphractidae,
                hemisotidae, hylidae, hyperoliidae, leiopelmatidae, leptodactylidae,
                limnodynastidae, mantellidae, megaphryidae, micrixalidae,
                microhylidae, myobatrachidae, nyctibatrachidae, odontophrynidae,
                phrynobatrachidae, phyllomedusidae, pyxicephalidae, ranidae,
                ranixalidae, rhacophoridae, telmatobiidae, ambystomatidae,
                hynobiidae, plethodontidae,proteidae, rhyacotritonidae, salamandridae,
                dermophiidae, herpelidae, ichthyophiidae)



longershortcut <- function(species_list, genus_list){
  newlist <- vector(mode = "character", length = length(species_list))
  for (genus in genus_list){
    test <- short_cut(species_list, genus)
    newlist <- c(newlist, test)
  }
  clean <- newlist[newlist != ""]
  return(clean)
}
vu_frogs <-longershortcut(vu, genus_list)
length(amphibians)

#NEAR THREATENED
                       
#anura 



allophrynidae <- c("Allophryne")
alsodidae <- c("Alsodes", "Eupsophus", "Limnomedusa")
alytidae <- c("Alytes", "Discoglossus")
aromobatidae <- c("Allobates", "Anomaloglossus", "Rheobates")
arthroleptidae <- c("Arthroleptis", "Astylosternus", "Cardioglossa", "Leptopelis",
                    "Nyctibates","Scotobleps", "Trichobatrachus")
ascsaphidae <- c("Ascaphus")


batrachylidae <- c("Batrachyla", "Hylorina")
bombinatoridae <- c("Bombina")
brachycephalidae <- c("Brachycephalus", "Ischnocnema")
brevicipitidae <- c("Breviceps", "Callulina", "Spelaeophryne")
bufonidae <- c("Amazophrynella", "Anaxyrus", "Ansonia", "Bufo andrewsi",
               "Bufo bankorensis", "Bufo bufo", 
               "Bufo gargarizans", "Bufo japonicus", "Bufo minshanicus",
               "Bufo stejnegeri", "Bufo tibetanus", "Bufo torrenticola",
               "Bufotes", 
                "Capensibufo", "Dendrophryniscus", "Duttaphrynus","Epidalea", "Frostius",
               "Incilius", "Ingerophrynus", "Leptophryne", "Melanophryniscus", "Mertensophryne", 
               "Nannophryne", "Nectophryne", "Nectophrynoides", "Osornophryne","Pedostibes",
               "Pelophryne", "Peltophryne", "Phrynoidis", "Poyntonophrynus", "Pseudobufo", "Rhaebo", "Rhinella", "Schismaderma",
               "Sclerophrys", "Strauchbufo", "Vandijkophrynus")




centrolenidae <- c("Centrolene", "Cochranella", "Espadarana", "Hyalinobatrachium",
                     "Nymphargus",
                   "Rulyrana", "Sachatamia","Teratohyla","Vitreorana")
ceratobatrachidae <- c("Alcalus", "Cornufer", "Platymantis")
ceratophryidae <- c("Ceratophrys","Chacophrys", "Lepidobatrachus")
ceuthomantidae<- c("Ceuthomantis")
conrauidae <- c("Conraua")
craugastoridae <- c("Barycholos","Bryophryne", "Craugastor",
                    "Euparkerella", "Haddadus", "Hypodactylus",
                    "Noblella", "Oreobates",
                    "Pristimantis", "Psychrophrynella", "Strabomantis","Tachiramantis",
                    "Yunganastes")
cycloramphidae <- c("Cycloramphus", "Thoropa", "Zachaenus")

dendrobatidae <- c("Adelphobates", "Ameerega", "Andinobates", "Colostethus", "Dendrobates",
                   "Epipedobates", "Excidobates", 
                   "Hyloxalus", "Oophaga", "Phyllobates",
                   "Silverstoneia", "Ranitomeya")
dicroglossidae <- c("Allopaa", "Chrysopaa", "Euphlyctis", "Fejervarya",
                    "Hoplobatrachus", "Ingerana",
                    "Limnonectes", "Nanorana", "Occidozyga","Ombrana", "Sphaerotheca")




eleutherodactylidae <- c("Adelophryne", "Diasporus", "Eleutherodactylus", "Phyzelaphryne")

heleophrynidae <- c("Hadromophryne", "Heleophryne")

hemiphractidae <- c("Flectonotus", "Fritziana", "Hemiphractus", "Gastrotheca", "Stefania")
hemisotidae <- c("Hemisus")


hylidae <- c("Acris", "Anotheca", "Aparasphenodon", "Aplastodiscus", "Bokermannohyla","Bromeliohyla",
             "Corythomantis", "Dendropsophus", "Diaglena", "Dryaderces", "Duellmanohyla",
             "Ecnomiohyla", "Exerodonta", "Hyla annectans","Hyla arborea","Hyla arenicolor","Hyla avivoca","Hyla chinensis",                  
             "Hyla chrysoscelis", "Hyla cinerea","Hyla eximia",
             "Hyla femoralis","Hyla gratiosa","Hyla hallowellii",                
             "Hyla immaculata","Hyla intermedia","Hyla japonica",                   
             "Hyla meridionalis","Hyla plicata","Hyla sanchiangensis","Hyla sarda",                       
             "Hyla savignyi" ,"Hyla simplex","Hyla squirella","Hyla tsinlingensis","Hyla versicolor","Hyla wrightorum", 
             "Hyloscirtus", "Hypsiboas",
             "Isthmohyla", "Itapotihyla",
             "Litoria","Lysapsus", "Myersiohyla", "Nyctimantis","Nyctimystes","Osteocephalus", "Osteopilus", "Phyllodytes","Plectrohyla",
             "Pseudacris", "Pseudis",
             "Ptychohyla","Scarthyla", "Scinax", "Smilisca","Sphaenorhynchus", "Tepuihyla",
             "Tlalocohyla", "Trachycephalus", "Triprion")

hylodidae <- c("Crossodactylus", "Hylodes", "Megaelosia")
hyperoliidae <- c("Acanthixalus", "Afrixalus", "Alexteroon", "Cryptothylax", "Heterixalus", "Hyperolius", 
                  "Kassina","Kassinula","Opisthothylax","Paracassina","Phlyctimantis","Semnodactylus",
                  "Tachycnemis")
leiopelmatidae <- c("Leiopelma")

leptodactylidae <- c("Adenomera", "Edalorhina", "Engystomops", "Hydrolaetare", "Leptodactylus", 
                    "Lithodytes", "Physalaemus", "Pleurodema", "Pseudopaludicola", "Scythrophrys")

limnodynastidae <- c("Heleioporus", "Lechriodus","Limnodynastes", "Neobatrachus", "Notaden", "Platyplectrum")
mantellidae <- c("Aglyptodactylus","Blommersia", "Boophis", "Gephyromantis", "Guibemantis", "Laliostoma", "Mantella",
                 "Mantidactylus", "Spinomantis")
megophryidae <- c( "Brachytarsophrys", "Leptobrachella",
                  "Leptobrachium","Leptolalax","Megophrys", "Ophryophryne",
                  "Oreolalax", "Scutiger")



#micrixalidae <- c("Micrixalus")
microhylidae <- c("Aphantophryne", "Arcovomer", "Asterophrys", "Austrochaperina", "Barygenys", 
                  "Callulops", "Chaperina", "Chiasmocleis", "Choerophryne", 
                   "Cophixalus","Cophyla", "Copiula","Ctenophryne", "Dermatonotus", "Dyscophus", "Elachistocleis", 
                  "Gastrophryne", "Genyophryne", "Glyphoglossus", "Hamptophryne", "Hylophorbus", 
                  "Hypopachus", "Kalophrynus", "Kaloula", "Liophryne", "Mantophryne", "Metamagnusia",
                  "Metaphrynella", 
                   "Microhyla","Myersiella", "Micryletta",  "Oreophryne", "Otophryne", "Oxydactyla","Paradoxophyla", "Phrynella", "Phrynomantis",
                  "Plethodontohyla", "Rhombophryne", "Sphenophryne",
                  "Scaphiophryne","Synapturanus", "Stereocyclops", "Uperodon", "Xenorhina")
myobatrachidae <- c("Arenophryne", "Assa", "Crinia", "Geocrinia", 
                    "Metacrinia", "Mixophyes", "Myobatrachus", "Paracrinia", 
                    "Pseudophryne", "Uperoleia")
nyctibatrachidae <- c("Lankanectes", "Nyctibatrachus")

#odontobatrachidae <- c(, "Proceratophrys", "Odontobatrachus")


odontophrynidae <- c("Macrogenioglottus", "Odontophrynus", "Proceratophrys")

pelobatidae <- c("Pelobates")
pelodytidae <- c("Pelodytes")
petropedetidae <- c("Petropedetes")
phrynobatrachidae <- c("Phrynobatrachus")
phyllomedusidae <- c("Agalychnis", "Callimedusa", "Cruziohyla", "Phasmahyla", "Phrynomedus",
                     "Phyllomedusa", "Pithecopus")
pipidae <- c("Hymenochirus", "Pipa", "Pseudhymenochirus", "Xenopus")
ptychadenidae <- c("Hildebrandtia", "Lanzarana", "Ptychadena")
pyxicephalidae <- c("Amietia","Anhydrophryne", "Arthroleptella",
                    "Aubria", 
                    "Cacosternum","Pyxicephalus", "Strongylopus", "Tomopterna")

ranidae <- c("Abavorana", "Amnirana", "Amolops","Babina", "Chalcorana", "Clinotarsus","Glandirana",
             "Huia", "Humerana", "Hydrophylax",
             "Hylarana","Indosylvirana", "Lithobates", "Meristogenys",
             "Odorrana","Papurana", "Pelophylax","Pulchrana",
             "Rana amurensis","Rana arvalis","Rana asiatica","Rana aurora","Rana chaochiaoensis",     
             "Rana chensinensis","Rana coreana","Rana dalmatina","Rana dybowskii","Rana graeca",
             "Rana huanrensis","Rana italica","Rana japonica","Rana johnsi","Rana kukunoris","Rana luteiventris",
             "Rana macrocnemis","Rana multidenticulata","Rana omeimontis","Rana ornativentris",         
             "Rana pirica","Rana pseudodalmatina","Rana sakuraii","Rana shuchinae","Rana tagoi","Rana temporaria",            
             "Rana tsushimensis","Rana zhenhaiensis", 
             "Sanguirana", "Sylvirana", "Staurois")
ranixalidae <- c("Indirana")

rhacophoridae <- c("Buergeria", "Chiromantis", "Feihyla", "Gracixalus","Kurixalus", "Philautus", "Pseudophilautus",
                   "Polypedates",
                   "Raorchestes", "Rhacophorus", "Theloderma")
rhinophrynidae <- c("Rhinophrynus")
scaphiopododidae <- c("Scaphiopus" , "Spea")
telmatobiidae <- c("Telmatobius")                   
#########




#caudata
ambystomatidae <- c("Ambystoma", "Dicamptodon")
amphiumidae <- c("Amphiuma")
#cryptobranchidae <- c("Andrias", "Cryptobranchus")
hynobiidae <- c("Hynobius", "Onychodactylus", "Salamandrella")
plethodontidae <- c("Aneides", "Aquiloeurycea", "Batrachoseps","Bolitoglossa", 
                    "Desmognathus","Ensatina", "Eurycea", "Gyrinophilus",
                    "Hemidactylium", "Hydromantes", "Karsenia", 
                    "Nototriton", "Oedipina",
                    "Plethodon albagula" ,          
                    "Plethodon angusticlavius",      "Plethodon cinereus",            "Plethodon cylindraceus"  ,     
                    "Plethodon dorsalis"       ,     "Plethodon dunni"    ,           "Plethodon electromorphus" ,    
                    "Plethodon glutinosus"      ,    "Plethodon hoffmani"  ,          "Plethodon idahoensis"     ,    
                    "Plethodon kentucki"         ,   "Plethodon kisatchie"  ,         "Plethodon metcalfi"        ,   
                    "Plethodon montanus"          ,  "Plethodon richmondi"   ,        "Plethodon serratus"         ,  
                    "Plethodon teyahalee"          , 
                    "Plethodon vandykei"            ,"Plethodon vehiculum"    ,       "Plethodon ventralis"         , 
                    "Plethodon websteri"            ,"Plethodon wehrlei"       ,      "Plethodon yonahlossee"        ,
                    
                    
                    "Pseudoeurycea", "Pseudotriton", "Stereochilus", "Urspelerpes")
proteidae <- c("Necturus")
rhyacotritonidae <- c("Rhyacotriton")
salamandridae <- c( "Cynops","Euproctus", "Hypselotriton","Ichthyosaura" ,"Notophthalmus","Ommatotriton",
                    "Pachytriton", "Lissotriton",
                    "Paramesotriton",
                   "Salamandra","Salamandrina", "Taricha", "Tylototriton", "Triturus")
sirenidae <- c("Pseudobranchus", "Siren")


#Gymnophiona
caeciliidae <- c("Caecilia", "Oscaecilia")
dermophiidae <- c("Dermophis", "Geotrypetes", "Gymnopis","Schistometopum")
herpelidae <- c("Boulengerula", "Herpele")
ichthyophiidae <- c("Ichthyophis")
indotyphilidae <- c("Gegeneophis", "Grandisonia", "Hypogeophis", "Sylvacaecilia")

rhinatrematidae <- c("Epicrionops", "Rhinatrema")
scolecomorphidae <- c("Scolecomorphus")
siphonopidae <- c("Brasilotyphlus", "Microcaecilia", "Siphonops")
typhlonectidae <- c("Chthonerpeton", "Nectocaecilia", "Potomotyphlus", "Typhlonectes")
                   
lc_genus_list <- c(allophrynidae, alsodidae,alytidae,aromobatidae, arthroleptidae, ascsaphidae, 
                    batrachylidae,bombinatoridae, 
                    brachycephalidae, brevicipitidae, bufonidae,
                    centrolenidae, ceratobatrachidae, ceratophryidae, ceuthomantidae, conrauidae,
                    craugastoridae, cycloramphidae, 
                    dendrobatidae, dicroglossidae,  eleutherodactylidae, heleophrynidae, hemiphractidae,hemisotidae,
                    hylidae, hylodidae, hyperoliidae, leiopelmatidae, leptodactylidae,
                    limnodynastidae, mantellidae, megophryidae, 
                    microhylidae, myobatrachidae,nyctibatrachidae, odontophrynidae,
                    pelobatidae, pelodytidae, petropedetidae, 
                    phrynobatrachidae, phyllomedusidae, pipidae,ptychadenidae, pyxicephalidae, ranidae,ranixalidae, 
                    rhacophoridae, rhinophrynidae, scaphiopododidae, telmatobiidae,
                    ambystomatidae, amphiumidae,
                    hynobiidae, plethodontidae,proteidae, rhyacotritonidae, salamandridae,sirenidae,
                    caeciliidae, dermophiidae, herpelidae,ichthyophiidae, indotyphilidae, rhinatrematidae, scolecomorphidae, 
                    siphonopidae, typhlonectidae) 
lc_genus_list_abc <- longershortcut(lc, lc_genus_list)
lc_genus_list_abc[2427:2465]
lc_threats_1 <- sp_threat_count(lc_genus_list_abc[1:100])
lc_threats_2 <- sp_threat_count(lc_genus_list_abc[101:200])
lc_threats_3 <- sp_threat_count(lc_genus_list_abc[201:300])
lc_threats_4 <- sp_threat_count(lc_genus_list_abc[301:400])
lc_threats_5 <- sp_threat_count(lc_genus_list_abc[401:500])
lc_threats_6 <- sp_threat_count(lc_genus_list_abc[501:600])
lc_threats_7 <- sp_threat_count(lc_genus_list_abc[601:700])
lc_threats_8 <- sp_threat_count(lc_genus_list_abc[701:800])
lc_threats_9 <- sp_threat_count(lc_genus_list_abc[801:900])
lc_threats_10 <- sp_threat_count(lc_genus_list_abc[901:1000])
lc_threats_11 <- sp_threat_count(lc_genus_list_abc[1001:1100])
lc_threats_12 <- sp_threat_count(lc_genus_list_abc[1101:1200])
lc_threats_13 <- sp_threat_count(lc_genus_list_abc[1201:1300])
lc_threats_14 <- sp_threat_count(lc_genus_list_abc[1301:1400])
lc_threats_15 <- sp_threat_count(lc_genus_list_abc[1401:2000])
lc_threats_16 <- sp_threat_count(lc_genus_list_abc[2001:2300])
lc_threats_17 <- sp_threat_count(lc_genus_list_abc[2301:2400])
lc_threats_18 <- sp_threat_count(lc_genus_list_abc[2401:2485])


lc_threats <- rbind(lc_threats_1, lc_threats_2, lc_threats_3, lc_threats_4, lc_threats_5, 
                    lc_threats_6, lc_threats_7, lc_threats_8, lc_threats_9, lc_threats_10, lc_threats_11, 
                    lc_threats_12, lc_threats_13, lc_threats_14, lc_threats_15, lc_threats_16,
                    lc_threats_17, lc_threats_18)
write.table(lc_threats, file = "lc_threats_1:2485.txt")


#NEAR THREATENED 
nt_frogs <- longershortcut(nt, nt_genus_list)

