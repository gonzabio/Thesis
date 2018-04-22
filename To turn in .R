#Data analysis to turn in 


#1. Make the tables summarizing data 
summary(ALLDATA$scope) #did this for every different level
summary(ALLDATA$Year.assessed)
      #Year assessed 
drake <- ggplot(data = ALLDATA, aes( x = Year.assessed, fill = Year.assessed)) +
  xlab(label = "Year Assessed") + ylab(label = "Species Count") + 
  geom_bar(stat = "count", width = 1) + coord_polar() + theme_minimal() + 
  theme(legend.position = "NULL")
summary(ALLDATA$Year.assessed)
      #year assessed by red list status 
KOD <- ggplot(data = ALLDATA, aes( x = Red.List.status, fill = Red.List.status)) + geom_bar(stat = "count") +
  coord_polar() + facet_wrap(~Year.assessed) + theme_minimal() +
  xlab(label = "Red List Status") + ylab(label = "Species Count") + 
  theme(legend.position = "NULL")

bodak7 <- ggplot(data = together, aes( x = Family, fill = threatened)) + geom_bar(stat = "count", position = "dodge") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  xlab(label = "Family") + ylab(label = "Count") +
  scale_fill_manual(values = c("dark grey", "red"))  

bodak7


#2: Do density plots, box plots and histograms for ALL AMPHIBIANS as previously completed 
lc_threat_table
cr_threat_table
en_threat_table
vu_threat_table
nt_threat_table

cr_threat_count
en_threat_count
vu_threat_count
lc_threat_count
      #get all LC data
library(rredlist)
lc1 <- threat_details(lc_threat_table$species_list[1:10])
lc2 <- threat_details(lc_threat_table$species_list[11:50])
lc3 <- threat_details(lc_threat_table$species_list[51:200])
lc4 <- threat_details(lc_threat_table$species_list[201:280])
lc5 <- threat_details(lc_threat_table$species_list[281:350])
lc6 <- threat_details(lc_threat_table$species_list[351:440])
lc7 <- threat_details(lc_threat_table$species_list[441:600])
lc8 <- threat_details(lc_threat_table$species_list[601:700])
lc9 <- threat_details(lc_threat_table$species_list[701:800])
lc10 <- threat_details(lc_threat_table$species_list[801:900])
lc11 <- threat_details(lc_threat_table$species_list[901:1000])
lc12 <- threat_details(lc_threat_table$species_list[1001:1100])
lc13 <- threat_details(lc_threat_table$species_list[1101:1200])
lc14 <- threat_details(lc_threat_table$species_list[1201:1300])
lc15 <- threat_details(lc_threat_table$species_list[1301:1400])
lc16 <- threat_details(lc_threat_table$species_list[1401:1500])
lc17 <- threat_details(lc_threat_table$species_list[1501:1550])

lc_data <- rbind(lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lc9,
                 lc10, lc11, lc12, lc13, lc14, lc15, lc16, lc17)
lc_threat_count <- sp_threat_count(lc_data)
rl_threats("Astylosternus batesi", key = token)
  #get all NT 
nt1 <- threat_details(nt_threat_table$species_list[1:50])
nt2 <- threat_details(nt_threat_table$species_list[51:100])
nt3 <- threat_details(nt_threat_table$species_list[101:160])
nt4 <- threat_details(nt_threat_table$species_list[161:220])
nt5 <- threat_details(nt_threat_table$species_list[201:300])
nt6 <- threat_details(nt_threat_table$species_list[301:400])
nt_data <- rbind(nt1, nt2, nt3, nt4, nt5, nt6)
nt_threat_count <- sp_threat_count(nt_data)

  #get all EN data 

en1 <- threat_details(en_threat_table$species_list[1:50])
en2 <- threat_details(en_threat_table$species_list[51:100])
en3 <- threat_details(en_threat_table$species_list[101:200])
en4 <- threat_details(en_threat_table$species_list[201:300])
en5 <- threat_details(en_threat_table$species_list[301:400])
en6 <- threat_details(en_threat_table$species_list[401:500])
en7 <- threat_details(en_threat_table$species_list[501:600])
en8 <- threat_details(en_threat_table$species_list[601:700])
en9 <- threat_details(en_threat_table$species_list[701:800])
en10 <- threat_details(en_threat_table$species_list[801:855])

  #get all vu data 
vu1 <- threat_details(vu_threat_table$species_list[1:50])
vu2 <- threat_details(vu_threat_table$species_list[51:150])
vu3 <- threat_details(vu_threat_table$species_list[151:250])
vu4 <- threat_details(vu_threat_table$species_list[251:350])
vu5 <- threat_details(vu_threat_table$species_list[351:450])
vu6 <- threat_details(vu_threat_table$species_list[451:550])
vu7 <- threat_details(vu_threat_table$species_list[551:650])
vu8 <- threat_details(vu_threat_table$species_list[651:670])

cr1 <- threat_details(cr_threat_table$species_list[1:50])
cr2 <- threat_details(cr_threat_table$species_list[51:150])

cr_threat_count$Red_List_status <- "CR"
en_threat_count$Red_List_status <- "EN"
vu_threat_count$Red_List_status <- "VU"
lc_threat_count$Red_List_status <- "LC"
nt_threat_count$Red_List_status <- "NT"

all_threat_count <- rbind(cr_threat_count, en_threat_count, vu_threat_count, lc_threat_count, nt_threat_count)
all_threat_count$Red_List_status <- as.factor(all_threat_count$Red_List_status)

      #ALL AMPHIBIAINS DENSITY PLOT 
library(ggplot2)
all1 <- ggplot(all_threat_count, aes(x = all_threat_count$threat_count, colour = Red_List_status)) + geom_density(size = 0.75) +
  scale_x_continuous(name = "Number of Threat Processes") + 
  scale_y_continuous(name = "Density") + 
  scale_color_manual(values = c("green3", "olivedrab2", "yellow","orangered", "red3"), 
                     name = c(""), 
                     labels = c("Least Concern", "Near Threatened", "Vulnerable", 
                                "Endangered", "Critically Endangered")) + 
  theme_minimal() 





ano1 <- aov(all_threat_count$threat_count ~ Red_List_status, data = all_threat_count)
tHSD <- TukeyHSD(ano1, ordered = FALSE, conf.level = 0.95)


all_threat_count$threat_count <- as.numeric(all_threat_count$threat_count)

        #ALL AMPHIBIAINS BOX PLOT 
all_threat_count2 <- all_threat_count

all_threat_count2 <- subset(all_threat_count, all_threat_count$threat_count <= 6)
all2 <- ggplot(data = all_threat_count, aes(x = Red_List_status, y = threat_count, fill = Red_List_status)) + geom_boxplot() +
  labs(title = NULL) +
  scale_fill_manual(values = c("green3", "olivedrab2", "yellow","orangered", "red3"), 
                    labels = c("Least Concern", "Near Threatened", "Vulnerable", 
                               "Endangered", "Critically Endangered"),
                    name = NULL) + 
  xlab("Red List Cateogry") + ylab("Number of Threats") +
  theme_minimal()

hist(all_threat_count$threat_count)
        #ANOVA OF THE NUMBER OF THREATS 
ano2 <- aov(all_threat_count2$threat_count ~ Red_List_status, data = all_threat_count2)
summary(ano2)
tHSD2 <- TukeyHSD(ano2, ordered = FALSE, conf.level = 0.95)

kruskal.test(threat_count ~ Red_List_status, data = all_threat_count2)

summary(all_threat_count$threat_count)
hist(all_threat_count$threat_count, 100)

library("pgirmess")
kruskalmc(threat_count ~ Red_List_status, data = all_threat_count2)
sd(all_threat_count$threat_count)
