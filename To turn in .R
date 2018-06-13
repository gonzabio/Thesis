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

      ####Begin: ALL AMPHIBIANS (WORLDWIDE EXPLORATORY DATA ANALSYS)
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
lc_data$Red_List_status <- "LC"
lc_data$threatened <- "Not Threatened"
lc_data$code <- as.factor(lc_data$code)

summary(lc_data$code)
nrow(lc_threat_count)

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
nt_data$Red_List_status <- "NT"
nt_data$threatened <- "Not Threatened"

nt_data$code <-as.factor(nt_data$code)
summary(nt_data$code)
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
en_data <- rbind(en1, en2, en3, en4, en5, en6, en7, en8, en9, en10)
en_data$Red_List_status <- "EN"
en_data$threatened <- "Threatened"
summary(en_data$code)

  #get all vu data 
vu1 <- threat_details(vu_threat_table$species_list[1:50])
vu2 <- threat_details(vu_threat_table$species_list[51:150])
vu3 <- threat_details(vu_threat_table$species_list[151:250])
vu4 <- threat_details(vu_threat_table$species_list[251:350])
vu5 <- threat_details(vu_threat_table$species_list[351:450])
vu6 <- threat_details(vu_threat_table$species_list[451:550])
vu7 <- threat_details(vu_threat_table$species_list[551:650])
vu8 <- threat_details(vu_threat_table$species_list[651:670])
vu_data <- rbind(vu1, vu2, vu3, vu4, vu5, vu6, vu7, vu8)
vu_data$Red_List_status <- "VU"
vu_data$threatened <- "Threatened"
summary(vu_data$code)


cr1 <- threat_details(cr_threat_table$species_list[1:50])
cr2 <- threat_details(cr_threat_table$species_list[51:150])
cr3 <- threat_details(cr_threat_table$species_list[151:200])
cr4 <- threat_details(cr_threat_table$species_list[201:300])
cr5 <- threat_details(cr_threat_table$species_list[301:400])
cr6 <- threat_details(cr_threat_table$species_list[401:500])
cr7 <- threat_details(cr_threat_table$species_list[501:546])
cr_data <- rbind(cr1, cr2, cr3, cr4, cr5, cr6, cr7)
cr_data$Red_List_status <- "CR"
cr_data$threatened <- "Threatened"
nrow(cr_threat_count)

summary(cr_data$code)


worldwide_data <- rbind(lc_data, nt_data, en_data, cr_data, vu_data)
worldwide_data$threatened <- as.factor(worldwide_data$threatened)

worldwide_data$code <- as.factor(worldwide_data$code)
summary(worldwide_data$code)

mad1 <- ggplot(data = worldwide_data, aes (x = code, fill = threatened)) +
  geom_bar(stat = "count", position = "dodge") + 
  xlab(label = "Red List Threat Code") + ylab(label = "Species Count") + 
  #Removes the grid lines in the back: 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values = c("dark grey", "red")) +
  scale_x_discrete(limits = c('1.1', '1.2', '1.3', '2.1', '2.2', '2.3', 
                              '2.4', '3.2', '3.3', '4.1', '4.2', '5.1', 
                              '5.2', '5.3', '5.4', '6.1', '6.2', '6.3', 
                              '7.1', '7.2', '7.3', '8.1', '8.2', '8.4', 
                              '9.1', '9.2', '9.3', '9.4', '10.1', '11.1',
                              '11.2', '11.3', '11.4', '12.1', NA))

           
        #LC

lcplot1 <- ggplot(data = lc_data, aes (x = code, fill = code)) +
  geom_bar(stat = "count", position = "dodge", color="black", fill="green3") + 
  xlab(label = "Red List Threat Code: Least Concern") + ylab(label = "Species Count") + 
  #Removes the grid lines in the back: 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position="none") + 
  scale_x_discrete(limits = c('1.1', '1.2', '1.3', '2.1', '2.2', '2.3', 
                              '2.4', '3.2', '3.3', '4.1', '4.2', '5.1', 
                              '5.2', '5.3', '5.4', '6.1', '6.2', '6.3', 
                              '7.1', '7.2', '7.3', '8.1', '8.2', '8.4', 
                              '9.1', '9.2', '9.3', '9.4', '10.1', '11.1',
                              '11.2', '11.3', '11.4', '12.1', NA))

ntplot1 <- ggplot(data = nt_data, aes (x = code, fill = code)) +
  geom_bar(stat = "count", position = "dodge", color="black", fill="olivedrab2") + 
  xlab(label = "Red List Threat Code: Near Threatened") + ylab(label = "Species Count") + 
  #Removes the grid lines in the back: 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position="none") + 
  scale_x_discrete(limits = c('1.1', '1.2', '1.3', '2.1', '2.2', '2.3', 
                              '2.4', '3.2', '3.3', '4.1', '4.2', '5.1', 
                              '5.2', '5.3', '5.4', '6.1', '6.2', '6.3', 
                              '7.1', '7.2', '7.3', '8.1', '8.2', '8.4', 
                              '9.1', '9.2', '9.3', '9.4', '10.1', '11.1',
                              '11.2', '11.3', '11.4', '12.1', NA))


vuplot1 <- ggplot(data = vu_data, aes (x = code, fill = code)) +
  geom_bar(stat = "count", position = "dodge", color="black", fill="yellow") + 
  xlab(label = "Red List Threat Code: Vulnerable") + ylab(label = "Species Count") + 
  #Removes the grid lines in the back: 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position="none") + 
  scale_x_discrete(limits = c('1.1', '1.2', '1.3', '2.1', '2.2', '2.3', 
                              '2.4', '3.2', '3.3', '4.1', '4.2', '5.1', 
                              '5.2', '5.3', '5.4', '6.1', '6.2', '6.3', 
                              '7.1', '7.2', '7.3', '8.1', '8.2', '8.4', 
                              '9.1', '9.2', '9.3', '9.4', '10.1', '11.1',
                              '11.2', '11.3', '11.4', '12.1', NA))

enplot1 <- ggplot(data = en_data, aes (x = code, fill = code)) +
  geom_bar(stat = "count", position = "dodge", color="black", fill="orangered") + 
  xlab(label = "Red List Threat Code: Endangered") + ylab(label = "Species Count") + 
  #Removes the grid lines in the back: 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position="none") + 
  scale_x_discrete(limits = c('1.1', '1.2', '1.3', '2.1', '2.2', '2.3', 
                              '2.4', '3.2', '3.3', '4.1', '4.2', '5.1', 
                              '5.2', '5.3', '5.4', '6.1', '6.2', '6.3', 
                              '7.1', '7.2', '7.3', '8.1', '8.2', '8.4', 
                              '9.1', '9.2', '9.3', '9.4', '10.1', '11.1',
                              '11.2', '11.3', '11.4', '12.1', NA))
crplot1 <- ggplot(data = cr_data, aes (x = code, fill = code)) +  
  geom_bar(stat = "count", position = "dodge", color="black", fill="red3") + 
  xlab(label = "Red List Threat Code:Critically Endangered") + ylab(label = "Species Count") + 
  #Removes the grid lines in the back: 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position="none") + 
  scale_x_discrete(limits = c('1.1', '1.2', '1.3', '2.1', '2.2', '2.3', 
                              '2.4', '3.2', '3.3', '4.1', '4.2', '5.1', 
                              '5.2', '5.3', '5.4', '6.1', '6.2', '6.3', 
                              '7.1', '7.2', '7.3', '8.1', '8.2', '8.4', 
                              '9.1', '9.2', '9.3', '9.4', '10.1', '11.1',
                              '11.2', '11.3', '11.4', '12.1', NA))


cr_threat_count$Red_List_status <- "CR"
en_threat_count$Red_List_status <- "EN"
vu_threat_count$Red_List_status <- "VU"
lc_threat_count$Red_List_status <- "LC"
nt_threat_count$Red_List_status <- "NT"

#All amphibains: mean and standard deviation
lc_threat_count$threat_count <- as.numeric(lc_threat_count$threat_count)
sd(lc_threat_count$threat_count)
mean(lc_threat_count$threat_count)

nt_threat_count$threat_count <- as.numeric(nt_threat_count$threat_count)
sd(nt_threat_count$threat_count)
mean(nt_threat_count$threat_count)

vu_threat_count$threat_count <- as.numeric(vu_threat_count$threat_count)
sd(vu_threat_count$threat_count)
mean(vu_threat_count$threat_count)

en_threat_count$threat_count <- as.numeric(en_threat_count$threat_count)
mean(en_threat_count$threat_count)
sd(en_threat_count$threat_count)

cr_threat_count$threat_count <- as.numeric(cr_threat_count$threat_count)
mean(cr_threat_count$threat_count)
sd(cr_threat_count$threat_count)

all_threat_count <- rbind(cr_threat_count, en_threat_count, vu_threat_count, lc_threat_count, nt_threat_count)
all_threat_count$Red_List_status <- as.factor(all_threat_count$Red_List_status)

      #ALL AMPHIBIAINS DENSITY PLOT 
library(ggplot2)
all_threat_count$Red_List_status <- factor(all_threat_count$Red_List_status, levels = c("LC", "NT", "VU", "EN", "CR"))

all1 <- ggplot(all_threat_count, aes(x = all_threat_count$threat_count, colour = Red_List_status)) + geom_density(size = 0.75) +
  scale_x_continuous(name = "Number of Threat Processes") + 
  scale_y_continuous(name = "Density") + 
  scale_color_manual(values = c("green3", "olivedrab2", "yellow","orangered", "red3"), 
                     name = c(""), 
                     labels = c("Least Concern", "Near Threatened", "Vulnerable", 
                                "Endangered", "Critically Endangered")) + 
  theme_minimal() 


sd(all_threat_count$threat_count)
mean(all_threat_count$threat_count)
nrow(all_threat_count)

ano1 <- aov(all_threat_count$threat_count ~ Red_List_status, data = all_threat_count)
tHSD <- TukeyHSD(ano1, ordered = FALSE, conf.level = 0.95)
str(tHSD)
all_threat_count$threat_count <- as.numeric(all_threat_count$threat_count)

        #ALL AMPHIBIAINS BOX PLOT 
all_threat_count2 <- all_threat_count
library(ggplot2)
all_threat_count2 <- subset(all_threat_count, all_threat_count$threat_count <= 6)
all3 <- ggplot(data = all_threat_count, aes(x = Red_List_status, y = threat_count, fill = Red_List_status)) + geom_boxplot() +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point") + 
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
w_model <- lm(threat_count ~ Red_List_status, data = all_threat_count2)
plot(w_model)
bptest(w_model)
oneway.test(threat_count ~ Red_List_status, data = all_threat_count2)
bartlett.test(threat_count ~ Red_List_status, data = all_threat_count2)

one.way2 <- oneway(all_threat_count2$Red_List_status, y = all_threat_count2$threat_count, posthoc = 'games-howell')


hist(w_model)
library(MASS)
sresid <- studres(w_model) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

sd(all_threat_count$threat_count)

          ####END ALL AMPHIBIANS (WORLDWIDE) EXPLORATORY DATA ANALYSIS 

#3. South and SE Asia Exploratory Data (plots)

together #together doesnt have DD 
se_cr_data <- together %>% filter(together$Red.List.status == "CR")
secrplot1 <- ggplot(data = se_cr_data, aes (x = code, fill = code)) +
  geom_bar(stat = "count", position = "dodge", color="black", fill="red3") + 
  xlab(label = "South & Southeast Asia Red List Threat Code:Critically Endangered") + ylab(label = "Species Count") + 
  #Removes the grid lines in the back: 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position="none") + 
  scale_x_discrete(limits = c('1.1', '1.2', '1.3', '2.1', '2.2', '2.3', 
                              '2.4', '3.2', '3.3', '4.1', '4.2', '5.1', 
                              '5.2', '5.3', '5.4', '6.1', '6.2', '6.3', 
                              '7.1', '7.2', '7.3', '8.1', '8.2', '8.4', 
                              '9.1', '9.2', '9.3', '9.4', '10.1', '11.1',
                              '11.2', '11.3', '11.4', '12.1', NA))

se_en_data <- together %>% filter(together$Red.List.status == "EN")
seenplot1 <- ggplot(data = se_en_data, aes (x = code, fill = code)) +
  geom_bar(stat = "count", position = "dodge", color="black", fill="orangered") + 
  xlab(label = "South & Southeast Asia Red List Threat Code: Endangered") + ylab(label = "Species Count") + 
  #Removes the grid lines in the back: 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position="none") + 
  scale_x_discrete(limits = c('1.1', '1.2', '1.3', '2.1', '2.2', '2.3', 
                              '2.4', '3.2', '3.3', '4.1', '4.2', '5.1', 
                              '5.2', '5.3', '5.4', '6.1', '6.2', '6.3', 
                              '7.1', '7.2', '7.3', '8.1', '8.2', '8.4', 
                              '9.1', '9.2', '9.3', '9.4', '10.1', '11.1',
                              '11.2', '11.3', '11.4', '12.1', NA))

se_vu_data <- together %>% filter(together$Red.List.status == "VU")

sevuplot1 <- ggplot(data = se_vu_data, aes (x = code, fill = code)) +
  geom_bar(stat = "count", position = "dodge", color="black", fill="yellow") + 
  xlab(label = "South & Southeast Asia Red List Threat Code: Vulnerable") + ylab(label = "Species Count") + 
  #Removes the grid lines in the back: 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position="none") + 
  scale_x_discrete(limits = c('1.1', '1.2', '1.3', '2.1', '2.2', '2.3', 
                              '2.4', '3.2', '3.3', '4.1', '4.2', '5.1', 
                              '5.2', '5.3', '5.4', '6.1', '6.2', '6.3', 
                              '7.1', '7.2', '7.3', '8.1', '8.2', '8.4', 
                              '9.1', '9.2', '9.3', '9.4', '10.1', '11.1',
                              '11.2', '11.3', '11.4', '12.1', NA))

se_nt_data <- together %>% filter(together$Red.List.status == "NT")
sentplot1 <- ggplot(data = se_nt_data, aes (x = code, fill = code)) +
  geom_bar(stat = "count", position = "dodge", color="black", fill="olivedrab2") + 
  xlab(label = "South & Southeast Asia Red List Threat Code: Near Threatened") + ylab(label = "Species Count") + 
  #Removes the grid lines in the back: 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position="none") + 
  scale_x_discrete(limits = c('1.1', '1.2', '1.3', '2.1', '2.2', '2.3', 
                              '2.4', '3.2', '3.3', '4.1', '4.2', '5.1', 
                              '5.2', '5.3', '5.4', '6.1', '6.2', '6.3', 
                              '7.1', '7.2', '7.3', '8.1', '8.2', '8.4', 
                              '9.1', '9.2', '9.3', '9.4', '10.1', '11.1',
                              '11.2', '11.3', '11.4', '12.1', NA))

se_lc_data <- together %>% filter(together$Red.List.status == "LC")
selcplot1 <- ggplot(data = se_lc_data, aes (x = code, fill = code)) +
  geom_bar(stat = "count", position = "dodge", color="black", fill="green3") + 
  xlab(label = "South & Southeast Red List Threat Code: Least Concern") + ylab(label = "Species Count") + 
  #Removes the grid lines in the back: 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, size = 9, hjust = 1)) +
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position="none") + 
  scale_x_discrete(limits = c('1.1', '1.2', '1.3', '2.1', '2.2', '2.3', 
                              '2.4', '3.2', '3.3', '4.1', '4.2', '5.1', 
                              '5.2', '5.3', '5.4', '6.1', '6.2', '6.3', 
                              '7.1', '7.2', '7.3', '8.1', '8.2', '8.4', 
                              '9.1', '9.2', '9.3', '9.4', '10.1', '11.1',
                              '11.2', '11.3', '11.4', '12.1', NA))



#3 continued: SE ASIA THREAT COUNT (DENSITY DITRIBUTION, ANOVA, BOXPLOT)


counting <- threat_count(details)

nrow(se_asian_amph)
nrow(counting)

counting$Red_list_status <- se_asian_amph$Red.List.status
counting$Red_list_status <- as.factor(counting$Red_list_status)

head(counting)
head(se_asian_amph)

#SE ASIA BOXPLOT 
together$Red.List.status <- factor(together$Red.List.status, levels = c("LC", "NT", "VU", "EN", "CR"))
counting$Red_list_status <- factor(counting$Red_list_status, levels = c("LC", "NT", "VU", "EN", "CR"))
g <- ggplot(data = counting, aes(x = Red_list_status, y = threat_count, fill = Red_list_status)) + geom_boxplot() +
  labs(title = NULL) +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point") + 
  scale_x_discrete(limits = c("LC", "NT", "VU", "EN", "CR")) +
  scale_fill_manual(values = c("green3", "olivedrab2", "yellow","orangered", "red3"), 
                    labels = c("Least Concern", "Near Threatened", "Vulnerable", 
                               "Endangered", "Critically Endangered"),
                    name = NULL) + 
  xlab("South & Southeast Asia Red List Cateogry") + ylab("Number of Threats") +
  theme_minimal() 

summary(counting)
    #SE DENSITY DISTRIBUTION 
counting$threat_count <- as.numeric(counting$threat_count)
counting2 <- counting %>% filter(counting$Red_list_status != "DD")
counting3 <- counting2 %>% filter(counting2$Red_list_status != "EX")

counting3$Red_list_status <- factor(counting3$Red_list_status, levels = c("LC", "NT", "VU", "EN", "CR"))
se_dens <- ggplot(counting3, aes(x = threat_count, colour = Red_list_status)) + geom_density(size = 0.75) +
  scale_x_continuous(name = "South & Southeast Asia Number of Threat Processes") + 
  scale_y_continuous(name = "Density") + 
  scale_color_manual(values = c("green3", "olivedrab2", "yellow","orangered", "red3"), 
                     name = c(""), 
                     labels = c("Least Concern", "Near Threatened", "Vulnerable", 
                                "Endangered", "Critically Endangered")) + 
  theme_minimal() 

library("pgirmess")
oneway.test(threat_count ~ Red_list_status, data = counting3)
bartlett.test(threat_count ~ Red_list_status, data = counting3)



onewayanova <- aov(threat_count ~ Red_list_status, data = counting3)
summary(onewayanova)

TukeyHSD(onewayanova)


  #Games-Howell Post hoc test 
library("userfriendlyscience")
one.way <- oneway(counting3$Red_list_status, y = counting3$threat_count, posthoc = 'games-howell')


kruskal.test(threat_count ~ Red_list_status, data = counting3)
hist(counting3$threat_count, 100)
kruskalmc(threat_count ~ Red_list_status, data = counting3)



worldwide_data$Red_List_status <- as.factor(worldwide_data$Red_List_status)

worldwide_data$title <- as.factor(worldwide_data$title)
  library(lmtest) # bptest for testing heteroscedasticity

model <- lm(threat_count ~ Red_list_status, data = counting3)
plot(model$residuals)
bptest(model) 
plot(model)


#4 (ALL) World Amphibians Correlation Heat Map  

worldwide_data <- rbind(lc_data, nt_data, en_data, cr_data, vu_data)
worldwide_data <- worldwide_data[!(worldwide_data$code == "9.5.2" | worldwide_data$code == "9.5.3" | worldwide_data$code == "9.5.4"),]
worldwide_data <-  worldwide_data[complete.cases(worldwide_data$species_name), ]
world_codes <- threat_code(worldwide_data)
tail(world_codes)
world_threat_table <- threat_tables(world_codes)


all_cor <- cor(world_threat_table[,2:42])
world_threat_table$I8.3 <- NULL
world_threat_table$G10.2 <- NULL
world_threat_table$P9.6 <- NULL

plot(all_cor)
library(reshape2)
melted_threats <- melt(all_cor)
head(melted_threats)
#testing the signifcance of the correlation: (need to see if 
#pearsons test is the best for this - is this data paired?)


library(ggplot2)
cardib4 <- ggplot(data = melted_threats, aes(x = Var1, y = Var2, fill = value)) + geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, size = 9, hjust = 1)) +
  xlab(label = "Red List Threat Code") + ylab(label = "Red List Threat Code") 

cor.test(world_table_threats$R1.1, world_table_threats$A2.2)

library("MCMCglmm")
?MCMCglmm

cor.test(world)

### FIXING LOGISTIC REGRESSIONS 

setwd("/Users/Alexandra_Gonzalez/Downloads")
iucn_data <- read.csv("export-89505.csv", header = TRUE)






