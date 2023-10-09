library(tidyverse)
library(ggplot2)
library(ggthemes)
library(plyr)
library(dplyr)
library(reshape2)
library(matrixStats)
library(data.table)
library(stringr)
library(ggpattern)
library(ggpmisc)
library(ggpubr)

#setwd("C:/Users/malco/BaylorThesisCodeAndData") #Please change this for your own workflow
data2022<-read.csv("July2022BioassayData.csv")
data2023<-read.csv("July2023BioassayData.csv")
data<-rbind(data2022,data2023)
datat48<-data %>% filter(data$IncTime == "48")
datat48$Constant1 <- 1
datat48$Year <- as.character(datat48$Year)


#PSII Efficiency (Y) by Group
cyanoY <- aggregate(P_Y_B ~ NutRat + VitAdd + Year, 
    data=datat48, 
    function(x) { 
      c(avg=mean(x), sd=sd(x)) 
   })
cyanoY$Group <- "Cyanos"
cyanoY[c('Value', 'SD')] <- str_split_fixed(cyanoY$P_Y_B, ' ', 2)
cyanoY <- cyanoY[c('NutRat','VitAdd','Year','Value', 'SD', 'Group')]
 
greenY <- aggregate(P_Y_G ~ NutRat + VitAdd + Year,
    data=datat48, 
    function(x) { 
      c(avg=mean(x), sd=sd(x)) 
   })
greenY$Group <- "Green"
greenY[c('Value', 'SD')] <- str_split_fixed(greenY$P_Y_G, ' ', 2)
greenY <- greenY[c('NutRat','VitAdd','Year', 'Value', 'SD', 'Group')]
brownY <- aggregate(P_Y_Br ~ NutRat + VitAdd + Year,
    data=datat48, 
    function(x) { 
      c(avg=mean(x), sd=sd(x))  
   })  
brownY$Group <- "Brown"
brownY[c('Value', 'SD')] <- str_split_fixed(brownY$P_Y_Br, ' ', 2)
brownY <- brownY[c('NutRat','VitAdd','Year','Value', 'SD', 'Group')]
PEY <- aggregate(P_Y_PE ~ NutRat + VitAdd +Year,
    data=datat48, 
    function(x) { 
      c(avg=mean(x), sd=sd(x))  
   })  
PEY$Group <-"PE"
PEY[c('Value', 'SD')] <- str_split_fixed(PEY$P_Y_PE, ' ', 2)
PEY <- PEY[c('NutRat','VitAdd','Year','Value', 'SD', 'Group')]
names(greenY) = names(cyanoY)
names(brownY) = names(cyanoY)
names(PEY) = names(cyanoY)
Ymeansdgroup <- rbind(cyanoY,greenY,brownY,PEY)
Ymeansdgroup$Value <- as.numeric(Ymeansdgroup$Value)
Ymeansdgroup$SD <- as.numeric(Ymeansdgroup$SD)
Ymeansdgroup$Year <- as.character(Ymeansdgroup$Year)

write.csv(Ymeansdgroup,"Exports_CSV/PSIIMeanStDev.csv", row.names = TRUE)


PSIIGroup <- ggplot(Ymeansdgroup)+ 
  aes(x = Group, y = Value, fill = Year, pattern=VitAdd) + 
  geom_bar_pattern(position="dodge", stat="identity", pattern_spacing = 0.012,
                    pattern_frequency = 5, pattern_angle = 45)+
  scale_fill_manual(values=c("#E1BE6A","#40B0A6"))+
  scale_pattern_manual(values=c('none','stripe'))+
  labs(y = "Photosystem II Efficiency", x="Phytoplankton Group", title = "Photosystem II Efficiency by Phytoplankton Group")+
  geom_errorbar(aes(ymin = Value-SD, ymax = Value+SD), width = 0.3, position = position_dodge(0.9))+
  guides(fill = guide_legend(override.aes = list(pattern = c("none")))) +
  theme(legend.key.width = unit(1, "cm"), legend.title = element_blank(),
          legend.background = element_rect(color = "transparent"), legend.position = "right",
          legend.direction = "vertical", legend.spacing.y = unit(0.4, 'cm'),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          text = element_text(size=16))+
  facet_wrap(~NutRat)+
  theme_bw();
PSIIGroup  
ggsave(filename="PSII.tiff",PSIIGroup,
       width=10,height=7.5,units="in")



  
#Chl by Group
datat48$P_Chl_Total <- rowSums(datat48[ ,c("P_Chl_B","P_Chl_G","P_Chl_Br","P_Chl_PE")], na.rm = FALSE)
datat48$Prop_Chl_B <- datat48$P_Chl_B/datat48$P_Chl_Total
datat48$Prop_Chl_G <- datat48$P_Chl_G/datat48$P_Chl_Total
datat48$Prop_Chl_Br <- datat48$P_Chl_Br/datat48$P_Chl_Total
datat48$Prop_Chl_PE <- datat48$P_Chl_PE/datat48$P_Chl_Total
datat48$Perc_Chl_B <- datat48$Prop_Chl_B*100
datat48$Perc_Chl_G <- datat48$Prop_Chl_G*100
datat48$Perc_Chl_Br <- datat48$Prop_Chl_Br*100
datat48$Perc_Chl_PE <- datat48$Prop_Chl_PE*100


cyanochl <- aggregate(Perc_Chl_B ~ NutRat + VitAdd + Year, 
                      data=datat48, 
                      function(x) { 
                        c(avg=mean(x), sd=sd(x)) 
                      })
cyanochl$Group <- "Cyanos"
cyanochl[c('Value', 'SD')] <- str_split_fixed(cyanochl$Perc_Chl_B, ' ', 2)
cyanochl <- cyanochl[c('NutRat','VitAdd','Year','Value', 'SD', 'Group')]

greenchl <- aggregate(Perc_Chl_G ~ NutRat + VitAdd + Year,
                      data=datat48, 
                      function(x) { 
                        c(avg=mean(x), sd=sd(x)) 
                      })
greenchl$Group <- "Green"
greenchl[c('Value', 'SD')] <- str_split_fixed(greenchl$Perc_Chl_G, ' ', 2)
greenchl <- greenchl[c('NutRat','VitAdd','Year', 'Value', 'SD', 'Group')]
brownchl <- aggregate(Perc_Chl_Br ~ NutRat + VitAdd + Year,
                      data=datat48, 
                      function(x) { 
                        c(avg=mean(x), sd=sd(x))  
                      })  
brownchl$Group <- "Brown"
brownchl[c('Value', 'SD')] <- str_split_fixed(brownchl$Perc_Chl_Br, ' ', 2)
brownchl <- brownchl[c('NutRat','VitAdd','Year','Value', 'SD', 'Group')]
PEchl <- aggregate(Perc_Chl_PE ~ NutRat + VitAdd +Year,
                   data=datat48, 
                   function(x) { 
                     c(avg=mean(x), sd=sd(x))  
                   })  
PEchl$Group <-"PE"
PEchl[c('Value', 'SD')] <- str_split_fixed(PEchl$Perc_Chl_PE, ' ', 2)
PEchl <- PEchl[c('NutRat','VitAdd','Year','Value', 'SD', 'Group')]
names(greenchl) = names(cyanochl)
names(brownchl) = names(cyanochl)
names(PEchl) = names(cyanochl)
chlmeansdgroup <- rbind(cyanochl,greenchl,brownchl,PEchl)
chlmeansdgroup$Value <- as.numeric(chlmeansdgroup$Value)
chlmeansdgroup$SD <- as.numeric(chlmeansdgroup$SD)
chlmeansdgroup$Year <- as.character(chlmeansdgroup$Year)

write.csv(chlmeansdgroup,"Exports_CSV/ChlByGroupMeanStDev.csv", row.names = TRUE)



ChlGroup <- ggplot(chlmeansdgroup)+ 
  aes(x = Group, y = Value, fill = Year, pattern=VitAdd) + 
  geom_bar_pattern(position="dodge", stat="identity", pattern_spacing = 0.012,
                   pattern_frequency = 5, pattern_angle = 45)+
  scale_fill_manual(values=c("#E1BE6A","#40B0A6"))+
  scale_pattern_manual(values=c('none','stripe'))+
  labs(y = "Percent of Community", x="Phytoplankton Group", title = "Phytoplankton Community Composition")+
  geom_errorbar(aes(ymin = Value-SD, ymax = Value+SD), width = 0.3, position = position_dodge(0.9))+
  guides(fill = guide_legend(override.aes = list(pattern = c("none")))) +
  theme(legend.key.width = unit(1, "cm"), legend.title = element_blank(),
        legend.background = element_rect(color = "transparent"), legend.position = "right",
        legend.direction = "vertical", legend.spacing.y = unit(0.4, 'cm'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size=16))+
  #scale_y_continuous(limits= c(-5, 110), breaks = c(0,20,40,60,80,100)) +
  facet_wrap(~NutRat)+ 
  theme_bw();
ChlGroup  
ggsave(filename="Community_Composition.tiff",ChlGroup,
       width=10,height=7.5,units="in")


#Phytopam Chl plot (bioassay results)
totalchl <- aggregate(P_Chl_Total ~ NutRat + VitAdd + Year, 
                      data=datat48, 
                      function(x) { 
                        c(avg=mean(x), sd=sd(x)) 
                      })

totalchl[c('Value', 'SD')] <- str_split_fixed(totalchl$P_Chl_Total, ' ', 2)
totalchl <- totalchl[c('NutRat','VitAdd','Year','Value', 'SD')]
totalchl$Value <- as.numeric(totalchl$Value)
totalchl$SD <- as.numeric(totalchl$SD)
totalchl$Year <- as.character(totalchl$Year)
totalchl$NutRat <- as.character(totalchl$NutRat)

write.csv(totalchl,"Exports_CSV/TotalChlMeanStDev.csv", row.names = TRUE)

datat48$Chl_445 <- as.numeric(datat48$Chl_445)
totalchl445 <- aggregate(Chl_445 ~ NutRat + VitAdd + Year, 
                         data=datat48, 
                         function(x) { 
                           c(avg=mean(x), sd=sd(x)) 
                         })

totalchl445[c('Value', 'SD')] <- str_split_fixed(totalchl445$Chl_445, ' ', 2)
totalchl445 <- totalchl445[c('NutRat','VitAdd','Year','Value', 'SD')]
totalchl445$Value <- as.numeric(totalchl445$Value)
totalchl445$SD <- as.numeric(totalchl445$SD)
totalchl445$Year <- as.character(totalchl445$Year)
totalchl445$NutRat <- as.character(totalchl445$NutRat)
write.csv(totalchl445,"Exports_CSV/Chl445MeanStDev.csv", row.names = TRUE)

ChlTotal <- ggplot(totalchl)+ 
  aes(x = NutRat, y = Value, fill = Year, pattern=VitAdd) + 
  geom_bar_pattern(position="dodge", stat="identity", pattern_spacing = 0.012,
                   pattern_frequency = 5, pattern_angle = 45)+
  scale_fill_manual(values=c("#E1BE6A","#40B0A6"))+
  scale_pattern_manual(values=c('none','stripe'))+
  scale_x_discrete(limits = c("0","2.2","16","55","110")) +
  labs(y = "Total Phyto-PAM-II Chlorophyll a (µg/L)", x="N:P Addition Ratio of Limnocorral", title = "Total Phytoplankton Biomass after 48H of incubation")+
  geom_errorbar(aes(ymin = Value-SD, ymax = Value+SD), width = 0.3, position = position_dodge(0.9))+
  guides(fill = guide_legend(override.aes = list(pattern = c("none")))) +
  theme(legend.key.width = unit(1, "cm"), legend.title = element_blank(),
        legend.background = element_rect(color = "transparent"), legend.position = "right",
        legend.direction = "vertical", legend.spacing.y = unit(0.4, 'cm'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size=16))+
  theme_bw();
ChlTotal 
ggsave(filename="BioassayResults.tiff",ChlTotal,
       width=10,height=7.5,units="in")

#DNP
##Log Transform Data
datat48$L_NOX <- as.numeric(datat48$L_NOX)
datat48$L_NH4 <- as.numeric(datat48$L_NH4)
datat48$L_DP <- as.numeric(datat48$L_DP)

NOX <- aggregate(L_NOX ~ NutRat + VitAdd + Year, 
                      data=datat48, 
                      function(x) { 
                        c(avg=mean(x), sd=sd(x)) 
                      })
NOX$Nut <- "NOX"
NOX[c('Value', 'SD')] <- str_split_fixed(NOX$L_NOX, ' ', 2)
NOX <- NOX[c('NutRat','VitAdd','Year','Value', 'SD', 'Nut')]

NH4 <- aggregate(L_NH4 ~ NutRat + VitAdd + Year, 
                 data=datat48, 
                 function(x) { 
                   c(avg=mean(x), sd=sd(x)) 
                 })
NH4$Nut <- "NH4"
NH4[c('Value', 'SD')] <- str_split_fixed(NH4$L_NH4, ' ', 2)
NH4 <- NH4[c('NutRat','VitAdd','Year','Value', 'SD', 'Nut')]

PO4 <- aggregate(L_DP ~ NutRat + VitAdd + Year, 
                 data=datat48, 
                 function(x) { 
                   c(avg=mean(x), sd=sd(x)) 
                 })
PO4$Nut <- "SRP"
PO4[c('Value', 'SD')] <- str_split_fixed(PO4$L_DP, ' ', 2)
PO4 <- PO4[c('NutRat','VitAdd','Year','Value', 'SD', 'Nut')]


names(NH4) = names(NOX)
names(PO4) = names(NOX)
DNPmeansdgroup <- rbind(NOX,NH4,PO4)
DNPmeansdgroup$Value <- as.numeric(DNPmeansdgroup$Value)
DNPmeansdgroup$SD <- as.numeric(DNPmeansdgroup$SD)
DNPmeansdgroup$Year <- as.character(DNPmeansdgroup$Year)

write.csv(DNPmeansdgroup,"Exports_CSV/DNPMeanStDev.csv", row.names = TRUE)
DNPmeansdgroup$Constant1 <- 1
DNPmeansdgroup$ValueT <- rowSums(DNPmeansdgroup[,c("Value", "Constant1")], na.rm=FALSE)
DNPmeansdgroup$SDT <- rowSums(DNPmeansdgroup[,c("SD", "Constant1")], na.rm=FALSE)
DNPmeansdgroupT <- DNPmeansdgroup %>%
  mutate_at(vars(ValueT), ~log10(.)) %>%
  mutate_at(vars(SDT), ~log10(.))

DNP <- ggplot(DNPmeansdgroupT)+ 
  aes(x = Nut, y = ValueT, fill = Year, pattern=VitAdd) + 
  geom_bar_pattern(position="dodge", stat="identity", pattern_spacing = 0.012,
                   pattern_frequency = 5, pattern_angle = 45)+
  scale_fill_manual(values=c("#E1BE6A","#40B0A6"))+
  scale_pattern_manual(values=c('none','stripe'))+
  labs(y = "log10(Nutrient Concentration+1) (µg/L)", x="Nutrient Type", title = "Dissolved Bioavailable N and P")+
  geom_errorbar(aes(ymin = ValueT-SDT, ymax = ValueT+SDT), width = 0.3, position = position_dodge(0.9))+
  guides(fill = guide_legend(override.aes = list(pattern = c("none")))) +
  theme(legend.key.width = unit(1, "cm"), legend.title = element_blank(),
        legend.background = element_rect(color = "transparent"), legend.position = "right",
        legend.direction = "vertical", legend.spacing.y = unit(0.4, 'cm'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size=16))+
  #scale_y_continuous(trans = "log10")+
  #scale_y_continuous(limits= c(-5, 110), breaks = c(0,20,40,60,80,100)) +
  facet_wrap(~NutRat)+ 
  theme_bw();
DNP  
ggsave(filename="DNP_barchart.tiff",DNP,
       width=10,height=7.5,units="in")

#TNP
datat48$L_TN <- as.numeric(datat48$L_TN)
datat48$L_TDN <- as.numeric(datat48$L_TDN)
datat48$L_TP <- as.numeric(datat48$L_TP)
datat48$L_TDP <- as.numeric(datat48$L_TDP)


TN <- aggregate(L_TN ~ NutRat + VitAdd + Year, 
                 data=datat48, 
                 function(x) { 
                   c(avg=mean(x), sd=sd(x)) 
                 })
TN$Nut <- "TN"
TN[c('Value', 'SD')] <- str_split_fixed(TN$L_TN, ' ', 2)
TN <- TN[c('NutRat','VitAdd','Year','Value', 'SD', 'Nut')]

TDN <- aggregate(L_TDN ~ NutRat + VitAdd + Year, 
                data=datat48, 
                function(x) { 
                  c(avg=mean(x), sd=sd(x)) 
                })
TDN$Nut <- "TDN"
TDN[c('Value', 'SD')] <- str_split_fixed(TDN$L_TDN, ' ', 2)
TDN <- TDN[c('NutRat','VitAdd','Year','Value', 'SD', 'Nut')]

TP <- aggregate(L_TP ~ NutRat + VitAdd + Year, 
                data=datat48, 
                function(x) { 
                  c(avg=mean(x), sd=sd(x)) 
                })
TP$Nut <- "TP"
TP[c('Value', 'SD')] <- str_split_fixed(TP$L_TP, ' ', 2)
TP <- TP[c('NutRat','VitAdd','Year','Value', 'SD', 'Nut')]

TDP <- aggregate(L_TDP ~ NutRat + VitAdd + Year, 
                data=datat48, 
                function(x) { 
                  c(avg=mean(x), sd=sd(x)) 
                })
TDP$Nut <- "TDP"
TDP[c('Value', 'SD')] <- str_split_fixed(TDP$L_TDP, ' ', 2)
TDP <- TDP[c('NutRat','VitAdd','Year','Value', 'SD', 'Nut')]

names(TDN) = names(TN)
names(TP) = names(TN)
names(TDP) = names(TN)
TNPmeansdgroup <- rbind(TN,TP,TDN,TDP)
TNPmeansdgroup$Value <- as.numeric(TNPmeansdgroup$Value)
TNPmeansdgroup$SD <- as.numeric(TNPmeansdgroup$SD)
TNPmeansdgroup$Year <- as.character(TNPmeansdgroup$Year)

write.csv(TNPmeansdgroup,"Exports_CSV/TNPMeanStDev.csv", row.names = TRUE)
TNPmeansdgroup$Constant1 <- 1
TNPmeansdgroup$ValueT <- rowSums(TNPmeansdgroup[,c("Value", "Constant1")], na.rm=FALSE)
TNPmeansdgroup$SDT <- rowSums(TNPmeansdgroup[,c("SD", "Constant1")], na.rm=FALSE)
TNPmeansdgroupT <- TNPmeansdgroup %>%
  mutate_at(vars(ValueT), ~log10(.)) %>%
  mutate_at(vars(SDT), ~log10(.))


TNP <- ggplot(TNPmeansdgroupT)+ 
  aes(x = Nut, y = ValueT, fill = Year, pattern=VitAdd) + 
  geom_bar_pattern(position="dodge", stat="identity", pattern_spacing = 0.012,
                   pattern_frequency = 5, pattern_angle = 45)+
  scale_fill_manual(values=c("#E1BE6A","#40B0A6"))+
  scale_pattern_manual(values=c('none','stripe'))+
  labs(y = "log10(Nutrient Concentration+1) (µg/L)", x="Nutrient Type", title = "Total and Total Dissolved N and P")+
  geom_errorbar(aes(ymin = ValueT-SDT, ymax = ValueT+SDT), width = 0.3, position = position_dodge(0.9))+
  guides(fill = guide_legend(override.aes = list(pattern = c("none")))) +
  theme(legend.key.width = unit(1, "cm"), legend.title = element_blank(),
        legend.background = element_rect(color = "transparent"), legend.position = "right",
        legend.direction = "vertical", legend.spacing.y = unit(0.4, 'cm'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size=16))+
  #scale_y_continuous(trans = "log10")+
  #scale_y_continuous(limits= c(-5, 110), breaks = c(0,20,40,60,80,100)) +
  facet_wrap(~NutRat)+ 
  theme_bw();
TNP  
ggsave(filename="TNP_barchart.tiff",TNP,
       width=10,height=7.5,units="in")

#Chlorophyll comparison
datat48$Chl_445 <- as.numeric(datat48$Chl_445)
datat48$P_Chl_Total <- as.numeric(datat48$P_Chl_Total)
datat48$Year <- as.character(datat48$Year)
datat48_2022 <- datat48 %>% filter(Year==2022)
datat48_2022$Chl_445 <- as.numeric(datat48_2022$Chl_445)
datat48_2022$P_Chl_Total <- as.numeric(datat48_2022$P_Chl_Total)
datat48_2022$Year <- as.character(datat48_2022$Year)
datat48_2023 <- datat48 %>% filter(Year==2023)
datat48_2023$Chl_445 <- as.numeric(datat48_2023$Chl_445)
datat48_2023$P_Chl_Total <- as.numeric(datat48_2023$P_Chl_Total)
datat48_2023$Year <- as.character(datat48_2023$Year)

reg2022 <- lm(P_Chl_Total~Chl_445, data = datat48_2022)
reg2022
summary(reg2022)

chlcomparison2022 <- ggplot(data = datat48_2022, aes(x = Chl_445, y = P_Chl_Total))+
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "adj.R2", "f", "p", "n"))) +
  geom_point(color="#E1BE6A") +
  labs(y = "Phyto-PAM-II (µg/L Chl a)", x="EPA Method 445.0 (µg/L Chl a)", title = "2022 Chlorophyll a")+ 
  theme_bw();

chlcomparison2022

reg2023 <- lm(P_Chl_Total~Chl_445, data = datat48_2023)
reg2023
summary(reg2023)

chlcomparison2023 <- ggplot(data = datat48_2023, aes(x = Chl_445, y = P_Chl_Total))+
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "adj.R2", "f", "p", "n"))) +
  geom_point(color="#40B0A6") +
  labs(y = "Phyto-PAM-II (µg/L Chl a)", x="EPA Method 445.0 (µg/L Chl a)", title = "2023 Chlorophyll a") +
  theme_bw();

chlcomparison2023

regboth <- lm(P_Chl_Total~Chl_445, data = datat48)
regboth
summary(regboth)

bothyearchlcomparison <- ggplot(data = datat48, aes(x = Chl_445, y = P_Chl_Total))+
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "adj.R2", "f", "p", "n"))) +
  geom_point(data = datat48_2022, aes(x = Chl_445, y = P_Chl_Total), color="#E1BE6A") +
  geom_point(data = datat48_2023, aes(x = Chl_445, y = P_Chl_Total), color="#40B0A6") +
  labs(y = "Phyto-PAM-II (µg/L Chl a)", x="EPA Method 445.0 (µg/L Chl a)", title = "2022 and 2023 Combined Chlorophyll a") +
  theme_bw();

bothyearchlcomparison

totalchlcomparison <- ggarrange(chlcomparison2022, chlcomparison2023, bothyearchlcomparison, 
                                      labels = c("A", "B", "C"),
                                      ncol = 1, nrow = 3);
totalchlcomparison

ggsave(filename="TotalChlComparison.tiff",totalchlcomparison,
       width=6,height=12,units="in")

#CNP
##Due to analytical error, this code is to set the 2023 PP data from Pond 15 to NA values. Can remove if data is not to be removed
datat48$PP1 <- datat48$PP #saving original data
datat48_2022PP <- datat48 %>% filter(Year == 2022)
datat48_2023PP <- datat48 %>% filter(Year == 2023,Pond != 15)
datat48_2023PP15 <- datat48 %>% filter(Year == 2023,Pond == 15)
datat48_2023PP15$PP <- NA
datat48_PP <- rbind(datat48_2022PP,datat48_2023PP,datat48_2023PP15)
datat48$PP <- datat48_PP$PP
#Calculate Ratios
datat48$CNRatio <- datat48$EA_C/datat48$EA_N
datat48$CPRatio <- datat48$EA_C/datat48$PP
datat48$NPRatio <- datat48$EA_N/datat48$PP
datat48$CNPRatio <- paste(datat48$CPRatio,datat48$NPRatio,datat48$Constant1,sep=":")
CN <- aggregate(CNRatio ~ NutRat + VitAdd + Year, 
                 data=datat48, 
                 function(x) { 
                   c(avg=mean(x), sd=sd(x)) 
                 })
CN$Nut <- "CN"
CN[c('Value', 'SD')] <- str_split_fixed(CN$CNRatio, ' ', 2)
CN <- CN[c('NutRat','VitAdd','Year','Value', 'SD', 'Nut')]

CP <- aggregate(CPRatio ~ NutRat + VitAdd + Year, 
                 data=datat48, 
                 function(x) { 
                   c(avg=mean(x), sd=sd(x)) 
                 })
CP$Nut <- "CP"
CP[c('Value', 'SD')] <- str_split_fixed(CP$CPRatio, ' ', 2)
CP <- CP[c('NutRat','VitAdd','Year','Value', 'SD', 'Nut')]

NP <- aggregate(NPRatio ~ NutRat + VitAdd + Year, 
                 data=datat48, 
                 function(x) { 
                   c(avg=mean(x), sd=sd(x)) 
                 })
NP$Nut <- "NP"
NP[c('Value', 'SD')] <- str_split_fixed(NP$NPRatio, ' ', 2)
NP <- NP[c('NutRat','VitAdd','Year','Value', 'SD', 'Nut')]


names(CP) = names(CN)
names(NP) = names(CN)
CNPmeansdgroup <- rbind(CN,CP,NP)
CNPmeansdgroup$Value <- as.numeric(CNPmeansdgroup$Value)
CNPmeansdgroup$SD <- as.numeric(CNPmeansdgroup$SD)
CNPmeansdgroup$Year <- as.character(CNPmeansdgroup$Year)


write.csv(CNPmeansdgroup,"Exports_CSV/CNPMeanStDev.csv", row.names = TRUE)

CNP <- ggplot(CNPmeansdgroup)+ 
  aes(x = Nut, y = Value, fill = Year, pattern=VitAdd) + 
  geom_bar_pattern(position="dodge", stat="identity", pattern_spacing = 0.012,
                   pattern_frequency = 5, pattern_angle = 45)+
  scale_fill_manual(values=c("#E1BE6A","#40B0A6"))+
  scale_pattern_manual(values=c('none','stripe'))+
  labs(y = "Ratio of Nutrients by Weight", x="Nutrient Ratio", title = "Particulate C:N:P Ratios by Weight")+
  geom_errorbar(aes(ymin = Value-SD, ymax = Value+SD), width = 0.3, position = position_dodge(0.9))+
  guides(fill = guide_legend(override.aes = list(pattern = c("none")))) +
  theme(legend.key.width = unit(1, "cm"), legend.title = element_blank(),
        legend.background = element_rect(color = "transparent"), legend.position = "right",
        legend.direction = "vertical", legend.spacing.y = unit(0.4, 'cm'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size=16))+
  #scale_y_continuous(trans = "log10")+
  #scale_y_continuous(limits= c(-5, 110), breaks = c(0,20,40,60,80,100)) +
  facet_wrap(~NutRat)+ 
  theme_bw();
CNP  
ggsave(filename="CNP_barchart.tiff",CNP,
       width=10,height=7.5,units="in")

#Creating figure without the 2023 Pond 15 Data Removed 
datat48$PP1 <- as.numeric(datat48$PP1)
datat48$CNRatio <- datat48$EA_C/datat48$EA_N
datat48$CPRatio1 <- datat48$EA_C/datat48$PP1
datat48$NPRatio1 <- datat48$EA_N/datat48$PP1
datat48$CNPRatio1 <- paste(datat48$CPRatio1,datat48$NPRatio1,datat48$Constant1,sep=":")
CN <- aggregate(CNRatio ~ NutRat + VitAdd + Year, 
                data=datat48, 
                function(x) { 
                  c(avg=mean(x), sd=sd(x)) 
                })
CN$Nut <- "CN"
CN[c('Value', 'SD')] <- str_split_fixed(CN$CNRatio, ' ', 2)
CN <- CN[c('NutRat','VitAdd','Year','Value', 'SD', 'Nut')]

CP1 <- aggregate(CPRatio1 ~ NutRat + VitAdd + Year, 
                data=datat48, 
                function(x) { 
                  c(avg=mean(x), sd=sd(x)) 
                })
CP1$Nut <- "CP"
CP1[c('Value', 'SD')] <- str_split_fixed(CP1$CPRatio1, ' ', 2)
CP1 <- CP1[c('NutRat','VitAdd','Year','Value', 'SD', 'Nut')]

NP1 <- aggregate(NPRatio1 ~ NutRat + VitAdd + Year, 
                data=datat48, 
                function(x) { 
                  c(avg=mean(x), sd=sd(x)) 
                })
NP1$Nut <- "NP"
NP1[c('Value', 'SD')] <- str_split_fixed(NP1$NPRatio1, ' ', 2)
NP1 <- NP1[c('NutRat','VitAdd','Year','Value', 'SD', 'Nut')]


names(CP1) = names(CN)
names(NP1) = names(CN)
CNPmeansdgroup1 <- rbind(CN,CP1,NP1)
CNPmeansdgroup1$Value <- as.numeric(CNPmeansdgroup1$Value)
CNPmeansdgroup1$SD <- as.numeric(CNPmeansdgroup1$SD)
CNPmeansdgroup1$Year <- as.character(CNPmeansdgroup1$Year)


CNP1 <- ggplot(CNPmeansdgroup1)+ 
  aes(x = Nut, y = Value, fill = Year, pattern=VitAdd) + 
  geom_bar_pattern(position="dodge", stat="identity", pattern_spacing = 0.012,
                   pattern_frequency = 5, pattern_angle = 45)+
  scale_fill_manual(values=c("#E1BE6A","#40B0A6"))+
  scale_pattern_manual(values=c('none','stripe'))+
  labs(y = "Ratio of Nutrients by Weight", x="Nutrient Ratio", title = "Particulate C:N:P Ratios by Weight")+
  geom_errorbar(aes(ymin = Value-SD, ymax = Value+SD), width = 0.3, position = position_dodge(0.9))+
  guides(fill = guide_legend(override.aes = list(pattern = c("none")))) +
  theme(legend.key.width = unit(1, "cm"), legend.title = element_blank(),
        legend.background = element_rect(color = "transparent"), legend.position = "right",
        legend.direction = "vertical", legend.spacing.y = unit(0.4, 'cm'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size=16))+
  #scale_y_continuous(trans = "log10")+
  #scale_y_continuous(limits= c(-5, 110), breaks = c(0,20,40,60,80,100)) +
  facet_wrap(~NutRat)+ 
  theme_bw();
CNP1  
ggsave(filename="CNP_unfiltered_barchart.tiff",CNP1,
       width=10,height=7.5,units="in")

write.csv(datat48,"Exports_CSV/datatable48hours.csv", row.names = TRUE)