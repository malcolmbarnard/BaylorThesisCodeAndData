library(tidyverse)
library(ggpubr)
library(rstatix)

#setwd("C:/Users/malco/BaylorThesisCodeAndData") #Please change this for your own workflow
##Data import is based on BarPlots.R as the data manipulation is required for creating the Bar Plots
datat48 <- read.csv("Exports_CSV/datatable48hours.csv")
datat48$Year <- as.character(datat48$Year)
datat48$NutRat <- as.character(datat48$NutRat)
#Total Chlorophyll a

datat48 %>%
  group_by(Year, NutRat, VitAdd) %>%
  get_summary_stats(P_Chl_Total, type = "mean_sd")

boxplottotchl <- ggboxplot(
  datat48, x = "VitAdd", y = "P_Chl_Total", 
  color = "Year", palette = "jco", facet.by = "NutRat"
)
boxplottotchl

###Check Check Normality
model  <- lm(P_Chl_Total ~ Year*NutRat*VitAdd, data = datat48)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
#Failed assumption of normality - need non-parametric test (Kruskal-Wallis)

#For each group
datat48 %>%
  group_by(Year, NutRat, VitAdd) %>%
  shapiro_test(P_Chl_Total)

ggqqplot(datat48, "P_Chl_Total", ggtheme = theme_bw()) +
  facet_grid(Year + NutRat ~ VitAdd, labeller = "label_both")

#Homogeny of varience
datat48%>% levene_test(P_Chl_Total ~ Year*NutRat*VitAdd)
#Failed test of homogeny of varience. As data also failed assumption of normality, the Kruskal-Wallis test is a test of medians not means

##Run Kruskal-Wallis Year
totchlres.kruskalYear <- datat48 %>% kruskal_test(P_Chl_Total ~ Year)
totchlres.kruskalYear

#Posthoc with Dunn's Test 
totchldunnYear <- datat48 %>% 
  dunn_test(P_Chl_Total ~ Year, p.adjust.method = "bonferroni") 
totchldunnYear

totchldunnYear <- totchldunnYear %>% add_xy_position(x = "Year")
TotchlboxplotYear <- ggboxplot(datat48, x = "Year", y = "P_Chl_Total", ylab = "Total Chlorophyll a (µg/L)", xlab = "Year") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(totchldunnYear, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(totchlres.kruskalYear, detailed = TRUE),
    caption = get_pwc_label(totchldunnYear)
  )
TotchlboxplotYear

totchlres.kruskalNutRat <- datat48 %>% kruskal_test(P_Chl_Total ~ NutRat)
totchlres.kruskalNutRat

totchldunnNutRat <- datat48 %>% 
  dunn_test(P_Chl_Total ~ NutRat, p.adjust.method = "bonferroni") 
totchldunnNutRat

totchldunnNutRat <- totchldunnNutRat %>% add_xy_position(x = "NutRat")
TotchlboxplotNutRat <- ggboxplot(datat48, x = "NutRat", y = "P_Chl_Total", ylab = "Total Chlorophyll a (µg/L)", xlab = "Limnocorral N:P Ratio", order = c("0","2.2","16","55","110")) +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(totchldunnNutRat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(totchlres.kruskalNutRat, detailed = TRUE),
    caption = get_pwc_label(totchldunnNutRat)
  )
TotchlboxplotNutRat

totchlres.kruskalVitAdd <- datat48 %>% kruskal_test(P_Chl_Total ~ VitAdd)
totchlres.kruskalVitAdd

totchldunnVitAdd <- datat48 %>% 
  dunn_test(P_Chl_Total ~ VitAdd, p.adjust.method = "bonferroni") 
totchldunnVitAdd

totchldunnVitAdd <- totchldunnVitAdd %>% add_xy_position(x = "VitAdd")
TotchlboxplotVitAdd <- ggboxplot(datat48, x = "VitAdd", y = "P_Chl_Total", ylab = "Total Chlorophyll a (µg/L)", xlab = "Vitamin Addition") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(totchldunnVitAdd, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(totchlres.kruskalVitAdd, detailed = TRUE),
    caption = get_pwc_label(totchldunnVitAdd)
  )
TotchlboxplotVitAdd

totalchlboxplotscombined <- ggarrange(TotchlboxplotYear, TotchlboxplotNutRat, TotchlboxplotVitAdd, 
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3);
totalchlboxplotscombined

ggsave(filename="Stat_Outputs/TotalChlBoxPlotsCombined.tiff",totalchlboxplotscombined,
       width=6,height=12,units="in")

#Photosystem II activity 
##Cyanos
datat48 %>%
  group_by(Year, NutRat, VitAdd) %>%
  get_summary_stats(P_Y_B, type = "mean_sd")

boxplotYcyano <- ggboxplot(
  datat48, x = "VitAdd", y = "P_Y_B", 
  color = "Year", palette = "jco", facet.by = "NutRat"
)
boxplotYcyano

###Check Check Normality
model  <- lm(P_Y_B ~ Year*NutRat*VitAdd, data = datat48)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
#Failed assumption of normality - need non-parametric test (Kruskal-Wallis)

#For each group
#datat48 %>%
  #group_by(Year, NutRat, VitAdd) %>%
  #shapiro_test(P_Y_B)

ggqqplot(datat48, "P_Y_B", ggtheme = theme_bw()) +
  facet_grid(Year + NutRat ~ VitAdd, labeller = "label_both")

#Homogeny of varience
datat48%>% levene_test(P_Y_B ~ Year*NutRat*VitAdd)
#Failed test of homogeny of varience. As data also failed assumption of normality, the Kruskal-Wallis test is a test of medians not means

##Run Kruskal-Wallis Year
YBres.kruskalYear <- datat48 %>% kruskal_test(P_Y_B ~ Year)
YBres.kruskalYear

#Posthoc with Dunn's Test 
YBdunnYear <- datat48 %>% 
  dunn_test(P_Y_B ~ Year, p.adjust.method = "bonferroni") 
YBdunnYear

YBdunnYear <- YBdunnYear %>% add_xy_position(x = "Year")
YBboxplotYear <- ggboxplot(datat48, x = "Year", y = "P_Y_B", ylab = "Cyanobacterial PSII Efficiency", xlab = "Year") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(YBdunnYear, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(YBres.kruskalYear, detailed = TRUE),
    caption = get_pwc_label(YBdunnYear)
  )
YBboxplotYear

YBres.kruskalNutRat <- datat48 %>% kruskal_test(P_Y_B ~ NutRat)
YBres.kruskalNutRat

YBdunnNutRat <- datat48 %>% 
  dunn_test(P_Y_B ~ NutRat, p.adjust.method = "bonferroni") 
YBdunnNutRat

YBdunnNutRat <- YBdunnNutRat %>% add_xy_position(x = "NutRat")
YBboxplotNutRat <- ggboxplot(datat48, x = "NutRat", y = "P_Y_B", ylab = "Cyanobacterial PSII Efficiency ", xlab = "Limnocorral N:P Ratio", order = c("0","2.2","16","55","110")) +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(YBdunnNutRat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(YBres.kruskalNutRat, detailed = TRUE),
    caption = get_pwc_label(YBdunnNutRat)
  )
YBboxplotNutRat

YBres.kruskalVitAdd <- datat48 %>% kruskal_test(P_Y_B ~ VitAdd)
YBres.kruskalVitAdd

YBdunnVitAdd <- datat48 %>% 
  dunn_test(P_Y_B ~ VitAdd, p.adjust.method = "bonferroni") 
YBdunnVitAdd

YBdunnVitAdd <- YBdunnVitAdd %>% add_xy_position(x = "VitAdd")
YBboxplotVitAdd <- ggboxplot(datat48, x = "VitAdd", y = "P_Y_B", ylab = "Cyanobacterial PSII Efficiency ", xlab = "Vitamin Addition") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(YBdunnVitAdd, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(YBres.kruskalVitAdd, detailed = TRUE),
    caption = get_pwc_label(YBdunnVitAdd)
  )
YBboxplotVitAdd

##Chlorophytes
datat48 %>%
  group_by(Year, NutRat, VitAdd) %>%
  get_summary_stats(P_Y_G, type = "mean_sd")

boxplotYgreen <- ggboxplot(
  datat48, x = "VitAdd", y = "P_Y_G", 
  color = "Year", palette = "jco", facet.by = "NutRat"
)
boxplotYgreen

###Check Check Normality
model  <- lm(P_Y_G ~ Year*NutRat*VitAdd, data = datat48)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
#Failed assumption of normality - need non-parametric test (Kruskal-Wallis)

#For each group
#datat48 %>%
  #group_by(Year, NutRat, VitAdd) %>%
  #shapiro_test(P_Y_G)

#ggqqplot(datat48, "P_Y_G", ggtheme = theme_bw()) +
  #facet_grid(Year + NutRat ~ VitAdd, labeller = "label_both")

#Homogeny of varience
datat48%>% levene_test(P_Y_G ~ Year*NutRat*VitAdd)
#Failed test of homogeny of varience. As data also failed assumption of normality, the Kruskal-Wallis test is a test of medians not means

##Run Kruskal-Wallis Year
YGres.kruskalYear <- datat48 %>% kruskal_test(P_Y_G ~ Year)
YGres.kruskalYear

#Posthoc with Dunn's Test 
YGdunnYear <- datat48 %>% 
  dunn_test(P_Y_G ~ Year, p.adjust.method = "bonferroni") 
YGdunnYear

YGdunnYear <- YGdunnYear %>% add_xy_position(x = "Year")
YGboxplotYear <- ggboxplot(datat48, x = "Year", y = "P_Y_G", ylab = "Green Algal PSII Efficiency", xlab = "Year") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(YGdunnYear, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(YGres.kruskalYear, detailed = TRUE),
    caption = get_pwc_label(YGdunnYear)
  )
YGboxplotYear

YGres.kruskalNutRat <- datat48 %>% kruskal_test(P_Y_G ~ NutRat)
YGres.kruskalNutRat

YGdunnNutRat <- datat48 %>% 
  dunn_test(P_Y_G ~ NutRat, p.adjust.method = "bonferroni") 
YGdunnNutRat

YGdunnNutRat <- YGdunnNutRat %>% add_xy_position(x = "NutRat")
YGboxplotNutRat <- ggboxplot(datat48, x = "NutRat", y = "P_Y_G", ylab = "Green Algal PSII Efficiency ", xlab = "Limnocorral N:P Ratio", order = c("0","2.2","16","55","110")) +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(YGdunnNutRat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(YGres.kruskalNutRat, detailed = TRUE),
    caption = get_pwc_label(YGdunnNutRat)
  )
YGboxplotNutRat

YGres.kruskalVitAdd <- datat48 %>% kruskal_test(P_Y_G ~ VitAdd)
YGres.kruskalVitAdd

YGdunnVitAdd <- datat48 %>% 
  dunn_test(P_Y_G ~ VitAdd, p.adjust.method = "bonferroni") 
YGdunnVitAdd

YGdunnVitAdd <- YGdunnVitAdd %>% add_xy_position(x = "VitAdd")
YGboxplotVitAdd <- ggboxplot(datat48, x = "VitAdd", y = "P_Y_G", ylab = "Green Algal PSII Efficiency ", xlab = "Vitamin Addition") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(YGdunnVitAdd, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(YGres.kruskalVitAdd, detailed = TRUE),
    caption = get_pwc_label(YGdunnVitAdd)
  )
YGboxplotVitAdd

##Brown
datat48 %>%
  group_by(Year, NutRat, VitAdd) %>%
  get_summary_stats(P_Y_Br, type = "mean_sd")

boxplotYbrown <- ggboxplot(
  datat48, x = "VitAdd", y = "P_Y_Br", 
  color = "Year", palette = "jco", facet.by = "NutRat"
)
boxplotYbrown

###Check Check Normality
model  <- lm(P_Y_Br ~ Year*NutRat*VitAdd, data = datat48)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
#Failed assumption of normality - need non-parametric test (Kruskal-Wallis)

#For each group
#datat48 %>%
  #group_by(Year, NutRat, VitAdd) %>%
  #shapiro_test(P_Y_Br)

#ggqqplot(datat48, "P_Y_Br", ggtheme = theme_bw()) +
  #facet_grid(Year + NutRat ~ VitAdd, labeller = "label_both")

#Homogeny of varience
datat48%>% levene_test(P_Y_Br ~ Year*NutRat*VitAdd)
#Failed test of homogeny of varience. As data also failed assumption of normality, the Kruskal-Wallis test is a test of medians not means

##Run Kruskal-Wallis Year
YBrres.kruskalYear <- datat48 %>% kruskal_test(P_Y_Br ~ Year)
YBrres.kruskalYear

#Posthoc with Dunn's Test 
YBrdunnYear <- datat48 %>% 
  dunn_test(P_Y_Br ~ Year, p.adjust.method = "bonferroni") 
YBrdunnYear

YBrdunnYear <- YBrdunnYear %>% add_xy_position(x = "Year")
YBrboxplotYear <- ggboxplot(datat48, x = "Year", y = "P_Y_Br", ylab = "Brown Algal PSII Efficiency", xlab = "Year") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(YBrdunnYear, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(YBrres.kruskalYear, detailed = TRUE),
    caption = get_pwc_label(YBrdunnYear)
  )
YBrboxplotYear

YBrres.kruskalNutRat <- datat48 %>% kruskal_test(P_Y_Br ~ NutRat)
YBrres.kruskalNutRat

YBrdunnNutRat <- datat48 %>% 
  dunn_test(P_Y_Br ~ NutRat, p.adjust.method = "bonferroni") 
YBrdunnNutRat

YBrdunnNutRat <- YBrdunnNutRat %>% add_xy_position(x = "NutRat")
YBrboxplotNutRat <- ggboxplot(datat48, x = "NutRat", y = "P_Y_Br", ylab = "Brown Algal PSII Efficiency ", xlab = "Limnocorral N:P Ratio", order = c("0","2.2","16","55","110")) +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(YBrdunnNutRat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(YBrres.kruskalNutRat, detailed = TRUE),
    caption = get_pwc_label(YBrdunnNutRat)
  )
YBrboxplotNutRat

YBrres.kruskalVitAdd <- datat48 %>% kruskal_test(P_Y_Br ~ VitAdd)
YBrres.kruskalVitAdd

YBrdunnVitAdd <- datat48 %>% 
  dunn_test(P_Y_Br ~ VitAdd, p.adjust.method = "bonferroni") 
YBrdunnVitAdd

YBrdunnVitAdd <- YBrdunnVitAdd %>% add_xy_position(x = "VitAdd")
YBrboxplotVitAdd <- ggboxplot(datat48, x = "VitAdd", y = "P_Y_Br", ylab = "Brown Algal PSII Efficiency ", xlab = "Vitamin Addition") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(YBrdunnVitAdd, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(YBrres.kruskalVitAdd, detailed = TRUE),
    caption = get_pwc_label(YBrdunnVitAdd)
  )
YBrboxplotVitAdd


##Combining Plots
Yboxplotscombined <- ggarrange(YBboxplotYear, YBboxplotNutRat, YBboxplotVitAdd, YGboxplotYear, YGboxplotNutRat, YGboxplotVitAdd,YBrboxplotYear, YBrboxplotNutRat, YBrboxplotVitAdd,
                                      labels = c("A", "B", "C","D", "E", "F","G", "H", "I"),
                                      ncol = 3, nrow = 3);
Yboxplotscombined

ggsave(filename="Stat_Outputs/PSIIEfficiencyBoxPlotsCombined.tiff",Yboxplotscombined,
       width=14,height=10,units="in")

#DNP
##NO3
datat48$L_NOX <- as.numeric(datat48$L_NOX)
datat48$L_NH4 <- as.numeric(datat48$L_NH4)
datat48$L_DP <- as.numeric(datat48$L_DP)
datat48 %>%
  group_by(Year, NutRat, VitAdd) %>%
  get_summary_stats(L_NOX, type = "mean_sd")

boxplotNO3 <- ggboxplot(
  datat48, x = "VitAdd", y = "L_NOX", 
  color = "Year", palette = "jco", facet.by = "NutRat"
)
boxplotNO3

###Check Check Normality
model  <- lm(L_NOX ~ Year*NutRat*VitAdd, data = datat48)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
#Failed assumption of normality - need non-parametric test (Kruskal-Wallis)

#For each group
#datat48 %>%
  #group_by(Year, NutRat, VitAdd) %>%
  #shapiro_test(L_NOX)

#ggqqplot(datat48, "L_NOX", ggtheme = theme_bw()) +
  #facet_grid(Year + NutRat ~ VitAdd, labeller = "label_both")

#Homogeny of varience
datat48%>% levene_test(L_NOX ~ Year*NutRat*VitAdd)
#Failed test of homogeny of varience. As data also failed assumption of normality, the Kruskal-Wallis test is a test of medians not means

##Run Kruskal-Wallis Year
NO3res.kruskalYear <- datat48 %>% kruskal_test(L_NOX ~ Year)
NO3res.kruskalYear

#Posthoc with Dunn's Test 
NO3dunnYear <- datat48 %>% 
  dunn_test(L_NOX ~ Year, p.adjust.method = "bonferroni") 
NO3dunnYear

NO3dunnYear <- NO3dunnYear %>% add_xy_position(x = "Year")
NO3boxplotYear <- ggboxplot(datat48, x = "Year", y = "L_NOX", ylab = "NOx Concentration (µg/L)", xlab = "Year") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(NO3dunnYear, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(NO3res.kruskalYear, detailed = TRUE),
    caption = get_pwc_label(NO3dunnYear)
  )
NO3boxplotYear

NO3res.kruskalNutRat <- datat48 %>% kruskal_test(L_NOX ~ NutRat)
NO3res.kruskalNutRat

NO3dunnNutRat <- datat48 %>% 
  dunn_test(L_NOX ~ NutRat, p.adjust.method = "bonferroni") 
NO3dunnNutRat

NO3dunnNutRat <- NO3dunnNutRat %>% add_xy_position(x = "NutRat")
NO3boxplotNutRat <- ggboxplot(datat48, x = "NutRat", y = "L_NOX", ylab = "NOx Concentration (µg/L)", xlab = "Limnocorral N:P Ratio", order = c("0","2.2","16","55","110")) +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(NO3dunnNutRat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(NO3res.kruskalNutRat, detailed = TRUE),
    caption = get_pwc_label(NO3dunnNutRat)
  )
NO3boxplotNutRat

NO3res.kruskalVitAdd <- datat48 %>% kruskal_test(L_NOX ~ VitAdd)
NO3res.kruskalVitAdd

NO3dunnVitAdd <- datat48 %>% 
  dunn_test(L_NOX ~ VitAdd, p.adjust.method = "bonferroni") 
NO3dunnVitAdd

NO3dunnVitAdd <- NO3dunnVitAdd %>% add_xy_position(x = "VitAdd")
NO3boxplotVitAdd <- ggboxplot(datat48, x = "VitAdd", y = "L_NOX", ylab = "NOx Concentration (µg/L)", xlab = "Vitamin Addition") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(NO3dunnVitAdd, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(NO3res.kruskalVitAdd, detailed = TRUE),
    caption = get_pwc_label(NO3dunnVitAdd)
  )
NO3boxplotVitAdd

##NH4
datat48 %>%
  group_by(Year, NutRat, VitAdd) %>%
  get_summary_stats(L_NH4, type = "mean_sd")

boxplotNH4 <- ggboxplot(
  datat48, x = "VitAdd", y = "L_NH4", 
  color = "Year", palette = "jco", facet.by = "NutRat"
)
boxplotNH4

###Check Check Normality
model  <- lm(L_NH4 ~ Year*NutRat*VitAdd, data = datat48)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
#Failed assumption of normality - need non-parametric test (Kruskal-Wallis)

#For each group
#datat48 %>%
  #group_by(Year, NutRat, VitAdd) %>%
  #shapiro_test(L_NH4)

#ggqqplot(datat48, "L_NH4", ggtheme = theme_bw()) +
  #facet_grid(Year + NutRat ~ VitAdd, labeller = "label_both")

#Homogeny of varience
datat48%>% levene_test(L_NH4 ~ Year*NutRat*VitAdd)
#Failed test of homogeny of varience. As data also failed assumption of normality, the Kruskal-Wallis test is a test of medians not means

##Run Kruskal-Wallis Year
NH4res.kruskalYear <- datat48 %>% kruskal_test(L_NH4 ~ Year)
NH4res.kruskalYear

#Posthoc with Dunn's Test 
NH4dunnYear <- datat48 %>% 
  dunn_test(L_NH4 ~ Year, p.adjust.method = "bonferroni") 
NH4dunnYear

NH4dunnYear <- NH4dunnYear %>% add_xy_position(x = "Year")
NH4boxplotYear <- ggboxplot(datat48, x = "Year", y = "L_NH4", ylab = "NH4 Concentration (µg/L)", xlab = "Year") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(NH4dunnYear, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(NH4res.kruskalYear, detailed = TRUE),
    caption = get_pwc_label(NH4dunnYear)
  )
NH4boxplotYear

NH4res.kruskalNutRat <- datat48 %>% kruskal_test(L_NH4 ~ NutRat)
NH4res.kruskalNutRat

NH4dunnNutRat <- datat48 %>% 
  dunn_test(L_NH4 ~ NutRat, p.adjust.method = "bonferroni") 
NH4dunnNutRat

NH4dunnNutRat <- NH4dunnNutRat %>% add_xy_position(x = "NutRat")
NH4boxplotNutRat <- ggboxplot(datat48, x = "NutRat", y = "L_NH4", ylab = "NH4 Concentration (µg/L)", xlab = "Limnocorral N:P Ratio", order = c("0","2.2","16","55","110")) +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(NH4dunnNutRat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(NH4res.kruskalNutRat, detailed = TRUE),
    caption = get_pwc_label(NH4dunnNutRat)
  )
NH4boxplotNutRat

NH4res.kruskalVitAdd <- datat48 %>% kruskal_test(L_NH4 ~ VitAdd)
NH4res.kruskalVitAdd

NH4dunnVitAdd <- datat48 %>% 
  dunn_test(L_NH4 ~ VitAdd, p.adjust.method = "bonferroni") 
NH4dunnVitAdd

NH4dunnVitAdd <- NH4dunnVitAdd %>% add_xy_position(x = "VitAdd")
NH4boxplotVitAdd <- ggboxplot(datat48, x = "VitAdd", y = "L_NH4", ylab = "NH4 Concentration (µg/L)", xlab = "Vitamin Addition") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(NH4dunnVitAdd, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(NH4res.kruskalVitAdd, detailed = TRUE),
    caption = get_pwc_label(NH4dunnVitAdd)
  )
NH4boxplotVitAdd

##SRP
datat48 %>%
  group_by(Year, NutRat, VitAdd) %>%
  get_summary_stats(L_DP, type = "mean_sd")

boxplotDP <- ggboxplot(
  datat48, x = "VitAdd", y = "L_DP", 
  color = "Year", palette = "jco", facet.by = "NutRat"
)
boxplotDP

###Check Check Normality
model  <- lm(L_DP ~ Year*NutRat*VitAdd, data = datat48)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
#Failed assumption of normality - need non-parametric test (Kruskal-Wallis)

#For each group
#datat48 %>%
  #group_by(Year, NutRat, VitAdd) %>%
  #shapiro_test(L_DP)

#ggqqplot(datat48, "L_DP", ggtheme = theme_bw()) +
  #facet_grid(Year + NutRat ~ VitAdd, labeller = "label_both")

#Homogeny of varience
datat48%>% levene_test(L_DP ~ Year*NutRat*VitAdd)
#Failed test of homogeny of varience. As data also failed assumption of normality, the Kruskal-Wallis test is a test of medians not means

##Run Kruskal-Wallis Year
DPres.kruskalYear <- datat48 %>% kruskal_test(L_DP ~ Year)
DPres.kruskalYear

#Posthoc with Dunn's Test 
DPdunnYear <- datat48 %>% 
  dunn_test(L_DP ~ Year, p.adjust.method = "bonferroni") 
DPdunnYear

DPdunnYear <- DPdunnYear %>% add_xy_position(x = "Year")
DPboxplotYear <- ggboxplot(datat48, x = "Year", y = "L_DP", ylab = "SRP Concentration (µg/L)", xlab = "Year") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(DPdunnYear, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(DPres.kruskalYear, detailed = TRUE),
    caption = get_pwc_label(DPdunnYear)
  )
DPboxplotYear

DPres.kruskalNutRat <- datat48 %>% kruskal_test(L_DP ~ NutRat)
DPres.kruskalNutRat

DPdunnNutRat <- datat48 %>% 
  dunn_test(L_DP ~ NutRat, p.adjust.method = "bonferroni") 
DPdunnNutRat

DPdunnNutRat <- DPdunnNutRat %>% add_xy_position(x = "NutRat")
DPboxplotNutRat <- ggboxplot(datat48, x = "NutRat", y = "L_DP", ylab = "SRP Concentration (µg/L) ", xlab = "Limnocorral N:P Ratio", order = c("0","2.2","16","55","110")) +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(DPdunnNutRat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(DPres.kruskalNutRat, detailed = TRUE),
    caption = get_pwc_label(DPdunnNutRat)
  )
DPboxplotNutRat

DPres.kruskalVitAdd <- datat48 %>% kruskal_test(L_DP ~ VitAdd)
DPres.kruskalVitAdd

DPdunnVitAdd <- datat48 %>% 
  dunn_test(L_DP ~ VitAdd, p.adjust.method = "bonferroni") 
DPdunnVitAdd

DPdunnVitAdd <- DPdunnVitAdd %>% add_xy_position(x = "VitAdd")
DPboxplotVitAdd <- ggboxplot(datat48, x = "VitAdd", y = "L_DP", ylab = "SRP Concentration (µg/L) ", xlab = "Vitamin Addition") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(DPdunnVitAdd, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(DPres.kruskalVitAdd, detailed = TRUE),
    caption = get_pwc_label(DPdunnVitAdd)
  )
DPboxplotVitAdd


##Combining Plots
DNPboxplotscombined <- ggarrange(NO3boxplotYear, NO3boxplotNutRat, NO3boxplotVitAdd, NH4boxplotYear, NH4boxplotNutRat, NH4boxplotVitAdd,DPboxplotYear, DPboxplotNutRat, DPboxplotVitAdd,
                               labels = c("A", "B", "C","D", "E", "F","G", "H", "I"),
                               ncol = 3, nrow = 3);
DNPboxplotscombined

ggsave(filename="Stat_Outputs/DNPBoxPlotsCombined.tiff",DNPboxplotscombined,
       width=14,height=10,units="in")

#Phytoplankton community composition
##Cyanos
datat48 %>%
  group_by(Year, NutRat, VitAdd) %>%
  get_summary_stats(Prop_Chl_B, type = "mean_sd")

boxplotPCcyano <- ggboxplot(
  datat48, x = "VitAdd", y = "Prop_Chl_B", 
  color = "Year", palette = "jco", facet.by = "NutRat"
)
boxplotPCcyano

###Check Check Normality
model  <- lm(Prop_Chl_B ~ Year*NutRat*VitAdd, data = datat48)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
#Failed assumption of normality - need non-parametric test (Kruskal-Wallis)

#For each group
#datat48 %>%
  #group_by(Year, NutRat, VitAdd) %>%
  #shapiro_test(Prop_Chl_B)

#ggqqplot(datat48, "Prop_Chl_B", ggtheme = theme_bw()) +
  #facet_grid(Year + NutRat ~ VitAdd, labeller = "label_both")

#Homogeny of varience
datat48%>% levene_test(Prop_Chl_B ~ Year*NutRat*VitAdd)
#Failed test of homogeny of varience. As data also failed assumption of normality, the Kruskal-Wallis test is a test of medians not means

##Run Kruskal-Wallis Year
ChlBres.kruskalYear <- datat48 %>% kruskal_test(Prop_Chl_B ~ Year)
ChlBres.kruskalYear

#Posthoc with Dunn's Test 
ChlBdunnYear <- datat48 %>% 
  dunn_test(Prop_Chl_B ~ Year, p.adjust.method = "bonferroni") 
ChlBdunnYear

ChlBdunnYear <- ChlBdunnYear %>% add_xy_position(x = "Year")
ChlBboxplotYear <- ggboxplot(datat48, x = "Year", y = "Prop_Chl_B", ylab = "Cyanobacterial Community Fraction", xlab = "Year") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(ChlBdunnYear, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(ChlBres.kruskalYear, detailed = TRUE),
    caption = get_pwc_label(ChlBdunnYear)
  )
ChlBboxplotYear

ChlBres.kruskalNutRat <- datat48 %>% kruskal_test(Prop_Chl_B ~ NutRat)
ChlBres.kruskalNutRat

ChlBdunnNutRat <- datat48 %>% 
  dunn_test(Prop_Chl_B ~ NutRat, p.adjust.method = "bonferroni") 
ChlBdunnNutRat

ChlBdunnNutRat <- ChlBdunnNutRat %>% add_xy_position(x = "NutRat")
ChlBboxplotNutRat <- ggboxplot(datat48, x = "NutRat", y = "Prop_Chl_B", ylab = "Cyanobacterial Community Fraction", xlab = "Limnocorral N:P Ratio", order = c("0","2.2","16","55","110")) +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(ChlBdunnNutRat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(ChlBres.kruskalNutRat, detailed = TRUE),
    caption = get_pwc_label(ChlBdunnNutRat)
  )
ChlBboxplotNutRat

ChlBres.kruskalVitAdd <- datat48 %>% kruskal_test(Prop_Chl_B ~ VitAdd)
ChlBres.kruskalVitAdd

ChlBdunnVitAdd <- datat48 %>% 
  dunn_test(Prop_Chl_B ~ VitAdd, p.adjust.method = "bonferroni") 
ChlBdunnVitAdd

ChlBdunnVitAdd <- ChlBdunnVitAdd %>% add_xy_position(x = "VitAdd")
ChlBboxplotVitAdd <- ggboxplot(datat48, x = "VitAdd", y = "Prop_Chl_B", ylab = "Cyanobacterial Community Fraction", xlab = "Vitamin Addition") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(ChlBdunnVitAdd, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(ChlBres.kruskalVitAdd, detailed = TRUE),
    caption = get_pwc_label(ChlBdunnVitAdd)
  )
ChlBboxplotVitAdd

##Chlorophytes
datat48 %>%
  group_by(Year, NutRat, VitAdd) %>%
  get_summary_stats(Prop_Chl_G, type = "mean_sd")

boxplotPCgreen <- ggboxplot(
  datat48, x = "VitAdd", y = "Prop_Chl_G", 
  color = "Year", palette = "jco", facet.by = "NutRat"
)
boxplotPCgreen

###Check Check Normality
model  <- lm(Prop_Chl_G ~ Year*NutRat*VitAdd, data = datat48)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
#Failed assumption of normality - need non-parametric test (Kruskal-Wallis)

#For each group
#datat48 %>%
  #group_by(Year, NutRat, VitAdd) %>%
  #shapiro_test(Prop_Chl_G)

#ggqqplot(datat48, "Prop_Chl_G", ggtheme = theme_bw()) +
  #facet_grid(Year + NutRat ~ VitAdd, labeller = "label_both")

#Homogeny of varience
datat48%>% levene_test(Prop_Chl_G ~ Year*NutRat*VitAdd)
#Failed test of homogeny of varience. As data also failed assumption of normality, the Kruskal-Wallis test is a test of medians not means

##Run Kruskal-Wallis Year
ChlGres.kruskalYear <- datat48 %>% kruskal_test(Prop_Chl_G ~ Year)
ChlGres.kruskalYear

#Posthoc with Dunn's Test 
ChlGdunnYear <- datat48 %>% 
  dunn_test(Prop_Chl_G ~ Year, p.adjust.method = "bonferroni") 
ChlGdunnYear

ChlGdunnYear <- ChlGdunnYear %>% add_xy_position(x = "Year")
ChlGboxplotYear <- ggboxplot(datat48, x = "Year", y = "Prop_Chl_G", ylab = "Green Algal Community Fraction", xlab = "Year") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(ChlGdunnYear, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(ChlGres.kruskalYear, detailed = TRUE),
    caption = get_pwc_label(ChlGdunnYear)
  )
ChlGboxplotYear

ChlGres.kruskalNutRat <- datat48 %>% kruskal_test(Prop_Chl_G ~ NutRat)
ChlGres.kruskalNutRat

ChlGdunnNutRat <- datat48 %>% 
  dunn_test(Prop_Chl_G ~ NutRat, p.adjust.method = "bonferroni") 
ChlGdunnNutRat

ChlGdunnNutRat <- ChlGdunnNutRat %>% add_xy_position(x = "NutRat")
ChlGboxplotNutRat <- ggboxplot(datat48, x = "NutRat", y = "Prop_Chl_G", ylab = "Green Algal Community Fraction", xlab = "Limnocorral N:P Ratio", order = c("0","2.2","16","55","110")) +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(ChlGdunnNutRat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(ChlGres.kruskalNutRat, detailed = TRUE),
    caption = get_pwc_label(ChlGdunnNutRat)
  )
ChlGboxplotNutRat

ChlGres.kruskalVitAdd <- datat48 %>% kruskal_test(Prop_Chl_G ~ VitAdd)
ChlGres.kruskalVitAdd

ChlGdunnVitAdd <- datat48 %>% 
  dunn_test(Prop_Chl_G ~ VitAdd, p.adjust.method = "bonferroni") 
ChlGdunnVitAdd

ChlGdunnVitAdd <- ChlGdunnVitAdd %>% add_xy_position(x = "VitAdd")
ChlGboxplotVitAdd <- ggboxplot(datat48, x = "VitAdd", y = "Prop_Chl_G", ylab = "Green Algal Community Fraction", xlab = "Vitamin Addition") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(ChlGdunnVitAdd, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(ChlGres.kruskalVitAdd, detailed = TRUE),
    caption = get_pwc_label(ChlGdunnVitAdd)
  )
ChlGboxplotVitAdd

##Brown
datat48 %>%
  group_by(Year, NutRat, VitAdd) %>%
  get_summary_stats(Prop_Chl_Br, type = "mean_sd")

boxplotPCbrown <- ggboxplot(
  datat48, x = "VitAdd", y = "Prop_Chl_Br", 
  color = "Year", palette = "jco", facet.by = "NutRat"
)
boxplotPCbrown

###Check Check Normality
model  <- lm(Prop_Chl_Br ~ Year*NutRat*VitAdd, data = datat48)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
#Failed assumption of normality - need non-parametric test (Kruskal-Wallis)

#For each group
#datat48 %>%
  #group_by(Year, NutRat, VitAdd) %>%
  #shapiro_test(Prop_Chl_Br)

#ggqqplot(datat48, "Prop_Chl_Br", ggtheme = theme_bw()) +
  #facet_grid(Year + NutRat ~ VitAdd, labeller = "label_both")

#Homogeny of varience
datat48%>% levene_test(Prop_Chl_Br ~ Year*NutRat*VitAdd)
#Failed test of homogeny of varience. As data also failed assumption of normality, the Kruskal-Wallis test is a test of medians not means

##Run Kruskal-Wallis Year
ChlBrres.kruskalYear <- datat48 %>% kruskal_test(Prop_Chl_Br ~ Year)
ChlBrres.kruskalYear

#Posthoc with Dunn's Test 
ChlBrdunnYear <- datat48 %>% 
  dunn_test(Prop_Chl_Br ~ Year, p.adjust.method = "bonferroni") 
ChlBrdunnYear

ChlBrdunnYear <- ChlBrdunnYear %>% add_xy_position(x = "Year")
ChlBrboxplotYear <- ggboxplot(datat48, x = "Year", y = "Prop_Chl_Br", ylab = "Brown Algal Community Fraction", xlab = "Year") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(ChlBrdunnYear, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(ChlBrres.kruskalYear, detailed = TRUE),
    caption = get_pwc_label(ChlBrdunnYear)
  )
ChlBrboxplotYear

ChlBrres.kruskalNutRat <- datat48 %>% kruskal_test(Prop_Chl_Br ~ NutRat)
ChlBrres.kruskalNutRat

ChlBrdunnNutRat <- datat48 %>% 
  dunn_test(Prop_Chl_Br ~ NutRat, p.adjust.method = "bonferroni") 
ChlBrdunnNutRat

ChlBrdunnNutRat <- ChlBrdunnNutRat %>% add_xy_position(x = "NutRat")
ChlBrboxplotNutRat <- ggboxplot(datat48, x = "NutRat", y = "Prop_Chl_Br", ylab = "Brown Algal Community Fraction", xlab = "Limnocorral N:P Ratio", order = c("0","2.2","16","55","110")) +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(ChlBrdunnNutRat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(ChlBrres.kruskalNutRat, detailed = TRUE),
    caption = get_pwc_label(ChlBrdunnNutRat)
  )
ChlBrboxplotNutRat

ChlBrres.kruskalVitAdd <- datat48 %>% kruskal_test(Prop_Chl_Br ~ VitAdd)
ChlBrres.kruskalVitAdd

ChlBrdunnVitAdd <- datat48 %>% 
  dunn_test(Prop_Chl_Br ~ VitAdd, p.adjust.method = "bonferroni") 
ChlBrdunnVitAdd

ChlBrdunnVitAdd <- ChlBrdunnVitAdd %>% add_xy_position(x = "VitAdd")
ChlBrboxplotVitAdd <- ggboxplot(datat48, x = "VitAdd", y = "Prop_Chl_Br", ylab = "Brown Algal Community Fraction", xlab = "Vitamin Addition") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(ChlBrdunnVitAdd, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(ChlBrres.kruskalVitAdd, detailed = TRUE),
    caption = get_pwc_label(ChlBrdunnVitAdd)
  )
ChlBrboxplotVitAdd


##Combining Plots
PCboxplotscombined <- ggarrange(ChlBboxplotYear, ChlBboxplotNutRat, ChlBboxplotVitAdd, ChlGboxplotYear, ChlGboxplotNutRat, ChlGboxplotVitAdd,ChlBrboxplotYear, ChlBrboxplotNutRat, ChlBrboxplotVitAdd,
                               labels = c("A", "B", "C","D", "E", "F","G", "H", "I"),
                               ncol = 3, nrow = 3);
PCboxplotscombined

ggsave(filename="Stat_Outputs/PhytoCommunityBoxPlotsCombined.tiff",PCboxplotscombined,
       width=13.5,height=10.5,units="in")

#CNP
##CN

datat48 %>%
  group_by(Year, NutRat, VitAdd) %>%
  get_summary_stats(CNRatio, type = "mean_sd")

boxplotCN <- ggboxplot(
  datat48, x = "VitAdd", y = "CNRatio", 
  color = "Year", palette = "jco", facet.by = "NutRat"
)
boxplotCN

###Check Check Normality
model  <- lm(CNRatio ~ Year*NutRat*VitAdd, data = datat48)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
#Failed assumption of normality - need non-parametric test (Kruskal-Wallis)

#For each group
#datat48 %>%
#group_by(Year, NutRat, VitAdd) %>%
#shapiro_test(CNRatio)

#ggqqplot(datat48, "CNRatio", ggtheme = theme_bw()) +
#facet_grid(Year + NutRat ~ VitAdd, labeller = "label_both")

#Homogeny of varience
datat48%>% levene_test(CNRatio ~ Year*NutRat*VitAdd)
#Failed test of homogeny of varience. As data also failed assumption of normality, the Kruskal-Wallis test is a test of medians not means

##Run Kruskal-Wallis Year
CN1res.kruskalYear <- datat48 %>% kruskal_test(CNRatio ~ Year)
CN1res.kruskalYear

#Posthoc with Dunn's Test 
CNdunnYear <- datat48 %>% 
  dunn_test(CNRatio ~ Year, p.adjust.method = "bonferroni") 
CNdunnYear

CNdunnYear <- CNdunnYear %>% add_xy_position(x = "Year")
CNboxplotYear <- ggboxplot(datat48, x = "Year", y = "CNRatio", ylab = "C:N Ratio", xlab = "Year") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(CNdunnYear, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(CN1res.kruskalYear, detailed = TRUE),
    caption = get_pwc_label(CNdunnYear)
  )
CNboxplotYear

CNres.kruskalNutRat <- datat48 %>% kruskal_test(CNRatio ~ NutRat)
CNres.kruskalNutRat

CNdunnNutRat <- datat48 %>% 
  dunn_test(CNRatio ~ NutRat, p.adjust.method = "bonferroni") 
CNdunnNutRat

CNdunnNutRat <- CNdunnNutRat %>% add_xy_position(x = "NutRat")
CNboxplotNutRat <- ggboxplot(datat48, x = "NutRat", y = "CNRatio", ylab = "C:N Ratio", xlab = "Limnocorral N:P Ratio", order = c("0","2.2","16","55","110")) +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(CNdunnNutRat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(CNres.kruskalNutRat, detailed = TRUE),
    caption = get_pwc_label(CNdunnNutRat)
  )
CNboxplotNutRat

CN1res.kruskalVitAdd <- datat48 %>% kruskal_test(CNRatio ~ VitAdd)
CN1res.kruskalVitAdd

CNdunnVitAdd <- datat48 %>% 
  dunn_test(CNRatio ~ VitAdd, p.adjust.method = "bonferroni") 
CNdunnVitAdd

CNdunnVitAdd <- CNdunnVitAdd %>% add_xy_position(x = "VitAdd")
CNboxplotVitAdd <- ggboxplot(datat48, x = "VitAdd", y = "CNRatio", ylab = "C:N Ratio", xlab = "Vitamin Addition") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(CNdunnVitAdd, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(CN1res.kruskalVitAdd, detailed = TRUE),
    caption = get_pwc_label(CNdunnVitAdd)
  )
CNboxplotVitAdd

##CP
datat48 %>%
  group_by(Year, NutRat, VitAdd) %>%
  get_summary_stats(CPRatio, type = "mean_sd")

boxplotCP <- ggboxplot(
  datat48, x = "VitAdd", y = "CPRatio", 
  color = "Year", palette = "jco", facet.by = "NutRat"
)
boxplotCP

###Check Check Normality
model  <- lm(CPRatio ~ Year*NutRat*VitAdd, data = datat48)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
#Failed assumption of normality - need non-parametric test (Kruskal-Wallis)

#For each group
#datat48 %>%
#group_by(Year, NutRat, VitAdd) %>%
#shapiro_test(CPRatio)

#ggqqplot(datat48, "CPRatio", ggtheme = theme_bw()) +
#facet_grid(Year + NutRat ~ VitAdd, labeller = "label_both")

#Homogeny of varience
datat48%>% levene_test(CPRatio ~ Year*NutRat*VitAdd)
#Failed test of homogeny of varience. As data also failed assumption of normality, the Kruskal-Wallis test is a test of medians not means

##Run Kruskal-Wallis Year
CPres.kruskalYear <- datat48 %>% kruskal_test(CPRatio ~ Year)
CPres.kruskalYear

#Posthoc with Dunn's Test 
CPdunnYear <- datat48 %>% 
  dunn_test(CPRatio ~ Year, p.adjust.method = "bonferroni") 
CPdunnYear

CPdunnYear <- CPdunnYear %>% add_xy_position(x = "Year")
CPboxplotYear <- ggboxplot(datat48, x = "Year", y = "CPRatio", ylab = "C:P Ratio", xlab = "Year") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(CPdunnYear, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(CPres.kruskalYear, detailed = TRUE),
    caption = get_pwc_label(CPdunnYear)
  )
CPboxplotYear

CPres.kruskalNutRat <- datat48 %>% kruskal_test(CPRatio ~ NutRat)
CPres.kruskalNutRat

CPdunnNutRat <- datat48 %>% 
  dunn_test(CPRatio ~ NutRat, p.adjust.method = "bonferroni") 
CPdunnNutRat

CPdunnNutRat <- CPdunnNutRat %>% add_xy_position(x = "NutRat")
CPboxplotNutRat <- ggboxplot(datat48, x = "NutRat", y = "CPRatio", ylab = "C:P Ratio", xlab = "Limnocorral N:P Ratio", order = c("0","2.2","16","55","110")) +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(CPdunnNutRat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(CPres.kruskalNutRat, detailed = TRUE),
    caption = get_pwc_label(CPdunnNutRat)
  )
CPboxplotNutRat

CPres.kruskalVitAdd <- datat48 %>% kruskal_test(CPRatio ~ VitAdd)
CPres.kruskalVitAdd

CPdunnVitAdd <- datat48 %>% 
  dunn_test(CPRatio ~ VitAdd, p.adjust.method = "bonferroni") 
CPdunnVitAdd

CPdunnVitAdd <- CPdunnVitAdd %>% add_xy_position(x = "VitAdd")
CPboxplotVitAdd <- ggboxplot(datat48, x = "VitAdd", y = "CPRatio", ylab = "C:P Ratio", xlab = "Vitamin Addition") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(CPdunnVitAdd, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(CPres.kruskalVitAdd, detailed = TRUE),
    caption = get_pwc_label(CPdunnVitAdd)
  )
CPboxplotVitAdd

##NP
datat48 %>%
  group_by(Year, NutRat, VitAdd) %>%
  get_summary_stats(NPRatio, type = "mean_sd")

boxplotNP <- ggboxplot(
  datat48, x = "VitAdd", y = "NPRatio", 
  color = "Year", palette = "jco", facet.by = "NutRat"
)
boxplotNP

###Check Check Normality
model  <- lm(NPRatio ~ Year*NutRat*VitAdd, data = datat48)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
#Failed assumption of normality - need non-parametric test (Kruskal-Wallis)

#For each group
#datat48 %>%
#group_by(Year, NutRat, VitAdd) %>%
#shapiro_test(NPRatio)

#ggqqplot(datat48, "NPRatio", ggtheme = theme_bw()) +
#facet_grid(Year + NutRat ~ VitAdd, labeller = "label_both")

#Homogeny of varience
datat48%>% levene_test(NPRatio ~ Year*NutRat*VitAdd)
#Failed test of homogeny of varience. As data also failed assumption of normality, the Kruskal-Wallis test is a test of medians not means

##Run Kruskal-Wallis Year
NPres.kruskalYear <- datat48 %>% kruskal_test(NPRatio ~ Year)
NPres.kruskalYear

#Posthoc with Dunn's Test 
NPdunnYear <- datat48 %>% 
  dunn_test(NPRatio ~ Year, p.adjust.method = "bonferroni") 
NPdunnYear

NPdunnYear <- NPdunnYear %>% add_xy_position(x = "Year")
NPboxplotYear <- ggboxplot(datat48, x = "Year", y = "NPRatio", ylab = "N:P Ratio", xlab = "Year") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(NPdunnYear, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(NPres.kruskalYear, detailed = TRUE),
    caption = get_pwc_label(NPdunnYear)
  )
NPboxplotYear

NPres.kruskalNutRat <- datat48 %>% kruskal_test(NPRatio ~ NutRat)
NPres.kruskalNutRat

NPdunnNutRat <- datat48 %>% 
  dunn_test(NPRatio ~ NutRat, p.adjust.method = "bonferroni") 
NPdunnNutRat

NPdunnNutRat <- NPdunnNutRat %>% add_xy_position(x = "NutRat")
NPboxplotNutRat <- ggboxplot(datat48, x = "NutRat", y = "NPRatio", ylab = "N:P Ratio", xlab = "Limnocorral N:P Ratio", order = c("0","2.2","16","55","110")) +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(NPdunnNutRat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(NPres.kruskalNutRat, detailed = TRUE),
    caption = get_pwc_label(NPdunnNutRat)
  )
NPboxplotNutRat

NPres.kruskalVitAdd <- datat48 %>% kruskal_test(NPRatio ~ VitAdd)
NPres.kruskalVitAdd

NPdunnVitAdd <- datat48 %>% 
  dunn_test(NPRatio ~ VitAdd, p.adjust.method = "bonferroni") 
NPdunnVitAdd

NPdunnVitAdd <- NPdunnVitAdd %>% add_xy_position(x = "VitAdd")
NPboxplotVitAdd <- ggboxplot(datat48, x = "VitAdd", y = "NPRatio", ylab = "N:P Ratio", xlab = "Vitamin Addition") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(NPdunnVitAdd, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(NPres.kruskalVitAdd, detailed = TRUE),
    caption = get_pwc_label(NPdunnVitAdd)
  )
NPboxplotVitAdd


##Combining Plots
CNPboxplotscombined <- ggarrange(CNboxplotYear, CNboxplotNutRat, CNboxplotVitAdd, CPboxplotYear, CPboxplotNutRat, CPboxplotVitAdd,NPboxplotYear, NPboxplotNutRat, NPboxplotVitAdd,
                                 labels = c("A", "B", "C","D", "E", "F","G", "H", "I"),
                                 ncol = 3, nrow = 3);
CNPboxplotscombined

ggsave(filename="Stat_Outputs/CNPBoxPlotsCombined.tiff",CNPboxplotscombined,
       width=14,height=10,units="in")

#CNP Unfiltered
##Keeping in the 2023 Pond 15 PP data
##CN

datat48 %>%
  group_by(Year, NutRat, VitAdd) %>%
  get_summary_stats(CNRatio, type = "mean_sd")

boxplotCN1 <- ggboxplot(
  datat48, x = "VitAdd", y = "CNRatio", 
  color = "Year", palette = "jco", facet.by = "NutRat"
)
boxplotCN1

###Check Check Normality
model  <- lm(CNRatio ~ Year*NutRat*VitAdd, data = datat48)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
#Failed assumption of normality - need non-parametric test (Kruskal-Wallis)

#For each group
#datat48 %>%
#group_by(Year, NutRat, VitAdd) %>%
#shapiro_test(CNRatio)

#ggqqplot(datat48, "CNRatio", ggtheme = theme_bw()) +
#facet_grid(Year + NutRat ~ VitAdd, labeller = "label_both")

#Homogeny of varience
datat48%>% levene_test(CNRatio ~ Year*NutRat*VitAdd)
#Failed test of homogeny of varience. As data also failed assumption of normality, the Kruskal-Wallis test is a test of medians not means

##Run Kruskal-Wallis Year
CN1res.kruskalYear <- datat48 %>% kruskal_test(CNRatio ~ Year)
CN1res.kruskalYear

#Posthoc with Dunn's Test 
CN1dunnYear <- datat48 %>% 
  dunn_test(CNRatio ~ Year, p.adjust.method = "bonferroni") 
CN1dunnYear

CN1dunnYear <- CN1dunnYear %>% add_xy_position(x = "Year")
CN1boxplotYear <- ggboxplot(datat48, x = "Year", y = "CNRatio", ylab = "C:N Ratio", xlab = "Year") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(CNdunnYear, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(CN1res.kruskalYear, detailed = TRUE),
    caption = get_pwc_label(CN1dunnYear)
  )
CN1boxplotYear

CN1res.kruskalNutRat <- datat48 %>% kruskal_test(CNRatio ~ NutRat)
CN1res.kruskalNutRat

CN1dunnNutRat <- datat48 %>% 
  dunn_test(CNRatio ~ NutRat, p.adjust.method = "bonferroni") 
CN1dunnNutRat

CN1dunnNutRat <- CN1dunnNutRat %>% add_xy_position(x = "NutRat")
CN1boxplotNutRat <- ggboxplot(datat48, x = "NutRat", y = "CNRatio", ylab = "C:N Ratio", xlab = "Limnocorral N:P Ratio", order = c("0","2.2","16","55","110")) +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(CNdunnNutRat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(CN1res.kruskalNutRat, detailed = TRUE),
    caption = get_pwc_label(CN1dunnNutRat)
  )
CN1boxplotNutRat

CN1res.kruskalVitAdd <- datat48 %>% kruskal_test(CNRatio ~ VitAdd)
CN1res.kruskalVitAdd

CN1dunnVitAdd <- datat48 %>% 
  dunn_test(CNRatio ~ VitAdd, p.adjust.method = "bonferroni") 
CN1dunnVitAdd

CN1dunnVitAdd <- CN1dunnVitAdd %>% add_xy_position(x = "VitAdd")
CN1boxplotVitAdd <- ggboxplot(datat48, x = "VitAdd", y = "CNRatio", ylab = "C:N Ratio", xlab = "Vitamin Addition") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(CNdunnVitAdd, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(CN1res.kruskalVitAdd, detailed = TRUE),
    caption = get_pwc_label(CN1dunnVitAdd)
  )
CN1boxplotVitAdd

##CP
datat48 %>%
  group_by(Year, NutRat, VitAdd) %>%
  get_summary_stats(CPRatio1, type = "mean_sd")

boxplotCP1 <- ggboxplot(
  datat48, x = "VitAdd", y = "CPRatio1", 
  color = "Year", palette = "jco", facet.by = "NutRat"
)
boxplotCP1

###Check Check Normality
model  <- lm(CPRatio1 ~ Year*NutRat*VitAdd, data = datat48)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
#Failed assumption of normality - need non-parametric test (Kruskal-Wallis)

#For each group
#datat48 %>%
#group_by(Year, NutRat, VitAdd) %>%
#shapiro_test(CPRatio1)

#ggqqplot(datat48, "CPRatio1", ggtheme = theme_bw()) +
#facet_grid(Year + NutRat ~ VitAdd, labeller = "label_both")

#Homogeny of varience
datat48%>% levene_test(CPRatio1 ~ Year*NutRat*VitAdd)
#Failed test of homogeny of varience. As data also failed assumption of normality, the Kruskal-Wallis test is a test of medians not means

##Run Kruskal-Wallis Year
CP1res.kruskalYear <- datat48 %>% kruskal_test(CPRatio1 ~ Year)
CP1res.kruskalYear

#Posthoc with Dunn's Test 
CP1dunnYear <- datat48 %>% 
  dunn_test(CPRatio1 ~ Year, p.adjust.method = "bonferroni") 
CP1dunnYear

CP1dunnYear <- CP1dunnYear %>% add_xy_position(x = "Year")
CP1boxplotYear <- ggboxplot(datat48, x = "Year", y = "CPRatio1", ylab = "C:P Ratio", xlab = "Year") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(CP1dunnYear, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(CP1res.kruskalYear, detailed = TRUE),
    caption = get_pwc_label(CP1dunnYear)
  )
CP1boxplotYear

CP1res.kruskalNutRat <- datat48 %>% kruskal_test(CPRatio1 ~ NutRat)
CP1res.kruskalNutRat

CP1dunnNutRat <- datat48 %>% 
  dunn_test(CPRatio1 ~ NutRat, p.adjust.method = "bonferroni") 
CP1dunnNutRat

CP1dunnNutRat <- CP1dunnNutRat %>% add_xy_position(x = "NutRat")
CP1boxplotNutRat <- ggboxplot(datat48, x = "NutRat", y = "CPRatio1", ylab = "C:P Ratio", xlab = "Limnocorral N:P Ratio", order = c("0","2.2","16","55","110")) +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(CP1dunnNutRat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(CP1res.kruskalNutRat, detailed = TRUE),
    caption = get_pwc_label(CP1dunnNutRat)
  )
CP1boxplotNutRat

CP1res.kruskalVitAdd <- datat48 %>% kruskal_test(CPRatio1 ~ VitAdd)
CP1res.kruskalVitAdd

CP1dunnVitAdd <- datat48 %>% 
  dunn_test(CPRatio1 ~ VitAdd, p.adjust.method = "bonferroni") 
CP1dunnVitAdd

CP1dunnVitAdd <- CP1dunnVitAdd %>% add_xy_position(x = "VitAdd")
CP1boxplotVitAdd <- ggboxplot(datat48, x = "VitAdd", y = "CPRatio1", ylab = "C:P Ratio", xlab = "Vitamin Addition") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(CP1dunnVitAdd, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(CP1res.kruskalVitAdd, detailed = TRUE),
    caption = get_pwc_label(CP1dunnVitAdd)
  )
CP1boxplotVitAdd

##NP
datat48 %>%
  group_by(Year, NutRat, VitAdd) %>%
  get_summary_stats(NPRatio1, type = "mean_sd")

boxplotNP1 <- ggboxplot(
  datat48, x = "VitAdd", y = "NPRatio1", 
  color = "Year", palette = "jco", facet.by = "NutRat"
)
boxplotNP1

###Check Check Normality
model  <- lm(NPRatio1 ~ Year*NutRat*VitAdd, data = datat48)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
#Failed assumption of normality - need non-parametric test (Kruskal-Wallis)

#For each group
#datat48 %>%
#group_by(Year, NutRat, VitAdd) %>%
#shapiro_test(NPRatio1)

#ggqqplot(datat48, "NPRatio1", ggtheme = theme_bw()) +
#facet_grid(Year + NutRat ~ VitAdd, labeller = "label_both")

#Homogeny of varience
datat48%>% levene_test(NPRatio1 ~ Year*NutRat*VitAdd)
#Failed test of homogeny of varience. As data also failed assumption of normality, the Kruskal-Wallis test is a test of medians not means

##Run Kruskal-Wallis Year
NP1res.kruskalYear <- datat48 %>% kruskal_test(NPRatio1 ~ Year)
NP1res.kruskalYear

#Posthoc with Dunn's Test 
NP1dunnYear <- datat48 %>% 
  dunn_test(NPRatio1 ~ Year, p.adjust.method = "bonferroni") 
NP1dunnYear

NP1dunnYear <- NP1dunnYear %>% add_xy_position(x = "Year")
NP1boxplotYear <- ggboxplot(datat48, x = "Year", y = "NPRatio1", ylab = "N:P Ratio", xlab = "Year") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(NP1dunnYear, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(NP1res.kruskalYear, detailed = TRUE),
    caption = get_pwc_label(NP1dunnYear)
  )
NP1boxplotYear

NP1res.kruskalNutRat <- datat48 %>% kruskal_test(NPRatio1 ~ NutRat)
NP1res.kruskalNutRat

NP1dunnNutRat <- datat48 %>% 
  dunn_test(NPRatio1 ~ NutRat, p.adjust.method = "bonferroni") 
NP1dunnNutRat

NP1dunnNutRat <- NP1dunnNutRat %>% add_xy_position(x = "NutRat")
NP1boxplotNutRat <- ggboxplot(datat48, x = "NutRat", y = "NPRatio1", ylab = "N:P Ratio", xlab = "Limnocorral N:P Ratio", order = c("0","2.2","16","55","110")) +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(NP1dunnNutRat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(NP1res.kruskalNutRat, detailed = TRUE),
    caption = get_pwc_label(NP1dunnNutRat)
  )
NP1boxplotNutRat

NP1res.kruskalVitAdd <- datat48 %>% kruskal_test(NPRatio1 ~ VitAdd)
NP1res.kruskalVitAdd

NP1dunnVitAdd <- datat48 %>% 
  dunn_test(NPRatio1 ~ VitAdd, p.adjust.method = "bonferroni") 
NP1dunnVitAdd

NP1dunnVitAdd <- NP1dunnVitAdd %>% add_xy_position(x = "VitAdd")
NP1boxplotVitAdd <- ggboxplot(datat48, x = "VitAdd", y = "NPRatio1", ylab = "N:P Ratio", xlab = "Vitamin Addition") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(NP1dunnVitAdd, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(NP1res.kruskalVitAdd, detailed = TRUE),
    caption = get_pwc_label(NP1dunnVitAdd)
  )
NP1boxplotVitAdd


##Combining Plots
CNP1boxplotscombined <- ggarrange(CN1boxplotYear, CN1boxplotNutRat, CN1boxplotVitAdd, CP1boxplotYear, CP1boxplotNutRat, CP1boxplotVitAdd,NP1boxplotYear, NP1boxplotNutRat, NP1boxplotVitAdd,
                                 labels = c("A", "B", "C","D", "E", "F","G", "H", "I"),
                                 ncol = 3, nrow = 3);
CNP1boxplotscombined

ggsave(filename="Stat_Outputs/CNPUnfilteredBoxPlotsCombined.tiff",CNP1boxplotscombined,
       width=14,height=10,units="in")


#TNP
##TN
datat48$L_TN <- as.numeric(datat48$L_TN)
datat48$L_TDN <- as.numeric(datat48$L_TDN)
datat48$L_TP <- as.numeric(datat48$L_TP)
datat48$L_TDP <- as.numeric(datat48$L_TDP)
datat48 %>%
  group_by(Year, NutRat, VitAdd) %>%
  get_summary_stats(L_TN, type = "mean_sd")

boxplotTN <- ggboxplot(
  datat48, x = "VitAdd", y = "L_TN", 
  color = "Year", palette = "jco", facet.by = "NutRat"
)
boxplotTN

###Check Check Normality
model  <- lm(L_TN ~ Year*NutRat*VitAdd, data = datat48)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
#Failed assumption of normality - need non-parametric test (Kruskal-Wallis)

#For each group
#datat48 %>%
#group_by(Year, NutRat, VitAdd) %>%
#shapiro_test(L_TN)

#ggqqplot(datat48, "L_TN", ggtheme = theme_bw()) +
#facet_grid(Year + NutRat ~ VitAdd, labeller = "label_both")

#Homogeny of varience
datat48%>% levene_test(L_TN ~ Year*NutRat*VitAdd)
#Failed test of homogeny of varience. As data also failed assumption of normality, the Kruskal-Wallis test is a test of medians not means

##Run Kruskal-Wallis Year
TNres.kruskalYear <- datat48 %>% kruskal_test(L_TN ~ Year)
TNres.kruskalYear

#Posthoc with Dunn's Test 
TNdunnYear <- datat48 %>% 
  dunn_test(L_TN ~ Year, p.adjust.method = "bonferroni") 
TNdunnYear

TNdunnYear <- TNdunnYear %>% add_xy_position(x = "Year")
TNboxplotYear <- ggboxplot(datat48, x = "Year", y = "L_TN", ylab = "TN Concentration (µg/L)", xlab = "Year") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(TNdunnYear, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(TNres.kruskalYear, detailed = TRUE),
    caption = get_pwc_label(TNdunnYear)
  )
TNboxplotYear

TNres.kruskalNutRat <- datat48 %>% kruskal_test(L_TN ~ NutRat)
TNres.kruskalNutRat

TNdunnNutRat <- datat48 %>% 
  dunn_test(L_TN ~ NutRat, p.adjust.method = "bonferroni") 
TNdunnNutRat

TNdunnNutRat <- TNdunnNutRat %>% add_xy_position(x = "NutRat")
TNboxplotNutRat <- ggboxplot(datat48, x = "NutRat", y = "L_TN", ylab = "TN Concentration (µg/L)", xlab = "Limnocorral N:P Ratio", order = c("0","2.2","16","55","110")) +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(TNdunnNutRat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(TNres.kruskalNutRat, detailed = TRUE),
    caption = get_pwc_label(TNdunnNutRat)
  )
TNboxplotNutRat

TNres.kruskalVitAdd <- datat48 %>% kruskal_test(L_TN ~ VitAdd)
TNres.kruskalVitAdd

TNdunnVitAdd <- datat48 %>% 
  dunn_test(L_TN ~ VitAdd, p.adjust.method = "bonferroni") 
TNdunnVitAdd

TNdunnVitAdd <- TNdunnVitAdd %>% add_xy_position(x = "VitAdd")
TNboxplotVitAdd <- ggboxplot(datat48, x = "VitAdd", y = "L_TN", ylab = "TN Concentration (µg/L)", xlab = "Vitamin Addition") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(TNdunnVitAdd, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(TNres.kruskalVitAdd, detailed = TRUE),
    caption = get_pwc_label(TNdunnVitAdd)
  )
TNboxplotVitAdd

##TDN

datat48 %>%
  group_by(Year, NutRat, VitAdd) %>%
  get_summary_stats(L_TDN, type = "mean_sd")

boxplotTDN <- ggboxplot(
  datat48, x = "VitAdd", y = "L_TDN", 
  color = "Year", palette = "jco", facet.by = "NutRat"
)
boxplotTDN

###Check Check Normality
model  <- lm(L_TDN ~ Year*NutRat*VitAdd, data = datat48)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
#Failed assumption of normality - need non-parametric test (Kruskal-Wallis)

#For each group
#datat48 %>%
#group_by(Year, NutRat, VitAdd) %>%
#shapiro_test(L_TDN)

#ggqqplot(datat48, "L_TDN", ggtheme = theme_bw()) +
#facet_grid(Year + NutRat ~ VitAdd, labeller = "label_both")

#Homogeny of varience
datat48%>% levene_test(L_TDN ~ Year*NutRat*VitAdd)
#Failed test of homogeny of varience. As data also failed assumption of normality, the Kruskal-Wallis test is a test of medians not means

##Run Kruskal-Wallis Year
TDNres.kruskalYear <- datat48 %>% kruskal_test(L_TDN ~ Year)
TDNres.kruskalYear

#Posthoc with Dunn's Test 
TDNdunnYear <- datat48 %>% 
  dunn_test(L_TDN ~ Year, p.adjust.method = "bonferroni") 
TDNdunnYear

TDNdunnYear <- TDNdunnYear %>% add_xy_position(x = "Year")
TDNboxplotYear <- ggboxplot(datat48, x = "Year", y = "L_TDN", ylab = "TDN Concentration (µg/L)", xlab = "Year") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(TDNdunnYear, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(TDNres.kruskalYear, detailed = TRUE),
    caption = get_pwc_label(TDNdunnYear)
  )
TDNboxplotYear

TDNres.kruskalNutRat <- datat48 %>% kruskal_test(L_TDN ~ NutRat)
TDNres.kruskalNutRat

TDNdunnNutRat <- datat48 %>% 
  dunn_test(L_TDN ~ NutRat, p.adjust.method = "bonferroni") 
TDNdunnNutRat

TDNdunnNutRat <- TDNdunnNutRat %>% add_xy_position(x = "NutRat")
TDNboxplotNutRat <- ggboxplot(datat48, x = "NutRat", y = "L_TDN", ylab = "TDN Concentration (µg/L)", xlab = "Limnocorral N:P Ratio", order = c("0","2.2","16","55","110")) +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(TDNdunnNutRat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(TDNres.kruskalNutRat, detailed = TRUE),
    caption = get_pwc_label(TDNdunnNutRat)
  )
TDNboxplotNutRat

TDNres.kruskalVitAdd <- datat48 %>% kruskal_test(L_TDN ~ VitAdd)
TDNres.kruskalVitAdd

TDNdunnVitAdd <- datat48 %>% 
  dunn_test(L_TDN ~ VitAdd, p.adjust.method = "bonferroni") 
TDNdunnVitAdd

TDNdunnVitAdd <- TDNdunnVitAdd %>% add_xy_position(x = "VitAdd")
TDNboxplotVitAdd <- ggboxplot(datat48, x = "VitAdd", y = "L_TDN", ylab = "TDN Concentration (µg/L)", xlab = "Vitamin Addition") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(TDNdunnVitAdd, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(TDNres.kruskalVitAdd, detailed = TRUE),
    caption = get_pwc_label(TDNdunnVitAdd)
  )
TDNboxplotVitAdd

##TP
datat48 %>%
  group_by(Year, NutRat, VitAdd) %>%
  get_summary_stats(L_TP, type = "mean_sd")

boxplotTP <- ggboxplot(
  datat48, x = "VitAdd", y = "L_TP", 
  color = "Year", palette = "jco", facet.by = "NutRat"
)
boxplotTP

###Check Check Normality
model  <- lm(L_TP ~ Year*NutRat*VitAdd, data = datat48)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
#Failed assumption of normality - need non-parametric test (Kruskal-Wallis)

#For each group
#datat48 %>%
#group_by(Year, NutRat, VitAdd) %>%
#shapiro_test(L_TP)

#ggqqplot(datat48, "L_TP", ggtheme = theme_bw()) +
#facet_grid(Year + NutRat ~ VitAdd, labeller = "label_both")

#Homogeny of varience
datat48%>% levene_test(L_TP ~ Year*NutRat*VitAdd)
#Failed test of homogeny of varience. As data also failed assumption of normality, the Kruskal-Wallis test is a test of medians not means

##Run Kruskal-Wallis Year
TPres.kruskalYear <- datat48 %>% kruskal_test(L_TP ~ Year)
TPres.kruskalYear

#Posthoc with Dunn's Test 
TPdunnYear <- datat48 %>% 
  dunn_test(L_TP ~ Year, p.adjust.method = "bonferroni") 
TPdunnYear

TPdunnYear <- TPdunnYear %>% add_xy_position(x = "Year")
TPboxplotYear <- ggboxplot(datat48, x = "Year", y = "L_TP", ylab = "TP Concentration (µg/L)", xlab = "Year") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(TPdunnYear, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(TPres.kruskalYear, detailed = TRUE),
    caption = get_pwc_label(TPdunnYear)
  )
TPboxplotYear

TPres.kruskalNutRat <- datat48 %>% kruskal_test(L_TP ~ NutRat)
TPres.kruskalNutRat

TPdunnNutRat <- datat48 %>% 
  dunn_test(L_TP ~ NutRat, p.adjust.method = "bonferroni") 
TPdunnNutRat

TPdunnNutRat <- TPdunnNutRat %>% add_xy_position(x = "NutRat")
TPboxplotNutRat <- ggboxplot(datat48, x = "NutRat", y = "L_TP", ylab = "TP Concentration (µg/L)", xlab = "Limnocorral N:P Ratio", order = c("0","2.2","16","55","110")) +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(TPdunnNutRat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(TPres.kruskalNutRat, detailed = TRUE),
    caption = get_pwc_label(TPdunnNutRat)
  )
TPboxplotNutRat

TPres.kruskalVitAdd <- datat48 %>% kruskal_test(L_TP ~ VitAdd)
TPres.kruskalVitAdd

TPdunnVitAdd <- datat48 %>% 
  dunn_test(L_TP ~ VitAdd, p.adjust.method = "bonferroni") 
TPdunnVitAdd

TPdunnVitAdd <- TPdunnVitAdd %>% add_xy_position(x = "VitAdd")
TPboxplotVitAdd <- ggboxplot(datat48, x = "VitAdd", y = "L_TP", ylab = "TP Concentration (µg/L)", xlab = "Vitamin Addition") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(TPdunnVitAdd, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(TPres.kruskalVitAdd, detailed = TRUE),
    caption = get_pwc_label(TPdunnVitAdd)
  )
TPboxplotVitAdd


##TDP
datat48 %>%
  group_by(Year, NutRat, VitAdd) %>%
  get_summary_stats(L_TDP, type = "mean_sd")

boxplotTDP <- ggboxplot(
  datat48, x = "VitAdd", y = "L_TDP", 
  color = "Year", palette = "jco", facet.by = "NutRat"
)
boxplotTDP

###Check Check Normality
model  <- lm(L_TDP ~ Year*NutRat*VitAdd, data = datat48)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
#Failed assumption of normality - need non-parametric test (Kruskal-Wallis)

#For each group
#datat48 %>%
#group_by(Year, NutRat, VitAdd) %>%
#shapiro_test(L_TDP)

#ggqqplot(datat48, "L_TDP", ggtheme = theme_bw()) +
#facet_grid(Year + NutRat ~ VitAdd, labeller = "label_both")

#Homogeny of varience
datat48%>% levene_test(L_TDP ~ Year*NutRat*VitAdd)
#Failed test of homogeny of varience. As data also failed assumption of normality, the Kruskal-Wallis test is a test of medians not means

##Run Kruskal-Wallis Year
TDPres.kruskalYear <- datat48 %>% kruskal_test(L_TDP ~ Year)
TDPres.kruskalYear

#Posthoc with Dunn's Test 
TDPdunnYear <- datat48 %>% 
  dunn_test(L_TDP ~ Year, p.adjust.method = "bonferroni") 
TDPdunnYear

TDPdunnYear <- TDPdunnYear %>% add_xy_position(x = "Year")
TDPboxplotYear <- ggboxplot(datat48, x = "Year", y = "L_TDP", ylab = "TDP Concentration (µg/L)", xlab = "Year") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(TDPdunnYear, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(TDPres.kruskalYear, detailed = TRUE),
    caption = get_pwc_label(TDPdunnYear)
  )
TDPboxplotYear

TDPres.kruskalNutRat <- datat48 %>% kruskal_test(L_TDP ~ NutRat)
TDPres.kruskalNutRat

TDPdunnNutRat <- datat48 %>% 
  dunn_test(L_TDP ~ NutRat, p.adjust.method = "bonferroni") 
TDPdunnNutRat

TDPdunnNutRat <- TDPdunnNutRat %>% add_xy_position(x = "NutRat")
TDPboxplotNutRat <- ggboxplot(datat48, x = "NutRat", y = "L_TDP", ylab = "TDP Concentration (µg/L)", xlab = "Limnocorral N:P Ratio", order = c("0","2.2","16","55","110")) +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(TDPdunnNutRat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(TDPres.kruskalNutRat, detailed = TRUE),
    caption = get_pwc_label(TDPdunnNutRat)
  )
TDPboxplotNutRat

TDPres.kruskalVitAdd <- datat48 %>% kruskal_test(L_TDP ~ VitAdd)
TDPres.kruskalVitAdd

TDPdunnVitAdd <- datat48 %>% 
  dunn_test(L_TDP ~ VitAdd, p.adjust.method = "bonferroni") 
TDPdunnVitAdd

TDPdunnVitAdd <- TDPdunnVitAdd %>% add_xy_position(x = "VitAdd")
TDPboxplotVitAdd <- ggboxplot(datat48, x = "VitAdd", y = "L_TDP", ylab = "TDP Concentration (µg/L)", xlab = "Vitamin Addition") +
  stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="black", fill="black") +
  stat_pvalue_manual(TDPdunnVitAdd, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(TDPres.kruskalVitAdd, detailed = TRUE),
    caption = get_pwc_label(TDPdunnVitAdd)
  )
TDPboxplotVitAdd

##Combining Plots
TNPboxplotscombined <- ggarrange(TNboxplotYear, TNboxplotNutRat, TNboxplotVitAdd, TDNboxplotYear, TDNboxplotNutRat, TDNboxplotVitAdd,TPboxplotYear, TPboxplotNutRat, TPboxplotVitAdd,TDPboxplotYear, TDPboxplotNutRat, TDPboxplotVitAdd,
                                 labels = c("A", "B", "C","D", "E", "F","G", "H", "I","J","K","L"),
                                 ncol = 3, nrow = 4);
TNPboxplotscombined

ggsave(filename="Stat_Outputs/TNPBoxPlotsCombined.tiff",TNPboxplotscombined,
       width=14,height=12,units="in")
