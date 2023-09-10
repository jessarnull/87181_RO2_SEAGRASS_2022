#load necessary R libraries
library(lme4)
library(lattice)
library(MuMIn)
library(lmtest)
library(dplyr)
library(ggplot2)# Load ggplot2
library(tidyr) #package containing filter etc
library(stargazer) 
library(esquisse)


setwd("~/Documents/PhD/RO2 2022/Isotope analysis")

library(readxl)
isotopes <- read_excel("RO2 2022 Isotopes.xlsx")
View(isotopes)

summary(isotopes)
bwplot(d15N~ Treatment |Site, data = isotopes) 

# Re-structure data into correct format. 
isotopes$Treatment <- as.factor(isotopes$Treatment)
isotopes$Site <- as.factor(isotopes$Site)         # making it into factor, i.e., numerical to categorical 
isotopes$AGB_BGB <- as.factor(isotopes$AGB_BGB)                 # change to fcctor making levels

#rename AGB and BGB to full 
levels(isotopes$AGB_BGB)[levels(isotopes$AGB_BGB)=="AGB"]<- "Above-ground biomass"
levels(isotopes$AGB_BGB)[levels(isotopes$AGB_BGB)=="BGB"]<- "Below-ground biomass"

#rename AGB_BGB to Biomass
names(isotopes)[names(isotopes) == "AGB_BGB"]<- "Biomass"

#rename polluted and nonpolluted
levels(isotopes$Site)[levels(isotopes$Site)=="Non-polluted"]<- "Pristine"
levels(isotopes$Site)[levels(isotopes$Site)=="Polluted"]<- "Impacted"

#reorder treatment levels
isotopes$Treatment <- factor(isotopes$Treatment, levels = c("Control", "Low N", "High N"))


#CAN DO SAME AS BELOW FOR ALL VARIABLES e.g., %C, %N, C:N, d13C

d15N_plot <-ggplot(isotopes) +
  aes(x = Treatment, y = d15N, fill = Site) +
  geom_boxplot()+
  stat_boxplot(geom ='errorbar', width = 0.3)+ #adds the caps on the whiskers
  scale_fill_brewer(palette = "Set2", direction = 1) +
  labs(x = "Treatment", y = "δ"^{"15"}~"N") +  
  theme_classic() +
  facet_grid(rows = vars(Biomass), cols = vars(Site), scales = "free") + 
  theme_bw() + # puts into 2 boxes/panes
  theme(text = element_text(family = "Calibri"))+
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.text = element_text(size = 10),
              axis.title.y = element_text(size = 11, face = "bold"),
              axis.title.x = element_text(size = 11, face = "bold"))
d15N_plot # show plot 
ggsave("RO2_2022_d15N.png", width = 8, height = 5, dpi = 300) #for when you want to save




----- 
CN_plot <-ggplot(isotopes) +
  aes(x = Treatment, y = C_N, fill = Site) +
  geom_boxplot()+
  stat_boxplot(geom ='errorbar', width = 0.3)+ #adds the caps on the whiskers
  scale_fill_brewer(palette = "Set2", direction = 1) +
  labs(x = "Treatment", y = "C:N") +  
  theme_classic() +
  facet_grid(rows = vars(Biomass), cols = vars(Site), scales = "free") +
  theme_bw() + # puts into 2 boxes/panes
  theme(text = element_text(family = "Calibri"))+
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11, face = "bold"))
CN_plot # show plot 
ggsave("RO2_2022_CN_plot.png", width = 8, height = 5, dpi = 300) #for when you want to save  

------
d13C_plot <-ggplot(isotopes) +
  aes(x = Treatment, y = d13C, fill = Site) +
  geom_boxplot()+
  stat_boxplot(geom ='errorbar', width = 0.3)+ #adds the caps on the whiskers
  scale_fill_brewer(palette = "Set2", direction = 1) +
  labs(x = "Treatment", y = "δ"^{"13"}~"C")+  
  theme_classic() +
  facet_grid(rows = vars(Biomass), cols = vars(Site), scales = "free") +
  theme_bw() + # puts into 2 boxes/panes
  theme(text = element_text(family = "Calibri"))+
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 11, face = "bold"))
d13C_plot # show plot 
ggsave("RO2_2022_d13C_plot.png", width = 8, height = 5, dpi = 300) #for when you want to save 


------
#create subset of data with just AGB

subset_AGB <- subset(isotopes, Biomass == "Above-ground biomass")
 

d15N_AGB_plot <-ggplot(subset_AGB) +
    aes(x = Treatment, y = d15N, fill = Treatment) +
    geom_boxplot()+
    stat_boxplot(geom ='errorbar', width = 0.3)+ #adds the caps on the whiskers
    scale_fill_brewer(palette = "Set2", direction = 1) +
    labs(x = "Treatment", y = "d15N") +  
    theme_classic() +
    facet_grid(rows = vars(NULL), cols = vars(Site), scales = "free") +
    theme_bw() + # puts into 2 boxes/panes
    theme(legend.position = "none")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

d15N_AGB_plot #show plot
ggsave("RO2 2022_AGB_isotopes.png", width = 8, height = 5, dpi = 300) #for when you want to save

------

#create subset of data with just BGB
  
subset_BGB <- subset(isotopes, Biomass == "Below-ground biomass")


d15N_BGB_plot <-ggplot(subset_BGB) +
  aes(x = Treatment, y = d15N, fill = Treatment) +
  geom_boxplot()+
  stat_boxplot(geom ='errorbar', width = 0.3)+ #adds the caps on the whiskers
  scale_fill_brewer(palette = "Set2", direction = 1) +
  labs(x = "Treatment", y = "d15N") +  
  theme_classic() +
  facet_grid(rows = vars(NULL), cols = vars(Site), scales = "free") +
  theme_bw() + # puts into 2 boxes/panes
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

d15N_BGB_plot #show plot
ggsave("RO2 2022_BGB_isotopes.png", width = 8, height = 5, dpi = 300) #for when you want to save  
  
  
------  
  
d15N_plot2 <-ggplot(isotopes) +
  aes(x = Treatment, y = d15N, fill = Site) +
  geom_boxplot()+
  stat_boxplot(geom ='errorbar', width = 0.3)+ #adds the caps on the whiskers
  scale_fill_brewer(palette = "Set2", direction = 1) +
  labs(x = "Treatment", y = "d15N") +  
  theme_classic() +
  facet_wrap(vars(Biomass), scales = "free_x") + 
  theme_bw() + # puts into 2 boxes/panes
  theme(legend.title=element_blank(),
        panel.grid = element_blank(),# gets rid of grid
        axis.text = element_text(size = 9),
        axis.title.y = element_text(size = 9, face="bold"),
        axis.title.x = element_blank(),
        legend.position = c(0.88,0.15))
d15N_plot2 # show plot 
ggsave("RO2_SG_growth.png", width = 8, height = 5, dpi = 300) #for when you want to save

------
  
# Shapiro-Wilk normality test 
shapiro.test(isotopes$d15N)
#W = 0.87028, p-value = 7.193e-08

#visual normality checks w QQplots and histogram  = both suggest non-normal so non-para tests.
qqnorm(isotopes$d15N)
qqline(isotopes$d15N)
hist(isotopes$d15N)

-----

-----
# (1) BETWEEN SITE DIFFERENCES - workings after these results
#All d15N biomass controls: W = 271, p=0.05589
#AGB d15N controls: w = 70, p = 0.1431
#BGB d15N controls: w = 68, p = 0.1903

#All d13C biomass controls: W = 355, p= 6.914e-06
#AGB d13C controls: w=99, p = 2.165c-05
#BGB d13C controls: w = 80, p = 0.02323

#All CN biomass controls: W = 355, p< 0.01
#AGB CN controls: w=62, p = 0.393
#BGB CN controls: w = 20, p = 0.02323

----
----
# check for differences between controls between sites for all biomass components
# create subset just controls

control_all_biomass <- subset(isotopes, Treatment == "Control")

wilcox.test(control_all_biomass$d15N ~ control_all_biomass$Site) 
#W = 271, p-value = 0.05589
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(control_all_biomass$d13C ~ control_all_biomass$Site)
#W = 355, p-value = 6.914e-06
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(control_all_biomass$C_N ~ control_all_biomass$Site)
#W = 182, p-value = 0.6395
#alternative hypothesis: true location shift is not equal to 0
-----

-----
# (1) BETWEEN SITE DIFFERENCES  
#want to check for differences in d15N, d13C values and C:N ratios between sites by treatments and by biomass
  
#first create subsets of treatment data for AGB using AGB subset dataframe
AGB_control_data <- subset(subset_AGB, Treatment == "Control")
AGB_LowN_data <- subset(subset_AGB, Treatment == "Low N")
AGB_HighN_data <-subset(subset_AGB, Treatment == "High N")

# perform MWU between two sites using d15N, d13C and C:N as dependent variable and site as independent for *control* treatment for AGB
wilcox.test(AGB_control_data$d15N ~ AGB_control_data$Site) #RESULTS = no stat.sig. differences in d15N in AGB of controls treatments between sites
#W = 70, p-value = 0.1431
# alternative hypothesis: true location shift is not equal to 0


wilcox.test(AGB_control_data$d13C ~ AGB_control_data$Site) # RESULTS = stat sig differences in d13C in AGB for controls
#W = 99, p-value = 2.165e-05
#alternative hypothesis: true location shift is not equal to 0


wilcox.test(AGB_control_data$C_N ~ AGB_control_data$Site) #RESULTS = no stat.sig. differences in C:N in AGB of controls treatments between sites
#W = 62, p-value = 0.393
#alternative hypothesis: true location shift is not equal to 0

----

# perform MWU between two sites using d15N as dependent variable and site as independent for *Low N treatment* for AGB
wilcox.test(AGB_LowN_data$d15N ~ AGB_LowN_data$Site) #RESULTS =stat.sig. differences in d15N in AGB of low N treatments between sites

#W = 100, p-value = 1.083e-05
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(AGB_LowN_data$d13C ~ AGB_LowN_data$Site) # RESULTS = stat sig difference in d13C values between low N in AGB between sites
#W = 100, p-value = 1.083e-05
#alternative hypothesis: true location shift is not equal to 0


wilcox.test(AGB_LowN_data$C_N ~ AGB_LowN_data$Site) # RESULTS = AGB C:N not stat sig between lowN treatments between sites
#W = 49, p-value = 0.9705
#alternative hypothesis: true location shift is not equal to 0

----
  
#second, create subsets of treatment data for BGB using BGB subset

BGB_control_data <- subset(subset_BGB, Treatment == "Control")
BGB_LowN_data <- subset(subset_BGB, Treatment == "Low N")
BGB_HighN_data <-subset(subset_BGB, Treatment == "High N")

# perform MWU between two sites using d15N, d13C and C:N as dependent variables and site as independent for *control* treatment for BGB
wilcox.test(BGB_control_data$d15N ~ BGB_control_data$Site) # RESULTS = no stat sig diff in BGB in control between sites

# W = 68, p-value = 0.1903
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(BGB_control_data$C_N ~ BGB_control_data$Site) # RESULTS = stat sig differences in BGB C:N ratios between control plots between sites
#W = 20, p-value = 0.02323
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(BGB_control_data$d13C ~ BGB_control_data$Site)
#W = 80, p-value = 0.02323
#alternative hypothesis: true location shift is not equal to 0

----

# perform MWU between two sites using d15N as dependent variable and site as independent for *Low N treatment* for BGB
wilcox.test(BGB_LowN_data$d15N ~ BGB_LowN_data$Site) # RESULTS = no stat sig diff in BGB in d15N Low N between sites
# W = 68, p-value = 0.1857
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(BGB_LowN_data$C_N ~ BGB_LowN_data$Site) # RESULTS = stat sig differences in BGB C:N ratios in low n plots betwen sites
#W = 4, p-value = 0.0005801
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(BGB_LowN_data$d13C ~ BGB_LowN_data$Site) 
#W = 87, p-value = 0.005777
#alternative hypothesis: true location shift is not equal to 0


#(2) BETWEEN TREATMENTS

----
  
# Use the Dunn post-hoc test corrected with Bonferroni method for between treatments in polluted.
  
install.packages("dunn.test")
library(dunn.test)
install.packages("FSA")
library(FSA)

-----
-----
#POLLUTED d15N 

#all treatments AGB polluted  
subset_AGB_poll_try <- isotopes[isotopes$Site == "Polluted" & isotopes$Biomass =="Above-ground biomass",]
kruskal.test(subset_AGB_poll_try$d15N ~ subset_AGB_poll_try$Treatment)
# Kruskal-Wallis chi-squared = 23.814, df = 2, p-value = 6.742e-06

dunnTest(subset_AGB_poll_try$d15N, subset_AGB_poll_try$Treatment, method ="bonferroni")
Comparison              Z      P.unadj        P.adj
1 Control - High N  4.876805 1.078180e-06 3.234540e-06
2  Control - Low N  2.286002 2.225412e-02 6.676237e-02
3   High N - Low N -2.590803 9.575240e-03 2.872572e-02


#all treatments BGB polluted
subset_BGB_poll_try <- isotopes[isotopes$Site == "Polluted" & isotopes$Biomass =="Below-ground biomass",]
kruskal.test(subset_BGB_poll_try$d15N ~ subset_BGB_poll_try$Treatment)
#Kruskal-Wallis chi-squared = 6, df = 2, p-value = 0.04979

dunnTest(subset_BGB_poll_try$d15N, subset_BGB_poll_try$Treatment, method ="bonferroni")
Comparison            Z        P.unadj      P.adj
1 Control - High N  2.2860023 0.02225412 0.06676237
2  Control - Low N  0.3810004 0.70320297 1.00000000
3   High N - Low N -1.9050019 0.05677982 0.17033947
-----
-----
# POLLUTED C:N
  
kruskal.test(subset_AGB_poll_try$C_N ~ subset_AGB_poll_try$Treatment)
#Kruskal-Wallis chi-squared = 0.96, df = 2, p-value = 0.6188

dunnTest(subset_AGB_poll_try$C_N, subset_AGB_poll_try$Treatment, method ="bonferroni")

Comparison            Z       P.unadj   P.adj
1 Control - High N 0.7620008 0.4460595     1
2  Control - Low N 0.9144009 0.3605062     1
3   High N - Low N 0.1524002 0.8788713     1


kruskal.test(subset_BGB_poll_try$C_N ~ subset_BGB_poll_try$Treatment)
#Kruskal-Wallis chi-squared = 1.6284, df = 2, p-value = 0.443

dunnTest(subset_BGB_poll_try$C_N, subset_BGB_poll_try$Treatment, method ="bonferroni")
  
Comparison               Z     P.unadj     P.adj
1 Control - High N  1.09220109 0.2747447 0.8242342
2  Control - Low N -0.02540003 0.9797359 1.0000000
3   High N - Low N -1.11760112 0.2637374 0.7912122

------
------
#POLLUTED d13C
  
kruskal.test(subset_AGB_poll_try$d13C ~ subset_AGB_poll_try$Treatment)
#Kruskal-Wallis chi-squared = 5.6439, df = 2, p-value = 0.05949

dunnTest(subset_AGB_poll_try$d13C, subset_AGB_poll_try$Treatment, method ="bonferroni")
Comparison            Z      P.unadj     P.adj
1 Control - High N -2.057402 0.03964757 0.1189427
2  Control - Low N -2.057402 0.03964757 0.1189427
3   High N - Low N  0.000000 1.00000000 1.0000000

kruskal.test(subset_BGB_poll_try$d13C ~ subset_BGB_poll_try$Treatment)
#Kruskal-Wallis chi-squared = 1.9071, df = 2, p-value = 0.3854

dunnTest(subset_BGB_poll_try$d13C, subset_BGB_poll_try$Treatment, method ="bonferroni")
Comparison              Z     P.unadj     P.adj
1 Control - High N -1.3462013 0.1782376 0.5347128
2  Control - Low N -0.9398009 0.3473197 1.0000000
3   High N - Low N  0.4064004 0.6844484 1.0000000


-----
----- 
# NON POLLUTED d15N

#control vs Low N NON-POLLUTED AGB
subset_AGB_nonpoll <- isotopes[isotopes$Site == "Non-polluted" & isotopes$Biomass =="Above-ground biomass",]
wilcox.test(subset_AGB_nonpoll$d15N ~ subset_AGB_nonpoll$Treatment)

#W = 77, p-value = 0.04326
#alternative hypothesis: true location shift is not equal to 0

#control vs Low N NON-POLLUTED BGB
subset_BGB_nonpoll <- isotopes[isotopes$Site == "Non-polluted" & isotopes$Biomass =="Below-ground biomass",]
wilcox.test(subset_BGB_nonpoll$d15N ~ subset_BGB_nonpoll$Treatment)

#W = 64, p-value = 0.3073
#alternative hypothesis: true location shift is not equal to 0

------
  
#NON POLLUTED d13C
  
#control vs Low N NON-POLLUTED AGB
subset_AGB_nonpoll <- isotopes[isotopes$Site == "Non-polluted" & isotopes$Biomass =="Above-ground biomass",]
wilcox.test(subset_AGB_nonpoll$d13C ~ subset_AGB_nonpoll$Treatment)

#W = 41, p-value = 0.5288
#alternative hypothesis: true location shift is not equal to 0

#control vs Low N NON-POLLUTED BGB
subset_BGB_nonpoll <- isotopes[isotopes$Site == "Non-polluted" & isotopes$Biomass =="Below-ground biomass",]
wilcox.test(subset_BGB_nonpoll$d13C ~ subset_BGB_nonpoll$Treatment)

#W = 62, p-value = 0.3845
#alternative hypothesis: true location shift is not equal to 0


------------
#NON POLLUTED C:N

  #control vs Low N NON-POLLUTED AGB
  subset_AGB_nonpoll <- isotopes[isotopes$Site == "Non-polluted" & isotopes$Biomass =="Above-ground biomass",]
wilcox.test(subset_AGB_nonpoll$C_N ~ subset_AGB_nonpoll$Treatment)

#W = 73, p-value = 0.08921
#alternative hypothesis: true location shift is not equal to 0

#control vs Low N NON-POLLUTED BGB
subset_BGB_nonpoll <- isotopes[isotopes$Site == "Non-polluted" & isotopes$Biomass =="Below-ground biomass",]
wilcox.test(subset_BGB_nonpoll$C_N ~ subset_BGB_nonpoll$Treatment)

#W = 84, p-value = 0.0113
#alternative hypothesis: true location shift is not equal to 0

------

  
  
------  
-------
  
# CAN IGNORE BELOW (individual MWU tests for the polluted site to find differences between the treatments)
# Better to do posthoc test on the krusal wallis which is what I've done, but wanted to keep this just in case  

  #control vs high N POLLUTED AGB
  subset_AGB_poll <- isotopes[isotopes$Site == "Polluted" & isotopes$Treatment %in% c("Control", "High N") & isotopes$Biomass =="Above-ground biomass",]
wilcox.test(subset_AGB_poll$d15N ~ subset_AGB_poll$Treatment)

# W = 100, p-value = 1.083e-05
# alternative hypothesis: true location shift is not equal to 0

# control vs low N POLLUTED AGB
subset_AGB_poll <- isotopes[isotopes$Site == "Polluted" & isotopes$Treatment %in% c("Control", "Low N") & isotopes$Biomass =="Above-ground biomass",]
wilcox.test(subset_AGB_poll$d15N ~ subset_AGB_poll$Treatment)

# W = 94, p-value = 0.0003248
# alternative hypothesis: true location shift is not equal to 0

# High N vs low N POLLUTED AGB
subset_AGB_poll <- isotopes[isotopes$Site == "Polluted" & isotopes$Treatment %in% c("Low N", "High N") & isotopes$Biomass =="Above-ground biomass",]
wilcox.test(subset_AGB_poll$d15N ~ subset_AGB_poll$Treatment)

# W = 98, p-value = 4.33e-05
#alternative hypothesis: true location shift is not equal to 0

---
#d15N
  #control vs high N POLLUTED BGB
  subset_BGB_poll <- isotopes[isotopes$Site == "Polluted" & isotopes$Treatment %in% c("Control", "High N") & isotopes$Biomass =="Below-ground biomass",]
wilcox.test(subset_BGB_poll$d15N ~ subset_BGB_poll$Treatment)

#W = 77, p-value = 0.04326
#alternative hypothesis: true location shift is not equal to 0

#control vs Low N POLLUTED BGB
subset_BGB_poll <- isotopes[isotopes$Site == "Polluted" & isotopes$Treatment %in% c("Control", "Low N") & isotopes$Biomass =="Below-ground biomass",]
wilcox.test(subset_BGB_poll$d15N ~ subset_BGB_poll$Treatment)

#W = 58, p-value = 0.5787
#alternative hypothesis: true location shift is not equal to 0

#Low N vs high N POLLUTED BGB
subset_BGB_poll <- isotopes[isotopes$Site == "Polluted" & isotopes$Treatment %in% c("Low N", "High N") & isotopes$Biomass =="Below-ground biomass",]
wilcox.test(subset_BGB_poll$d15N ~ subset_BGB_poll$Treatment)

#W = 78, p-value = 0.03546
#alternative hypothesis: true location shift is not equal to 0
---
#d13C
#control vs high N POLLUTED AGB
subset_AGB_poll <- isotopes[isotopes$Site == "Polluted" & isotopes$Treatment %in% c("Control", "High N") & isotopes$Biomass =="Above-ground biomass",]
wilcox.test(subset_AGB_poll$d13C ~ subset_AGB_poll$Treatment)
#W = 22, p-value = 0.03546

# control vs low N POLLUTED AGB
subset_AGB_poll <- isotopes[isotopes$Site == "Polluted" & isotopes$Treatment %in% c("Control", "Low N") & isotopes$Biomass =="Above-ground biomass",]
wilcox.test(subset_AGB_poll$d13C ~ subset_AGB_poll$Treatment)
#W = 24, p-value = 0.05243

# High N vs low N POLLUTED AGB
subset_AGB_poll <- isotopes[isotopes$Site == "Polluted" & isotopes$Treatment %in% c("Low N", "High N") & isotopes$Biomass =="Above-ground biomass",]
wilcox.test(subset_AGB_poll$d13C ~ subset_AGB_poll$Treatment)
#W = 51, p-value = 0.9705

--
  #control vs high N POLLUTED BGB
  subset_BGB_poll <- isotopes[isotopes$Site == "Polluted" & isotopes$Treatment %in% c("Control", "High N") & isotopes$Biomass =="Below-ground biomass",]
wilcox.test(subset_BGB_poll$d13C ~ subset_BGB_poll$Treatment)
#W = 35, p-value = 0.2799

#control vs Low N POLLUTED BGB
subset_BGB_poll <- isotopes[isotopes$Site == "Polluted" & isotopes$Treatment %in% c("Control", "Low N") & isotopes$Biomass =="Below-ground biomass",]
wilcox.test(subset_BGB_poll$d13C ~ subset_BGB_poll$Treatment)
#W = 35, p-value = 0.2799

#Low N vs high N POLLUTED BGB
subset_BGB_poll <- isotopes[isotopes$Site == "Polluted" & isotopes$Treatment %in% c("Low N", "High N") & isotopes$Biomass =="Below-ground biomass",]
wilcox.test(subset_BGB_poll$d13C ~ subset_BGB_poll$Treatment)
#W = 42, p-value = 0.5787

#CN
#control vs high N POLLUTED AGB
subset_AGB_poll <- isotopes[isotopes$Site == "Polluted" & isotopes$Treatment %in% c("Control", "High N") & isotopes$Biomass =="Above-ground biomass",]
wilcox.test(subset_AGB_poll$C_N ~ subset_AGB_poll$Treatment)
#W = 63, p-value = 0.3527

# control vs low N POLLUTED AGB
subset_AGB_poll <- isotopes[isotopes$Site == "Polluted" & isotopes$Treatment %in% c("Control", "Low N") & isotopes$Biomass =="Above-ground biomass",]
wilcox.test(subset_AGB_poll$C_N ~ subset_AGB_poll$Treatment)

#W = 59, p-value = 0.5288

# High N vs low N POLLUTED AGB
subset_AGB_poll <- isotopes[isotopes$Site == "Polluted" & isotopes$Treatment %in% c("Low N", "High N") & isotopes$Biomass =="Above-ground biomass",]
wilcox.test(subset_AGB_poll$C_N ~ subset_AGB_poll$Treatment)

# W = 45, p-value = 0.7394
#alternative hypothesis: true location shift is not equal to 0

---
  
  #control vs high N POLLUTED BGB
  subset_BGB_poll <- isotopes[isotopes$Site == "Polluted" & isotopes$Treatment %in% c("Control", "High N") & isotopes$Biomass =="Below-ground biomass",]
wilcox.test(subset_BGB_poll$C_N ~ subset_BGB_poll$Treatment)
#W = 67, p-value = 0.2176

#control vs Low N POLLUTED BGB
subset_BGB_poll <- isotopes[isotopes$Site == "Polluted" & isotopes$Treatment %in% c("Control", "Low N") & isotopes$Biomass =="Below-ground biomass",]
wilcox.test(subset_BGB_poll$C_N ~ subset_BGB_poll$Treatment)
#W = 47, p-value = 0.8534

#Low N vs high N POLLUTED BGB
subset_BGB_poll <- isotopes[isotopes$Site == "Polluted" & isotopes$Treatment %in% c("Low N", "High N") & isotopes$Biomass =="Below-ground biomass",]
wilcox.test(subset_BGB_poll$C_N ~ subset_BGB_poll$Treatment)
#W = 62, p-value = 0.393

-----    
  
#ignore below... have created means dataframe and structured it correctly but probs best to use full isotopes dataframe

d15N_means<- aggregate(isotopes$d15N,
                             by = list(Treatment = isotopes$Treatment, Site = isotopes$Site, Biomass = isotopes$AGB_BGB),
                             FUN = mean)
summary(d15N_means)


# change order of treatment using factor function to desired control medium high. ggplot automatically does xaxis alphabetically. 
d15N_means$Treatment <- factor(d15N_means$Treatment, levels = c("Control", "Low N", "High N"))
d15N_means$Biomass <- factor(d15N_means$Biomass, levels = c("Above-ground", "Below-ground",))


# grass_plotmeans <- do.call(data.frame, grass_plotmeans) #change from matrix to data frame
# grass_plotmeans_sd < grass_plotmeans(sd)
# grass_plotmeans$se <- sd(grass_plotmeans) / sqrt(grass_plotmeans$n) #SE calculation from SD and length



















