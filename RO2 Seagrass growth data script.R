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
library(dunn.test)
---

# set working directory 
setwd("~/Documents/PhD/RO2 2022/Seagrass growth data/RO2 2022 Seagrass growth data")

# read in the data (has been saved with a space after growth!!)
grass <- read.csv("RO2 Seagrass Growth.csv")

#read in new growth data 
newgrowth <- read.csv("RO2 New seagrass growth.csv")

# summarise dataset 
summary(grass)
summary(newgrowth)

# Re-structure data into correct format. 
grass$Treatment <- as.factor(grass$Treatment)
grass$Site.type <- as.factor(grass$Site.type)         # making it into factor, i.e., numerical to categorical 
grass$Plot <- as.factor(grass$Plot)                   # change to fcctor making levels 
grass$Block <- as.factor(grass$Block)                 # change to fcctor making levels 

#visualise data quickly
bwplot(Growth.rate..g.DW.day.1.~ Treatment |Site.type, data = grass) # this row and 24 will work if data isn't both sites
bwplot(NG..g.DW.day.1. ~ Treatment, data = newgrowth)



#create new column for growth rate day-1 for cm within 'grass dataframe'- will use this in paper
grass$Growth_rate_cm_day <- grass$Growth.rate..cm./14

#rename Medium to Low N and high to high N in grass
levels(grass$Treatment)[levels(grass$Treatment)=="Medium"]<- "Low N"
levels(grass$Treatment)[levels(grass$Treatment)=="High "]<- "High N"

levels(grass_plotmeans$Treatment)[levels(grass_plotmeans$Treatment)=="Medium"]<- "Low N"
levels(grass_plotmeans$Treatment)[levels(grass_plotmeans$Treatment)=="High "]<- "High N"


# Making graph grouped by site and treatment
ggplot(grass, aes(x = Treatment, y = Growth.rate..g.DW.day.1., color=Site.type)) +            
  geom_boxplot()
ggplot(newgrowth, aes(x = Treatment, y = NG..g.DW.day.1., color=Site.type)) +            
  geom_boxplot()


# Rearrange data and create new data frame called grass_plotmeans, using aggregate function, finding means of each plot for both sites.
grass_plotmeans <- aggregate(grass$Growth_rate_cm_day,
                         by = list(Treatment = grass$Treatment, Site.type = grass$Site.type, Block = grass$Block, Plot = grass$Plot),
                         FUN = mean)
summary(grass_plotmeans)

#rename Medium to Low N and high to high N in grass and in grassplotmeans
levels(grass_plotmeans$Treatment)[levels(grass_plotmeans$Treatment)=="Medium"]<- "Low N"
levels(grass_plotmeans$Treatment)[levels(grass_plotmeans$Treatment)=="High "]<- "High N"

#rename site type
levels(grass_plotmeans$Site.type)[levels(grass_plotmeans$Site.type)=="Non-polluted"]<- "Pristine"
levels(grass_plotmeans$Site.type)[levels(grass_plotmeans$Site.type)=="Polluted"]<- "Impacted"

# change order of treatment using factor function to desired control medium high. ggplot automatically does xaxis alphabetically. 
grass_plotmeans$Treatment <- factor(grass_plotmeans$Treatment, levels = c("Control", "Low N", "High N"))

# grass_plotmeans <- do.call(data.frame, grass_plotmeans) #change from matrix to data frame
# grass_plotmeans_sd < grass_plotmeans(sd)
# grass_plotmeans$se <- sd(grass_plotmeans) / sqrt(grass_plotmeans$n) #SE calculation from SD and length


#make box plot

growth_plot <-ggplot(grass_plotmeans) +
  aes(x = Treatment, y = x, fill = Treatment) +
  geom_boxplot()+
  stat_boxplot(geom ='errorbar', width = 0.3)+ #adds the caps on the whiskers
  scale_fill_brewer(palette = "Set2", direction = 1) +
  labs(x = "Treatment", y = "Growth rate"~(cm~day^{"-1"})) +  
  theme_classic() +
  facet_wrap(vars(Site.type), scales = "free_x") + 
  theme_bw() + # puts into 2 boxes/panes
  theme(legend.title=element_blank(),
        panel.grid = element_blank(),# gets rid of grid
        axis.text = element_text(size = 9),
        axis.title.y = element_text(size = 9, face="bold"),
        axis.title.x = element_blank(),
        legend.position = c(0.88,0.15))
growth_plot # show plot 
ggsave("RO2_SG_growth.png", width = 8, height = 5, dpi = 300) #for when you want to save

---- #same as above but remove legend

growth_plot_nolegend <-ggplot(grass_plotmeans) +
  aes(x = Treatment, y = x, fill = Treatment) +
  geom_boxplot()+
  stat_boxplot(geom ='errorbar', width = 0.3)+ #adds the caps on the whiskers
  scale_fill_brewer(palette = "Set2", direction = 1) +
  labs(x = "Treatment", y = "Growth rate"~(cm~day^{"-1"})) +  
  theme_classic() +
  facet_wrap(vars(Site.type), scales = "free_x") + 
  theme_bw() + # puts into 2 boxes/panes
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"))
growth_plot_nolegend # show plot 
ggsave("RO2_SG_growth_nolegend.png", width = 8, height = 5, dpi = 300)


----

# Shapiro-Wilk normality test for full data set = shapiro wilk tests suggest non-normally distributed, but QQ plots and histograms look relatively normal
shapiro.test(grass$Growth_rate_cm_day)
#data:  grass$Growth_rate_cm_day
#W = 0.98434, p-value = 3.144e-10

#visual normality checks w QQplots and histogram
qqnorm(grass$Growth_rate_cm_day)
qqline(grass$Growth_rate_cm_day)
hist(grass$Growth_rate_cm_day)  

# Shapiro-Wilk normality test for plot means data set = shapiro wilk tests suggest non-normally distributed, QQ plots and histograms look relatively left skewed. 

shapiro.test(grass_plotmeans$x)
#data:  grass_plotmeans$x
#W = 0.93636, p-value = 0.009697
qqnorm(grass_plotmeans$x)
qqline(grass_plotmeans$x)
hist(grass_plotmeans$x)


#have tried transforming data but none seem to really improve skewness. 
transformed_grass_x2 <-sqrt(grass$Growth_rate_cm_day)
#W = 0.98814, p-value = 2.008e-08
transformed_grass_x3 <-grass$Growth_rate_cm_day^(1/3)
#W = 0.97954, p-value = 3.66e-12
transformed_log <-log(grass$Growth_rate_cm_day)
#W = 0.9433, p-value < 2.2e-16
transformed_log10 <-log(grass$Growth_rate_cm_day, base = 10)
#W = 0.9433, p-value < 2.2e-16

#have conducted both parametric and non-parametric tests, results consistent between both. 
#Will use non-parametric test results.
----
  
#ANOVA = check to see if there are significant differences in growth rates between controls plots between sites = YES
res.aov <-aov(x ~ Site.type, data = grass_plotmeans[which(grass_plotmeans$Treatment=='Control'),]) 
summary(res.aov)
#             Df    Sum Sq   Mean Sq F value   Pr(>F)    
# Site.type    1 4.316e-06 4.316e-06    39.1 6.74e-06 ***
#  Residuals   18 1.987e-06 1.100e-07                     
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#MWU (create subsets)
grass_control <-grass_plotmeans[grass_plotmeans$Treatment == 'Control', ]
grass_impacted_control <- grass_control[grass_control$Site.type == 'Impacted', 'x']
grass_pristine_control <- grass_control[grass_control$Site.type == 'Pristine', 'x']

mwu_grass_controls <- wilcox.test(grass_impacted_control, grass_pristine_control)
mwu_grass_controls

#data:  grass_impacted_control and grass_pristine_control
#W = 98, p-value = 4.33e-05

----

#ANOVA2 = check to see if differences in growth rates between treatments at non-polluted site = YES
res.aov2 <-aov(x ~ Treatment , data = grass_plotmeans[which(grass_plotmeans$Site.type=='Pristine'),]) 
summary(res.aov2)

#             Df  Sum Sq Mean Sq F value   Pr(>F)    
#Treatment    1 0.10488 0.10488   53.74 8.31e-07 ***
# Residuals   18 0.03513 0.00195      
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#MWU alternative (create subsets)
grass_plotmeans_nonpoll <- subset(grass_plotmeans, Site.type == "Pristine")
grass_plotmeans_polluted <- subset(grass_plotmeans, Site.type == "Impacted")

grass_plotmeans_nonpoll$Treatment <-as.numeric(grass_plotmeans_nonpoll$Treatment)
grass_plotmeans_polluted$Treatment <-as.numeric(grass_plotmeans_polluted$Treatment)

mwu_pristine_treatments <- wilcox.test(grass_plotmeans_nonpoll$x, grass_plotmeans_nonpoll$Treatment)
mwu_pristine_treatments


#data:  grass_plotmeans_nonpoll$x and grass_plotmeans_nonpoll$Treatment
#W = 0, p-value = 4.205e-08


------
  
#ANOVA = check to see if differences in growth rates between treatments at polluted site = NO
res.aov3 <-aov(x ~ Treatment , data = grass_plotmeans[which(grass_plotmeans$Site.type=='Impacted'),]) 
summary(res.aov3)

# Df    Sum Sq   Mean Sq F value Pr(>F)
# Treatment    2 3.095e-07 1.547e-07   1.862  0.175
# Residuals   27 2.243e-06 8.308e-08   

#No need to do post-hoc test as no significance


#Kruskal wallis alternative = NOT SIGNIF

kruskal_impacted_treatments <- kruskal.test(x ~ Treatment, data = grass_plotmeans_polluted)
kruskal_impacted_treatments

#data:  x by Treatment
#Kruskal-Wallis chi-squared = 3.2881, df = 2, p-value = 0.1932



------------------------------
  

  
# NEWGROWTH 

summary(newgrowth)

#create new column for growth rate day-1 for cm within 'grass dataframe'- will use this in paper
newgrowth$NG_Growth_rate_cm_day <- newgrowth$NG.Growth.rate..cm./14


#rename Medium to Low N and high to high N in newgrowth
levels(newgrowth$Treatment)[levels(newgrowth$Treatment)=="Medium"]<- "Low N"
levels(newgrowth$Treatment)[levels(newgrowth$Treatment)=="High"]<- "High N"


#reorder variables into factors from integers and characters
newgrowth$Treatment <- as.factor(newgrowth$Treatment)
newgrowth$Site.type <- as.factor(newgrowth$Site.type)         # making it into factor, i.e., numerical to categorical 
newgrowth$Plot <- as.factor(newgrowth$Plot)                   # change to fcctor making levels 
newgrowth$Block <- as.factor(newgrowth$Block)                 # change to fcctor making levels 

#create new dataframe w just means
newgrowth_plotmeans <- aggregate(newgrowth$NG_Growth_rate_cm_day,
                             by = list(Treatment = newgrowth$Treatment, Site.type = newgrowth$Site.type, Block = newgrowth$Block, Plot = newgrowth$Plot),
                             FUN = mean)
  
#rename Medium to Low N and high to high N in grassplotmeans
levels(newgrowth_plotmeans$Treatment)[levels(newgrowth_plotmeans$Treatment)=="Medium"]<- "Low N"
levels(newgrowth_plotmeans$Treatment)[levels(newgrowth_plotmeans$Treatment)=="High"]<- "High N"

# change order of treatment using factor function to desired control medium high. ggplot automatically does xaxis alphabetically. 
newgrowth_plotmeans$Treatment <- factor(newgrowth_plotmeans$Treatment, levels = c("Control", "Low N", "High N"))
#rename site type
levels(newgrowth_plotmeans$Site.type)[levels(newgrowth_plotmeans$Site.type)=="Non-polluted"]<- "Pristine"
levels(newgrowth_plotmeans$Site.type)[levels(newgrowth_plotmeans$Site.type)=="Polluted"]<- "Impacted"

----

#make box plot of new growth

newgrowth_plot <-ggplot(newgrowth_plotmeans) +
  aes(x = Treatment, y = x, fill = Treatment) +
  geom_boxplot()+
  stat_boxplot(geom ='errorbar', width = 0.3)+ #adds the caps on the whiskers
  scale_fill_brewer(palette = "Set2", direction = 1) +
  labs(x = "Treatment", y = "Growth rate"~(cm~day^{"-1"})) +  
  theme_classic() +
  facet_wrap(vars(Site.type), scales = "free_x") + 
  theme_bw() + # puts into 2 boxes/panes
  theme(legend.title=element_blank(),
        panel.grid = element_blank(),# gets rid of grid
        axis.text = element_text(size = 9),
        axis.title.y = element_text(size = 9, face="bold"),
        axis.title.x = element_blank(),
        legend.position = c(0.88,0.15))
newgrowth_plot # show plot 
ggsave("RO2_SG_newgrowth.png", width = 8, height = 5, dpi = 300) #for when you want to save

--- #without legend
newgrowth_plot_nolegend <-ggplot(newgrowth_plotmeans) +
  aes(x = Treatment, y = x, fill = Treatment) +
  geom_boxplot()+
  stat_boxplot(geom ='errorbar', width = 0.3)+ #adds the caps on the whiskers
  scale_fill_brewer(palette = "Set2", direction = 1) +
  labs(x = "Treatment", y = "Growth rate"~(cm~day^{"-1"})) +  
  theme_classic() +
  facet_wrap(vars(Site.type), scales = "free_x") + 
  theme_bw() + # puts into 2 boxes/panes
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 11, face = "bold"))
newgrowth_plot_nolegend # show plot 
ggsave("RO2_SG_newgrowth_nolegend.png", width = 8, height = 5, dpi = 300) #for when you want to save



-----
  
  
  
#NEW-GROWTH RATES

#check for normality - using full data set, test suggests non-normal, but graphs suggest normal
shapiro.test(newgrowth$NG_Growth_rate_cm_day)
#data:  grass$Growth_rate_cm_day
#W = 0.96201, p-value = 2.753e-11

#visual normality checks w QQplots and histogram
qqnorm(newgrowth$NG_Growth_rate_cm_day)
qqline(newgrowth$NG_Growth_rate_cm_day)
hist(newgrowth$NG_Growth_rate_cm_day)  

#check for normality - using plotmeans = suggest non-normal; left skewed again
shapiro.test(newgrowth_plotmeans$x)
#data:  newgrowth_plotmeans$x
#W = 0.92955, p-value = 0.005312

#visual normality checks w QQplots and histogram
qqnorm(newgrowth_plotmeans$x)
qqline(newgrowth_plotmeans$x)
hist(newgrowth_plotmeans$x)  

-----
  
#ANOVA = check to see if there are significant differences in new growth rates between controls plots between sites = YES
res.aov_ng <-aov(x ~ Site.type, data = newgrowth_plotmeans[which(newgrowth_plotmeans$Treatment=='Control'),]) 
summary(res.aov_ng)
#           Df    Sum Sq  Mean Sq F value   Pr(>F)    
#Site.type    1 0.07280 0.07280   21.24 0.000218 ***
# Residuals   18 0.06169 0.00343                        
---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#MWU alternative (create subsets) = YES
  
ng_control <-newgrowth_plotmeans[newgrowth_plotmeans$Treatment == 'Control', ]
ng_impacted_control <- ng_control[ng_control$Site.type == 'Impacted', 'x']
ng_pristine_control <- ng_control[ng_control$Site.type == 'Pristine', 'x']

mwu_ng_controls <- wilcox.test(ng_impacted_control, ng_pristine_control)
mwu_ng_controls

#data:  ng_impacted_control and ng_pristine_control
#W = 92, p-value = 0.001693

-----
  
#ANOVA2 = check to see if differences in new growth rates between treatments at non-polluted site = YES
res.aov_ng2 <-aov(x ~ Treatment , data = newgrowth_plotmeans[which(newgrowth_plotmeans$Site.type=='Pristine'),]) 
summary(res.aov_ng2)
  
#           Df  Sum Sq   Mean Sq F value   Pr(>F)    
#Treatment  1  0.07458 0.07458   28.68 4.33e-05 ***
#Residuals  18 0.04681 0.00260                      
---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#MWU alternative  (create subsets) = YES

newgrowth_plotmeans_impacted <- newgrowth_plotmeans[newgrowth_plotmeans$Site.type == 'Impacted',]
newgrowth_plotmeans_pristine <- newgrowth_plotmeans[newgrowth_plotmeans$Site.type == 'Pristine',]

newgrowth_plotmeans_pristine$Treatment <- as.numeric(newgrowth_plotmeans_pristine$Treatment)

mwu_NG_pristine_treatments <- wilcox.test(newgrowth_plotmeans_pristine$x, newgrowth_plotmeans_pristine$Treatment)
mwu_NG_pristine_treatments

#data:  newgrowth_plotmeans_pristine$x and newgrowth_plotmeans_pristine$Treatment
#W = 0, p-value = 4.199e-08


------
  

#ANOVA = check to see if differences in new growth rates between treatments at polluted site = YES
res.aov_ng3 <-aov(x ~ Treatment , data = newgrowth_plotmeans[which(newgrowth_plotmeans$Site.type=='Impacted'),]) 
summary(res.aov_ng3)
#             Df  Sum Sq  Mean Sq F value  Pr(>F)   
#Treatment    2 0.02578 0.012892   5.848 0.00776 **
#Residuals   27 0.05952 0.002205                   
---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  

# Showing signif differences ^, so need to do post-hoc test to see individual treatment effects, probs tukey.
  
tukey_result_ng3aov <- TukeyHSD(res.aov_ng3)  
#tukey_result_ng3aov

#95% family-wise confidence level

#Fit: aov(formula = x ~ Treatment, data = newgrowth_plotmeans[which(newgrowth_plotmeans$Site.type == "Impacted"), ])

#$Treatment
#diff          lwr        upr     p adj
#Low N-Control  0.02666648 -0.025395991 0.07872895 0.4238724
#High N-Control 0.07107684  0.019014370 0.12313931 0.0060121 ** signif
#High N-Low N   0.04441036 -0.007652109 0.09647283 0.1055953

-----
#Kruskal wallis alternative = 

kruskal_impacted_treatments <- kruskal.test(x ~ Treatment, data = newgrowth_plotmeans_impacted)
kruskal_impacted_treatments 

#data:  x by Treatment
#Kruskal-Wallis chi-squared = 9.0797, df = 2, p-value = 0.01068

# Showing signif differences ^, so need to do post-hoc test to see individual treatment effects Dunn test
install.packages("dunn.test")
library(dunn.test)

dunn_result_ng_impacted_treatments <- dunn.test(newgrowth_plotmeans_impacted$x, g = newgrowth_plotmeans_impacted$Treatment, method = "bonferroni")

#Comparison of x by group                            
#(Bonferroni)                                  
#Col Mean-|
#  Row Mean |    Control     High N
---------+----------------------
#  High N |  -2.934682
#         |    0.0050*
#         |
#  Low N  |  -0.876593   2.058088
#         |     0.5711     0.0594

#alpha = 0.05
#Reject Ho if p <= alpha/2

------

#PRODUCTION RATES

#want to look at number of new blades appearing between treatment at each site
# at the impacted site, an 7.2% increase in the low N treatment of new blades and a 13.4% increase in High N
# at the pristine, the Low N caused a 12.2% increase in number of blades relative to control
  
--------

# want a graph that shows total number of new blades for both sites for treatment

frequencies_total <- newgrowth %>%
    group_by(Treatment, Site.type) %>%
    summarise(Frequency = n()) %>%
    ungroup()

levels(frequencies_total$Site.type)[levels(frequencies_total$Site.type)=="Non-polluted"]<- "Pristine"
levels(frequencies_total$Site.type)[levels(frequencies_total$Site.type)=="Polluted"]<- "Impacted"

newgrowth_production_total <-ggplot(frequencies_total) +
  aes(x = Treatment, y = Frequency, fill=Treatment) +       #total number of blades
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_brewer(palette = "Set2", direction = 1)+
  labs(x = "Treatment", y = "Total number of new blades") +  
  theme_classic() +
  facet_wrap(vars(Site.type), scales = "free_x") + 
  theme_bw() + # puts into 2 boxes/panes
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.text = element_text(size = 9),
        axis.title.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 9, face = "bold"))
newgrowth_production_total # show plot
ggsave("RO2_2022_newgrowth_production_total.png", width = 8, height = 5, dpi = 300)
-----
  
newgrowth_production_day <-ggplot(frequencies) +
   aes(x = Treatment, y = Frequency/14, fill=Treatment) +       #total number of blades/day 
       geom_bar(stat = "identity", position = position_dodge())+
       scale_fill_brewer(palette = "Set2", direction = 1)+
       labs(x = "Treatment", y = "Number of new blades"~(day^{"-1"})) +  
       theme_classic() +
       facet_wrap(vars(Site.type), scales = "free_x") + 
       theme_bw() + # puts into 2 boxes/panes
       theme(legend.position = "none") +
       theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                        axis.text = element_text(size = 9),
                        axis.title.y = element_text(size = 9, face = "bold"),
                        axis.title.x = element_text(size = 9, face = "bold"))
newgrowth_production_day # show plot
ggsave("RO2_2022_newgrowth_production_day.png", width = 8, height = 5, dpi = 300)

----

# want a graph that shows average number of new blades appearing across site and treatment  
  
averagecount <- aggregate(NG_Growth_rate_cm_day ~ Treatment + Site.type + Plot, data = newgrowth, FUN = length)
colnames(averagecount) <- c('Treatment', 'Site.type', 'Plot', 'Frequency')

levels(averagecount$Site.type)[levels(averagecount$Site.type)=="Non-polluted"]<- "Pristine"
levels(averagecount$Site.type)[levels(averagecount$Site.type)=="Polluted"]<- "Impacted"

levels(averagecount$Treatment)[levels(averagecount$Treatment)=="Medium"]<- "Low N"
levels(averagecount$Treatment)[levels(averagecount$Treatment)=="High"]<- "High N"

averagecount$Treatment <- factor(averagecount$Treatment, levels = c("Control", "Low N", "High N"))

---- 
# probably want to use this one 
newgrowth_averagecount <-ggplot(averagecount) +
  aes(x = Treatment, y = Frequency, fill=Treatment) +       #total number of blades/day
  geom_boxplot()+
  stat_boxplot(geom ='errorbar', width = 0.3)+ #adds the caps on the whiskers
  scale_fill_brewer(palette = "Set2", direction = 1)+
  labs(x = "Treatment", y = "Number of new blades"~(sheath^{"-1"}~day^{"-1"})) +  
  theme_classic() +
  facet_wrap(vars(Site.type), scales = "free_x") + 
  theme_bw() + # puts into 2 boxes/panes
  theme(legend.position = "none") +
  theme(text = element_text(family = "Calibri"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 11, face = "bold"))
newgrowth_averagecount # show plot
ggsave("RO2_2022_newgrowth_averagecount_day.png", width = 8, height = 5, dpi = 300)

---

# Want to know if these increases are significant = no. 
  
shapiro.test(averagecount$Frequency)

#data:  averagecount$Frequency
#W = 0.97228, p-value = 0.2857

qqnorm(averagecount$Frequency)
qqline(averagecount$Frequency)
hist(averagecount$Frequency)

#Pristine site
res.aov_ng4 <-aov(Frequency ~ Treatment , data = averagecount[which(averagecount$Site.type=='Pristine'),]) 
summary(res.aov_ng4)


#             Df Sum Sq Mean Sq F value Pr(>F)
#Treatment    1   16.2   16.20   0.738  0.402
#Residuals   18  395.0   21.94   


#Impacted site 
res.aov_ng5 <-aov(Frequency ~ Treatment , data = averagecount[which(averagecount$Site.type=='Impacted'),]) 
summary(res.aov_ng5)

#             Df Sum Sq Mean Sq F value Pr(>F)
#Treatment    2   33.1   16.53   0.647  0.531
#Residuals   27  689.6   25.54 


----


# was there a signif diff in the number of new blades between controls at sites?

res.aov_ng6 <- aov(Frequency ~ Site.type, data = averagecount[which(averagecount$Treatment=='Control'),])
summary(res.aov_ng6)
#             Df Sum Sq Mean Sq F value Pr(>F)  
#Site.type    1  68.45   68.45   4.555 0.0468 *
#  Residuals   18 270.50   15.03                 
---

#first create subset of newgrowth data for just control treatment.
control_ng_production <- subset(averagecount, Treatment == "Control")


# MWU between number of new blades appearing in the controls between sites = significantly different
wilcox.test(control_ng_production$Frequency ~ control_ng_production$Site)

#Results:
#Wilcoxon rank sum test with continuity correction

#data:  control_ng_data$Frequency by control_ng_data$Site
#W = 76.5, p-value = 0.04715
#alternative hypothesis: true location shift is not equal to 0


# Do same for Low N = not significantly different
Low_N_ng_production <- subset(averagecount, Treatment == "Low N")
wilcox.test(Low_N_ng_production$Frequency ~ Low_N_ng_production$Site)
#Results:

# 	Wilcoxon rank sum test with continuity correction

#data:  Low_N_ng_data$Frequency by Low_N_ng_data$Site
#W = 69, p-value = 0.1596
#alternative hypothesis: true location shift is not equal to 0



------

  
  
# sum for total cm growth data to work out % changes and differences
sum_leaf_growth_cm <- grass %>%
  group_by(Site.type, Treatment) %>%
  summarize(sum_leaf_growth_cm = sum(Growth.rate..cm.))

sum_leaf_new_growth_cm <- newgrowth %>%
  group_by(Site.type, Treatment) %>%
  summarize(sum_leaf_new_growth_cm = sum(NG.Growth.rate..cm.))


merged_summary_cm <-merge(sum_leaf_growth_cm, sum_leaf_new_growth_cm, by = c("Site.type", "Treatment"))
print(merged_summary_cm)

#     Site.type Treatment sum_leaf_growth sum_leaf_new_growth
#1 Non-polluted   Control          1628.0               501.0
#2 Non-polluted     Low N          1958.0               778.5
#3     Polluted   Control          1758.9               492.0
#4     Polluted    High N          1553.5               729.0
#5     Polluted     Low N          1793.5               630.0

---
# sum for total g data
sum_leaf_growth_g <- grass %>%
  group_by(Site.type, Treatment) %>%
  summarize(sum_leaf_growth_g = sum(Growth.rate..g.DW.))

sum_leaf_new_growth_g <- newgrowth %>%
  group_by(Site.type, Treatment) %>%
  summarize(sum_leaf_new_growth_g = sum(NG..g.DW.)) 


merged_summary_g <-merge(sum_leaf_growth_g, sum_leaf_new_growth_g, by = c("Site.type", "Treatment"))
print(merged_summary_g)
#     Site.type Treatment sum_leaf_growth sum_leaf_new_growth
#1 Non-polluted   Control         8.72152            2.684024
#2 Non-polluted     Low N        10.48963            4.170683
#3     Polluted   Control         9.42260            2.635808
#4     Polluted    High N         8.32260            3.905496
#5     Polluted     Low N         9.60890            3.375120

---------
  
# want to see if water depth influences leaf extension rates
# multiple linear regression model, where y = growth rates, plot means, with treatment, site and water depth as dependent.
# below can be ignored not significant influence
  
# first need to add in water depth data into grass_plotmeans data.
  
grass_plotmeans <- within(grass_plotmeans, Water_depth_m <- c(0.55,	0.66,	0.55,	0.56,	0.53,	0.48,	0.47,	0.54,	0.46,	0.57,	0.54,	0.57,	0.56,
                                                              0.54,	0.54,	0.5,	0.56,	0.5,	0.5,	0.51,	0.53,	0.56,	0.53,	0.49,	0.49,	0.54,
                                                              0.6,	0.49,	0.51,	0.45,	0.13,	0.17,	0.12,	0.16,	0.13,	0.14,	0.13,	0.19,	0.17,
                                                              0.17,	0.15,	0.19,	0.15,	0.11,	0.21,	0.2,	0.15,	0.18,	0.12,	0.18))

# fit the multiple linear regression model

# across both sites
model_WD <- lm(x ~ Water_depth_m, data = grass_plotmeans)
summary(model_WD)

Call:
  lm(formula = x ~ Water_depth_m, data = grass_plotmeans)

Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.145444 -0.047546  0.004138  0.057282  0.111791 

Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    0.42577    0.02212  19.249  < 2e-16 ***
#  Water_depth_m  0.22210    0.05221   4.254 9.67e-05 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.06869 on 48 degrees of freedom
#Multiple R-squared:  0.2738,	Adjusted R-squared:  0.2586 
#F-statistic:  18.1 on 1 and 48 DF,  p-value: 9.669e-05



#across both sites with treatment
model_MLR <- lm(x ~ Site.type + Treatment + Water_depth_m, data = grass_plotmeans)
summary(model_MLR)

Call:
  lm(formula = x ~ Site.type + Treatment + Water_depth_m, data = grass_plotmeans)

Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.140371 -0.037764  0.003334  0.036898  0.132288 

Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        0.421979   0.039531  10.675 6.47e-14 ***
#  Site.typePolluted  0.084810   0.090487   0.937  0.35363    
#TreatmentLow N     0.067074   0.019789   3.390  0.00147 ** 
#  TreatmentHigh N   -0.005116   0.025961  -0.197  0.84465    
#Water_depth_m      0.030572   0.235254   0.130  0.89718    
---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.06199 on 45 degrees of freedom
#Multiple R-squared:  0.4455,	Adjusted R-squared:  0.3962 
#F-statistic: 9.038 on 4 and 45 DF,  p-value: 1.908e-05




#for just polluted, using subsets
model_WD_polluted <- lm(x ~ Treatment + Water_depth_m, data = grass_plotmeans_polluted)
summary(model_WD_polluted)

Call:
  lm(formula = x ~ Treatment + Water_depth_m, data = grass_plotmeans_polluted)

Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.101557 -0.031184  0.003241  0.033391  0.093885 

Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      0.599781   0.125068   4.796 5.76e-05 ***
#  TreatmentLow N  -0.009114   0.024602  -0.370   0.7141    
#TreatmentHigh N -0.044343   0.024468  -1.812   0.0815 .  
#Water_depth_m   -0.072428   0.235044  -0.308   0.7604    
---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.05465 on 26 degrees of freedom
#Multiple R-squared:  0.1233,	Adjusted R-squared:  0.02209 
#F-statistic: 1.218 on 3 and 26 DF,  p-value: 0.3228



# for non-polluted
model_WD_nonpoll <- lm(x ~ Treatment + Water_depth_m, data = grass_plotmeans_nonpoll)
summary(model_WD_nonpoll)

Call:
  lm(formula = x ~ Treatment + Water_depth_m, data = grass_plotmeans_nonpoll)

Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.066621 -0.022981 -0.003029  0.023275  0.076157 

Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     0.32437    0.05531   5.864 1.88e-05 ***
#  TreatmentLow N  0.14024    0.01992   7.041 1.98e-06 ***
#  Water_depth_m   0.41804    0.35238   1.186    0.252    
---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.04369 on 17 degrees of freedom
#Multiple R-squared:  0.7683,	Adjusted R-squared:  0.741 
#F-statistic: 28.18 on 2 and 17 DF,  p-value: 4.002e-06