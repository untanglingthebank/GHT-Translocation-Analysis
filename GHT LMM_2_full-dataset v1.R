###Green Hound's-tongue Translocation Analysis###
##Which environmental variable best predicts plot success?

#1. Load in the data
data<-read.csv("Plot Data for R_3.csv", header=T)
head(data)
summary(data)

######
#2. Exploring Data 
##Box plots of Env Vars
install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)

#Boxplot by Plot - coloured by group
ggplot(data, aes(x=Plot, y=Xlost, colour=Group))+
  geom_boxplot()+theme_bw()

#Boxplot by Plot - coloured by cluster
ggplot(data, aes(x=Plot, y=Xlost, colour=Community.Cluster))+
  geom_boxplot()+theme_bw()

p_moi<-ggplot(data, aes(y=Mean.Moisture))+
  geom_boxplot()+theme_bw()
p_ph<-ggplot(data, aes(y=Mean.pH))+
  geom_boxplot()+theme_bw()
p_lux<-ggplot(data, aes(y=Lux))+
  geom_boxplot()+theme_bw()
p_comp<-ggplot(data, aes(y=Bulk.density))+
  geom_boxplot()+theme_bw()
p_rhod<-ggplot(data, aes(y=Rhod..Prox.))+
  geom_boxplot()+theme_bw()
p_org<-ggplot(data, aes(y=X..Organic.Matter))+
  geom_boxplot()+theme_bw()

install.packages("cowplot")
library(cowplot)
var_boxplots<-plot_grid(p_moi, p_ph, p_lux,p_comp,p_rhod,p_org, align=c("hv"), nrow=3, ncol=2)
var_boxplots

##Examining distribution of loss data
hist(data$X..Lost)
#Non-normal distribution - weighted towards 100% loss

######
#3. Exploring relationships
comp_scatter<-pairs(data[,9:15], pch=19, lower.panel=NULL)
#+ correlation panels
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

upper.panel<-function(x, y){
  points(x,y, pch = 19)
}

comp_scatter<-pairs(data[,9:14], lower.panel=panel.cor, upper.panel=upper.panel)

cor(data$X..Lost, data$Lux) #+0.0785

######
#4. Effect of soil type
##Plots
bp_soil<-ggplot(data, aes(y=X..Lost, x=Soil.Type))+
  geom_boxplot()+theme_bw()
bp_soil

(line_soil<-ggline(data, x = "Soil.Type", y = "X..Lost",
                  add = c("mean_se", "jitter"),
                  ylab = "% Lost", xlab = "Soil Type"))
#Silty clay at two extremes - 2 data points @ 0% and 100%
# Skewing the data - can exclude by subsetting if desirable

##K-W test
kruskal.test(X..Lost~Soil.Type, data=data)
#Kruskal-Wallis chi-squared = 0.98369, df = 5, p-value = 0.9639
#no significant difference

######
#5. Effect of organic matter
(organic_matter<-ggplot(data, aes(y=X..Lost, x=X..Organic.Matter))+
  geom_smooth()+theme_bw())
#include in linear model

######
#6. Difference between plot group in environmental variables?
groupdiff_moi<-ggplot(data, aes(y=Mean.Moisture, x=Group))+
  geom_boxplot()+theme_bw()
groupdiff_ph<-ggplot(data, aes(y=Mean.pH, x=Group))+
  geom_boxplot()+theme_bw()
groupdiff_lux<-ggplot(data, aes(y=Lux, x=Group))+
  geom_boxplot()+theme_bw()
groupdiff_comp<-ggplot(data, aes(y=Bulk.density, x=Group))+
  geom_boxplot()+theme_bw()
groupdiff_rhod<-ggplot(data, aes(y=Rhod..Prox., x=Group))+
  geom_boxplot()+theme_bw()
groupdiff_org<-ggplot(data, aes(y=X..Organic.Matter, x=Group))+
  geom_boxplot()+theme_bw()

(groupvar_boxplots<-plot_grid(groupdiff_moi, groupdiff_ph, groupdiff_lux,groupdiff_comp,
                             groupdiff_rhod,groupdiff_org, align=c("hv"), nrow=3, ncol=2))

#K-W tests
kruskal.test(Mean.Moisture~Group, data=data) 
#Kruskal-Wallis chi-squared = 0.50109, df = 2, p-value = 0.7784
kruskal.test(Mean.pH~Group, data=data) 
#Kruskal-Wallis chi-squared = 3.3106, df = 2, p-value = 0.191
kruskal.test(Lux~Group, data=data)
#Kruskal-Wallis chi-squared = 0.49788, df = 2, p-value = 0.7796
kruskal.test(Bulk.density~Group, data=data) 
#Kruskal-Wallis chi-squared = 8.6809, df = 2, p-value = 0.01303
kruskal.test(Rhod..Prox.~Group, data=data) 
#Kruskal-Wallis chi-squared = 3.2372, df = 2, p-value = 0.1982
kruskal.test(X..Organic.Matter~Group, data=data) 
#Kruskal-Wallis chi-squared = 3.1425, df = 2, p-value = 0.2078

##No sig diff between different groups - bar bulk density

######
#7. Difference by cluster?
clustdiff_moi<-ggplot(data, aes(y=Mean.Moisture, group=Community.Cluster, x=Community.Cluster))+
  geom_boxplot()+theme_bw()
clustdiff_ph<-ggplot(data, aes(y=Mean.pH, group=Community.Cluster, x=Community.Cluster))+
  geom_boxplot()+theme_bw()
clustdiff_lux<-ggplot(data, aes(y=Lux, group=Community.Cluster, x=Community.Cluster))+
  geom_boxplot()+theme_bw()
clustdiff_comp<-ggplot(data, aes(y=Bulk.density, group=Community.Cluster, x=Community.Cluster))+
  geom_boxplot()+theme_bw()
clustdiff_rhod<-ggplot(data, aes(y=Rhod..Prox., group=Community.Cluster, x=Community.Cluster))+
  geom_boxplot()+theme_bw()
clustdiff_org<-ggplot(data, aes(y=X..Organic.Matter, group=Community.Cluster, x=Community.Cluster))+
  geom_boxplot()+theme_bw()

(clustvar_boxplots<-plot_grid(clustdiff_moi, clustdiff_ph, clustdiff_lux,clustdiff_comp,
                             clustdiff_rhod,clustdiff_org, align=c("hv"), nrow=3, ncol=2))

#K-W tests
kruskal.test(Mean.Moisture~Community.Cluster, data=data) 
#Kruskal-Wallis chi-squared = 15.07, df = 8, p-value = 0.05781
kruskal.test(Mean.pH~Community.Cluster, data=data) 
#Kruskal-Wallis chi-squared = 15.419, df = 8, p-value = 0.05149
kruskal.test(Lux~Community.Cluster, data=data)
#Kruskal-Wallis chi-squared = 6.0226, df = 8, p-value = 0.6447
kruskal.test(Bulk.density~Community.Cluster, data=data) 
#Kruskal-Wallis chi-squared = 16.726, df = 8, p-value = 0.0331
kruskal.test(Rhod..Prox.~Community.Cluster, data=data) 
#Kruskal-Wallis chi-squared = 26.476, df = 8, p-value = 0.0008702
kruskal.test(X..Organic.Matter~Community.Cluster, data=data) 
#Kruskal-Wallis chi-squared = 12.345, df = 8, p-value = 0.1365

#Mean.Moisture, Mean.pH, Bulk.density, and Rhod.Prox all sig different between clusters

######
#8. Difference between deciduous and coniferous
(tree_boxplot<-ggplot(data, aes(y=X..Lost, x=Dominant.Community))+
  geom_boxplot()+theme_bw())

kruskal.test(X..Lost~Dominant.Community, data=data)
#Kruskal-Wallis chi-squared = 0.64639, df = 1, p-value = 0.4214
##No sig diff in % plants lost coniferous-deciduous forest

######
#9.Difference between moss levels - REMEMBER THIS IS QUALITATIVE
(moss_boxplot<-ggplot(data, aes(y=X..Lost, group=Moss.Cover, x=Moss.Cover))+
  geom_boxplot()+theme_bw())

kruskal.test(X..Lost~Moss.Cover, data=data)
#Kruskal-Wallis chi-squared = 2.0746, df = 3, p-value = 0.5571
##No sig diff in moss cover

######
#10. Building the first LMM
#Many if not all of these environmental variables are interrelated and will be interacting
#Sample size is limited

#Grouping factors:
#1/ Group
#2/ Cluster

##a. Centering the data
Xlost<-data$X..Lost

Mean.Moisture2<-scale(data$Mean.Moisture, center=T, scale=T)
Mean.pH2<-scale(data$Mean.pH, center=T, scale=T)
Lux2<-scale(data$Lux, center=T, scale=T)
BulkD2<-scale(data$Bulk.density, center=T, scale=T)
OrgMat2<-scale(data$X..Organic.Matter, center=T, scale=T)
RhodProx2<-scale(data$Rhod..Prox., center=T, scale=T)
#What else would we want to include? Moss cover?
Moss2<-scale(data$Moss.Cover, center=T, scale=T)

##b. Assumption testing###

#MOISTURE#
moi_lm<-lm(Xlost~Mean.Moisture2, data=data)
summary(moi_lm)
(prelim_moi<-ggplot(data, aes(x = Mean.Moisture, y=X..Lost)) +
    geom_point() +
    geom_smooth(method = "lm"))
#i. Relationship is linear
plot(moi_lm, which=1)
#not bad, expecially for (relatively) small sample
#ii. Y is normally distributed
plot(moi_lm, which=2)
#again not bad, but deviating tails - both fitted vs. residual plot and the qq plot indicate outliers
#iii. Homoscedasticity
plot(moi_lm, which=3)
#not bad

#Influential cases?
plot(moi_lm, which=4)
#3,12,31

(grpclus_moi<-ggplot(data, aes(x=Mean.Moisture2, y=Xlost, colour=Group))+
    geom_point(size=2)+
    theme_bw()+
    theme(legend.position = "none"))
#No apparent pattern

#pH#
pH_lm<-lm(Xlost~Mean.pH2, data=data)
summary(pH_lm)
(prelim_pH<-ggplot(data, aes(x = Mean.pH, y=X..Lost)) +
    geom_point() +
    geom_smooth(method = "lm"))
#i. Relationship is linear
plot(pH_lm, which=1)
# good
#ii. Y is normally distributed
plot(pH_lm, which=2)
# not bad, but deviating tails again
#iii. Homoscedasticity
plot(pH_lm, which=3)
#A pretty big kink around 73, likely a consequence of influential cases (31, 3)
#Overall line is roughly horizontal

#Influential cases?
plot(pH_lm, which=4)
#1,12,31

(grpclus_pH<-ggplot(data, aes(x=Mean.pH2, y=Xlost, colour=Group))+
    geom_point(size=2)+
    theme_bw()+
    theme(legend.position = "none"))
#No apparent pattern

#LUX#
lux_lm<-lm(Xlost~Lux2, data=data)
summary(lux_lm)

(prelim_lux<-ggplot(data, aes(x = Lux, y=X..Lost)) +
    geom_point() +
    geom_smooth(method = "lm"))

#i. Relationship is linear
plot(lux_lm, which=1)
# good
#ii. Y is normally distributed
plot(lux_lm, which=2)
# not bad, but deviating tails again
#iii. Homoscedasticity
plot(lux_lm, which=3)
#Heteroscedastic - transform data to build homoscedasticity

#Influential cases?
plot(lux_lm, which=4)
#3,12,31

(grpclus_lux<-ggplot(data, aes(x=Lux2, y=Xlost, colour=Group))+
    geom_point(size=2)+
    theme_bw()+
    theme(legend.position = "none"))
#No apparent pattern

#BULK DENSITY#
BD_lm<-lm(Xlost~BulkD2, data=data)
summary(BD_lm)
(prelim_BD<-ggplot(data, aes(x = Bulk.density, y=X..Lost)) +
    geom_point() +
    geom_smooth(method = "lm"))
#i. Relationship is linear
plot(BD_lm, which=1)
# relationship is NOT LINEAR between BD and % lost

#ii. Y is normally distributed
plot(BD_lm, which=2)
# not bad, but deviating tails again
#iii. Homoscedasticity
plot(BD_lm, which=3)
#Heteroscedastic

#Influential cases?
plot(BD_lm, which=4)
#1,12,31

(grpclus_BD<-ggplot(data, aes(x=BulkD2, y=Xlost, colour=Group))+
    geom_point(size=2)+
    theme_bw()+
    theme(legend.position = "right"))
#Bit more clustered - D group and NW group


#ORGANIC MATTER#
orgm_lm<-lm(Xlost~OrgMat2, data=data)
summary(orgm_lm)
(prelim_orgm<-ggplot(data, aes(x = X..Organic.Matter, y=X..Lost)) +
    geom_point() +
    geom_smooth(method = "lm"))

#i. Relationship is linear
plot(orgm_lm, which=1)
#good, roughly linear
#ii. Y is normally distributed
plot(orgm_lm, which=2)
# not bad, but deviating tails again
#iii. Homoscedasticity
plot(orgm_lm, which=3)
#Roughly homoscedastic, with slight central kink

#Influential cases?
plot(orgm_lm, which=4)
#12,29,31

(grpclus_orgm<-ggplot(data, aes(x=OrgMat2, y=Xlost, colour=Group))+
    geom_point(size=2)+
    theme_bw()+
    theme(legend.position = "right"))
#No apparent pattern


#RHODODENDRON PROXIMITY#
rhod_lm<-lm(Xlost~RhodProx2, data=data)
summary(rhod_lm)
(prelim_rhod<-ggplot(data, aes(x = Rhod..Prox., y=X..Lost)) +
    geom_point() +
    geom_smooth(method = "lm"))

#i. Relationship is linear
plot(rhod_lm, which=1)
#good, linear
#ii. Y is normally distributed
plot(rhod_lm, which=2)
# not bad, but deviating tails again - perhaps more so than the above data
hist(RhodProx2)
#Peaks at extremes

#iii. Homoscedasticity
plot(rhod_lm, which=3)
#Roughly homoscedastic - should quantify heteroscedasticity for each explanatory variable

#Influential cases?
plot(rhod_lm, which=4)
#12,23,31

(grpclus_rhod<-ggplot(data, aes(x=RhodProx2, y=Xlost, colour=Group))+
    geom_point(size=2)+
    theme_bw()+
    theme(legend.position = "right"))
#A weird one because of the very few levels, and the skew towards positive
#All W group in >50 and so observations within-group are not independent of being in that group


#MOSS COVER#
moss_lm<-lm(Xlost~Moss2, data=data)
summary(moss_lm)
(prelim_moss<-ggplot(data, aes(x = Moss.Cover, y=X..Lost)) +
    geom_point() +
    geom_smooth(method = "lm"))

#i. Relationship is linear
plot(moss_lm, which=1)
#good, roughly linear
#ii. Y is normally distributed
plot(moss_lm, which=2)
# not bad, but deviating tails again
#iii. Homoscedasticity
plot(moss_lm, which=3)
#Homoscedastic

#Influential cases?
plot(moss_lm, which=4)
#3,12,31

#Are within-group observations correlated?
#Cluster point by colour for each explanatory variable
(grpclus_moss<-ggplot(data, aes(x=Moss2, y=Xlost, colour=Group))+
    geom_point(size=2)+
    theme_bw()+
    theme(legend.position = "none"))
#Again, W group tends towards positive


##SUMMARY - KEY POINTS
#1/ Relationship between BD and % lost is not linear
#2/Deviating tails on all qq plots - no variable is perfectly normal
#3/ Bulk density, moss cover, rhod. proximity are not independent of their groups
#4/ Bulk density and lux heteroscedastic


#Summary plot
(lm.summ.plot<-plot_grid(prelim_moi,prelim_BD,prelim_lux,prelim_moss,prelim_orgm,prelim_pH,prelim_rhod, align=c("hv"), 
                        nrow=4,ncol=2))

#####NEWSFLASH: LINEAR MODELLING CAN ACCOMMODATE CURVES!####
#Let's go ahead with LMM

#d. Building the linear mixed model

library(lme4)
##FIXED EFFECTS: Moisture, pH, lux, BD, % organic matter, rhodo proximity, group, type
##RANDOM EFFECTS: plot group, group, type, community cluster
#Golden rule: random effect has AT LEAST 5 LEVELS - this excludes group and type....
#Remodelled Group to be a blend of cluster and group = WOODLAND
#Factors with fewer than 5 can be fitted as fixed effects

PlotGroup<-data$Plot.Group
Wood<-data$Woodland
ComClus<-data$Community.Cluster
PlotType<-data$Type

full.LMM<-lmer(Xlost~Mean.Moisture2+Mean.pH2+Lux2+BulkD2+OrgMat2+RhodProx2+PlotType+(1|PlotGroup)+(1|Wood)+(1|ComClus))
summary(full.LMM)

isSingular(full.LMM, tol=1e-4) #TRUE - I think this must be the PlotGroup and ComClus random effects

#Ignoring singularity for now.....

##Random effects:
#Groups    Name        Variance Std.Dev.
#PlotGroup (Intercept)   0.0     0.00       ###effectively 0 variance (isSingular) for PlotGroup and ComClus
#ComClus   (Intercept)   0.0     0.00   
#Wood      (Intercept) 105.6    10.27   
#Residual              568.3    23.84   
#Number of obs: 31, groups:  PlotGroup, 24; ComClus, 9; Wood, 5

#Wood captures the most variation
105.6/(105.6+568.3)*100 #15.66
#But still only 15.7% of variation remaining after the variation explained by fixed effects 
# - something else (important) going on

#Test assumptions:
plot(full.LMM) #no patterns evident
qqnorm(resid(full.LMM))
qqline(resid(full.LMM)) #fairly good, not amazing but not massively deviating from line

#On the fixed effects:
#                Estimate Std. Error t value
#(Intercept)      65.696      7.278   9.027
#Mean.Moisture2  -13.681      7.705  -1.776
#Mean.pH2        -15.728      7.971  -1.973
#Lux2             -6.684      5.263  -1.270
#BulkD2          -13.041      7.713  -1.691
#OrgMat2         -12.182      8.192  -1.487
#RhodProx2        -7.422      6.697  -1.108
#PlotTypeSeed     19.514     10.029   1.946

#RhodProx and Lux have comparatively smaller estimates


##NESTED RANDOM EFFECTS
#All plots occur within woods, and same plot groupings are always in the same wood and in the same cluster
#So the structure is nested
full.LMM_nested<-lmer(Xlost~Mean.Moisture2+Mean.pH2+Lux2+BulkD2+OrgMat2+RhodProx2+PlotType+
                        (1|PlotGroup/Wood)+(1|PlotGroup/ComClus))
summary(full.LMM_nested)

##FIXED EFFECT SELECTION BASED ON AICc
#Nested:
full.LMM_nested<-lmer(Xlost~Mean.Moisture2+Mean.pH2+Lux2+BulkD2+OrgMat2+RhodProx2+PlotType+
                        (1|PlotGroup/Wood)+(1|PlotGroup/ComClus), REML=F)
full.LMM.nomoi_n<-lmer(Xlost~Mean.pH2+Lux2+BulkD2+OrgMat2+RhodProx2+PlotType+
                        (1|PlotGroup/Wood)+(1|PlotGroup/ComClus), REML=F)
full.LMM.nopH_n<-lmer(Xlost~Mean.Moisture2+Lux2+BulkD2+OrgMat2+RhodProx2+PlotType+
                        (1|PlotGroup/Wood)+(1|PlotGroup/ComClus), REML=F) #no singuler
full.LMM.nolux_n<-lmer(Xlost~Mean.Moisture2+Mean.pH2+BulkD2+OrgMat2+RhodProx2+PlotType+
                        (1|PlotGroup/Wood)+(1|PlotGroup/ComClus), REML=F)
full.LMM.noBD_n<-lmer(Xlost~Mean.Moisture2+Mean.pH2+Lux2+OrgMat2+RhodProx2+PlotType+
                        (1|PlotGroup/Wood)+(1|PlotGroup/ComClus), REML=F)
full.LMM.noOM_n<-lmer(Xlost~Mean.Moisture2+Mean.pH2+Lux2+BulkD2+RhodProx2+PlotType+
                        (1|PlotGroup/Wood)+(1|PlotGroup/ComClus), REML=F)
full.LMM.noRP_n<-lmer(Xlost~Mean.Moisture2+Mean.pH2+Lux2+BulkD2+OrgMat2+PlotType+
                        (1|PlotGroup/Wood)+(1|PlotGroup/ComClus), REML=F)
full.LMM.noPlotT_n<-lmer(Xlost~Mean.Moisture2+Mean.pH2+Lux2+BulkD2+OrgMat2+RhodProx2+
                        (1|PlotGroup/Wood)+(1|PlotGroup/ComClus), REML=F)

#COMPARING FIXED EFFECTS
#Use REML=FALSE, REML is good for generating the model but when comparing models ML is better
install.packages("MuMIn")
library(MuMIn)
AICc(full.LMM_nested,full.LMM.noBD_n,full.LMM.nolux_n,full.LMM.nomoi_n,full.LMM.nopH_n,full.LMM.noOM_n,full.LMM.noPlot_n,
     full.LMM.noRP_n)
#                    df     AICc
#full.LMM_nested   13 326.2701
#full.LMM.noBD_n   12 322.3075
#full.LMM.nolux_n  12 321.2798
#full.LMM.nomoi_n  12 323.2482
#full.LMM.nopH_n   12 324.4424
#full.LMM.noOM_n   12 322.4211
#full.LMM.noPlotT_n 12 324.5623
#full.LMM.noRP_n   12 322.6110

#Range: 321.2798 - 326.1701 - =~ 5 units difference
#All quite similar, full model is worst

#Removing next layer of fixed effects:
full.LMM.noL.BD_n<-lmer(Xlost~Mean.Moisture2+Mean.pH2+OrgMat2+RhodProx2+PlotType+
                         (1|PlotGroup/Wood)+(1|PlotGroup/ComClus), REML=F)
full.LMM.noL.RP_n<-lmer(Xlost~Mean.Moisture2+Mean.pH2+BulkD2+OrgMat2+PlotType+
                        (1|PlotGroup/Wood)+(1|PlotGroup/ComClus), REML=F)
full.LMM.noL.OM_n<-lmer(Xlost~Mean.Moisture2+Mean.pH2+BulkD2+RhodProx2+PlotType+
                        (1|PlotGroup/Wood)+(1|PlotGroup/ComClus), REML=F)
full.LMM.noL.BD.RP.OM_n<-lmer(Xlost~Mean.Moisture2+Mean.pH2+PlotType+
                        (1|PlotGroup/Wood)+(1|PlotGroup/ComClus), REML=F)
AICc(full.LMM.noL.BD_n,full.LMM.noL.RP_n,full.LMM.noL.OM_n,full.LMM.noL.BD.RP.OM_n)

#                       df  AICc  
#full.LMM.noL.BD_n       11 317.9209
#full.LMM.noL.RP_n       11 317.8205
#full.LMM.noL.OM_n       11 318.0622
#full.LMM.noL.BD.RP.OM_n  9 310.3005
#Model containing none of these variables is best

#Keep going...
lmm2.nomoi_n<-lmer(Xlost~+Mean.pH2+PlotType+(1|PlotGroup/Wood)+(1|PlotGroup/ComClus), REML=F)
lmm2.nopH_n<-lmer(Xlost~Mean.Moisture2+PlotType+(1|PlotGroup/Wood)+(1|PlotGroup/ComClus), REML=F)
lmm2.noPlotT_n<-lmer(Xlost~Mean.Moisture2+Mean.pH2+(1|PlotGroup/Wood)+(1|PlotGroup/ComClus), REML=F)
lmm2.onlyRE_n<-lmer(Xlost~(1|PlotGroup/Wood)+(1|PlotGroup/ComClus), REML=F)
AICc(lmm2.nomoi_n,lmm2.nopH_n,lmm2.noPlotT_n,lmm2.onlyRE_n)

#               df     AICc
#lmm2.nomoi_n    8 307.7240
#lmm2.nopH_n     8 307.5670
#lmm2.noPlotT_n  8 313.2145
#lmm2.onlyRE_n   6 306.9961

#So... best models, containing fixed effects, are those with plot type and either moisture or pH
#Plot Type accounts for a significant amount of variation*********

##################
#Un-nested:
full.LMM<-lmer(Xlost~Mean.Moisture2+Mean.pH2+Lux2+BulkD2+OrgMat2+RhodProx2+PlotType+(1|PlotGroup)+(1|Wood)+(1|ComClus))
full.LMM.nomoi<-lmer(Xlost~Mean.pH2+Lux2+BulkD2+OrgMat2+RhodProx2+PlotType+(1|PlotGroup)+(1|Wood)+(1|ComClus))
full.LMM.nopH<-lmer(Xlost~Mean.Moisture2+Lux2+BulkD2+OrgMat2+RhodProx2+PlotType+(1|PlotGroup)+(1|Wood)+(1|ComClus))
full.LMM.nolux<-lmer(Xlost~Mean.Moisture2+Mean.pH2+BulkD2+OrgMat2+RhodProx2+PlotType+(1|PlotGroup)+(1|Wood)+(1|ComClus))
full.LMM.noBD<-lmer(Xlost~Mean.Moisture2+Mean.pH2+Lux2+OrgMat2+RhodProx2+PlotType+(1|PlotGroup)+(1|Wood)+(1|ComClus))
full.LMM.noOM<-lmer(Xlost~Mean.Moisture2+Mean.pH2+Lux2+BulkD2+RhodProx2+PlotType+(1|PlotGroup)+(1|Wood)+(1|ComClus))
full.LMM.noRP<-lmer(Xlost~Mean.Moisture2+Mean.pH2+Lux2+BulkD2+OrgMat2+PlotType+(1|PlotGroup)+(1|Wood)+(1|ComClus))
full.LMM.noPlot<-lmer(Xlost~Mean.Moisture2+Mean.pH2+Lux2+BulkD2+OrgMat2+RhodProx2+(1|PlotGroup)+(1|Wood)+(1|ComClus))

AICc(full.LMM,full.LMM.noBD,full.LMM.nolux,full.LMM.nomoi,full.LMM.nopH,full.LMM.noOM,
     full.LMM.noPlot,full.LMM.noRP)

#                   df     AICc
#full.LMM           12 277.3951
#full.LMM.noBD      11 280.2427
#full.LMM.nolux     11 278.5555
#full.LMM.nomoi     11 280.8309
#full.LMM.nopH      11 281.6117
#full.LMM.noOM      11 280.1202
#full.LMM.noPlot    11 282.0373
#full.LMM.noRP      11 278.7505

#Range: 277.3951 - 282.0373
#Again ~5 difference in units, very similar

#Next stage of fixed effect removal:
lmm2.noL.RP<-lmer(Xlost~Mean.Moisture2+Mean.pH2+BulkD2+OrgMat2+PlotType+(1|PlotGroup)+(1|Wood)+(1|ComClus))
lmm2.noL.Moi.pH<-lmer(Xlost~BulkD2+OrgMat2+RhodProx2+PlotType+(1|PlotGroup)+(1|Wood)+(1|ComClus))


AICc(lmm2.noL.RP, lmm2.noL.Moi.pH)
#                df     AICc
#lmm2.noL.RP     10 280.5275
#lmm2.noL.Moi.pH  9 282.9229

#With variables unnested, the best model takes takes into account all variables
#All variables having equally small effect?
#Removal of light is effectively the same

###SO MUCH VARIATION IS NOT EXPLAINED BY THESE VARIABLES THAT THEIR INCLUSION OR EXCLUSION FROM
# THE MODEL IS EFFECTIVELY IRRELEVANT
#Bulk density still largest effect size WHEN IN NON-MIXED LINEAR MODEL

#Plot type (translocated plants vs seeds) is important and accounts for the most variation of any variable
#*See other R file for splitting of plant plots and seed plots


####TO DO###
#1. K-W assumptions met?
#2. Is % lost the best output variable?
#3. Has moss variation been captured?
#4. Exclude outliers and re-run analysis
#5. Address failed assumptions most notably heteroscedasticity
#6. Fit random slopes