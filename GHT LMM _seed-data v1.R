###In model outputs and AICc, sig amount of variation was being explained by Plot Type
#i.e. whether it was a seed plot or plant plot
#This means that effects of variables on each may be different, and we should examine each separately

#Here we will change the response variable from % lost - not a great measure of seedlings
#Few of the 1000s sown sprout 
#Response variable changed to no. sprouting

#A. Reading in the data
seed<-read.csv("Plot Data - seeds.csv", header=T)
head(seed)
#N. sprouting is response variable
N.sprout<-seed$N.Sprouting...July

#Exploring N sprouting
library(ggplot2)
(Nsprout_boxplot<-ggplot(seed, aes(y=N.sprout, x=Plot, group=Plot))+
  geom_boxplot()+theme_bw())
(Nsprout_scatter<-ggplot(seed, aes(y=N.sprout, x= Plot, group=Plot))+
    geom_point()+theme_bw())
#N.sprouting appears to vary between plots

seed_moi<-ggplot(seed, aes(y=Mean.Moisture))+
  geom_boxplot()+theme_bw()
seed_ph<-ggplot(seed, aes(y=Mean.pH))+
  geom_boxplot()+theme_bw()
seed_lux<-ggplot(seed, aes(y=Lux))+
  geom_boxplot()+theme_bw()
seed_comp<-ggplot(seed, aes(y=Bulk.density))+
  geom_boxplot()+theme_bw()
seed_rhod<-ggplot(seed, aes(y=Rhod..Prox.))+
  geom_boxplot()+theme_bw()
seed_org<-ggplot(seed, aes(y=X..Organic.Matter))+
  geom_boxplot()+theme_bw()

library(cowplot)
seed_boxplots<-plot_grid(seed_moi, seed_ph, seed_lux,seed_comp,seed_rhod,
                         seed_org, align=c("hv"), nrow=3, ncol=2)
seed_boxplots
#There is variation in the explanatory variables
#Does any of this variation explain variation in N.sprouting?

#1. Does soil type have an effect?
seed_soil_box<-ggplot(seed, aes(y=N.sprout, x=Soil.Type))+
  geom_boxplot()+theme_bw()
seed_soil_box

library(ggpubr)
(line_soil.seed<-ggline(seed, x = "Soil.Type", y = "N.Sprouting...July",
                       add = c("mean_se", "jitter"),
                       ylab = "No. Sprouting", xlab = "Soil Type"))


##K-W test
kruskal.test(N.sprout~Soil.Type, data=seed)
#Kruskal-Wallis chi-squared = 5.7879, df = 4, p-value = 0.2156

#no effect of soil type

#2. Does organic matter have an effect?
seed.OM<-ggplot(seed, aes(y=N.sprout, x=X..Organic.Matter))+
  geom_smooth()+theme_bw()
seed.OM
#OM % @ 0 and 80 equates to v similar % lost i.e. ~95%
OM_lm.seed<-lm(N.sprout~seed$X..Organic.Matter)
summary(OM_lm.seed)
#unlikely to be important - don't include in linear model

#3. Difference between plot group in environmental variables?
groupdiff_moi.seed<-ggplot(seed, aes(y=Mean.Moisture, x=Group))+
  geom_boxplot()+theme_bw()
groupdiff_ph.seed<-ggplot(seed, aes(y=Mean.pH, x=Group))+
  geom_boxplot()+theme_bw()
groupdiff_lux.seed<-ggplot(seed, aes(y=Lux, x=Group))+
  geom_boxplot()+theme_bw()
groupdiff_comp.seed<-ggplot(seed, aes(y=Bulk.density, x=Group))+
  geom_boxplot()+theme_bw()
groupdiff_rhod.seed<-ggplot(seed, aes(y=Rhod..Prox., x=Group))+
  geom_boxplot()+theme_bw()
groupdiff_org.seed<-ggplot(seed, aes(y=X..Organic.Matter, x=Group))+
  geom_boxplot()+theme_bw()

groupvar_boxplots.seed<-plot_grid(groupdiff_moi.seed, groupdiff_ph.seed, groupdiff_lux.seed,
                                  groupdiff_comp.seed,groupdiff_rhod.seed,groupdiff_org.seed, 
                                  align=c("hv"), nrow=3, ncol=2)
groupvar_boxplots.seed

#K-W tests
kruskal.test(Mean.Moisture~Group, data=seed) 
#Kruskal-Wallis chi-squared = 2.0417, df = 1, p-value = 0.153
kruskal.test(Mean.pH~Group, data=seed) 
#Kruskal-Wallis chi-squared = 1.0417, df = 1, p-value = 0.3074
kruskal.test(Lux~Group, data=seed)
#Kruskal-Wallis chi-squared = 0.375, df = 1, p-value = 0.5403
kruskal.test(Bulk.density~Group, data=seed) 
#Kruskal-Wallis chi-squared = 6.055, df = 1, p-value = 0.01387
kruskal.test(Rhod..Prox.~Group, data=seed) 
#Kruskal-Wallis chi-squared = 0.18519, df = 1, p-value = 0.667
kruskal.test(X..Organic.Matter~Group, data=seed) 
#Kruskal-Wallis chi-squared = 6, df = 1, p-value = 0.01431

#Again bulk density is significantly different, as well as OM, no others
seed.lost<-(seed$X..Lost)

BD_lm.seed<-lm(N.sprout~seed$Bulk.density)
summary(BD_lm.seed)

#4.How about difference by cluster?
clustdiff_moi.seed<-ggplot(seed, aes(y=Mean.Moisture, group=Community.Cluster, x=Community.Cluster))+
  geom_boxplot()+theme_bw()
clustdiff_ph.seed<-ggplot(seed, aes(y=Mean.pH, group=Community.Cluster, x=Community.Cluster))+
  geom_boxplot()+theme_bw()
clustdiff_lux.seed<-ggplot(seed, aes(y=Lux, group=Community.Cluster, x=Community.Cluster))+
  geom_boxplot()+theme_bw()
clustdiff_comp.seed<-ggplot(seed, aes(y=Bulk.density, group=Community.Cluster, x=Community.Cluster))+
  geom_boxplot()+theme_bw()
clustdiff_rhod.seed<-ggplot(seed, aes(y=Rhod..Prox., group=Community.Cluster, x=Community.Cluster))+
  geom_boxplot()+theme_bw()
clustdiff_org.seed<-ggplot(seed, aes(y=X..Organic.Matter, group=Community.Cluster, x=Community.Cluster))+
  geom_boxplot()+theme_bw()

(clustvar_boxplots.seed<-plot_grid(clustdiff_moi.seed, clustdiff_ph.seed, clustdiff_lux.seed,clustdiff_comp.seed,
                                  clustdiff_rhod.seed,clustdiff_org.seed, align=c("hv"), nrow=3, ncol=2))



#K-W tests
kruskal.test(Mean.Moisture~Community.Cluster, data=seed) 
#Kruskal-Wallis chi-squared = 5.6212, df = 4, p-value = 0.2293
kruskal.test(Mean.pH~Community.Cluster, data=seed) 
#Kruskal-Wallis chi-squared = 4.7348, df = 4, p-value = 0.3156
kruskal.test(Lux~Community.Cluster, data=seed)
#Kruskal-Wallis chi-squared = 2.2576, df = 4, p-value = 0.6885
kruskal.test(Bulk.density~Community.Cluster, data=seed) 
#Kruskal-Wallis chi-squared = 7.1693, df = 4, p-value = 0.1272
kruskal.test(Rhod..Prox.~Community.Cluster, data=seed) 
#Kruskal-Wallis chi-squared = 9.5455, df = 4, p-value = 0.04882
kruskal.test(X..Organic.Matter~Community.Cluster, data=seed) 
#Kruskal-Wallis chi-squared = 7.2045, df = 4, p-value = 0.1255


#5. Difference between woodland types?
(tree_boxplot.seed<-ggplot(seed, aes(y=N.sprout, x=Dominant.Community))+
  geom_boxplot()+theme_bw())

kruskal.test(N.sprout~Dominant.Community, data=seed)
#Kruskal-Wallis chi-squared = 0.041667, df = 1, p-value = 0.8383
#no significant difference

#6. Moss?
(moss_boxplot.seed<-ggplot(seed, aes(y=N.sprout, group=Moss.Cover, x=Moss.Cover))+
  geom_boxplot()+theme_bw())

kruskal.test(N.sprout~Moss.Cover, data=seed)
#Kruskal-Wallis chi-squared = 0.70909, df = 3, p-value = 0.8711
#no sig diff

#7. Linear mixed modelling
#a. Centring the data
N.sprout<-seed$N.Sprouting...July

seed.Mean.Moisture2<-scale(seed$Mean.Moisture, center=T, scale=T)
seed.Mean.pH2<-scale(seed$Mean.pH, center=T, scale=T)
seed.Lux2<-scale(seed$Lux, center=T, scale=T)
seed.BulkD2<-scale(seed$Bulk.density, center=T, scale=T)
seed.OrgMat2<-scale(seed$X..Organic.Matter, center=T, scale=T)
seed.RhodProx2<-scale(seed$Rhod..Prox., center=T, scale=T)
seed.Moss2<-scale(seed$Moss.Cover, center=T, scale=T)

#b. Assumption testing

#MOISTURE#
moi_lm.seed<-lm(X..Lost~seed.Mean.Moisture2, data=seed)
summary(moi_lm.seed)
(prelim_moi.seed<-ggplot(seed, aes(x = Mean.Moisture, y=X..Lost)) +
    geom_point() +
    geom_smooth(method = "lm"))
#i. Relationship is linear
plot(moi_lm.seed, which=1) #not bad, expecially for (relatively) small sample

#ii. Y is normally distributed
plot(moi_lm.seed, which=2) #again not bad, but deviating tails - both fitted vs. residual plot and the qq plot indicate outliers

#iii. Homoscedasticity
plot(moi_lm.seed, which=3) #variance not equal

#pH#
pH_lm.seed<-lm(X..Lost~seed.Mean.pH2, data=seed)
summary(pH_lm.seed)
(prelim_moi.seed<-ggplot(seed, aes(x = Mean.pH, y=X..Lost)) +
    geom_point() +
    geom_smooth(method = "lm"))

#i. Relationship is linear
plot(pH_lm.seed, which=1) #relationship not exactly linear, but deviation in trend caused by dp7
#dp7 has by far the lowest % lost, an outlier but also an important case
(prelim_pH.seed<-ggplot(seed, aes(x = Mean.pH, y=X..Lost)) +
    geom_point() +
    geom_smooth(method = "lm"))
#doesn't shift the line here, so let's assume ok

#ii. Y is normally distributed
plot(pH_lm.seed, which=2) #not bad

#iii. Homoscedasticity
plot(pH_lm.seed, which=3) #variance not equal, quite wiggly but again skewed by 7

#LUX#
lux_lm.seed<-lm(X..Lost~seed.Lux2, data=seed)
summary(moi_lm.seed)
(prelim_lux.seed<-ggplot(seed, aes(x = Lux, y=X..Lost)) +
    geom_point() +
    geom_smooth(method = "lm"))
#slight relationship between lux and % lost - higher lux=less lost

#i. Relationship is linear
plot(lux_lm.seed, which=1) #not linear - dp2 and dp7 pulling a kink in the line

#ii. Y is normally distributed
plot(lux_lm.seed, which=2) #again not bad, but (slightly) deviating tails 

#iii. Homoscedasticity
plot(lux_lm.seed, which=3) #variance not equal - lots of points impacting

#BULK DENSITY#
BD_lm.seed<-lm(X..Lost~seed.BulkD2, data=seed)
summary(BD_lm.seed)
(prelim_BD.seed<-ggplot(seed, aes(x = Bulk.density, y=X..Lost)) +
    geom_point() +
    geom_smooth(method = "lm"))

#i. Relationship is linear
plot(BD_lm.seed, which=1) #not bad

#ii. Y is normally distributed
plot(BD_lm.seed, which=2) #pretty good
#again not bad, but deviating tails - both fitted vs. residual plot and the qq plot indicate outliers

#iii. Homoscedasticity
plot(BD_lm.seed, which=3) #variance mostly equal, bar kink towards end of tail

#ORGANIC MATTER#
OM_lm.seed<-lm(X..Lost~seed.OrgMat2, data=seed)
summary(OM_lm.seed)
(prelim_OM.seed<-ggplot(seed, aes(x = X..Organic.Matter, y=X..Lost)) +
    geom_point() +
    geom_smooth())
#slight relationship here - lower % OM=lower % lost
cor(seed$X..Lost,seed$X..Organic.Matter) #0.2731291

#i. Relationship is linear
plot(OM_lm.seed, which=1) #very kinky - not linear

#ii. Y is normally distributed
plot(OM_lm.seed, which=2) #pretty good

#iii. Homoscedasticity
plot(OM_lm.seed, which=3) #variance not equal at all

#RHOD PROX#
RP_lm.seed<-lm(X..Lost~seed.RhodProx2, data=seed)
summary(RP_lm.seed)
(prelim_RP.seed<-ggplot(seed, aes(x = Rhod..Prox., y=X..Lost)) +
    geom_point() +
    geom_smooth(method="lm"))
#inverted U curve - but spread at >50 causes the upward trend

###
#Removing >50 samples:
#A tiny sample size, but what happens?
no0RP<-read.csv("Plot Data - seeds no50+.csv", header=T)
seed.RP.no50<-scale(no0RP$Rhod..Prox., center=T, scale=T)

RP.no50.lm<-lm(X..Lost~seed.RP.no50, data=no0RP)
summary(RP.no50.lm)
(prelim_no50.RP.seed<-ggplot(no0RP, aes(x = Rhod..Prox., y=X..Lost)) +
    geom_point() +
    geom_smooth(method="lm"))

#Assumptions:
#1. Relationship is linear
plot(RP.no50.lm, which=1) #not at all linear

#2. Y is normally distributed
plot(RP.no50.lm, which=2) #yes, follows dashed line, bar dp3 (=big outlier)
hist(no0RP$X..Lost) #right skew

#3. Homoscedasticity (equal variance in residuals)
plot(RP.no50.lm, which=3) #not at all homoscedastic

#Very small sample size is messing things up here
#But simply put there appears to be a relationship, even if lm assumptions hugely 
# violated
cor(no0RP$X..Lost, no0RP$Rhod..Prox.) #-0.9477377
#but not parametric, so:
cor(no0RP$X..Lost, no0RP$Rhod..Prox., method="spearman") #-0.5002164
#still indicating negative correlation

ggplot(no0RP, aes(x=Rhod..Prox., y=X..Lost))+
  geom_point()+theme_bw()

##Removing 'extreme' (x 2) cases
noxtrRP<-read.csv("Plot Data - seeds no extremes.csv", header=T)
seed.RP.noxtr<-scale(noxtrRP$Rhod..Prox., center=T, scale=T)

RP.noxtr.lm<-lm(X..Lost~seed.RP.noxtr, data=noxtrRP)
summary(RP.noxtr.lm)
(prelim_noxtr.RP.seed<-ggplot(noxtrRP, aes(x = Rhod..Prox., y=X..Lost)) +
    geom_point() +
    geom_smooth(method="lm"))+theme_bw()+
  annotate(geom="text", x=30, y=100, label="Spearman's Rho = - 0.42",
           color="red")

plot(RP.noxtr.lm)
#Linear relationship
#Y not normally distributed (see below)
#Heteroscedastic (see below)

cor(noxtrRP$X..Lost, noxtrRP$Rhod..Prox., method="spearman") #-0.4189412
#still indicating moderate negative correlation

ggplot(noxtrRP, aes(x=Rhod..Prox., y=X..Lost))+
  geom_point()+theme_bw()

###

##Back to full data assumption testing
#i. Relationship is linear
plot(RP_lm.seed, which=1) #big single kink - not linear

#ii. Y is normally distributed
plot(RP_lm.seed, which=2) #pretty good

#iii. Homoscedasticity
plot(RP_lm.seed, which=3) #variance not equal at all, no kinks but big downward trend (linear)

#MOSS COVER#
MC_lm.seed<-lm(X..Lost~seed.Moss2, data=seed)
summary(MC_lm.seed)
(prelim_MC.seed<-ggplot(seed, aes(x = Moss.Cover, y=X..Lost)) +
    geom_point() +
    geom_smooth())
###ON SECOND THOUGHT: can ordinal data be used in LMM? (might need GLMM)

#Some tests:
boxplot(X..Lost~Moss.Cover, data=seed)
hist(seed$Moss.Cover) #non-normal
kruskal.test(X..Lost~Moss.Cover, data=seed)
#no sig diff: Kruskal-Wallis chi-squared = 0.70909, df = 3, p-value = 0.8711

plot(MC_lm.seed, which=1) #linear
plot(MC_lm.seed, which=2) # normal-ish
plot(MC_lm.seed, which=3) #homoscedastic


#SUMMARY
#1. Heteroscedasticity not met: moisture, pH, lux, OM, RP
#2. Non-linear relationship: ?pH, lux, organic matter, RP

###SUMMARISING DP7 - influential case
#Higher pH
#Middling org matter cover

###Handling heteroscedasticity: 
#Can use a Breush-Pagan test to quantitatively examine heteroscedasticity
install.packages("lmtest")
library(lmtest)

bptest(moi_lm.seed) #BP = 0.69645, df = 1, p-value = 0.404
bptest(pH_lm.seed) #BP = 0.51361, df = 1, p-value = 0.4736
bptest(lux_lm.seed) #BP = 0.16539, df = 1, p-value = 0.6842
bptest(RP_lm.seed) #BP = 5.4153, df = 1, p-value = 0.01996
bptest(BD_lm.seed) #BP = 0.0070657, df = 1, p-value = 0.933
#With non-significant p-values, this means we cannot reject the null hypothesis that
# the data is homoscedastic
#So actually using this method, only RP is truly heteroscedastic

#Handling RP heteroscedasticity:
install.packages("sandwich")
library(sandwich)
#Need to use a Heteroscedasticty Consistent to correct the error on RP
#NOTE: RP is not looking significant, alone at least, anyway
#Use HC3 - good for small sample sizes and handles bias of influential dps

summary(RP_lm.seed)

library(lmtest)
coeftest(RP_lm.seed, vcovHC(RP_lm.seed, type="HC3"))
#vcovHC = variance covariance Heteroscedasticity Consistent
#Error similar between the two

###Assessing non-linearity:
###NOTE: LMM can hangle non-linear relationships
#What is non-linear?
plot(lux_lm.seed, which=1)
plot(pH_lm.seed, which=1)
plot(OM_lm.seed, which=1) # kinky but not terrible
plot(RP_lm.seed, which=1)

ggplot(seed, aes(x=seed.Lux2, y=N.sprout))+
  geom_point()+geom_smooth()+theme_bw()
#Beyond non-linear: no relationship! Points scattered everywhere

ggplot(seed, aes(x=seed.Mean.pH2, y=X..Lost))+
  geom_point()+geom_smooth()+theme_bw()
#Non-linear but, as stated, due to dp7 (low % lost - ~69%)
#Appears to be an outlier, but also a critical datapoint!

ggplot(seed, aes(x=seed.OrgMat2, y=X..Lost))+
  geom_point()+geom_smooth()+theme_bw()
#geom_smooth plots a non linear line, but is effectively flat through the middle
#Little dip in the middle due to dp7 and others

ggplot(seed, aes(x=seed.RhodProx2, y=X..Lost))+
  geom_point()+geom_smooth()+theme_bw()
#As discussed, the upward curve is related to the spread around 50
#This spread is generated by non-fine data collection, and analysis excluding >50 values
# detects a significant relationship
#Actually only 2 points causing deviation from the negative trend and perhaps with more
# data these would be isolated cases
#Repeat above analysis losing only two 'outliers'
plot(RP.noxtr.lm, which=1)
#now linear, but can't be included in lmm (I don't think) because different number of 
# datapoints

#So lux not in model (no relationship plus previous low model scores)
#RP can't be in model unless transformed
#pH also needs transforming
#OM doesn't

------

###7a. a first LMM

library(lme4)

PlotGroup.seed<-(seed$Group)
Wood.seed<-(seed$Dominant.Community)
ComClus.seed<-(seed$Community.Cluster)

full.seed.lmm<-lmer(seed.lost~seed.Mean.Moisture2+seed.Mean.pH2+seed.Lux2+seed.BulkD2+
                      seed.OrgMat2+seed.RhodProx2+seed.Moss2+(1|PlotGroup.seed)+
                      (1|Wood.seed)+(1|ComClus.seed))
summary(full.seed.lmm)
#This is different to the full model
#Moisture and pH and lux and rhod prox seem more important

#Based on above analyses, not including moss or lux, and plot group makes no sense
new.seed.lmm<-lmer(seed.lost~seed.Mean.Moisture2+seed.Mean.pH2+seed.BulkD2+seed.OrgMat2+
                     seed.RhodProx2+(1|Wood.seed)+(1|ComClus.seed))
summary(new.seed.lmm)
#huge error around all fixed and random effects
#lots of noise, not much signal


##FITTING RANDOM SLOPES
randmoi.seedlmm<-lmer(seed.lost~seed.Mean.Moisture2+seed.Mean.pH2+seed.BulkD2+seed.OrgMat2+
                        seed.RhodProx2+(1+seed.Mean.Moisture2|Wood.seed)+(1|ComClus.seed))
summary(randmoi.seedlmm)

randall.seedlmm<-lmer(seed.lost~seed.Mean.Moisture2+seed.Mean.pH2+seed.BulkD2+seed.OrgMat2+
                        seed.RhodProx2+(1+seed.Mean.Moisture2|Wood.seed)+
                        +(1+seed.Mean.pH2|Wood.seed)++(1+seed.OrgMat2|Wood.seed)+
                        (1|ComClus.seed))
summary(randall.seedlmm)

####TEST WHETHER THERE ARE DIFFERENT RELATIONSHIPS BETWEEN E.G. pH and woodland 
# to determine which random slopes to include in the model

######TO DO
#c. AIC for above model - compare with stargazer!
#d. interpret model for best predictor 