#packages im using
if(!require(jtools)){install.packages("jtools")}
if(!require(DHARMa)){install.packages("DHARMa")} 
if(!require(emmeans)){install.packages("emmeans")} 
if(!require(lmerTest)){install.packages("lmerTest")}
#if(!require(knitr)){install.packages("knitr")} 
if(!require(dplyr)){install.packages("dplyr")} 
if(!require(caret)){install.packages("caret")} 
if(!require(report)){install.packages("report")}
if(!require(car)){install.packages("car")} 
if(!require(lme4)){install.packages("lme4")} 
if(!require(rcompanion)){install.packages("rcompanion")} 
if(!require(effects)){install.packages("effects")}
if(!require(glmm)){install.packages("glmm")} 
if(!require(multcomp)){install.packages("multcomp")}
if(!require(purrr)){install.packages("purrr")} 
#if(!require(PerformanceAnalytics)){install.packages("PerformanceAnalytics")} 
if(!require(GGally)){install.packages("reshape2")} 
#if(!require(MASS)){install.packages("MASS")} 
#if(!require(sjPlot)){install.packages("sjPlot")} 
#if(!require(sjlabelled)){install.packages("sjlabelled")} 
#if(!require(sjmisc)){install.packages("sjmisc")} 

library(reshape2)
library(tidyverse) # data wrangling and visualization
library(sjPlot)    # to visualizing mixed-effects models
library(effects)   # to visualizing mixed-effects models
library(lme4)      # "golden standard" for mixed-effects modelling in R (no p-values)
library(lmerTest)  # p-values for MEMs based on the Satterthwaite approximation
library(emmeans)   # post-hoc analysis
library(knitr)     # beautifying tables
library(sjstats)   # ICC - intraclass-correlation coefficient
library(caret)     # ML, model comparison & utility functions
library(ggplot2)
library(ggeffects)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(glmulti)
library(rJava)
library(MASS)
library(rcompanion)
library(multcomp)
library(correlation)
library(corrplot)
library(jtools)
library(car)

#load the df - "Full MODEL variables" which was uploaded as supplementary material
setwd("C:/Users/...")
d <- read.csv("Full MODEL variables.csv")

head(d)

# Modify specified columns to a factor class
df<-d %>%
  # select(-X) %>% 
  purrr::modify_at(c("Jul", "depth", "brooding","lunar_Day"), factor) %>% 
rename( daily.range=Var)

df$date <- as.Date(df$date,format="%m/%d/%Y")
df$year <- format(df$date, "%Y")

view(df)

#Discard the the NA's in 2020 in 45 m depth
data <- df %>% 
  dplyr::filter(year!=2020 | depth != 45)
head(data)

#select cont. variables 
continuous <-na.omit(select_if(data, is.numeric))
view(continuous)
###see which variables have a high correlation between them 

#head(continuous)#select only variables that we will want to use-  
continuous <- continuous %>% 
  dplyr::select(mean_temp ,daily.range ,daily_max,Delta.temp.1.day, Delta.temp.2.day,Delta.temp.3.day,five.day.temp.trend.Jul,lunar_Day_number)

see.cor <- cor(continuous,method = "pearson",use = "pairwise")
View(see.cor)
#colinearity - MAX temp with daily mean and daily Var
#degree heating days with daily mean
# delta 1 temp and delta 2 temp

M=cor(continuous,method="p") #create Pearson correlation matrix
res1 <- cor.mtest(continuous, conf.level = .95)
res1
#Illustrait it 
corrplot(M, type = "upper", tl.pos = "td",
         method = "color", addCoef.col = "black",tl.cex = 0.75, tl.srt = 45,tl.col = 'black',
         order = "hclust", diag = FALSE)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
#check which variable out of range and d. max is preferable
head(data)
Max.mod <-  glm(brooding~ daily_max, data = data, family = 'binomial')
daily.range.mod <-  glm(brooding~ daily.range, data = data, family = 'binomial')
tab_model(Max.mod,daily.range.mod, show.aic = T)
#They are pretty similar in their AIC and r.
# However, because Daily max has high colinearity with two variables (daily mean and daily.range)
#i decided to discard it and not the RANGE variable. 

data <- data %>% 
  dplyr::select(c(-daily_max))
view(data)

#clearly we have a problem with delta.1, delta.2 and delta.3. days. and we can understand why... 
#delta.3.days has also high colinearity with trend 5. days, so we will filter out and
# Select between 1 and 2 according to their AIC i the final model 

#First, delta temp as an individual variable
mean_delta.1.mod <-  glm(brooding~ Delta.temp.1.day, data = data, family = 'binomial')
mean_delta.2.mod <-  glm(brooding~ Delta.temp.2.day, data = data, family = 'binomial')
mean_delta.3.mod <-  glm(brooding~ Delta.temp.3.day, data = data, family = 'binomial')
summary(mean_delta.2.mod)
tab_model(mean_delta.1.mod,mean_delta.2.mod,mean_delta.3.mod, show.aic = T)
#Delta 2 and 3 days are better than delta 1 day with almost 2AIC difference. 
#In addition, Delta 2.day is almost significant (0.06) alone!
head(data)
str(data)
#lets try now another comparison using full model
all_delta.1.mod <-  glm(brooding~ mean_temp+daily.range+Delta.temp.1.day+
                           five.day.temp.trend.Jul+sin(lunar_Day_number)+cos(lunar_Day_number), data = data, family = 'binomial')
all_delta.2.mod <-  glm(brooding~ mean_temp+daily.range+Delta.temp.2.day+
                           five.day.temp.trend.Jul+sin(lunar_Day_number)+cos(lunar_Day_number), data = data, family = 'binomial')
all_delta.3.mod <-  glm(brooding~ mean_temp+daily.range+Delta.temp.3.day+
                          five.day.temp.trend.Jul+sin(lunar_Day_number)+cos(lunar_Day_number), data = data, family = 'binomial')
tab_model(all_delta.1.mod,all_delta.2.mod,all_delta.3.mod, show.aic = T)

#Here also the model with delta.2.day looks like the best option, althpugh with only 0.5AIC better
#than delta.1.day

#To Sum up, Both comparisons have shown a slight preference to delta 2 days - lower AIC in both,
#and an almost sign. p value if only delta 2. is present. 
#and therefore im continuing with it the delta.2.days

plot_coefs(all_delta.1.mod,all_delta.2.mod,scale=TRUE,robust=TRUE)

###MODEL SELECTION PROCESS###

#subset the only relevant data - excluding colinear parameters
mod.data <- data %>% 
  dplyr::select(c(brooding, mean_temp,daily.range,Delta.temp.2.day,five.day.temp.trend.Jul,lunar_Day_number))
str(mod.data)

#Model Selection 
#method #1 using step AIC
#The stepAIC function (in MASS) uses AIC as a criterion to march through the
#inclusion and removal of different covariates to find the one with lowest AIC.
require(MASS)
brood.effects <- glm(brooding ~mean_temp+ daily.range + Delta.temp.2.day+five.day.temp.trend.Jul+sin(lunar_Day_number)+cos(lunar_Day_number), family="binomial", data=data, na.action = na.pass)
#anova(brood.effects, test="Chi")
best.step.AIC <- stepAIC(brood.effects)

#Two models were found with less than 2delta AIC
#Two models with delta AIC <2 are selected 
best1 <- glm(brooding ~ daily.range + Delta.temp.2.day + cos(lunar_Day_number), family="binomial", data=data)
best2 <- glm(brooding ~daily.range + Delta.temp.2.day + five.day.temp.trend.Jul + 
               cos(lunar_Day_number), family="binomial", data=data)
#best3 <- glm(brooding ~ mean_temp:Var , family="binomial", data=data)
 # mean_temp:Var                                  1   36.665 52.665
summary(best2)
summary(best1)
#summary(best3)
#see their stat
tab_model(best1,best2, show.aic = T)

#Both are pretty similar, however, best 2 adds another parameter- 5 days trend. 

anova(best2,best1, test="Chi")
#These models are not different from each other, but one is more simple than the other
#Therefore i'll choose the simplest model, which is best1 

# According to this the best model has r2 of 31% (or #Cragg-Uhler r2 - 42% )and two significant factors are delta 2 days temp and
#cosin(lunar), which is full moon or new moon.  
summ(best1)

print(summary(best1),corr=FALSE)

#lets try another method
maineffects.glm <- glm(brooding ~mean_temp+ daily.range + Delta.temp.2.day+five.day.temp.trend.Jul+sin(lunar_Day_number)+cos(lunar_Day_number), family="binomial", data=data, na.action = na.pass)
mega.model.comparison <- dredge(maineffects.glm)
head(mega.model.comparison)

mega.model.comparison
summ(maineffects.glm)
summary(maineffects.glm)
#So, this function did 44 different models and we can also see that the best fit model is 
mega.model.comparison[1:3]

dredge.mod.1 <- glm(brooding ~ daily.range+Delta.temp.2.day+cos(lunar_Day_number),family ="binomial", data=data)

dredge.mod.2 <- glm(brooding ~ Delta.temp.2.day+cos(lunar_Day_number),family ="binomial", data=data)
dredge.mod.3 <- glm(brooding ~ mean_temp+Delta.temp.2.day+cos(lunar_Day_number),family ="binomial", data=data)
anova(dredge.mod.1,dredge.mod.2,dredge.mod.3, test="Chi")
tab_model(dredge.mod.1,dredge.mod.2,dredge.mod.3, show.aic = T)

#Basically, the results are similar to what was achieved using by the stepwise methods, however here i got 2 other models that
#are not so different, but the bottom line stays the same- significant parameters are 2.days.diffrence and cos)lunar_day). 

final.mod

#same results as the ones i got from using the stepAIC function. 

#BUT
#What about with interactions? So another method is to use the dredge function 
#lets check what happens with interactions 
mean.temp.lunar.int <- glm(brooding ~ Delta.temp.2.day+mean_temp*cos(lunar_Day_number),family ="binomial", data=data)
lunar.delta.int <- glm(brooding ~ daily.range+Delta.temp.2.day*cos(lunar_Day_number),family ="binomial", data=data)
lunar.range.int <-  glm(brooding ~ Delta.temp.2.day+daily.range*cos(lunar_Day_number),family ="binomial", data=data)
all.int <- glm(brooding ~ Delta.temp.2.day*daily.range*cos(lunar_Day_number),family ="binomial", data=data)
tab_model(dredge.mod.1,mean.temp.lunar.int,lunar.delta.int,lunar.range.int,all.int, show.aic = T) 

warning()
#nothing interesting
#using the previous comparison I can see that there is one model that is better than the rest
# the interaction between delta.2.days.temp and lunar resulted in a lower AIC of 3-4 units, 
#a#nd higher R2 by 14% , the interactions between the moon and 2 days delta is important! And it also clearly shows that it the
#cos(lunar) is not significant after interaction term
#So, it is the best model !! 

#lets see how it looks like 

final.mod <- glm(brooding ~ daily.range+Delta.temp.2.day*cos(lunar_Day_number),family ="binomial", data=data)
summary(final.mod)
#######
#i can also try the stepwise approach

search = step(best1, ~.^2)
search$anova
# it seems as though this approach has led me to similar result 
#- AIC of suggested model = 43.5 - like the final.mod

######################################
#is there a difference to the best full model without interezctins? 
anova(best1, final.mod, test="LRT")
#yes! 

# there is a sign difference between an interaction model an non-interaction model
tab_model(best1,final.mod, show.aic = T) # lower AIC and higher R2 by 14% , the interactions between the moon and 2 days delta is important! 

#by how much does the interaction improve the model? 
summ(final.mod)
summ(best1)
summ(best2)
#lets check for the assumptions of the test, i.e, overdispersion and residual plot
#Check for overdispersion, see that model is less than 1.5
summary(final.mod)$deviance / summary(final.mod)$df.residual
#overdispersion is 0.94  - good! 
car::vif(final.mod)
car::vif(best1)
#VIF is higher in the final.mod, as expected for a model with interactions, but it is not too bad.  

#checking for overdisperstion using the DHARMapackage 
simulationOutput <- DHARMa::simulateResiduals(final.mod)
simulationOutput
graphics.off()
par("mar")
par(mar=c(1,1,1,1)) 
plot(simulationOutput) ### looking good, the model stand in the assumptions of the test 
#dHARMA qqplot look good - ot sign problems

### summerize best model 
summary.glm(final.mod)
summ(final.mod,vifs = TRUE)
#another method for summerzing
tab_model(final.mod,show.intercept = TRUE, show.est = TRUE)

#export
export_summs(int.mod, scale = TRUE,vifs = TRUE, to.file = "docx", file.name = "best_model.docx")
######################

#########################################################3
#graphics and comparison to full-parameters model 
#First we name the column in the name i want to show up in the plot
head(data)
md <- data %>% 
  rename(
    Mean_temperature = mean_temp ,
    Daily_range = daily.range,
    Two_days_temp_difference = Delta.temp.2.day, 
    Five_days_temp._change_rate = five.day.temp.trend.Jul,
    Lunar_day=lunar_Day_number)
head(md)

brood.effects_new <- glm(brooding ~Mean_temperature+ Daily_range + Two_days_temp_difference+Five_days_temp._change_rate+sin(Lunar_day)+cos(Lunar_day), family="binomial", data=md, na.action = na.pass)
best1_new <- glm(brooding ~ Daily_range + Two_days_temp_difference * cos(Lunar_day), family="binomial", data=md)

p <- plot_summs(brood.effects_new, best1_new, colors = "CUD", model.names = c("Full model", "Best model"))+
  theme_classic()+
  theme(axis.text=element_text(color="black",size=12), axis.title=element_text(size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p

##Table S2 - appendix data - with the exception that i use r
tab_model(best1_new,brood.effects_new,show.intercept = TRUE, show.est = TRUE)
summ(brood.effects_new,vifs = TRUE)
##export graphs 
ggsave("Model comparison.tiff", plot = p, width = 18, height = 9,dpi=600, 
       units = "cm")

##############################################################################
###statistics for s.brooding intensity####################


#Here i use the binary dataset with year and events - the data is similar to the dataset that creates Figure 4 - 
#i.e 'Data of s.brooding intensity_.csv' - BUT only that i changed every colony in a given transect into 0 or
# 1 according to their reproduction stat, i.e, brooded - 1, not - 0
#to start the script use the Data "Binary data of surface brooding events" 
#which was uploaded as supplementary material
setwd("C:/Users/...")
md <- read.csv("Binary data of surface brooding events.csv")  
md <- Binary.data.of.surface.brooding.events

head(md)
view(md)
str(md)

#The letters represent the depth-intervals - 
#A = 40-45 m
#B - 30-40 m
#C- 20-30 m
#D - 10-20 m
#E - 0-10 m
sum <- md %>%
  dplyr::group_by(year,depth,event) %>% 
  dplyr::summarize(n= n(), brood= sum(brood))
sum
view(sum)
#histogram(~ n | depth,
 #         data=sum,
  #        layout=c(1,5)      #  columns and rows of individual plots
#)
md$year <- as.character(md$year)
md$depth <- as.character(md$depth)
md$event <-  as.factor(md$event)
#### now we can continue for the statistics 
#here i perform a glmer test to find out the differences between depth groups.
#I use year a random factor and also event nested in each year, to deal with repeating annual events like in 2020

md$depth <- as.factor(md$depth)
#levels(md$depth) <- c("0-10", "10-20", "20-30", "30-40", "40-45")
#levels(md$depth) <- c( "40-45", "30-40","20-30", "10-20" , "0-10")
str(md)
#levels(md$depth) <- list('0-10'="1", '10-20'="2", '20-30'="3", '30-40' ="4", '40-45' = "5")
fit_binary <- glmer(brood~ depth + (1|year)+(1|year:event), data= md, family = ("binomial"))
#summary of the model
summary(fit_binary)   
#using jtools to summarize the model 
summ(fit_binary)
#using sjplot to summarize the model
tab_model(fit_binary) 
# is the difference in the glmer significant - WALD test? 

car::Anova(fit_binary, type=c("II"), test.statistic=c("Chisq"))

# yes, depth is significant 

#Then,i check what is actually intersest me! differences between groups using emmeans
binary.emm = emmeans(fit_binary, "depth", type = "response",adjust = "bonferroni") 
binary.emm

#Then summarize the pairwise comparison
pairs(binary.emm, reverse = TRUE,type = "response", adjust = "bonferroni")

#grouping  according to prob.

tb <- cld(binary.emm,
          alpha   = 0.05,
          Letters = letters,    ### Use lower-case letters for .group
          type    = "response",
          adjust = "bonferroni")### Report emmeans in orginal scale
#adjust =  "tukey")    ### Tukey adjustment for multiple comparisons
tb

#SO, A and B the deeder depth intervals are always diffent than C,D,E 
#however there is not difference between the A,B,C and within A,B

pwpm(binary.emm,means = TRUE,
     diffs = TRUE,pvals = T,adjust = "bonferroni")

#emmeans(binary.emm, pairwise ~ depth,adjust = "bonferroni")
#another way to show this
sum_pair <- summary(binary.emm,alpha   = 0.05,infer=TRUE,
        Letters = letters,    ### Use lower-case letters for .group
        type    = "response",
        adjust = "bonferroni",as.df=T)
sum_pair
str(sum_pair)

dat <- as.data.frame(sum_pair)
head(sum_pair)

write.csv(file= "summery.of.pairwise.brood.intensity.csv",dat)



#loadfonts(device="win")
#Now plot it - basically this is the plot in Table 1.
emmp <- plot(binary.emm,comparisons = TRUE, horizontal = T, colors = c( "black","#08519c"))+theme_bw() %+replace% 
  theme(legend.position="bottom", axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor.y=element_blank(),
        panel.background = element_blank(),
        axis.title =element_text(size=10),
        axis.text=element_text(size=10),
        axis.text.y=element_blank(),
        axis.text.x = element_text( size=10))+ 
  labs(y = "Depth intervals", x = "Estimated marginal mean")+
  theme(axis.title.y=element_blank())
emmp

p <- emmp+  scale_x_continuous(position = 'top')
p

p <- p+ geom_hline(yintercept=1.5, linetype="dashed", color = "gray75")+
  geom_hline(yintercept=2.5, linetype="dashed", color = "gray75")+
  geom_hline(yintercept=3.5, linetype="dashed", color = "gray75")+
  geom_hline(yintercept=4.5, linetype="dashed", color = "gray75")

#comparisons - âThe blue bars are confidence intervals for the EMMs, and the red arrows are for the comparisons among them. 
#If an arrow from one mean overlaps an arrow from another group, the difference is not significant.

#export
ggsave("s.brooding.depth.zone.comparison_fliped_for legend.tiff", plot = p, width = 7 , height = 5.5 ,dpi=600, 
       units = "cm")
#EPS
ggsave("s.brooding.depth.zone.comparison_fliped_for legend", plot = p, device = cairo_ps,width = 7 , height = 4.5 ,dpi=600, 
       units = "cm")

#looking at effect.size this data may go into the text 
eff <- eff_size(binary.emm, sigma = sigma(fit_binary), edf = Inf)
eff
#Determine R2:
r.squaredGLMM(fit_binary) 
#checking the residulas of the test using the DHARMa package 
simulationOutput <- DHARMa::simulateResiduals(fit_binary)
plot(simulationOutput,  asFactor = TRUE)

#making a  nice result.table
install.packages("kableExtra")
library(kableExtra)
kbl(tb)

dt %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Cambria")

eff %>%
  kbl() %>%
  kable_classic_2(full_width = F)
eff
write.csv(file="eff.size.pariwise.csv",sum_pair)

head(tb)

#########################################33######################

