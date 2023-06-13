library(readr)
library(readxl)
#import needed data 

climate <- read_csv("DT_weather_2007-2022.csv")
head(climate)

idsimp <- read_csv("DT_SIMP_Data_Idaho.csv")
head(idsimp)

sites <- read_csv("IdahoSites.csv")
head(sites)

sites <- sites[sites$Last_Year - sites$First_Year >= 5, ]
head(sites)

releases <- read_xls("IDAHO_RELEASE_SITES.xls")
head(releases)

releases <- releases[,c("ReleaseYear", "SITE_NAME")]
names(releases)

#keep the earliest release year per site 
library(dplyr)
releases <- releases %>%
  group_by(SITE_NAME) %>%
  arrange(ReleaseYear) %>%
  slice(1L)

#join release years to sites 
library(plyr)
data<-join(idsimp, releases, by=c("SITE_NAME"))
names(data)

#join sites and climate for data with site names 
climate<-join(climate, sites, by= c("Longitude","Latitude"))
head(climate)

library(reshape)
library(reshape2)
library(MASS)
#melt and calc stats
clim.melt<-melt(climate, id.vars=c("SITE_NAME" , "month", "year","day" ), measure.vars=c("tmax","tmin","pr", "srad", "swe", "soil"))
head(clim.melt)

#seasonal prec calculations - average over the season for the year 
##I want to create a new data file with the average of each season's prec as a column
##SPRING
MarPrec <- cast(clim.melt, SITE_NAME + year + month + day ~ variable, 
                subset = clim.melt$variable == "pr" & clim.melt$month == "3" & 
                  clim.melt$day >= 21 & clim.melt$day <= 31, 
                fun = sum)
AprPrec<-cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "pr" & clim.melt$month == "4" ,fun= sum )
MayPrec<-cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "pr" & clim.melt$month == "5" ,fun= sum )
JunPrec<-cast(clim.melt,SITE_NAME + year+ month+day  ~ variable, subset= clim.melt$variable == "pr" & clim.melt$month == "6"& 
                clim.melt$day >= 1 & clim.melt$day <= 20 ,fun= sum )

MarPrec <- aggregate(pr ~ SITE_NAME + year, data=MarPrec, sum)
JunPrec <- aggregate(pr ~ SITE_NAME + year, data=JunPrec, sum)

SpringPr<-join(MarPrec,AprPrec, by=c("SITE_NAME","year"))
SpringPr<-join(SpringPr,MayPrec, by=c("SITE_NAME","year"))
SpringPr<-join(SpringPr,JunPrec, by=c("SITE_NAME","year"))


head(SpringPr)
SpringPr<-SpringPr[,c(1,2,3,5,7,8)]
colnames(SpringPr)<-c("SITE_NAME","YEAR","Mar","Apr","May", "Jun")


SpringPr$SpringPr <-apply(SpringPr[,3:6],1,sum)  
head(SpringPr)

SpringPr<-SpringPr[,c(1,2,7)]
head(SpringPr)

#Summer prec now - June 22 to Sept 21 
JunPrec2 <- cast(clim.melt, SITE_NAME + year + month + day ~ variable, 
                 subset = clim.melt$variable == "pr" & clim.melt$month == "6" & 
                   clim.melt$day >= 21 & clim.melt$day <= 30, 
                 fun = sum)
JulPrec<-cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "pr" & clim.melt$month == "7" ,fun= sum )
AugPrec<-cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "pr" & clim.melt$month == "8" ,fun= sum )
SepPrec<-cast(clim.melt,SITE_NAME + year+ month+day  ~ variable, subset= clim.melt$variable == "pr" & clim.melt$month == "9"& 
                clim.melt$day >= 1 & clim.melt$day <= 20 ,fun= sum )

JunPrec2 <- aggregate(pr ~ SITE_NAME + year, data=JunPrec2, sum)
SepPrec <- aggregate(pr ~ SITE_NAME + year, data=SepPrec, sum)

SummerPr<-join(JunPrec2,JulPrec, by=c("SITE_NAME","year"))
SummerPr<-join(SummerPr,AugPrec, by=c("SITE_NAME","year"))
SummerPr<-join(SummerPr,SepPrec, by=c("SITE_NAME","year"))


head(SummerPr)
SummerPr<-SummerPr[,c(1,2,3,5,7,8)]
colnames(SummerPr)<-c("SITE_NAME","YEAR","Jun","Jul","Aug", "Sep")


SummerPr$SummerPr <-apply(SummerPr[,3:6],1,sum)  
head(SummerPr)

SummerPr<-SummerPr[,c(1,2,7)]
head(SummerPr)

#Autumn prec now - Sep 21 to Dec 20
SepPrec2 <- cast(clim.melt, SITE_NAME + year + month + day ~ variable, 
                 subset = clim.melt$variable == "pr" & clim.melt$month == "9" & 
                   clim.melt$day >= 21 & clim.melt$day <= 30, 
                 fun = sum)
OctPrec<-cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "pr" & clim.melt$month == "10" ,fun= sum )
NovPrec<-cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "pr" & clim.melt$month == "11" ,fun= sum )
DecPrec<-cast(clim.melt,SITE_NAME + year+ month+day  ~ variable, subset= clim.melt$variable == "pr" & clim.melt$month == "12"& 
                clim.melt$day >= 1 & clim.melt$day <= 20 ,fun= sum )

SepPrec2 <- aggregate(pr ~ SITE_NAME + year, data=SepPrec2, sum)
DecPrec <- aggregate(pr ~ SITE_NAME + year, data=DecPrec, sum)

AutumnPr<-join(SepPrec2,OctPrec, by=c("SITE_NAME","year"))
AutumnPr<-join(AutumnPr,NovPrec, by=c("SITE_NAME","year"))
AutumnPr<-join(AutumnPr,DecPrec, by=c("SITE_NAME","year"))


head(AutumnPr)
AutumnPr<-AutumnPr[,c(1,2,3,5,7,8)]
colnames(AutumnPr)<-c("SITE_NAME","YEAR","Sep","Oct","Nov", "Dec")


AutumnPr$AutumnPr <-apply(AutumnPr[,3:6],1,sum)  
head(AutumnPr)

AutumnPr<-AutumnPr[,c(1,2,7)]
head(AutumnPr)

#winter prec - Dec 21 to Mar 20
DecPrec2 <- cast(clim.melt, SITE_NAME + year + month + day ~ variable, 
                 subset = clim.melt$variable == "pr" & clim.melt$month == "12" & 
                   clim.melt$day >= 21 & clim.melt$day <= 31, 
                 fun = sum)
JanPrec<-cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "pr" & clim.melt$month == "1" ,fun= sum )
FebPrec<-cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "pr" & clim.melt$month == "2" ,fun= sum )
MarPrec2<-cast(clim.melt,SITE_NAME + year+ month+day  ~ variable, subset= clim.melt$variable == "pr" & clim.melt$month == "3"& 
                 clim.melt$day >= 1 & clim.melt$day <= 20 ,fun= sum )

DecPrec2 <- aggregate(pr ~ SITE_NAME + year, data=DecPrec2, sum)
MarPrec2 <- aggregate(pr ~ SITE_NAME + year, data=MarPrec2, sum)

WinterPr<-join(DecPrec2,JanPrec, by=c("SITE_NAME","year"))
WinterPr<-join(WinterPr,FebPrec, by=c("SITE_NAME","year"))
WinterPr<-join(WinterPr,MarPrec2, by=c("SITE_NAME","year"))


head(WinterPr)
WinterPr<-WinterPr[,c(1,2,3,5,7,8)]
colnames(WinterPr)<-c("SITE_NAME","YEAR","Dec","Jan","Feb", "Mar")


WinterPr$WinterPr <-apply(WinterPr[,3:6],1,sum)  
head(WinterPr)

WinterPr<-WinterPr[,c(1,2,7)]
head(WinterPr)

#combine season data into one file 
SeasonPrec <- join(SpringPr,SummerPr, by=c("SITE_NAME", "YEAR"))
SeasonPrec <- join(SeasonPrec, AutumnPr, by=c("SITE_NAME", "YEAR"))
SeasonPrec <- join(SeasonPrec, WinterPr, by=c("SITE_NAME", "YEAR"))
head(SeasonPrec)
SeasonPrec <- na.omit(SeasonPrec)

#From here, I want to combine the data about the percent cover of all cover types
#with the seasonal precipitation data to see how that impacts plant cover and number
#of stems. I say start from scratch with the data files and see where that gets you. 
#You're working not with plant growth rate but with plant cover and number of stems 
#so you may need to change your approach to figure out how precipitation will impact 
#the plant. Maybe try making a new category for total plant cover and then subset that 
#category by type of cover to see how precipitation impacts each specifically. You will 
#need to take into account sites so make that your random variable - and maybe going 
#back to regional data may make more sense as well to see how that effects the data, since 
#that could be impacting the overall results of the model.... just something to think about

#make a new df with subsetting categories 
#take away sites with less than 5 years of data 
idsimp <- join(idsimp, sites, by=c("SITE_NAME"))
idsimp <- idsimp[idsimp$Last_Year - idsimp$First_Year >= 5, ]

new_idsimp<-idsimp[,c("SITE_NAME", "longitude","latitude","YEAR","TargetWeed","OtherWeed","Forbs","Grass","BareGround", "Litter",
              "Moss","NumStems","HeightTall","Insects")]

new_idsimp$per_cover <- new_idsimp$TargetWeed + new_idsimp$OtherWeed + new_idsimp$Forbs + new_idsimp$Grass + new_idsimp$BareGround 
                      + new_idsimp$Litter + new_idsimp$Moss

new_idsimp <- join(new_idsimp, SeasonPrec, by=c("SITE_NAME", "YEAR"))
new_idsimp <- na.omit(new_idsimp)

#checking assumptions adn modeling 
library(lme4)
preclmer <- lmer(per_cover ~ SpringPr + SummerPr + AutumnPr+ WinterPr + (1 |SITE_NAME), 
                   REML=FALSE, na.action = "na.fail", data = new_idsimp)
summary(preclmer)
anova(preclmer)

hist(resid(preclmer), 
     main = "Histogram of Residuals",
     xlab = "Residuals",
     ylab = "Frequency")

res<-resid(preclmer)
#produce residual vs. fitted plot
plot(fitted(preclmer), res)

#add a horizontal line at 0 
abline(0,0)

#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 

#no random effect for sites
preclm <- lm(per_cover ~ SpringPr + SummerPr + AutumnPr+ WinterPr, data = new_idsimp)
summary(preclm)
anova(preclm)

#since there so many between site differences, ignoring the sites might be a good idea
#TargetWeed
TW_preclm <- lm(TargetWeed ~ SpringPr + SummerPr + AutumnPr + WinterPr, data = new_idsimp)
summary(TW_preclm)
anova(TW_preclm)


#OtherWeed 
OW_preclm <- lm(OtherWeed ~ SpringPr + SummerPr + AutumnPr + WinterPr, data = new_idsimp)
summary(OW_preclm)
anova(OW_preclm)

#Forbs
Forbs_preclm <- lm(Forbs ~ SpringPr + SummerPr + AutumnPr + WinterPr, data = new_idsimp)
summary(Forbs_preclm)
anova(Forbs_preclm)

#Grass
Grass_preclm <- lm(Grass ~ SpringPr + SummerPr + AutumnPr + WinterPr, data = new_idsimp)
summary(Grass_preclm)
anova(Grass_preclm)

#Moss
Moss_preclm <- lm(Moss ~ SpringPr + SummerPr + AutumnPr + WinterPr, data = new_idsimp)
summary(Moss_preclm)
anova(Moss_preclm)
 
#BareGround
BG_preclm <- lm(BareGround ~ SpringPr + SummerPr + AutumnPr + WinterPr, data = new_idsimp)
summary(BG_preclm)
anova(BG_preclm)

#change in the percent cover would be interesting 
#maybe stop with this bullshit rn and look at monthly maxtemp for each site 
Jan_MaxT <- cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "tmax" & clim.melt$month == "1" ,fun= max)
Feb_MaxT <- cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "tmax" & clim.melt$month == "2" ,fun= max )
Mar_MaxT <- cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "tmax" & clim.melt$month == "3" ,fun= max )
Apr_MaxT <- cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "tmax" & clim.melt$month == "4" ,fun= max )
May_MaxT <- cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "tmax" & clim.melt$month == "5" ,fun= max )
Jun_MaxT <- cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "tmax" & clim.melt$month == "6" ,fun= max )
Jul_MaxT <- cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "tmax" & clim.melt$month == "7" ,fun= max )
Aug_MaxT <- cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "tmax" & clim.melt$month == "8" ,fun= max )
Sep_MaxT <- cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "tmax" & clim.melt$month == "9" ,fun= max )
Oct_MaxT <- cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "tmax" & clim.melt$month == "10" ,fun= max )
Nov_MaxT <- cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "tmax" & clim.melt$month == "11" ,fun= max )
Dec_MaxT <- cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "tmax" & clim.melt$month == "12" ,fun= max )

month_maxT <- join(Jan_MaxT, Feb_MaxT, by=c("SITE_NAME", "year"))
month_maxT <- join(month_maxT, Mar_MaxT, by=c("SITE_NAME", "year"))
month_maxT <- join(month_maxT, Apr_MaxT, by=c("SITE_NAME", "year"))
month_maxT <- join(month_maxT, May_MaxT, by=c("SITE_NAME", "year"))
month_maxT <- join(month_maxT, Jun_MaxT, by=c("SITE_NAME", "year"))
month_maxT <- join(month_maxT, Jul_MaxT, by=c("SITE_NAME", "year"))
month_maxT <- join(month_maxT, Aug_MaxT, by=c("SITE_NAME", "year"))
month_maxT <- join(month_maxT, Sep_MaxT, by=c("SITE_NAME", "year"))
month_maxT <- join(month_maxT, Oct_MaxT, by=c("SITE_NAME", "year"))
month_maxT <- join(month_maxT, Nov_MaxT, by=c("SITE_NAME", "year"))
month_maxT <- join(month_maxT, Dec_MaxT, by=c("SITE_NAME", "year"))


head(month_maxT)
month_maxT<-month_maxT[,c(1,2,4,6,8,10,12,14,16,18,20,22,24,26)]
colnames(month_maxT)<-c("SITE_NAME","YEAR","Jan_mxT","Feb_mxT","Mar_mxT","Apr_mxT","May_mxT","Jun_mxT","Jul_mxT","Aug_mxT","Sep_mxT","Oct_mxT","Nov_mxT","Dec_mxT")

new_idsimp <- join(new_idsimp, month_maxT, by=c("SITE_NAME","YEAR"))

#TargetWeed maxT 
TW_mxT <- lm(TargetWeed ~ Jan_mxT + Feb_mxT + Mar_mxT + Apr_mxT + May_mxT 
             + Jun_mxT + Jul_mxT + Aug_mxT + Sep_mxT + Oct_mxT + Nov_mxT + Dec_mxT, 
             data = new_idsimp)
summary(TW_mxT)
anova(TW_mxT)

#OtherWeed maxT 
OW_mxT <- lm(OtherWeed ~ Jan_mxT + Feb_mxT + Mar_mxT + Apr_mxT + May_mxT 
             + Jun_mxT + Jul_mxT + Aug_mxT + Sep_mxT + Oct_mxT + Nov_mxT + Dec_mxT, 
             data = new_idsimp)
summary(OW_mxT)
anova(OW_mxT)

#Forbs maxT
Forbs_mxT <- lm(Forbs ~ Jan_mxT + Feb_mxT + Mar_mxT + Apr_mxT + May_mxT 
             + Jun_mxT + Jul_mxT + Aug_mxT + Sep_mxT + Oct_mxT + Nov_mxT + Dec_mxT, 
             data = new_idsimp)
summary(Forbs_mxT)
anova(Forbs_mxT)

#Grass maxT
Grass_mxT <- lm(Grass ~ Jan_mxT + Feb_mxT + Mar_mxT + Apr_mxT + May_mxT 
             + Jun_mxT + Jul_mxT + Aug_mxT + Sep_mxT + Oct_mxT + Nov_mxT + Dec_mxT, 
             data = new_idsimp)
summary(Grass_mxT)
anova(Grass_mxT)

#BareGround maxT
BG_mxT <- lm(BareGround ~ Jan_mxT + Feb_mxT + Mar_mxT + Apr_mxT + May_mxT 
             + Jun_mxT + Jul_mxT + Aug_mxT + Sep_mxT + Oct_mxT + Nov_mxT + Dec_mxT, 
             data = new_idsimp)
summary(BG_mxT)
anova(BG_mxT)

#Moss
Moss_mxT <- lm(Moss ~ Jan_mxT + Feb_mxT + Mar_mxT + Apr_mxT + May_mxT 
             + Jun_mxT + Jul_mxT + Aug_mxT + Sep_mxT + Oct_mxT + Nov_mxT + Dec_mxT, 
             data = new_idsimp)
summary(Moss_mxT)
anova(Moss_mxT)

#add 0.01 to each of the percent cover so there aren't zeros anymore 


#I think the next step is to try and calculate interannual growth rate and then look at
#the other climate factors - how to do this ahhhhhhhahhhh

new_idsimp<-new_idsimp%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(TW_GR = ((TargetWeed+1 - lag(TargetWeed+1))/lag(TargetWeed+1))*100) %>%
  mutate(OW_GR = ((OtherWeed+1 - lag(OtherWeed+1))/lag(OtherWeed+1))*100)%>%
  mutate(ForbsGR = ((Forbs+1 - lag(Forbs+1))/lag(Forbs+1))*100)%>%
  mutate(Grass_GR = ((Grass+1 - lag(Grass+1))/lag(Grass+1))*100)

new_idsimp<-na.omit(new_idsimp)
  
library(ggplot2)
#dt growth rate with spring pr 
ggplot(new_idsimp, aes(x=SpringPr, y=TW_GR)) +
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

#dt growth rate with summer pr 
ggplot(new_idsimp, aes(x=SummerPr, y=TW_GR)) +
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

#see if growth rate has anything to do with shit 
pr_grlmer <- lmer(TW_GR ~ SpringPr + SummerPr + AutumnPr+ WinterPr + (1 |SITE_NAME), 
                 REML=FALSE, na.action = "na.fail", data = new_idsimp)
summary(pr_grlmer)
anova(pr_grlmer)

hist(resid(pr_grlmer), 
     main = "Histogram of Residuals",
     xlab = "Residuals",
     ylab = "Frequency")

res<-resid(pr_grlmer)
#produce residual vs. fitted plot
plot(fitted(pr_grlmer), res)

#add a horizontal line at 0 
abline(0,0)

#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 



#try glmer 
library(lme4)
TW_prgr<-glmer(TW_GR ~ SpringPr + SummerPr + AutumnPr+ WinterPr + (1|SITE_NAME), 
             family=Gamma(), data = new_idsimp)
summary(TW_pr)
anova(Tw_pr)
#control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)


#lets try a basic transformation first 
## Stem density
new_idsimpt<-new_idsimp%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(lnTW_GR = log(TW_GR+92))%>%#log trans
  mutate(lnSprPr = log(SpringPr+92))%>%#log trans
  mutate(lnSummPr = log(SummerPr+92))%>%#log trans
  mutate(lnAutPr = log(AutumnPr+92))%>%#log trans
  mutate(lnWinPr = log(WinterPr+92))#log trans
  
#run it again and see  
library(lme4)
TW_prgr<-lmer(lnTW_GR ~ lnSprPr + lnSummPr + lnAutPr+ lnWinPr + (1|SITE_NAME), 
               REML=FALSE, na.action="na.fail", data = new_idsimpt)
summary(TW_prgr)
anova(TW_prgr) 

hist(resid(TW_prgr), 
     main = "Histogram of Residuals",
     xlab = "Residuals",
     ylab = "Frequency", 
     freq=FALSE)

density_est <- density(resid(TW_prgr))
lines(density_est, col = "red")

res<-resid(TW_prgr)
#produce residual vs. fitted plot
plot(fitted(TW_prgr), res)

#add a horizontal line at 0 
abline(0,0)

#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 

#check for outliers 
cooks_distance <- cooks.distance(TW_prgr)
plot(cooks_distance, pch = 20, main = "Cook's Distance")

leverage_values <- hatvalues(TW_prgr)
plot(leverage_values, pch = 20, main = "Leverage Plot")

high_leverage_indices <- which(leverage_values > 2 * (4 + 1) / 250)
print(high_leverage_indices)

#test for normality
shapiro.test(res)
#it's not normal lmao 

#checking assumptions adn modeling 
TW_grPr_new <- lm(lnTW_GR ~ lnSprPr + lnSummPr + lnAutPr+ lnWinPr, data = new_idsimpt)
summary(TW_grPr_new)
anova(TW_grPr_new)

hist(resid(TW_grPr_new), 
     main = "Histogram of Residuals",
     xlab = "Residuals",
     ylab = "Frequency", 
     freq=FALSE)

density_est <- density(resid(TW_grPr_new))
lines(density_est, col = "red")

res<-resid(TW_grPr_new)
#produce residual vs. fitted plot
plot(fitted(TW_grPr_new), res)

#add a horizontal line at 0 
abline(0,0)

#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 

#Aaron helpppppppppppppppp
#play around with something else now i think. leave this for aaron....
#so i have growth rate for all of the variables - try the other methods ig

TWGR_mxT <- lm(TW_GR ~ Jan_mxT + Feb_mxT + Mar_mxT + Apr_mxT + May_mxT 
             + Jun_mxT + Jul_mxT + Aug_mxT + Sep_mxT + Oct_mxT + Nov_mxT + Dec_mxT, 
             data = new_idsimpt)
summary(TWGR_mxT)
anova(TWGR_mxT)

hist(resid(TWGR_mxT), 
     main = "Histogram of Residuals",
     xlab = "Residuals",
     ylab = "Frequency", 
     freq=FALSE)

density_est <- density(resid(TWGR_mxT))
lines(density_est, col = "red")

res<-resid(TWGR_mxT)
#produce residual vs. fitted plot
plot(fitted(TWGR_mxT), res)

#add a horizontal line at 0 
abline(0,0)

#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 

