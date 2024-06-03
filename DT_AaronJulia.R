##### Imports and does a basic TS analysis on DT SIMP data collected through 2015

library(plyr)
library(dplyr)
library(reshape)
#source("panel_cor.R")
require(lattice)
require(visreg)
library(MuMIn)
library(nlme)
library(latticeExtra)
library(ggplot2)
### import file from Joey
##if on mac
#setwd("~/Dropbox/Univ Idaho collabortion/DT Manuscript/DT SIMP thru 2015")
## if on PC
#setwd("C:/Users/aweed/Dropbox/Univ Idaho collabortion/DT Manuscript/DT SIMP thru 2015")

data <- read.csv("DT_2015.csv")
names(data)


sites<-read.csv("DT_SIMP sites.csv") ### site metadata
head(sites)
sites<-na.omit(sites)
#####weather data import and manip ####
## read in daily data from J Abatz 
climate<-read.csv("DT_weather.csv")
head(climate)
siteJ<-read.csv("DT_SIMP sites_John.csv")

## append site name to daily weather data	
clim<-join(siteJ, climate, by= c("Latitude","Longitude"))
head(clim)
clim$date<-as.Date(clim$date,"%Y/%m/%d")
write.table(clim, "dailyclimate_data.csv", sep=",", row.names=F)	## daily data with site names

# melt and calc stats
clim.melt<-melt(clim, id.vars=c("Site.Code" , "month", "year","date" ), measure.vars=c("tmax_C","tmin_C","ppt_mm"))
head(clim.melt)
# clim.melt$month<-as.factor(format(clim.melt$date,"%m"))
# clim.melt$year<-as.factor(format(clim.melt$date,"%Y"))

###### Coldest night of winter between July year n to July n+1----
### Because R is calc'd lnNt+1 -lnNt, the Tmin that matters would occur in either Nov or Dec of yeart or Jan/Feb of year t+1. 
## becuase of this, I added 1 to each year in which Nov/Dec Tmin was calculated to calc abs Tmin in the same winter
#When does the coldest night occur? 
absTminJan<-cast(clim.melt,Site.Code + year+ month  ~ variable, subset= clim.melt$variable == "tmin_C" & clim.melt$month == "1" ,fun= min )
absTminFeb<-cast(clim.melt,Site.Code + year+ month  ~ variable, subset= clim.melt$variable == "tmin_C" & clim.melt$month == "2" ,fun= min )
absTminDec<-cast(clim.melt,Site.Code + year+ month  ~ variable, subset= clim.melt$variable == "tmin_C" & clim.melt$month == "12" ,fun= min )
absTminDec$year<-absTminDec$year+1
absTminNov<-cast(clim.melt,Site.Code + year+ month  ~ variable, subset= clim.melt$variable == "tmin_C" & clim.melt$month == "11" ,fun= min )
absTminNov$year<-absTminNov$year+1
## join temps to determine the absol Tmin during same winter
absTmin<-join(absTminNov,absTminDec, by=c("Site.Code","year"))
absTmin<-join(absTmin,absTminJan, by=c("Site.Code","year"))
absTmin<-join(absTmin,absTminFeb, by=c("Site.Code","year"))

head(absTmin)
absTmin<-absTmin[,c(1,2,4,6,8, 10)]
colnames(absTmin)<-c("Site.Code","YEAR","Nov","Dec","Jan", "Feb")
absTmin<-na.omit(absTmin)
# obj1 <- xyplot(Dec ~ YEAR|Site.Code,data=absTmin,par.strip.text=list(cex=.7),
# 							 scales=list(alternating=1, x=list(cex=.8,rot=90),y=list(cex=1.2)),
# 							 type = "l",ylab="other",  xlab= "Year",lty=2, lwd=2)
# obj2 <- xyplot(Jan ~ YEAR|Site.Code, data=absTmin,type = "l",
# 							 scales=list(alternating=1, x=list(cex=.8,rot=90),y=list(cex=1.2)),
# 							 par.strip.text=list(cex=.7),ylab="Jan", col= "magenta", 
# 							 xlab= "Year", lwd=2)
# 
# doubleYScale(obj1 = obj1, obj2 = obj2, add.ylab2 = TRUE, style1 = 0, style2 = 0, xlab="Year", 
# 						 scales=list(alternating=1, x=list(cex=.8,rot=90),y=list(cex=1.2)),
# 						 par.settings = simpleTheme(lty = 1:2),text = c("other","Jan"), columns = 2)

## find coldest night of winter, from Nov to Feb, in each year
absTmin$absTmin <-apply(absTmin[,3:6],1,min)  
head(absTmin)

####### Cummulative precip from 1 May in year t-1 to 30 April in year t----
## only calc for 2008 to 2015 because missing 2006 data
# sum ppt per month
summpptMon<-cast(clim.melt,Site.Code + year+ month  ~ variable, subset= clim.melt$variable == "ppt_mm" ,fun= sum )
## create variable for summing ppt in each "water" year
## the water yr is the year that could affect R (between dates when pops were measured)
summpptMon$wateryr<-ifelse(summpptMon$month >4, summpptMon$year+1, summpptMon$year)
head(summpptMon)

summpptMay<-aggregate.data.frame(summpptMon$ppt_mm, by= list(summpptMon$Site.Code,summpptMon$wateryr), FUN= sum)
head(summpptMay)
colnames(summpptMay)<-c("Site.Code","YEAR", "ppt_May")## naming wateryr to YEAR for binding

##drop 2006/2007 water yr becasue of incomplete record
summpptMay<-summpptMay[summpptMay$YEAR > "2007",]

summpptMay<-summpptMay[order(summpptMay$Site.Code,summpptMay$YEAR),]

# xyplot(ppt_May~YEAR|Site.Code,data=summpptMay,par.strip.text=list(cex=.7),
# 			 scales=list(alternating=1, x=list(cex=.8,rot=90),y=list(cex=1.2)),
# 			 type = "l",ylab="ppt (cm)",xlab= "Year",lty=2, lwd=2)

### calculate standardized values (Z-scores) for climate data
## i.e. calc Mean and SD then subtract the mean from the original value, then divide that by the standard error

##### Combine climate data for analysis
climate<-join(summpptMay, absTmin[,c(1,2,7)], by=c("Site.Code", "YEAR"))
head(climate)
climate<-na.omit(climate)
### Compare climate data to previous study data #####
# import df from JPPL study
# climate_JAPPL <- read.delim("~/Documents/Idaho weeds/Toadflax/SIMP data/Datasets (new 7.28.11)/Years combined/DT_growth rate_NA.txt")
# names(climate_JAPPL)
# 
# #subset old climate df
# old<-climate_JAPPL[,c("code", "year", "lat", "tmin","tmin_t.1","ppt_May","ppt_May_t.1")]
# #rename some cols
# old$Site.Code<-old$code
# climate$year<-climate$YEAR
# head(old)
# old$ppt_MayOLD<-old$ppt_May
# old$ppt_May<-NULL
# 
# head(climate);head(old)
# intersect(unique(climate$Site.Code),unique(old$code))# what sites are the smae in each df?
# 
# # join dfs
# comb<-join(climate, old, by= c("Site.Code", "year"))
# names(comb)
# #precip
# z<-na.omit(comb[,c("ppt_May","ppt_MayOLD","ppt_May_t.1")])
# pairs(z, upper.panel = panel.cor);
# 
# hist(freq=F,z$ppt_May,col=rgb(1,0,0,0.5))
# hist(freq=F,z$ppt_MayOLD, add= T, col=rgb(0,0,1,0.5))
# hist(freq=F,z$ppt_May_t.1, add= T, col=rgb(0,0,1,1))
# 
# #Tmin
# y<-na.omit(comb[,c("absTmin","tmin","tmin_t.1")])
# pairs(y, upper.panel = panel.cor);
# 
# hist(freq=F,y$absTmin,col=rgb(1,0,0,0.5))
# hist(freq=F,y$tmin, add= T, col=rgb(0,0,1,0.5))

#### Some DT data manipulations ----
data$StartDate<-as.Date(data$DATE, format= "%m/%d/%Y") #convert to StartDate

### Take the annual average of each measurement per site
data$Avg_T_WEED<-rowMeans(data[,c("T_WEED1","T_WEED2","T_WEED3","T_WEED4","T_WEED5","T_WEED6","T_WEED7","T_WEED8","T_WEED9","T_WEED10")], na.rm=TRUE)
data$Avg_O_WEED<-rowMeans(data[,c("O_WEED1","O_WEED2","O_WEED3","O_WEED4","O_WEED5","O_WEED6","O_WEED7","O_WEED8","O_WEED9","O_WEED10")], na.rm=TRUE)
data$Avg_FORB<-rowMeans(data[,c("FORB1","FORB2","FORB3","FORB4","FORB5","FORB6","FORB7","FORB8","FORB9","FORB10")], na.rm=TRUE)
data$Avg_GRASS<-rowMeans(data[,c("GRASS1","GRASS2","GRASS3","GRASS4","GRASS5","GRASS6","GRASS7","GRASS8","GRASS9","GRASS10")], na.rm=TRUE)
data$Avg_B_GROUND<-rowMeans(data[,c("B_GROUND1","B_GROUND2","B_GROUND3","B_GROUND4","B_GROUND5","B_GROUND6","B_GROUND7","B_GROUND8","B_GROUND9","B_GROUND10")], na.rm=TRUE)
data$Avg_LITTER<-rowMeans(data[,c("LITTER1","LITTER2","LITTER3","LITTER4","LITTER5","LITTER6","LITTER7","LITTER8","LITTER9","LITTER10")], na.rm=TRUE)
data$Avg_MOSS<-rowMeans(data[,c("MOSS1","MOSS2","MOSS3","MOSS4","MOSS5","MOSS6","MOSS7","MOSS8","MOSS9","MOSS10")], na.rm=TRUE)
data$Avg_NU_STEM<-rowMeans(data[,c("NU_STEM1","NU_STEM2","NU_STEM3","NU_STEM4","NU_STEM5","NU_STEM6","NU_STEM7","NU_STEM8","NU_STEM9","NU_STEM10")], na.rm=TRUE)
data$Avg_HEIGHT<-rowMeans(data[,c("HEIGHT1","HEIGHT2","HEIGHT3","HEIGHT4","HEIGHT5","HEIGHT6","HEIGHT7","HEIGHT8","HEIGHT9","HEIGHT10")], na.rm=TRUE)
data$Avg_INSECTS<-rowMeans(data[,c("INSECTS_1","INSECTS_2","INSECTS_3","INSECTS_4","INSECTS_5","INSECTS_6")], na.rm=TRUE)

#### subset df to get the averages
df<-data[,c("SITE_NAME","Site.Code", "X_Coord","Y_Coord","YEAR","Avg_T_WEED","Avg_O_WEED","Avg_FORB","Avg_GRASS","Avg_B_GROUND", "Avg_LITTER",
            "Avg_MOSS","Avg_NU_STEM","Avg_HEIGHT","Avg_INSECTS")]

### bind climate data to DT data
df<-join(df, climate, by=c("Site.Code", "YEAR"))
names(df)
df<-na.omit(df)

# #monthly site data 
# data$DATE<-as.Date(data$DATE, format= "%m/%d/%Y")
# library(data.table)
# setDT(data)[, month := format(as.Date(DATE), "%m") ]
# setDT(data)[, day := format(as.Date(DATE), "%d") ]

# df2<-data[,c("Avg_T_WEED","Avg_O_WEED","Avg_FORB","Avg_GRASS","Avg_B_GROUND", "Avg_LITTER","Avg_MOSS","Avg_NU_STEM","Avg_HEIGHT","Avg_INSECTS","ppt_May","absTmin")]
# df2$month <- as.numeric(df2$month)

library(tidyr)
library(dplyr)


df.new <- df %>%
  pivot_longer(
    cols = c("Avg_T_WEED","Avg_O_WEED","Avg_FORB","Avg_GRASS","Avg_B_GROUND", "Avg_LITTER","Avg_MOSS","Avg_NU_STEM","Avg_HEIGHT","Avg_INSECTS","ppt_May","absTmin"),
    names_to = "variable",
    values_to = "value"
  )

# Add missing annual observations using min function
df.min <- df.new %>%
  group_by(SITE_NAME, YEAR, variable) %>%
  dplyr::summarise(min_value = min(value)) %>%
  pivot_wider(
    names_from = variable,
    values_from = min_value
  ) %>%
  ungroup() %>%
  arrange(SITE_NAME, YEAR)

df.min<-na.omit(df.min)
# sort
df.min<-df.min[order(df.min$SITE_NAME,df.min$YEAR),]
head(df.min)

# # check to make sure all site by year combos are here
# length(levels(df.min$SITE_NAME))*length(levels(df.min$YEAR)) ## number of rows (site X year combos)

#### Create lags and calculate interannual change in stem density (NU_STEM) and Mecinus----
dt<-tibble::as_tibble(df.min)
dt
glimpse(dt)
# Exporting a data frame to a CSV file
#write.csv(dt, "dt.csv", row.names = FALSE)


names(dt)
## Stem density
library(dplyr)
dt <- dt %>%
  arrange(SITE_NAME, YEAR) %>%
  group_by(SITE_NAME) %>%
  mutate(lnStems = log(Avg_NU_STEM + 1),    
         lnStemst_1 = lag(lnStems),   
         R_Stems = lnStems - lnStemst_1
  )%>%
  ungroup()

## Create lagged vars for Mecinus
names(dt)
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(lnMecinus = log(Avg_INSECTS+1), 
         lnMecinust_1 = lag(lnMecinus), 
         R_Mecinus = lnMecinus-lnMecinust_1
  )%>%
  ungroup()

## Create lagged vars and R for other weeds

dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(lnO_WEED = log(Avg_O_WEED+1),
         lnO_WEEDt_1 = lag(lnO_WEED)
  )%>%
  ungroup()

## Create lagged vars for GRASS

dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(lnGRASS = log(Avg_GRASS+1), 
         lnGRASSt_1 = lag(lnGRASS)
         )%>%
  ungroup()


## Create lagged vars for B_GROUND
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(lnBGROUND = log(Avg_B_GROUND+1), 
         lnBGROUNDt_1 = lag(lnBGROUND)
         )%>%
  ungroup()


## Create lagged vars for FORBS
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(lnFORB = log(Avg_FORB+1), 
         lnFORBt_1 = lag(lnFORB)
         )%>%
  ungroup()
  	

## Create standardized vals and lag for pptMay
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(ppt_Mayt_1 = lag(ppt_May))### Create lagged var  ( lag)

## Create lagged vars for absTmin
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(absTmint_1 = lag(absTmin))### Create lagged var  ( lag)

#### Drop sites with less than 5 years of data----
sitesamp<-unique(data[,c("SITE_NAME","YEAR")])
siteyr<-aggregate.data.frame(sitesamp$YEAR, by=list(sitesamp$SITE_NAME), FUN= length)

keep<-siteyr[siteyr$x >4,]
keep<-droplevels(keep)
colnames(keep)<-c("SITE_NAME", "yr_mon")
site<-c(keep$SITE_NAME)
dt<-dt[dt$SITE_NAME %in% site,]

#write.csv(dt, "dt.csv", row.names = FALSE)
#### Append site metdata to dt
df<-join(dt, sites, by ="SITE_NAME")
head(df)

# calc years after Mecinus release of monitoring year
df$yrs_aft_rel<-as.numeric(as.character(df$YEAR))-as.numeric(df$rel_yr)	
str(df)
max(df$yrs_aft_rel, na.rm=T)
df<-na.omit(df)


library(ggplot2)
ggplot(data = df, aes(x = lnStemst_1, y = R_Stems)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col="red")+
  labs(
    x = "Stem density/site/year (log)",
    y = "Interannual change in stem density (log)") +
  theme_minimal()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15)) 
library(piecewiseSEM)
library(nlme)
lm_model <- lme(R_Stems ~ lnStemst_1, method="ML", random = ~1|SITE_NAME, data = df)
rsquared(lm_model)

library(nlme)
resid<-lme(R_Stems~lnStemst_1,na.action= na.exclude, data=df, method="ML", random = ~1 |SITE_NAME)
df$R_resids<-resid(resid)


ggplot(data = df, aes(x = ppt_Mayt_1, y = R_resids)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col="red")+
  labs(
    x = "Annual precipitation/site (mm)",
    y = "Interannual change in stem density (log)") +
  theme_minimal()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15)) 
lm_model <- lme(R_resids ~ ppt_May, method="ML", random = ~1|SITE_NAME, data = df)
rsquared(lm_model)

