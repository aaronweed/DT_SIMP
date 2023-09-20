#I think its a good idea to go over Aaron's code again and compare with what ur 
#doing 
#plop code in here and make sure to annotate and check often that things look okay

#edit cast/melt so that they're in tidyverse 
library(tidyverse)
library(magrittr)
library(readr)
#import needed data 

climate <- read_csv("DT_weather_2007-2022.csv")
head(climate)

idsimp <- read_csv("DT_SIMP_Data_Idaho.csv")
head(idsimp)

sites <- read_csv("IdahoSites.csv")
head(sites)

releases <- readxl::read_xls("IDAHO_RELEASE_SITES.xls")
head(releases)

releases <- releases[,c("ReleaseYear", "SITE_NAME")]
names(releases)

#keep the earliest release year per site - deleting the rest 
library(dplyr)
releases <- releases %>%
  group_by(SITE_NAME) %>%
  arrange(ReleaseYear) %>%
  slice(1L)  

#join release years to sites 
library(plyr) 
data<-left_join(idsimp, releases, by=c("SITE_NAME")) #adding the "ReleaseYear variable to idsimp
names(data)

#join sites and climate for data with site names 
climate<-left_join(sites, climate, by= c("Longitude","Latitude"))
head(climate)

#converted to tidyverse 
library(tidyverse)

clim.melt <- climate %>%
  pivot_longer(cols = c(tmax, tmin, pr, srad, swe, soil),
               names_to = "variable",
               values_to = "value") %>%
  dplyr::select(SITE_NAME, month, day, year, variable, value)


# Filter the data for tmin, and month == 1
absTminJan <- clim.melt %>%
  filter(variable == "tmin", month == 1) %>%
  group_by(SITE_NAME, year, month) %>%
  dplyr::summarise(pr_month=min(value))
  
#need to figure out how to find the tmin for the month=1


# Filter the data for tmin, and month == 2, then calculate the minimum value
absTminFeb <- clim.melt %>%
  filter(variable == "tmin", month == 2) %>%
  group_by(SITE_NAME, year, month) %>%
  dplyr::summarise(pr_month=min(value))

# Filter the data for tmin, and month == 12, then calculate the minimum value
absTminDec <- clim.melt %>%
  filter(variable == "tmin", month == 12) %>%
  group_by(SITE_NAME, year, month) %>%
  dplyr::summarise(pr_month=min(value))


# Filter the data for tmin, and month == 11, then calculate the minimum value
absTminNov <- clim.melt %>%
  filter(variable == "tmin", month == 11) %>%
  group_by(SITE_NAME, year, month) %>%
  dplyr::summarise(pr_month=min(value))


# Join absTminNov and absTminDec by SITE_NAME and year
absTmin <- join(absTminNov, absTminDec, by = c("SITE_NAME", "year"))

# Join the result with absTminJan by SITE_NAME and year
absTmin <- left_join(absTmin, absTminJan, by = c("SITE_NAME", "year"))

# Join the result with absTminFeb by SITE_NAME and year
absTmin <- left_join(absTmin, absTminFeb, by = c("SITE_NAME", "year"))

absTmin<-absTmin[,c(1,2,4,6,8,10)]
colnames(absTmin)<-c("SITE_NAME","YEAR","Nov","Dec","Jan", "Feb")
absTmin$absTmin <-apply(absTmin[,3:6],1,min)  

####### Cummulative precip from 1 May in year t-1 to 30 April in year t----
# sum pr per month

summprMon <- clim.melt %>%
  filter(variable == "pr") %>%
  group_by(SITE_NAME, year, month) %>%
  dplyr::summarise(pr_month=sum(value/10)) #convert mm to cm

library(dplyr)

# Create variable for summing pr in each "water" year
# The water year is the year that could affect R (between dates when pops were measured)
summprMon <- summprMon %>%
  mutate(wateryr = ifelse(month > 4, year + 1, year))

# Calculate the sum of precipitation for May by SITE_NAME and wateryr
summprMay <- summprMon %>%
  group_by(SITE_NAME, wateryr) %>%
  dplyr::summarise(pr_May = sum(pr_month))

# Rename columns
colnames(summprMay) <- c("SITE_NAME", "YEAR", "pr_May")

# Filter out records from the year 2022
summprMay <- summprMay %>%
  filter(YEAR < 2022)


# Join the summprMay and absTmin data frames by SITE_NAME and YEAR
climate<-join(summprMay, absTmin, by=c("SITE_NAME", "YEAR"))
climate<-climate[,c(1,2,3,8)]

# Remove rows with NA values
climate <- na.omit(climate)

# Convert the DATE column to StartDate using as.Date
data <- data %>%
  mutate(StartDate = as.Date(DATE, format = "%m/%d/%Y"))

#### subset df to get the averages
df<-data[,c("SITE_NAME", "longitude","latitude","YEAR","TargetWeed","OtherWeed","Forbs","Grass","BareGround", "Litter",
            "Moss","NumStems","HeightTall","Insects", "ReleaseYear")]
### bind climate data to DT data
df<-join(df, climate, by=c("SITE_NAME", "YEAR"))
names(df)

#monthly site data 
data$DATE<-as.Date(data$DATE, format= "%m/%d/%Y")
library(data.table)
setDT(data)[, month := format(as.Date(DATE), "%m") ]
setDT(data)[, day := format(as.Date(DATE), "%d") ]

df2<-data[,c("SITE_NAME", "longitude","latitude","YEAR", "month", "day","TargetWeed","OtherWeed","Forbs","Grass","BareGround", "Litter",
             "Moss","NumStems","HeightTall","Insects", "ReleaseYear")]
df2$month <- as.numeric(df2$month)

library(tidyr)
library(dplyr)

df$ReleaseYear <- as.numeric(df$ReleaseYear)
df.new <- df %>%
  pivot_longer(
    cols = c("longitude", "latitude", "TargetWeed", "OtherWeed", "Forbs", "Grass",
             "BareGround", "Litter", "Moss", "NumStems", "HeightTall", "Insects",
             "ReleaseYear", "pr_May", "absTmin"),
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
  
#### Create lags and calculate interannual change in stem density (NU_STEM) and Mecinus----
dt<-tbl_df(df.min)
dt
glimpse(dt)
names(dt)  

## Stem density
dt$NumStems <- as.numeric(dt$NumStems)
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(lnStems = log(NumStems+1))%>%### log trans
  mutate(lnStemst_1 = lag(lnStems))%>%### Create lagged var  ( lag)
  mutate(lnStemst_2 = lag(lnStemst_1))%>%### Create lagged var  ( lag)
  mutate(R_Stems = lnStems-lnStemst_1)

## Create lagged vars for Mecinus
dt$Insects<-as.numeric(dt$Insects)
names(dt)
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(lnMecinus = log(Insects+1))%>%### log trans
  mutate(lnMecinust_1 = lag(lnMecinus))%>%### Create lagged var  ( lag)
  mutate(lnMecinust_2 = lag(lnMecinust_1))%>%### Create lagged var  ( lag)
  mutate(R_Mecinus = lnMecinus-lnMecinust_1)

## Create lagged vars and R for other weeds
dt$OtherWeed<-as.numeric(dt$OtherWeed)
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(lnO_WEED = log(OtherWeed+1))%>%### log trans
  mutate(lnO_WEEDt_1 = lag(lnO_WEED))%>%### Create lagged var  ( lag)
  mutate(lnO_WEEDt_2 = lag(lnO_WEEDt_1))### Create lagged var  ( lag)

## Create lagged vars for GRASS
dt$Grass<-as.numeric(dt$Grass)
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(lnGRASS = log(Grass+1))%>%### log trans
  mutate(lnGRASSt_1 = lag(lnGRASS))%>%### Create lagged var  ( lag)
  mutate(lnGRASSt_2 = lag(lnGRASSt_1))### Create lagged var  ( lag)

## Create lagged vars for B_GROUND
dt$BareGround<-as.numeric(dt$BareGround)
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(lnBGROUND = log(BareGround+1))%>%### log trans
  mutate(lnBGROUNDt_1 = lag(lnBGROUND))%>%### Create lagged var  ( lag)
  mutate(lnBGROUNDt_2 = lag(lnBGROUNDt_1))### Create lagged var  ( lag)

## Create lagged vars for FORBS
dt$Forbs<-as.numeric(dt$Forbs)
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(lnFORB = log(Forbs+1))%>%### log trans
  mutate(lnFORBt_1 = lag(lnFORB))%>%### Create lagged var  ( lag)
  mutate(lnFORBt_2 = lag(lnFORBt_1))### Create lagged var  ( lag)		

## Create standardized vals and lag for pptMay
dt$pr_May <- as.numeric(dt$pr_May)
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(lnpr_May = log(pr_May+1))%>%### Create lagged var  ( lag)
  mutate(lnpr_May_1 = lag(lnpr_May))%>%
  mutate(R_pr_May = lnpr_May - lnpr_May_1)

## Create lagged vars for absTmin
dt$absTmin <- as.numeric(dt$absTmin)
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(pabsTmin = abs(absTmin))%>%
  mutate(lnabsTmint_1 = log(pabsTmin+1))%>%### Create lagged var  ( lag)
  mutate(lnabsTmint_2 = lag(lnabsTmint_1))%>%
  mutate(R_absTmin = absTmin - lnabsTmint_1)

## Create standardized vals and lag for prMay
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(pr_Mayt_1 = lag(pr_May))### Create lagged var  ( lag)

## Create lagged vars for absTmin
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(absTmint_1 = lag(absTmin))### Create lagged var  ( lag)

sitesamp<-unique(data[,c("SITE_NAME","YEAR")])
siteyr<-aggregate.data.frame(sitesamp$YEAR, by=list(sitesamp$SITE_NAME), FUN= length)

keep<-siteyr[siteyr$x >4,]
keep<-droplevels(keep)
colnames(keep)<-c("SITE_NAME", "yr_mon")
site<-c(keep$SITE_NAME)
dt<-dt[dt$SITE_NAME %in% site,]
#### Append site metdata to dt
df<-join(dt, sites, by ="SITE_NAME")

## calc years after Mecinus release of monitoring year
df$yrs_aft_rel<-as.numeric(as.character(df$YEAR))-as.numeric(df$ReleaseYear)	
str(df)
max(df$yrs_aft_rel, na.rm=T)

#global model to see which model and what variables actually matter 
##This is where i am having troubles and where it isn't working 
stemdf<-df[,c("SITE_NAME","YEAR","R_Stems","lnStemst_1","lnMecinust_1","lnGRASSt_1","lnO_WEEDt_1","lnBGROUNDt_1","pr_May","pr_Mayt_1", "absTmin", "absTmint_1")]
is.na(stemdf[,3])
stemdf <- na.omit(stemdf)

library(nlme)
lmm2<-lme(R_Stems ~ lnMecinust_1 +  
            + pr_Mayt_1 + pr_Mayt_1*lnMecinust_1 , method="ML", random = ~1|SITE_NAME, data = stemdf)
VarCorr(lmm2)
summary(lmm2)
anova(lmm2)

lmm3<-lme(R_Stems ~ lnMecinust_1 +lnGRASSt_1 + lnO_WEEDt_1 + lnBGROUNDt_1 + pr_Mayt_1 +absTmint_1 + 
            pr_May*absTmin+ pr_May*lnStemst_1 +absTmin*lnStemst_1 +absTmin*lnMecinust_1, 
          method="ML", random = ~1|SITE_NAME, data = stemdf, na.action= na.fail)
VarCorr(lmm3)
summary(lmm3)
anova(lmm3)

library(MuMIn)
ms3<-dredge(lmm3)
head(ms3)
top.models.2 <-model.avg(get.models(ms3, subset = delta < 2))# Model averaging based on an information criterion, here chnadifference in AIC score <4)(models with delta.aicc < 2)
top.models.2

library(MuMIn)
ms2<-dredge(lmm2)
head(ms2)
top.models.1 <-model.avg(get.models(ms2, subset = delta < 2))# Model averaging based on an information criterion, here chnadifference in AIC score <4)(models with delta.aicc < 2)
top.models.1


lmmplant<-lme(R_Stems ~ lnMecinust_1 + pr_Mayt_1 + absTmint_1, method="ML", random = ~1|SITE_NAME, data=stemdf)
VarCorr(lmmplant)
summary(lmmplant)
anova(lmmplant)


library(effects)
library(graphics)
plot(effect("lnStemst_1", lmm3), main = expression("Effects of Dalmatian Toadflax Stems on " * italic("M.janthiniformis")), 
     xlab = "log DT Stems",
     ylab = expression("Change in " *italic("M.janthiniformis")))


library(effects)
library(graphics)
plot(effect("absTmint_1", lmm3), main = expression("Effects of Coldest Night of Winter on " * italic("M.janthiniformis")), 
     xlab = "log Temperature in C",
     ylab = expression("Change in " *italic("M.janthiniformis")))

library(effects)
library(graphics)
plot(effect("pr_Mayt_1", lmm3), main = expression("Effects of Precipitation on " * italic("M.janthiniformis")), 
     xlab = "log Precipitation in mm",
     ylab = expression("Change in " *italic("M.janthiniformis")))

