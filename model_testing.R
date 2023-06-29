#I think its a good idea to go over Aaron's code again and compare with what ur 
#doing 
#plop code in here and make sure to annotate and check often that things look okay

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
  ## recieved warning message: "Warning message:
    #In left_join(sites, climate, by = c("Longitude", "Latitude")) :
    #Each row in `x` is expected to match at most 1 row in `y`." I think it's benign but going to make a note of it
  
#for right now, I'm just going to focus on calculating precipitation not temp
  #Aaron said he could help convert cast to tidyverse - don't trust cast/melt
library(reshape)
library(reshape2)
library(MASS)
  #melt and calc stats
  clim.melt<-melt(climate, id.vars=c("SITE_NAME" , "month", "year","day" ), measure.vars=c("tmax","tmin","pr", "srad", "swe", "soil"))
  head(clim.melt) #going to go with it anyway and fix later 
  
  #avg monthly mean prec for each site
  meanPrec <-cast(clim.melt, SITE_NAME + year + month ~ variable, subset = clim.melt$variable == "pr", fun=mean) 
  ####### Cummulative precip from 1 May in year t-1 to 30 April in year t----
  # sum pr per month
   summprMon<-cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "pr" ,fun= sum )
  
  ## create variable for summing pr in each "water" year
  ## the water yr is the year that could affect R (between dates when pops were measured)
  summprMon$wateryr<-ifelse(summprMon$month >4, summprMon$year+1, summprMon$year)
  head(summprMon)
  
  summprMay<-aggregate.data.frame(summprMon$pr, by= list(summprMon$SITE_NAME,summprMon$wateryr), FUN=sum)
  head(summprMay)
  colnames(summprMay)<-c("SITE_NAME","YEAR", "pr_May")## naming wateryr to YEAR for joining
  head(summprMay)
  
  ##drop 2022 water yr becasue of incomplete record
  summprMay<-summprMay[summprMay$YEAR < "2022",]
  
  #### Some DT data manipulations ----
  data$StartDate<-as.Date(data$DATE, format= "%m/%d/%Y") #convert to StartDate
  
  # subset df to get the averages
  df<-data[,c("SITE_NAME", "longitude","latitude","YEAR","TargetWeed","OtherWeed","Forbs","Grass","BareGround", "Litter",
              "Moss","NumStems","HeightTall","Insects", "ReleaseYear")]
  # bind climate data to DT data
  df<-join(df, summprMay, by=c("SITE_NAME", "YEAR"))
  names(df)
  
  #### create molten data frame (all values are represented for each site*time combination)
  #COME BACK TO THIS TO ADD MORE CLIMATE VARIABLES~~~~~
  df.melt<-melt(df, id.vars=c("SITE_NAME" ,"YEAR" ), measure.vars=c("longitude" , "latitude"   ,   "TargetWeed", "OtherWeed" , "Forbs", "Grass"   ,  
                                                                    "BareGround", "Litter"  ,   "Moss"    ,   "NumStems"  , "HeightTall" ,"Insects"   , "ReleaseYear" , 
                                                                    "pr_May" ))
  head(df.melt)
  
##### Add in missing annual observations for final analysis file
  ## this just reshapes the dataframe, if there are more than two obs per site/month/Year combination than the 'fun' argument may have to change. 
  df.long<-cast(df.melt,SITE_NAME + YEAR  ~ variable , min)# using min function because only one value per cell
  
  # sort
  df.long<-df.long[order(df.long$SITE_NAME,df.long$YEAR),]
  head(df.long)

#### Create lags and calculate interannual change in stem density (NU_STEM) and Mecinus----
  dt<-tbl_df(df.long)
  dt
  glimpse(dt)
  
  names(dt)
  
#the last couple of steps, I'm unsure about. I need to look at them again and decide how it looks 
  
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
  
  ## Create standardized vals and lag for prMay
  dt<-dt%>%
    arrange(SITE_NAME,YEAR)%>%
    group_by(SITE_NAME)%>%
    mutate(pr_Mayt_1 = lag(pr_May))### Create lagged var  ( lag)
  
  view(dt)
  
#### Drop sites with less than 5 years of data----
  sitesamp<-unique(data[,c("SITE_NAME","YEAR")])
  siteyr<-aggregate.data.frame(sitesamp$YEAR, by=list(sitesamp$SITE_NAME), FUN= length)
  
  keep<-siteyr[siteyr$x >4,]
  keep<-droplevels(keep)
  colnames(keep)<-c("SITE_NAME", "yr_mon")
  site<-c(keep$SITE_NAME)
  dt<-dt[dt$SITE_NAME %in% site,]
  
  #### Append site metdata to dt
df<-join(dt, sites, by=c("SITE_NAME"))
 
  
#removing NA values/rows throughout data
  df<- na.omit(df)
  
  
#clean up df a little 
  stemdf2<-df[,c("SITE_NAME","YEAR","R_Stems","lnStemst_1","lnMecinust_1","lnGRASSt_1","lnO_WEEDt_1","lnBGROUNDt_1","pr_May","pr_Mayt_1")]
  
  stemdf2$lnStemst_1 <- as.numeric(stemdf2$lnStemst_1)
  stemdf2$lnMecinust_1 <- as.numeric(stemdf2$lnMecinust_1)
  stemdf2$lnGRASSt_1 <- as.numeric(stemdf2$lnGRASSt_1)
  stemdf2$lnO_WEEDt_1 <- as.numeric(stemdf2$lnO_WEEDt_1)
  stemdf2$lnBGROUNDt_1 <- as.numeric(stemdf2$lnBGROUNDt_1)
  stemdf2$pr_Mayt_1 <- as.numeric(stemdf2$pr_Mayt_1)
  stemdf2$pr_May <- as.numeric(stemdf2$pr_May)
  
#modeling time ayo ----
  library(nlme)
  lmm1<-lme(R_Stems ~ lnStemst_1 + lnMecinust_1 +lnGRASSt_1 + lnO_WEEDt_1 + lnBGROUNDt_1
            + pr_Mayt_1 + pr_May*lnMecinust_1 + pr_May*lnStemst_1, 
            method="ML", random = ~1|SITE_NAME, data = stemdf2, na.action= na.fail)
  VarCorr(lmm1)
  summary(lmm1)
  anova(lmm1)
  
#correlation matrix to see how it's looking ----
  # Create a subset of the variables of interest
  predictors <- c("lnStemst_1", "lnMecinust_1", "lnGRASSt_1", "lnBGROUNDt_1", "pr_May")
  subset_df <- stemdf2[, predictors]
  
  # Calculate the correlation matrix
  cor_matrix <- cor(subset_df)
  
  # Print the correlation matrix
  print(cor_matrix)
  
  
#okay, I don't know when I'm having issues with this model right off the bat
  #its giving me NaNs for all the p-values and I don't remember what I did for 
  #stemdf for it to work ahahaha -- i figured it out :)
  
#looking at the global model and seeing what's up 
  library(MuMIn)
  ms1<-dredge(lmm1)
  head(ms1)
  top.models.1 <-model.avg(get.models(ms1, subset = delta < 2))# Model averaging based on an information criterion, here chnadifference in AIC score <4)(models with delta.aicc < 2)
  top.models.1

  
# What Aaron did for prec: From these data, we summed precipitation (mm) that fell at a site 
 # between 1 May [Julian day (JD): 122] in the year prior (t-1) to 30 April (JD:
 # 121) in the next year (t). We choose to sum precipitation during this 
 # timeframe because all sites were monitored from mid-May until mid-July 
 # (Table 1) and we expected plant growth in each year to be most strongly 
 # affected by precipitation that fell between
 # monitoring dates.
  
  
