#just going to edit a little and upload it to git and see what happens

#again another change
library(tidyverse)
library(magrittr)
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
  
#keep the earliest release year per site 
library(dplyr)
  releases <- releases %>%
    group_by(SITE_NAME) %>%
    arrange(ReleaseYear) %>%
    slice(1L)

#join release years to sites 
library(plyr)
  data<-left_join(idsimp, releases, by=c("SITE_NAME"))
  names(data)

#join sites and climate for data with site names 
  climate<-left_join(sites, climate, by= c("Longitude","Latitude"))
  head(climate)
  
library(reshape)
library(reshape2)
library(MASS)
#melt and calc stats
  clim.melt<-melt(climate, id.vars=c("SITE_NAME" , "month", "year","day" ), measure.vars=c("tmax","tmin","pr", "srad", "swe", "soil"))
  head(clim.melt)
  
#absolute values for coldest night of winter 
#Coldest night of winter between July year n to July n+1----
### Because R is calc'd lnNt+1 -lnNt, the Tmin that matters would occur in either Nov or Dec of yeart or Jan/Feb of year t+1. 
## becuase of this, I added 1 to each year in which Nov/Dec Tmin was calculated to calc abs Tmin in the same winter
#When does the coldest night occur? 
#related to the fact that the coldest night of the year is the night that is most physiologcally important to the insect
  absTminJan<-cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "tmin" & clim.melt$month == "1" ,fun= min )
  absTminFeb<-cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "tmin" & clim.melt$month == "2" ,fun= min )
  absTminDec<-cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "tmin" & clim.melt$month == "12" ,fun= min )
  absTminDec$year<-absTminDec$year+1
  absTminNov<-cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "tmin" & clim.melt$month == "11" ,fun= min )
  absTminNov$year<-absTminNov$year+1

#join the temps determine the absol Tmin during same winter
  absTmin<-join(absTminNov,absTminDec, by=c("SITE_NAME","year"))
  absTmin<-join(absTmin,absTminJan, by=c("SITE_NAME","year"))
  absTmin<-join(absTmin,absTminFeb, by=c("SITE_NAME","year"))
  
  head(absTmin)
  absTmin<-absTmin[,c(1,2,4,6,8, 10)]
  colnames(absTmin)<-c("SITE_NAME","YEAR","Nov","Dec","Jan", "Feb")
  
  ## find coldest night of winter, from Nov to Feb, in each year
  absTmin$absTmin <-apply(absTmin[,3:6],1,min)  
  head(absTmin)
  
  ##average monthly min/max, prec, radiance, snow water equivalent, and soil moisture for each site 
  #monthly min temp 
  maxTemp <-cast(clim.melt, SITE_NAME + year + month ~ variable, subset = clim.melt$variable == "tmax", fun=max)
  minTemp <-cast(clim.melt, SITE_NAME + year + month ~ variable, subset = clim.melt$variable == "tmin", fun=min)
  meanPrec <-cast(clim.melt, SITE_NAME + year + month ~ variable, subset = clim.melt$variable == "pr", fun=mean) 
  #to convert mm to cm 
  meanPrec$pr <- as.numeric(meanPrec$pr) / 10
  meansrad <-cast(clim.melt, SITE_NAME + year + month ~ variable, subset = clim.melt$variable == "srad", fun=mean)
  meanswe <-cast(clim.melt, SITE_NAME + year + month ~ variable, subset = clim.melt$variable == "swe", fun=mean)
  meansoil <-cast(clim.melt, SITE_NAME + year + month ~ variable, subset = clim.melt$variable == "soil", fun=mean)
  
  #join mean climate data for monthly data 
  meanclim <- join(maxTemp, minTemp, by=c("SITE_NAME", "year", "month"))
  meanclim <- join(meanclim, meanPrec, by=c("SITE_NAME", "year", "month"))
  meanclim <- join(meanclim, meansrad, by=c("SITE_NAME", "year", "month"))
  meanclim <- join(meanclim, meanswe, by=c("SITE_NAME", "year", "month"))
  meanclim <- join(meanclim, meansoil, by=c("SITE_NAME", "year", "month"))
  colnames(meanclim)<-c("SITE_NAME", "YEAR", "month","tmax", "tmin","meansrad", "meanswe", "meansoil", "meanpr")
  
  
  ####### Cummulative precip from 1 May in year t-1 to 30 April in year t----
  # sum pr per month
  summprMon<-cast(clim.melt,SITE_NAME + year+ month  ~ variable, subset= clim.melt$variable == "pr" ,fun= sum )
  summprMon$pr <- as.numeric(summprMon$pr) / 10
  
  ## create variable for summing pr in each "water" year
  ## the water yr is the year that could affect R (between dates when pops were measured)
  summprMon$wateryr<-ifelse(summprMon$month >4, summprMon$year+1, summprMon$year)
  head(summprMon)
  
  summprMay<-aggregate.data.frame(summprMon$pr, by= list(summprMon$SITE_NAME,summprMon$wateryr), FUN=sum)
  head(summprMay)
  colnames(summprMay)<-c("SITE_NAME","YEAR", "pr_May")## naming wateryr to YEAR for binding
  
  
##drop 2022 water yr becasue of incomplete record
  summprMay<-summprMay[summprMay$YEAR < "2022",]
  
##### Combine climate data for analysis
  climate<-join(summprMay, absTmin[,c(1,2,7)], by=c("SITE_NAME", "YEAR"))
  climate<-join(climate, meanclim, by=c("SITE_NAME", "YEAR"))
  head(climate)
  
#### Some DT data manipulations ----
  data$StartDate<-as.Date(data$DATE, format= "%m/%d/%Y") #convert to StartDate
  
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
  
  df2<-join(df2, meanclim, by=c("SITE_NAME","month","YEAR"))
#from here df2 has temperature data for the observations specifically, but the rest of the climate data is not included 
#perhaps make another dataframe with the rest of the climate variables in it and then join it to this one? 
#has to be a way to make a dataframe that has the corresponding data matching with the rest of the data also there
#-------
#annual 
  
  
#### create molten data frame (all values are represented for each site*time combination)
#COME BACK TO THIS TO ADD MORE CLIMATE VARIABLES~~~~~
  df.melt<-melt(df, id.vars=c("SITE_NAME" ,"YEAR" ), measure.vars=c("longitude" , "latitude"   ,   "TargetWeed", "OtherWeed" , "Forbs", "Grass"   ,  
                                                                    "BareGround", "Litter"  ,   "Moss"    ,   "NumStems"  , "HeightTall" ,"Insects"   , "ReleaseYear" , 
                                                                    "pr_May"  ,   "absTmin"  , "tmin" , "tmax" ))
  head(df.melt)
  
##### Add in missing annual observations for final analysis file
## this just reshapes the dataframe, if there are more than two obs per site/month/Year combination than the 'fun' argument may have to change. 
  df.long<-cast(df.melt,SITE_NAME + YEAR  ~ variable , min)# using min function becauise only one value per cell
  
# sort
  df.long<-df.long[order(df.long$SITE_NAME,df.long$YEAR),]
  head(df.long)
  
#### Create lags and calculate interannual change in stem density (NU_STEM) and Mecinus----
  dt<-tbl_df(df.long)
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
  
## Create lagged vars for tmin 
  dt<-dt%>%
    arrange(SITE_NAME,YEAR)%>%
    group_by(SITE_NAME)%>%
    mutate(tmin_1 = lag(tmin))
  
## Create lagged vars for tmax
  dt<-dt%>%
    arrange(SITE_NAME,YEAR)%>%
    group_by(SITE_NAME)%>%
    mutate(tmax_1 = lag(tmax))
  
  View(dt)
  
  dt %>% select(SITE_NAME, YEAR,lnStems, lnMecinus) %>% 
    pivot_longer(cols=  c(lnStems, lnMecinus), values_to = "value", names_to = "variable") %>% 
    ggplot(aes(x = YEAR, y = value, color= variable)) +
    geom_point()+ geom_line()+
    theme_classic() +
    facet_wrap(~SITE_NAME)+
    theme(legend.position = "top", axis.text = element_text(size = 14), axis.title = element_text(size = 14),
          axis.text.x= element_text(angle = 0), strip.text = element_text(size =  14, face ="bold"),legend.text= element_text(size = 14),legend.title= element_text(size = 14))
  
  
  
  
#### Drop sites with less than 5 years of data----
  sitesamp<-unique(data[,c("SITE_NAME","YEAR")])
  siteyr<-aggregate.data.frame(sitesamp$YEAR, by=list(sitesamp$SITE_NAME), FUN= length)
  
  keep<-siteyr[siteyr$x >4,]
  keep<-droplevels(keep)
  colnames(keep)<-c("SITE_NAME", "yr_mon")
  site<-c(keep$SITE_NAME)
  dt<-dt[dt$SITE_NAME %in% site,]
  #### Append site metdata to dt
  df<-join(dt, sites, by ="SITE_NAME")
  df<-join(df, meanclim, by =c("SITE_NAME", "YEAR"))
  
#remove sites with no data
  df<-subset(df, SITE_NAME!="IDAHO BORDER" & SITE_NAME!="PINE CREEK" & SITE_NAME!="BALDY PALISADES")
  head(df)
  
#removing NA values/rows throughout data
  df <- na.omit(df)
## calc years after Mecinus release of monitoring year
  df$yrs_aft_rel<-as.numeric(as.character(df$YEAR))-as.numeric(df$ReleaseYear)	
  str(df)
  max(df$yrs_aft_rel, na.rm=T)
  
#global model to see which model and what variables actually matter 
##This is where i am having troubles and where it isn't working 
  stemdf<-df[,c("SITE_NAME","YEAR","R_Stems","lnStemst_1","lnMecinust_1","lnGRASSt_1","lnO_WEEDt_1","lnBGROUNDt_1","pr_May","pr_Mayt_1","absTmin", "meansrad", "meanswe", "meansoil")]

# import stemdf data from Julia
  stemdf <- read_csv("stemdf.csv")
  
  names(stemdf)
# plot the data ----- 
  
  stemdf %>% 
    ggplot(aes(x = lnStemst_1, y = R_Stems)) +
    geom_point()+
    geom_smooth(method ="lm")+
    theme_classic() +
    facet_wrap(~SITE_NAME)+
    theme(legend.position = "top", axis.text = element_text(size = 14), axis.title = element_text(size = 14),
          axis.text.x= element_text(angle = 0), strip.text = element_text(size =  14, face ="bold"),legend.text= element_text(size = 14),legend.title= element_text(size = 14))
  
  
  stemdf %>% 
    ggplot(aes(x = lnMecinust_1, y = R_Stems)) +
    geom_point()+
    geom_smooth(method ="lm")+
    theme_classic() +
    facet_wrap(~SITE_NAME)+
    theme(legend.position = "top", axis.text = element_text(size = 14), axis.title = element_text(size = 14),
          axis.text.x= element_text(angle = 0), strip.text = element_text(size =  14, face ="bold"),legend.text= element_text(size = 14),legend.title= element_text(size = 14))
  
  
  library(nlme)
  lmm1<-lme(R_Stems ~ lnStemst_1 + lnMecinust_1 + lnGRASSt_1 + lnBGROUNDt_1 
            + pr_May + pr_May*lnGRASSt_1+ pr_May*lnMecinust_1 +
              pr_May*lnMecinust_1 , method="ML", random = ~1|SITE_NAME, data = stemdf, na.action= na.fail)
  VarCorr(lmm1)
  summary(lmm1)
  anova(lmm1)

  
#some places on the internet told me to use lmer instead of lme, below is my attempt - still wrong
library(lme4)
  lmm2<-lmer(R_Stems ~ lnStemst_1 + lnMecinust_1 + lnGRASSt_1 + lnBGROUNDt_1 + pr_May + absTmin + pr_May*lnGRASSt_1+
              pr_May*lnMecinust_1 +  (1 | SITE_NAME), data=stemdf, REML = TRUE, na.action= na.fail)
  summary(lmm2)   
  
library(MuMIn)
  ms1<-dredge(lmm1)
  head(ms1)
  top.models.1 <-model.avg(get.models(ms1, subset = delta < 2))# Model averaging based on an information criterion, here chnadifference in AIC score <4)(models with delta.aicc < 2)
  top.models.1
  
  ms2<-dredge(lmm2)
  head(ms2)
  top.models.2 <-model.avg(get.models(ms2, subset = delta < 2))# Model averaging based on an information criterion, here chnadifference in AIC score <4)(models with delta.aicc < 2)
  top.models.2
  