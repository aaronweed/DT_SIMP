#Import climate data
library(readr)
climate<-read.csv("DT_weather_2007-2022.csv")
head(climate)

#Import SIMP and site data 
data <- read_csv("Full_DT_Data.csv")
head(data)

sites<-read.csv("IdahoSites.csv")
head(sites)

#Take averages of each site for vegetation (data was scaled in excel)
library(dplyr)
data <- data %>%
  group_by(SITE_NAME, YEAR, DATE) %>%
  dplyr::reframe(
    B_GROUND = rowMeans(select_if(across(starts_with("B_GROUND")), is.numeric), na.rm = TRUE),
    LITTER = rowMeans(select_if(across(starts_with("LITTER")), is.numeric), na.rm = TRUE),
    MOSS = rowMeans(select_if(across(starts_with("MOSS")), is.numeric), na.rm = TRUE),
    T_WEED = rowMeans(select_if(across(starts_with("T_WEED")), is.numeric), na.rm = TRUE),
    O_WEED = rowMeans(select_if(across(starts_with("O_WEED")), is.numeric), na.rm = TRUE),
    NU_STEM = rowMeans(select_if(across(starts_with("NU_STEM")), is.numeric), na.rm = TRUE),
    FORB = rowMeans(select_if(across(starts_with("FORB")), is.numeric), na.rm = TRUE),
    GRASS = rowMeans(select_if(across(starts_with("GRASS")), is.numeric), na.rm = TRUE),
    INSECTS = rowMeans(select_if(across(starts_with("INSECTS")), is.numeric), na.rm = TRUE)
  )


#join climate and sites together 
library(plyr)
climate<-join(sites, climate, by= c("Longitude","Latitude"))
head(climate)

#Begin climate data manipulation 
library(tidyverse)
clim.melt <- climate %>%
  pivot_longer(cols = c(tmax, tmin, pr, srad, swe, soil),
               names_to = "variable",
               values_to = "value") %>%
  dplyr::select(SITE_NAME, month, day, year, variable, value)

##filter data 
absTminJan <- clim.melt %>%
  filter(variable == "tmin", month == 1) %>%
  group_by(SITE_NAME, year, month) %>%
  dplyr::summarise(pr_month=min(value))

absTminFeb <- clim.melt %>%
  filter(variable == "tmin", month == 2) %>%
  group_by(SITE_NAME, year, month) %>%
  dplyr::summarise(pr_month=min(value))

absTminDec <- clim.melt %>%
  filter(variable == "tmin", month == 12) %>%
  group_by(SITE_NAME, year, month) %>%
  dplyr::summarise(pr_month=min(value))

absTminNov <- clim.melt %>%
  filter(variable == "tmin", month == 11) %>%
  group_by(SITE_NAME, year, month) %>%
  dplyr::summarise(pr_month=min(value))

##create absTmin dataframe 
absTmin <- join(absTminNov, absTminDec, by = c("SITE_NAME", "year"))
absTmin <- left_join(absTmin, absTminJan, by = c("SITE_NAME", "year"))
absTmin <- left_join(absTmin, absTminFeb, by = c("SITE_NAME", "year"))
absTmin<-absTmin[,c(1,2,4,6,8,10)]
colnames(absTmin)<-c("SITE_NAME","YEAR","Nov","Dec","Jan", "Feb")
absTmin$absTmin <-apply(absTmin[,3:6],1,min) 

#Now work on precipitation 
summprMon <- clim.melt %>%
  filter(variable == "pr") %>%
  group_by(SITE_NAME, year, month) %>%
  dplyr::summarise(pr_month=sum(value)) #convert cm to mm

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

##join absTmin and summprMay together 
climate<-join(summprMay, absTmin, by=c("SITE_NAME", "YEAR"))
climate<-climate[,c(1,2,3,8)]
climate <- na.omit(climate)

##
#### subset df to get the averages
df<-data[,c("SITE_NAME", "YEAR","T_WEED","O_WEED","FORB","GRASS",
            "B_GROUND", "LITTER", "MOSS","NU_STEM","INSECTS")]

### bind climate data to DT data
df<-join(df, climate, by=c("SITE_NAME", "YEAR"))
names(df)

##create molten data frame (all values are represented for each site*time combination)
library(reshape2)
df.melt<-melt(df, id.vars=c("SITE_NAME" ,"YEAR" ), 
              measure.vars=c("T_WEED","O_WEED","FORB","GRASS","B_GROUND", 
                             "LITTER","MOSS","NU_STEM","INSECTS","pr_May",
                             "absTmin"))
head(df.melt)

##### Add in missing annual observations for final analysis file
## this just reshapes the dataframe, if there are more than two obs per site/month/Year combination than the 'fun' argument may have to change. 
library(reshape)
df.long<-cast(df.melt,SITE_NAME + YEAR  ~ variable, fun= sum , add.missing=T , fill=NA )

# sort
df.long<-df.long[order(df.long$SITE_NAME,df.long$YEAR),]
head(df.long)

# check to make sure all site by year combos are here
length(levels(df.long$SITE_NAME))*length(levels(df.long$YEAR)) ## number of rows (site X year combos)

#### Create lags and calculate interannual change in stem density (NU_STEM) and Mecinus----
dt<-tibble::as_tibble(df.long)
dt
glimpse(dt)
names(dt)

##stem density
library(dplyr)
dt <- dt %>%
  arrange(SITE_NAME, YEAR) %>%            # Ensure the data is sorted by SITE_NAME and YEAR
  group_by(SITE_NAME) %>%                 # Group by SITE_NAME
  mutate(
    lnStems = log(NU_STEM + 1),  # Log transform
    lnStemst_1 = lag(lnStems),           # Create lagged variable (lag 1)
    R_Stems = lnStems - lnStemst_1       # Calculate difference
  ) %>%
  ungroup()                              # Ungroup the data to avoid unintended consequences later


##mecinus density 
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(lnMecinus = log(INSECTS+ 1),
         lnMecinust_1 = lag(lnMecinus),
         R_Mecinus = lnMecinus-lnMecinust_1
  ) %>%
  ungroup()

##other weeds 
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(lnO_WEED = log(O_WEED+ 1), 
         lnO_WEEDt_1 = lag(lnO_WEED)
  )%>%
  ungroup()

##grass 
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(lnGRASS = log(GRASS+ 1), 
         lnGRASSt_1 = lag(lnGRASS)
  )%>%
  ungroup()

##bare ground 
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(lnBGROUND = log(B_GROUND+ 1), 
         lnBGROUNDt_1 = lag(lnBGROUND)
  )%>%
  ungroup()

##forbs 
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(lnFORB = log(FORB+ 1),
         lnFORBt_1 = lag(lnFORB)
  )%>%
  ungroup()

##precipitation 
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(ppt_Mayt_1 = lag(pr_May), 
         ppt_Mayt_2 = lag(ppt_Mayt_1)
  )%>%
  ungroup()

##temperature 
dt<-dt%>%
  arrange(SITE_NAME,YEAR)%>%
  group_by(SITE_NAME)%>%
  mutate(absTmint_1 = lag(absTmin))### Create lagged var  ( lag)

##Drop sites with less than 5 years of data----
sitesamp<-unique(data[,c("SITE_NAME","YEAR")])
siteyr<-aggregate.data.frame(sitesamp$YEAR, by=list(sitesamp$SITE_NAME), FUN= length)

keep<-siteyr[siteyr$x >4,]
keep<-droplevels(keep)
colnames(keep)<-c("SITE_NAME", "yr_mon")
site<-c(keep$SITE_NAME)
dt<-dt[dt$SITE_NAME %in% site,]

##
#### Append site metdata to dt
df<-join(dt, sites, by ="SITE_NAME")
head(df)


##DD plot for stems
library(nlme)
topmod<-lme(R_Stems ~ lnStemst_1+ lnMecinust_1+pr_May, method="REML", na.action=na.exclude,
            random = ~1 |SITE_NAME, data = df)

df$fitted<-fitted(topmod)

##DD plot
plot(df$lnStemst_1, fitted)

library(ggplot2)
ggplot(data = df, aes(x = lnStemst_1, y = fitted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col="red")+
  labs(
    x = "Stem density/site/year (log)",
    y = "Interannual change in stem density (log)") +
  theme_minimal()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15)) 
library(MuMIn)
rsq_stems<-lme(fitted ~ lnStemst_1, method="REML", na.action=na.exclude, random = ~1|SITE_NAME, data=df)
r.squaredGLMM(rsq_stems)

##
DDmod<-lme(fitted~lnStemst_1, method="REML", na.action=na.exclude, random = ~1 |SITE_NAME, data = df)

df$DDresids<-resid(DDmod)

##mecinus DD plot 
plot(df$lnMecinust_1, df$DDresids)

library(ggplot2)
ggplot(data = df, aes(x = lnMecinust_1, y = DDresids)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col="red")+
  labs(
    x = "Mecinus density/3 min count/site/year (log)",
    y = "Interannual change in stem density (log)") +
  theme_minimal()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15)) 
library(MuMIn)
rsq_mecinus<-lme(DDresids ~ lnMecinust_1, method="REML", na.action=na.exclude, random = ~1|SITE_NAME, data=df)
r.squaredGLMM(rsq_mecinus)

##precipitation DD plot 
plot(df$pr_May, df$DDresids)

library(ggplot2)
ggplot(data = df, aes(x = pr_May, y = DDresids)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col="red")+
  labs(
    x = "Annual precipitation/site/year",
    y = "Interannual change in stem density (log)") +
  theme_minimal()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15)) 

rsq_pr<-lme(DDresids ~ pr_May, method="REML", na.action=na.exclude, random = ~1|SITE_NAME, data=df)
r.squaredGLMM(rsq_pr)
