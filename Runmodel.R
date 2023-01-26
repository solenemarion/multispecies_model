# Library -----------------------------------------------------------------
library(Hmsc)
library(corrplot)
library(tidyr)
library(plotrix)
library(dplyr)
library(RColorBrewer)
library(MuMIn)
library(stringr)
library(HDInterval)
library(tidyverse)  # data wrangling
library(broom)  # tidy regression output
library(mosaic)  # standardizing variables
library(factoextra) #check collinearity
library(stringr)
library(PerformanceAnalytics)
library(FactoMineR)



# Get data ----------------------------------------------------------------
# Raw station data
BH_XData <- read.csv("./data/BH_covariate_month.csv", header=T) #covariates
CA_XData <- read.csv("./dataCA_covariate_month.csv", header=T) #covariates

# Detection

## Bighorn
BH_obs20 <- read.csv("./data/Bighorn20_30min_Independent_Monthly_observations.csv")
BH_obs21 <- read.csv("./data/Bighorn20_30min_Independent_Monthly_observations.csv")

## Castle  
CA_obs21 <- read.csv("./data/Castle21_30min_Independent_Monthly_observations.csv") 
CA_obs22 <- read.csv("./data/Castle22_30min_Independent_Monthly_observations.csv") 

BH_XData <- BH_XData %>% 
  dplyr::select(Site, Date, NDVI, HFIperc, TYPE, vegclass,usrt_strv_mean,Alltrail_nb_revw, Use_Type,
                season, Elevation, forest_perc, distance_water, 
                distance_trail, Density_trail)%>% 
  rename(Deployment.Location.ID=Site) %>% 
  mutate(Deployment.Location.ID=paste("BH",Deployment.Location.ID,sep="-")) 

BH_XData$area <- "Bighorn" # add area column

CA_XData <- CA_XData %>% 
  rename(Site=Site_ID,
         forest_perc=forest_prec) %>% 
  dplyr::select(Site, Date, NDVI, HFIperc, TYPE, vegclass,usrt_strv_mean,Alltrail_nb_revw, Use_Type,
                season, Elevation, forest_perc, distance_water, 
                distance_trail,Density_trail)%>% 
  rename(Deployment.Location.ID=Site) %>% 
  mutate(Deployment.Location.ID=paste("CA",Deployment.Location.ID,sep="-")) 

CA_XData$area <- "Castle" # add area column

#combine together
XData <- rbind(BH_XData, CA_XData)


# for the season
XData_season <- XData %>% 
  select(-Date) %>% 
  group_by(Deployment.Location.ID, HFIperc, TYPE, vegclass,usrt_strv_mean,Alltrail_nb_revw, Use_Type, 
           season, Elevation, forest_perc, distance_water, 
           distance_trail, Density_trail,area) %>%
  dplyr::summarise(NDVI=mean(NDVI))

## Management 
## We want a continuous variable instead of a categorical variable

XData_season$TYPE_n <-NA
XData_season$TYPE_n <- as.numeric(XData_season$TYPE_n)

XData_season$TYPE_n[XData_season$TYPE=="No type"] <- 1
XData_season$TYPE_n[XData_season$TYPE=="PLREC"| XData_season$TYPE=="PLUZ"|XData_season$TYPE=="PRA"] <- 2
XData_season$TYPE_n[XData_season$TYPE=="PP"] <- 3
XData_season$TYPE_n[XData_season$TYPE=="WPP"] <- 4

#Need to edit the use type 
XData_season$Use_Type <- case_when(duplicated(XData_season[c("Deployment.Location.ID","season")])~"Mixed-Use", 
                                   TRUE ~ XData_season$Use_Type
)

XData_season <- XData_season %>% 
  group_by(Deployment.Location.ID, HFIperc, TYPE, vegclass,usrt_strv_mean,Alltrail_nb_revw, Use_Type, 
           season, Elevation, forest_perc, distance_water, 
           distance_trail, Density_trail,area,TYPE_n) %>%
  dplyr::summarise(NDVI=mean(NDVI))


# Recoding some variables -------------------------------------------------


## vegetation
table(XData_season$vegclass)

XData_season$vegclass[XData_season$vegclass== "Broadleaf Forest" | XData_season$vegclass == "Coniferous Forest" |XData_season$vegclass== "Mixed Forest" ] <- "Forest"
XData_season$vegclass[XData_season$vegclass=="Grassland"| XData_season$vegclass== "Shrubland"] <- "Grassland-Shrubland" # not a lot of "grassland" so group with shrubland
XData_season$vegclass[XData_season$vegclass=="Rock/Rubble"| XData_season$vegclass=="Water"] <- "Other" # not a lot of this so group together

XData_season$vegclass <- factor(XData_season$vegclass, levels = c("Developed", "Forest", "Grassland-Shrubland", "Other"))


## trail use mixed as motorized
XData_season$Use_Type[XData_season$Use_Type=="Mixed-Use"] <- "Motorized"
XData_season$Use_Type[XData_season$Use_Type=="Trail_no_info"] <- "Motorized"

## change level categorical variable use type
XData_season$Use_Type <- factor(XData_season$Use_Type, levels = c("No_trail_inbuf", "Non-Motorized", "Motorized"))

## trail
XData_season$Use_Type[is.na(XData_season$Use_Type)] <- "No_trail_inbuf"

## strv
XData_season$usrt_strv_mean[is.na(XData_season$usrt_strv_mean)] <- 0 


# Collinearity ------------------------------------------------------------
XData_season_coli <- XData_season %>% 
  group_by(Deployment.Location.ID, HFIperc, TYPE, vegclass,usrt_strv_mean,Alltrail_nb_revw, Use_Type, 
           season, Elevation, forest_perc, distance_water, 
           distance_trail, Density_trail,area,TYPE_n) %>%
  summarise(NDVI=mean(NDVI),
  )

#check collinearity 
XData_season_numerical <-  XData_season_coli %>% 
  ungroup() %>% 
  select(HFIperc,usrt_strv_mean,Alltrail_nb_revw, Elevation, forest_perc, distance_water, 
         distance_trail, Density_trail,TYPE_n)


chart.Correlation(XData_season_numerical, histogram=TRUE, pch=3, cex.labels = 10)
cor(XData_season$distance_water, XData_season$HFIperc,  method = "pearson", use = "complete.obs")

#### Collinearity #####
XData_FAMD <-  XData %>% 
  mutate(Distance_water = distance_water,
         Forest_perc=forest_perc,
         Season = season,
         Area=area,
         Vegetation=vegclass,
         Strava= usrt_strv_mean,
         AllTrails=Alltrail_nb_revw,
         Type_Recreation=Use_Type,
         Management=TYPE_n,
         Distance_to_trail=(distance_trail)
  ) %>% 
  select(-TYPE,-distance_water, -season, -area, -vegclass,-usrt_strv_mean,-Alltrail_nb_revw,-Use_Type,-TYPE_n,-distance_trail,-forest_perc, -x, -Deployment.Location.ID) 


FAMD(XData_FAMD, ncp = 5, sup.var = NULL, ind.sup = NULL, graph = TRUE)



#### Y data (obs) ####
#issue with BH-8 #need to properly fix that at some point 
BH_obs20 <- subset(BH_obs20, BH_obs20$Deployment.Location.ID!="BH-8")

## add area column
BH_obs20$area <- "Bighorn"
BH_obs21$area <- "Bighorn"

CA_obs21$area <- "Castle"
CA_obs22$area <- "Castle"

df_tot_s <- dplyr::bind_rows(BH_obs20, BH_obs21, CA_obs21, CA_obs22) #using bind_rows as different number of columns
df_tot_s[is.na(df_tot_s)] <- 0

df_tot_summary <- df_tot_s %>% 
  select(-Deployment.Location.ID, -Effort,-Date, -area) %>% 
  summarise_all(sum) 

df_tot_summary <- t(df_tot_summary)
df_tot_summary <- as.data.frame(df_tot_summary)
df_tot_summary <- rename(df_tot_summary, detection=V1)

df_tot_summary <- order(df_tot_summary$detection)

df_tot_summary
df_tot_s <- subset(df_tot_s, select=c("Deployment.Location.ID", "Effort","Date", "area","Odocoileus.virginianus",
                                      "Lepus.americanus", "Odocoileus.hemionus","Vulpes.vulpes" ,"Ursus.americanus",
                                      "Tamiasciurus.hudsonicus","Alces.alces","Canis.latrans","Ursus.arctos","Martes.americana",
                                      "Lynx.canadensis","Canis.lupus","Cervus.canadensis","Puma.concolor","Spermophilus.columbianus"))


df_tot_s <- df_tot_s %>% 
  rename("White-tailed deer"="Odocoileus.virginianus",
         "Snowshoe Hare"="Lepus.americanus", 
         "Mule deer"="Odocoileus.hemionus",
         "Red fox"="Vulpes.vulpes" ,
         "Black Bear"="Ursus.americanus",
         "Red Squirrel"="Tamiasciurus.hudsonicus",
         "Moose"="Alces.alces",
         "Coyote"="Canis.latrans",
         "Grizzly bear"="Ursus.arctos",
         "Marten"="Martes.americana",
         "Canada Lynx"="Lynx.canadensis",
         "Gray Wolf"="Canis.lupus",
         "Elk wapiti"="Cervus.canadensis",
         "Cougar"="Puma.concolor",
         "Ground squirrel"="Spermophilus.columbianus")


# group month into season 
unique(df_tot_s$Date)
df_tot_s$season <- NA

#from 30 November to April  = winter 
df_tot_s$season[ df_tot_s$Date=="2019-12"| df_tot_s$Date=="2020-01" | df_tot_s$Date=="2020-02" | df_tot_s$Date=="2020-03"|
                   df_tot_s$Date== "2020-04" | df_tot_s$Date=="2020-12"| df_tot_s$Date== "2021-01" | df_tot_s$Date=="2021-02" |
                   df_tot_s$Date=="2021-03" | df_tot_s$Date=="2021-04"| df_tot_s$Date=="2021-12"|  df_tot_s$Date== "2022-01" | df_tot_s$Date=="2022-02" |
                   df_tot_s$Date=="2022-03" | df_tot_s$Date=="2022-04"]<-"winter"

#from May to November = summer
df_tot_s$season[df_tot_s$Date=="2019-08" |df_tot_s$Date=="2019-09" |df_tot_s$Date=="2019-10" | df_tot_s$Date=="2019-11" |   
                  df_tot_s$Date=="2020-05" | df_tot_s$Date=="2020-06" | df_tot_s$Date== "2020-07" |
                  df_tot_s$Date=="2020-08" | df_tot_s$Date=="2020-09" | df_tot_s$Date=="2020-10" | df_tot_s$Date=="2020-11" |
                  df_tot_s$Date=="2021-05" | df_tot_s$Date=="2021-06" | df_tot_s$Date=="2021-07" | df_tot_s$Date=="2021-08"|
                  df_tot_s$Date=="2021-09" | df_tot_s$Date=="2021-10" | df_tot_s$Date=="2021-11" |
                  df_tot_s$Date=="2022-05" | df_tot_s$Date=="2022-06" | df_tot_s$Date=="2022-07" | df_tot_s$Date=="2022-08"]<-"summer"

df_tot_s2 <- df_tot_s %>%
  select(-Date) %>% 
  group_by(Deployment.Location.ID,  season,area) %>%
  summarise(across(everything(), ~ sum(., is.na(.), 0))) %>%
  mutate(x=paste(Deployment.Location.ID, season, sep="_"))

# offset
offset <- select(df_tot_s2, Effort, Deployment.Location.ID, season)
sum(offset$Effort)

# save for later
Y <- df_tot_s2

#### Name column ####

# edit name column and add offset
XData <- XData_season %>% 
  mutate(x=paste(Deployment.Location.ID, season,sep="_"))%>% 
  left_join(offset, by=c("Deployment.Location.ID","season"))

# filter camera trap which are present in both data frame #### need to check that is fine
Y <- Y %>% 
  filter(x %in% XData$x) %>%
  group_by(Deployment.Location.ID, season,x, area) %>% #some camera trap have 2 records per month (SD change)
  summarise(across(everything(), sum)) %>% 
  ungroup()%>%
  as.data.frame()

XData <- XData %>%
  filter(x %in% Y$x) %>% 
  distinct() %>% # remove duplicate of when we join two months together
  as.data.frame()

#change row name, need to match in Y
row.names(Y) <- Y$x
row.names(XData) <- XData$x

namerow <- XData$x



#### Random effects ####
xy <- read.csv("./data/xy.csv", header=T,fileEncoding="UTF-8-BOM")

row.names(xy) <- paste(xy$Deployment.Location.ID)

xy[,c("Deployment.Location.ID")] <- NULL

##study design
Deployment.Location.ID <- XData$Deployment.Location.ID
season <- XData$season
studyDesign = data.frame(XData$season)

studyDesign = data.frame(site = as.factor(XData$Deployment.Location.ID), season = as.factor(XData$season))

rL1 = HmscRandomLevel(units = studyDesign$site)
rL2 = HmscRandomLevel(units = levels(XData$season))
rL = HmscRandomLevel(sData = as.matrix(xy))


Y <- select(Y, -Deployment.Location.ID, -season, -x, -area, -Effort)


XData$NDVI[is.na(XData$NDVI)] <- 0


#### standarised predictors ####
XData <- XData %>% 
  mutate(NDVI = as.numeric(scale(NDVI)),
         HFIperc = as.numeric(scale(HFIperc)),
         forest_perc = as.numeric(scale(forest_perc)),
         distance_water =as.numeric(scale(distance_water)),
         Elevation = as.numeric(scale(Elevation)),
         #Date = as.factor(Date),
         season = as.factor(season),
         Effort=as.numeric(Effort),
         area=as.factor(area),
         TYPE=as.factor(TYPE),
         vegclass=as.factor(vegclass),
         usrt_strv_mean= as.numeric(usrt_strv_mean),
         Alltrail_nb_revw = as.numeric(Alltrail_nb_revw),
         Use_Type=as.factor(Use_Type),
         distance_trail =as.numeric(scale(distance_trail)), # reverse distance
         Density_trail =as.numeric(scale(Density_trail))
  )


XData[,c("Deployment.Location.ID","x")] <- NULL
Y[, c("Effort","x", "Deployment.Location.ID","Date","year","area","season")] <- NULL
Y <- Y[,order(colnames(Y))]



# Running model -----------------------------------------------------------

# Test settings
nChains   = 4 # Ultimately use 4
thin      = 100 # build up from 100
samples   = 1000 # #1000
transient = 10*thin
verbose   = T

# Model specification
XFormula = ~ (usrt_strv_mean+ distance_trail + Density_trail) * (Use_Type + forest_perc + TYPE_n + HFIperc  + season) +
  Elevation   + distance_water + vegclass + NDVI  + Effort 


mod = Hmsc(Y = Y, XData = XData, XFormula = XFormula, 
           studyDesign = studyDesign, 
           ranLevels = list("site" = rL),
           distr="lognormal poisson")


mod= sampleMcmc(mod, thin = thin, samples = samples, transient = transient,nChains = nChains, verbose = verbose, nParallel = 4)

saveRDS(mod,file="./Model Objects/mod_int_tot_no_trait_thin100_sample1000_151222.RData")
