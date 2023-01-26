# Setup -------------------------------------------------------------------

# HMSc analysis
library(Hmsc)
library(corrplot)
library(tidyr)
library(plotrix)
library(dplyr)
library(RColorBrewer)
library(MuMIn)
library(stringr)
library(HDInterval)
library(ggplot2)

# Colour gradient setup
cols <- c("#4168DB", "#FE0000")


# load model --------------------------------------------------------------
m <- readRDS("./Model Objects/mod_int_tot_no_trait_thin100_sample1000_151222.RData")

# check model -------------------------------------------------------------
mpost = convertToCodaObject(m)
postBeta = getPostEstimate(m, parName = "Beta")
summary(mpost$Beta)

plot(mpost$Beta)


# table 1 -----------------------------------------------------------------
# Detection summary table
tmpBH21 <- read.csv("./data//Bighorn21_30min_Independent_Monthly_observations.csv", header=T)
## Bighorn
tmpBH20 <- read.csv("./data/Bighorn20_30min_Independent_total_observations.csv")

## Castle  
tmpCA20 <- read.csv("./data/Castle21_30min_Independent_Monthly_observations.csv") 
tmpCA22 <- read.csv("./data/Castle22_30min_Independent_Monthly_observations.csv") 


tmpBH20 <- subset(tmpBH20, tmpBH20$Deployment.Location.ID!="BH-8")

## add area colum
df_tot_s <- dplyr::bind_rows(tmpBH21, tmpBH20, tmpCA20, tmpCA22) #using bind_rows as different number of columns

df_tot_s[is.na(df_tot_s)] <- 0

df_tot_summary <- df_tot_s %>% 
  select(-Deployment.Location.ID, -Effort,-Date) %>% 
  summarise_all(sum) 

df_tot_summary <- t(df_tot_summary)
df_tot_summary <- as.data.frame(df_tot_summary)

df_tot_summary <- rename(df_tot_summary, Count=V1)
df_tot_summary$Species <- rownames(df_tot_summary) 

df_tot_summary <- df_tot_summary[order(-df_tot_summary$Count),] 


df_tot_summary <- subset(df_tot_summary, df_tot_summary$Species=="Odocoileus.virginianus" | df_tot_summary$Species=="Lepus.americanus"|
                           df_tot_summary$Species== "Odocoileus.hemionus"| df_tot_summary$Species=="Vulpes.vulpes" | 
                           df_tot_summary$Species=="Ursus.americanus"| df_tot_summary$Species=="Tamiasciurus.hudsonicus"| 
                           df_tot_summary$Species=="Alces.alces"| df_tot_summary$Species=="Canis.latrans"| 
                           df_tot_summary$Species=="Ursus.arctos"| df_tot_summary$Species=="Martes.americana"| 
                           df_tot_summary$Species=="Lynx.canadensis"| df_tot_summary$Species=="Canis.lupus"| 
                           df_tot_summary$Species=="Cervus.canadensis"| df_tot_summary$Species=="Puma.concolor"| 
                           df_tot_summary$Species=="Spermophilus.columbianus")
table1 <- df_tot_summary

# Number of observations
total.obs <- read.csv("./data/Bighorn20_30min_Independent_total_observations.csv", header = T )
total.obs2 <- read.csv("./data/Bighorn21_30min_Independent_total_observations.csv", header = T )
total.obs3 <- read.csv("./data/Castle21_30min_Independent_total_observations.csv", header = T )
total.obs4 <- read.csv("./data/Castle22_30min_Independent_total_observations.csv", header = T )

total.obs <- dplyr::bind_rows(total.obs, total.obs2, total.obs3,total.obs4)

#table1$Events <- as.data.frame(colSums(total.obs[, 3:ncol(total.obs)]))[,1]
tmp <- as.matrix(total.obs[,3:ncol(total.obs)])
tmp[tmp>0] <- 1

#table1$Occupancy <- as.data.frame(round(colSums(tmp)/73, 2))[,1]

# Extract the effective sample sizes
library(stringr)
ESS <- as.data.frame(effectiveSize(mpost$Beta))

# Clean up Table one data
table1$Species <-  str_replace_all(table1$Species , c("Odocoileus.virginianus"    ="White-tailed Deer"        ,
                                                      "Lepus.americanus"        ="Snowshoe Hare"            ,
                                                      "Odocoileus.hemionus"     ="Mule Deer"                ,
                                                      "Vulpes.vulpes"           ="Red Fox"                  ,
                                                      "Ursus.americanus"        ="Black Bear"               ,
                                                      "Tamiasciurus.hudsonicus" ="Red Squirrel"             ,
                                                      "Alces.alces"             ="Moose"                    ,
                                                      "Canis.latrans"           ="Coyote"                   ,
                                                      "Ursus.arctos"            ="Grizzly Bear"             ,
                                                      "Martes.americana"        ="Marten"                   ,
                                                      "Lynx.canadensis"         ="Canada Lynx"              ,
                                                      "Canis.lupus"             ="Gray Wolf"                ,
                                                      "Cervus.canadensis"       ="Elk"               ,
                                                      "Puma.concolor"           ="Cougar"                   ,
                                                      "Spermophilus.columbianus"="Ground Squirrel"))

# Extract the data from the ESS labels
ESS$Species[str_detect(row.names(ESS), "White-tailed deer"        )] <- "White-tailed Deer" 
ESS$Species[str_detect(row.names(ESS), "Snowshoe Hare"            )] <- "Snowshoe Hare"             
ESS$Species[str_detect(row.names(ESS), "Mule deer"                )] <- "Mule Deer"                 
ESS$Species[str_detect(row.names(ESS), "Red fox"                  )] <- "Red Fox"                   
ESS$Species[str_detect(row.names(ESS), "Black Bear"               )] <- "Black Bear"                
ESS$Species[str_detect(row.names(ESS), "Red Squirrel"             )] <- "Red Squirrel"              
ESS$Species[str_detect(row.names(ESS), "Moose"                    )] <- "Moose"                     
ESS$Species[str_detect(row.names(ESS), "Coyote"                   )] <- "Coyote"                    
ESS$Species[str_detect(row.names(ESS), "Grizzly bear"             )] <- "Grizzly Bear"              
ESS$Species[str_detect(row.names(ESS), "Marten"                   )] <- "Marten"                    
ESS$Species[str_detect(row.names(ESS), "Canada Lynx"              )] <- "Canada Lynx"               
ESS$Species[str_detect(row.names(ESS), "Gray Wolf"                )] <- "Gray Wolf"                 
ESS$Species[str_detect(row.names(ESS), "Elk wapiti"               )] <- "Elk"                
ESS$Species[str_detect(row.names(ESS), "Cougar"                   )] <- "Cougar"                    
ESS$Species[str_detect(row.names(ESS), "Ground squirrel")] <- "Ground Squirrel" 


lst <- strsplit(row.names(ESS)," ")
ESS$Covariate <- sapply(lst, '[[', 1) 


tmp <- aggregate(ESS$`effectiveSize(mpost$Beta)`, by=c(list(ESS$Species)), FUN=mean)
table1 <- left_join(table1,tmp, by=c("Species"="Group.1"))
table1$Eff <- round(round(table1$x,0)/4,1)
table1$x <- NULL
table1 <- subset(table1, !is.na(Eff))


# Second step, check Potential Scale Reduction Factors - They should all be close to one (less than 1.05)
GSRF <- gelman.diag(mpost$Beta,multivariate=FALSE)$psrf

# Take the mean and the maximum
lst <- strsplit(row.names(GSRF)," ")
GSRF <- as.data.frame(GSRF)

GSRF$Species[str_detect(row.names(GSRF), "White-tailed deer"        )] <- "White-tailed Deer" 
GSRF$Species[str_detect(row.names(GSRF), "Snowshoe Hare"            )] <- "Snowshoe Hare"             
GSRF$Species[str_detect(row.names(GSRF), "Mule deer"                )] <- "Mule Deer"                 
GSRF$Species[str_detect(row.names(GSRF), "Red fox"                  )] <- "Red Fox"                   
GSRF$Species[str_detect(row.names(GSRF), "Black Bear"               )] <- "Black Bear"                
GSRF$Species[str_detect(row.names(GSRF), "Red Squirrel"             )] <- "Red Squirrel"              
GSRF$Species[str_detect(row.names(GSRF), "Moose"                    )] <- "Moose"                     
GSRF$Species[str_detect(row.names(GSRF), "Coyote"                   )] <- "Coyote"                    
GSRF$Species[str_detect(row.names(GSRF), "Grizzly bear"             )] <- "Grizzly Bear"              
GSRF$Species[str_detect(row.names(GSRF), "Marten"                   )] <- "Marten"                    
GSRF$Species[str_detect(row.names(GSRF), "Canada Lynx"              )] <- "Canada Lynx"               
GSRF$Species[str_detect(row.names(GSRF), "Gray Wolf"                )] <- "Gray Wolf"                 
GSRF$Species[str_detect(row.names(GSRF), "Elk wapiti"               )] <- "Elk"                
GSRF$Species[str_detect(row.names(GSRF), "Cougar"                   )] <- "Cougar"                    
GSRF$Species[str_detect(row.names(GSRF), "Ground squirrel")] <- "Ground Squirrel" 


GSRF$Covariate <- sapply(lst, '[[', 1) 
tmp <- aggregate(GSRF$`Point est.`, by=c(list(GSRF$Species)), FUN=mean)
colnames(tmp)[2] <- "GSRF.Mean" 

tmp$GSRF.Max <- aggregate(GSRF$`Point est.`, by=c(list(GSRF$Species)), FUN=max)[,2]
table1 <- left_join(table1,tmp, by=c("Species"="Group.1"))
table1 <- table1[is.na(table1$Eff)==F,]

table1$GSRF.Mean <- round(table1$GSRF.Mean,2)
table1$GSRF.Max <- round(table1$GSRF.Max,2)

table1 <- table1[, c("Species", "Count", "Eff", "GSRF.Mean", "GSRF.Max")] # reorder
table1

write.table(table1, file="TableCount.csv", sep = "\t", row.names=FALSE)            


# Assess model explantory power -------------------------------------------
# Assess the models explantory power
preds = computePredictedValues(m)

mod.eval <- evaluateModelFit(hM = m, predY = preds)

# Estimated species-species associations
OmegaCor = computeAssociations(m)

# Determine the plot order and groupings using heirarchical clustering (Wards)
plotOrder = corrMatOrder(OmegaCor[[1]]$mean,order="hclust", hclust.method="ward.D2")

# Extract CI's
ci95 <- getPostEstimate(m, parName = "Beta", q=c(0.05, 0.95))

# Organising the data
rownames(ci95$mean) <- m$covNames
tmp1 <- as.data.frame.table(ci95$mean)
colnames(tmp1) <- c("Var2", "Var3", "Freq")
tmp1$Var1 <- "Mean"
tmp1 <- tmp1[,c(4,1,2,3)]


# Credible intervals
# Specify the names
column.names <- m$covNames
row.names <- c("5%",  "95%")
matrix.names <- m$spNames

dimnames(ci95$q) <- list(row.names, column.names, matrix.names)
tmp2 <- as.data.frame.table(ci95$q)

tmp <- rbind(tmp1,tmp2)

df1 <- spread(data = tmp, key = Var1, value = Freq)

df1 <- df1 %>% 
  rename(Variable=Var2,
         Species=Var3)

unique(df1$Variable)

{
  df1$Variable <- as.character(df1$Variable)
  df1$Variable[df1$Variable=="usrt_strv_mean"] <- "Strava"                         
  df1$Variable[df1$Variable=="distance_trail"] <- "Distance to trail"
  df1$Variable[df1$Variable=="Density_trail"] <- "Density trail"
  df1$Variable[df1$Variable=="Use_TypeNon-Motorized"] <- "Non-motorized trail"
  df1$Variable[df1$Variable=="Use_TypeMixed-Use"] <- "Mixed-use trail"
  df1$Variable[df1$Variable== "Use_TypeMotorized" ] <- "Motorized trail"
  df1$Variable[df1$Variable=="TYPE_n" ] <- "Type of management"
  df1$Variable[df1$Variable=="HFIperc"  ] <- "HFI"
  df1$Variable[df1$Variable=="areaCastle" ] <- "Area Castle"
  df1$Variable[df1$Variable=="forest_perc"] <- "Forest %"
  df1$Variable[df1$Variable=="distance_water"                     ] <- "Distance to water"
  df1$Variable[df1$Variable=="NDVI"                               ] <- "NDVI"
  df1$Variable[df1$Variable=="usrt_strv_mean:Use_TypeMotorized"   ] <- "Strava: Motorized"
  df1$Variable[df1$Variable=="usrt_strv_mean:areaCastle"          ] <- "Strava: Area Castle"
  df1$Variable[df1$Variable=="distance_trail:forest_perc"         ] <- "Distance: Forest %"
  df1$Variable[df1$Variable=="Density_trail:Use_TypeNon-Motorized"] <- "Density trail: Non-motorized trail"
  df1$Variable[df1$Variable=="Density_trail:TYPE_n"               ] <- "Density trail: Management"
  df1$Variable[df1$Variable=="vegclassForest"                       ] <- "Forest vegetation class"
  df1$Variable[df1$Variable=="seasonwinter"                         ] <- "Season winter"
  df1$Variable[df1$Variable=="usrt_strv_mean:forest_perc"           ] <- "Strava: Forest %"
  df1$Variable[df1$Variable=="distance_trail:Use_TypeNon-Motorized" ] <- "Distance to trail: Motorized trails"
  df1$Variable[df1$Variable=="distance_trail:TYPE_n"                ] <- "Distance to trail: Management"
  df1$Variable[df1$Variable=="Density_trail:Use_TypeMixed-Use"      ] <- "Density trail: Mixed-use trail"
  df1$Variable[df1$Variable=="Density_trail:HFIperc"                ] <- "Density trail: HFI"
  df1$Variable[df1$Variable=="vegclassGrassland-Shrubland"          ] <- "Grassland-Shrubland vegetation class"
  df1$Variable[df1$Variable=="Effort"                               ] <- "Sampling Effort"
  df1$Variable[df1$Variable=="usrt_strv_mean:Use_TypeNon-Motorized" ] <- "Strava: Non-motorized trail"
  df1$Variable[df1$Variable=="usrt_strv_mean:TYPE_n"                ] <- "Strava:Mixed-use trail"
  df1$Variable[df1$Variable=="distance_trail:Use_TypeMixed-Use"     ] <- "Distance to trail: Mixed-use trail"
  df1$Variable[df1$Variable=="distance_trail:HFIperc"               ] <- "Distance to trail: HFI"
  df1$Variable[df1$Variable=="Density_trail:Use_TypeMotorized"      ] <- "Density: Motorized trails"
  df1$Variable[df1$Variable=="Density_trail:areaCastle" ] <- "Density:Area Castle"
  df1$Variable[df1$Variable=="Elevation"  ] <- "Elevation"
  df1$Variable[df1$Variable=="vegclassOther" ] <- "Other vegetation class"
  df1$Variable[df1$Variable== "usrt_strv_mean:Use_TypeMixed-Use" ] <- "Strava: Mixed-use trail"
  df1$Variable[df1$Variable== "usrt_strv_mean:HFIperc"           ] <- "Strava: HFI"
  df1$Variable[df1$Variable== "distance_trail:Use_TypeMotorized" ] <- "Distance to trail: Motorized trails"
  df1$Variable[df1$Variable== "distance_trail:areaCastle"        ] <- "Distance to trail: Area Castle"
  df1$Variable[df1$Variable== "Density_trail:forest_perc"        ] <- "Distance to trail: Forest %"
  df1$Variable[df1$Variable== "usrt_strv_mean:seasonwinter" ] <- "Strava: Season winter"
  df1$Variable[df1$Variable== "distance_trail:seasonwinter"] <- "Distance: Season winter"
  df1$Variable[df1$Variable==  "Density_trail:seasonwinter"] <- "Density trail: Season winter"
}

write.table(df1, file="Fullcoeff.csv", sep = "\t", row.names=FALSE)   

# Get variance partitioning
VP = computeVariancePartitioning(m)


# overall results ---------------------------------------------------------
column.names <- m$spNames

row.names <- c("Recreation intensity","Recreation type", "Recreation Interaction","Landscape","Season","Effort","Random site")
VP$groupnames



VP.custom <- array(c(
  apply(VP$vals[c(1:3),],2,sum), # Recreation alone
  VP$vals[4,], # Recreation type
  apply(VP$vals[c(15:32),],2,sum), # Recreation interaction
  
  apply(VP$vals[c(5:8,10:13),],2,sum), # Landscape 
  VP$vals[9,], # Season
  VP$vals[14,], # effort
  
  VP$vals[33,]), # Random site
  dim = c(length(column.names),length(row.names)),
  dimnames = list(column.names,row.names)
)
VP.custom <- t(VP.custom)

colnames(VP.custom) <- gsub('\\.', ' ', colnames(VP.custom))
m$spNames[order(mod.eval$SR2, decreasing=T)]
mod.eval$SR2[order(mod.eval$SR2, decreasing=T)]

cex = 2
par(mfrow=c(1,1))

layout(matrix(c(1,2,2), 3, 1, byrow = TRUE))
labels <- data.frame("Species"=colnames(VP.custom[,order(mod.eval$SR2, decreasing=T)]))

# Read in the key
common <- read.csv("./data/common_species.csv", header=T)
common <- rename(common, Species=ï..Species)

labels <- left_join(labels,common)
labels$Species[labels$Species=="Red fox"] <- "Red Fox"
labels$Species[labels$Species=="White-tailed deer"] <- "White-tailed Deer"
labels$Species[labels$Species=="Mule deer"] <- "Mule Deer"
labels$Species[labels$Species=="Grizzly bear"] <- "Grizzly Bear"
labels$Species[labels$Species=="Elk wapiti"] <- "Elk"
labels$Species[labels$Species=="Columbian ground squirrel"] <- "Ground Squirrel"

barplot(mod.eval$SR2[order(mod.eval$SR2, decreasing=T)], las=2,
        cex.lab = cex,cex.axis = cex,cex.names= cex,
        names.arg=labels$Species,
        xlab = labels$Species,
        ylim=c(0,max(mod.eval$SR2)+0.1), xlim=c(0,length(column.names)+6.5), ylab="Pseudo R2")

par(mfrow=c(1,1))

#summary
as.data.frame(mod.eval$SR2, m$spNames)


## overall species variance
color <- c("#99CCFF","#6699CC","#336699","#66CC99","#CC9900","#663366","#CCCCCC") 


barplot(VP.custom[,order(mod.eval$SR2, decreasing=T)], col=color, las=2,
        cex.lab = cex,cex.axis = cex,cex.names= 2,
        names.arg=labels$Species,
        xlim=c(0,length(column.names)+6.5), main="",
        ylab="Variation partitioning")

legend(18, 2, legend=rev(row.names), fill= rev(color), cex = cex)

#summary
VP.custom


# Strava ---------------------------------------------------------------

# Subest to just the envi
strava.VP <- VP$vals[VP$groupnames %in% c("usrt_strv_mean","usrt_strv_mean:Use_Type","usrt_strv_mean:forest_perc","usrt_strv_mean:TYPE_n","usrt_strv_mean:HFIperc","usrt_strv_mean:area","usrt_strv_mean:season"),]

strava.VP <- strava.VP[-8,] # to remove the random effect and only have the recreation 

for(i in 1:ncol(strava.VP))
{
  strava.VP[,i] <- strava.VP[,i]*mod.eval$SR2[i] 
}



par(mfrow=c(1,1))
colnames(strava.VP) <- gsub('\\.', ' ', colnames(strava.VP))

#pdf(height=7, width=7, "figures//Figure3.pdf")
par(mar=c(10,4,1,2))

labels <- data.frame("Species"=colnames(strava.VP[,order(colSums(strava.VP), decreasing=T)]))
labels <- rename(labels, Common=Species)

labels$Common[labels$Common=="Red fox"] <- "Red Fox"
labels$Common[labels$Common=="White-tailed deer"] <- "White-tailed Deer"
labels$Common[labels$Common=="Mule deer"] <- "Mule Deer"
labels$Common[labels$Common=="Grizzly bear"] <- "Grizzly Bear"
labels$Common[labels$Common=="Elk wapiti"] <- "Elk"
labels$Common[labels$Common=="Columbian ground squirrel"] <- "Ground Squirrel"

rownames <- c("Strava","Strava: Type recreation","Strava: % Forest","Strava: Management","Strava: HFI","Strava: Area","Strava: Season")


color2 <- c("#6699CC","#99CCFF","#336600","#666600","#99CC66","#39452e","#CC9900") 


barplot(strava.VP[,order(colSums(strava.VP), decreasing =T)]
        , las=2, col=color2, ylab="Prop. of tot. variation explained", ylim=c(0,max(colSums(strava.VP))+0.02 ),
        names.arg=labels$Common,
        main="Strava")


legend("topright", legend=rev(rownames), fill= rev(color2))

#summary only of strava
sumstrava <- as.data.frame(strava.VP)
rowMeans(sumstrava)

# Distance  ---------------------------------------------------------------

# Subest to just the envi
distance.VP <- VP$vals[VP$groupnames %in% c("distance_trail","distance_trail:Use_Type","distance_trail:forest_perc","distance_trail:TYPE_n","distance_trail:HFIperc","distance_trail:area","distance_trail:season"),]

distance.VP <- distance.VP[-8,] # to remove the random effect and only have the recreation 

for(i in 1:ncol(distance.VP))
{
  distance.VP[,i] <- distance.VP[,i]*mod.eval$SR2[i] 
}



par(mfrow=c(1,1))
colnames(distance.VP) <- gsub('\\.', ' ', colnames(distance.VP))

#pdf(height=7, width=7, "figures//Figure3.pdf")
par(mar=c(10,4,1,2))

labels <- data.frame("Species"=colnames(distance.VP[,order(colSums(distance.VP), decreasing=T)]))
labels <- rename(labels, Common=Species)

labels$Common[labels$Common=="Red fox"] <- "Red Fox"
labels$Common[labels$Common=="White-tailed deer"] <- "White-tailed Deer"
labels$Common[labels$Common=="Mule deer"] <- "Mule Deer"
labels$Common[labels$Common=="Grizzly bear"] <- "Grizzly Bear"
labels$Common[labels$Common=="Elk wapiti"] <- "Elk"
labels$Common[labels$Common=="Columbian ground squirrel"] <- "Ground Squirrel"

rownames <- c("Distance to trail","Distance to trail: Type recreation","Distance to trail: % Forest","Distance to trail: Management","Distance to trail: HFI","Distance to trail: area","Distance to trail: Season")


color2 <- c("#6699CC","#99CCFF","#336600","#666600","#99CC66","#39452e","#CC9900") 


barplot(distance.VP[,order(colSums(distance.VP), decreasing =T)]
        , las=2, col=color2, ylab="Prop. of tot. variation explained", ylim=c(0,max(colSums(distance.VP))+0.01 ),
        names.arg=labels$Common,
        main="Distance")


legend("topright", legend=rev(rownames), fill= rev(color2))

#summary only of distance
sumdist <- as.data.frame(distance.VP)
rowMeans(sumdist)

# Density  ---------------------------------------------------------------

# Subest to just the envi
density_VP <- VP$vals[VP$groupnames %in% c("Density_trail","Density_trail:Use_Type","Density_trail:forest_perc","Density_trail:TYPE_n","Density_trail:HFIperc","Density_trail:area","Density_trail:season"),]

density_VP <- density_VP[-8,] # to remove the random effect and only have the recreation 

for(i in 1:ncol(density_VP))
{
  density_VP[,i] <- density_VP[,i]*mod.eval$SR2[i] 
}



par(mfrow=c(1,1))
colnames(density_VP) <- gsub('\\.', ' ', colnames(density_VP))

#pdf(height=7, width=7, "figures//Figure3.pdf")
par(mar=c(10,4,1,2))

labels <- data.frame("Species"=colnames(density_VP[,order(colSums(density_VP), decreasing=T)]))
labels <- rename(labels, Common=Species)

labels$Common[labels$Common=="Red fox"] <- "Red Fox"
labels$Common[labels$Common=="White-tailed deer"] <- "White-tailed Deer"
labels$Common[labels$Common=="Mule deer"] <- "Mule Deer"
labels$Common[labels$Common=="Grizzly bear"] <- "Grizzly Bear"
labels$Common[labels$Common=="Elk wapiti"] <- "Elk"
labels$Common[labels$Common=="Columbian ground squirrel"] <- "Ground Squirrel"

rownames <- c("Density trails","Density trails: Type recreation","Density trails: % Forest","Density trails: Management","Density trails: HFI","Density trails: Area","Density trails: Season")


color2 <- c("#6699CC","#99CCFF","#336600","#666600","#99CC66","#39452e","#CC9900") 


barplot(density_VP[,order(colSums(density_VP), decreasing =T)]
        , las=2, col=color2, ylab="Prop. of tot. variation explained", ylim=c(0,max(colSums(density_VP))+0.02 ),
        names.arg=labels$Common,
        main="Density")


legend("topright", legend=rev(rownames), fill= rev(color2))

#summary only of density
sumden <- as.data.frame(density_VP)
rowMeans(sumden)


# Interaction summary -----------------------------------------------------


row.names <- c("Recr: recr type","Recr: forest","Recr: Mgmnt","Recr: HFI","Recr: area", "Recre: Season")

VP.custom <- array(c(
  apply(VP$vals[c(15,21,27),],2,sum), #"Recr. intensity: type recreation"
  apply(VP$vals[c(16,22,28),],2,sum), #"Recr. intensity: forest"
  apply(VP$vals[c(17,23,29),],2,sum), # Recr. intensity: management"
  apply(VP$vals[c(18,24,30),],2,sum), # Recr. intensity: HFI"
  apply(VP$vals[c(19,25,31),],2,sum), # Recr. intensity: area"
  apply(VP$vals[c(20,26,32),],2,sum)), # Recr. intensity: season"
  
  dim = c(length(column.names),length(row.names)),
  dimnames = list(column.names,row.names)
)



VP.custom <- t(VP.custom)

colnames(VP.custom) <- gsub('\\.', ' ', colnames(VP.custom))
m$spNames[order(mod.eval$SR2, decreasing=T)]
mod.eval$SR2[order(mod.eval$SR2, decreasing=T)]


#pdf(width=7, height=7, "figures//Figure2.pdf")layout(matrix(c(1,2,2), 3, 1, byrow = TRUE))
labels <- data.frame("Species"=colnames(VP.custom[,order(colSums(VP.custom), decreasing =T)]))

# Read in the key
common <- read.csv("./data/common_species.csv", header=T)
common <- rename(common, Species=ï..Species)

labels <- left_join(labels,common)

par(mar=c(10,4,1,2))
barplot(VP.custom[,order(colSums(VP.custom), decreasing =T)], col=brewer.pal(n = 6, name = "Set3"), las=2,
        names.arg=labels$Species,
        xlim=c(0,length(column.names)+6.5), main="",
        ylab="Prop. of tot. variation explained")

legend(14, .8, inset=c(-0.2,0),legend=rev(row.names), fill= rev(brewer.pal(n = 6, name = "Set3")), cex = 1)


sumVP.custom <- as.data.frame(VP.custom)
rowMeans(sumVP.custom)



# Direction ---------------------------------------------------------------
postBeta <- getPostEstimate(m, parName = "Beta"
)

postBeta.df_1 <- as.data.frame(postBeta$mean)

postBeta.df_1$Fixed <- factor(c(
  "Intercept","Strava","Distance to trail",
  "Density trail","Non-Motorized trail","Mixed-Use trail",
  "Motorized trail","Forest %","Management",
  "Human Footprint Index","Area Castle","Season Winter",
  "Elevation","Distance to water","Habitat Forest",
  "Habitat Grassland-Shurbland","Habitat Other","NDVI",
  "Effort","Strava: Non-Motorized","Strava:Mixed-Use",
  "Strava: Motorized","Strava: Forest %","Strava: Management",
  "Strava: HFI","Strava: Area (Castle)","Strava: Season (Winter)",             
  "Distance: Non-Motorized","Distance:Mixed-Use","Distance: Motorized","Distance: Forest %",
  "Distance: Management","Distance: HFI","Distance: Area (Castle)","Distance: Season (Winter)",
  "Density: Non-Motorized","Density:Mixed-Use","Density: Motorized","Density: Forest %",
  "Density: Management","Density: HFI","Density: Area (Castle)","Density: Season (Winter)"),
  levels = c(
    "Intercept","Strava","Distance to trail",
    "Density trail","Non-Motorized trail","Mixed-Use trail",
    "Motorized trail",
    
    "Strava: Non-Motorized","Strava:Mixed-Use",
    "Strava: Motorized","Strava: Forest %","Strava: Management",
    "Strava: HFI","Strava: Area (Castle)","Strava: Season (Winter)",             
    "Distance: Non-Motorized","Distance:Mixed-Use","Distance: Motorized","Distance: Forest %",
    "Distance: Management","Distance: HFI","Distance: Area (Castle)","Distance: Season (Winter)",
    "Density: Non-Motorized","Density:Mixed-Use","Density: Motorized","Density: Forest %",
    "Density: Management","Density: HFI","Density: Area (Castle)","Density: Season (Winter)",
    
    "Forest %","Management","Human Footprint Index","Area Castle",
    "Elevation","Distance to water","Habitat Forest",
    "Habitat Grassland-Shurbland","Habitat Other","NDVI",
    "Season Winter","Effort"),
  order = TRUE)

postBeta.df_1 <- gather(postBeta.df_1, key = Species, value = "estimate", -Fixed)

postBeta.df_1$Support <- c(postBeta$support)
postBeta.df_1$supportNeg <- c(postBeta$supportNeg)

postBeta.df_1 <- postBeta.df_1 %>%
  group_by(Species, Fixed) %>%
  mutate(Value = max(Support, supportNeg)*sign(estimate)) %>%
  mutate(Value_thres = max(Support, supportNeg))

postBeta.df_1$Value_thres_95 <- NA
postBeta.df_1$Value_thres_95[postBeta.df_1$Value<(-0.95) ] <- "-"
postBeta.df_1$Value_thres_95[postBeta.df_1$Value> 0.95 ] <- "+"

postBeta.df_1$Value_thres <- postBeta.df_1$Value_thres*sign(postBeta.df_1$estimate)

postBeta.df_1$Species <- factor(postBeta.df_1$Species, levels = colnames(m$Y)[order(mod.eval$SR2, decreasing=T)], ordered = TRUE)

postBeta.df_1$Species <- str_replace_all(postBeta.df_1$Species,"\\."," ")


postBeta.df_1$Species[postBeta.df_1$Species=="Red fox"] <- "Red Fox"
postBeta.df_1$Species[postBeta.df_1$Species=="White-tailed deer"] <- "White-tailed Deer"
postBeta.df_1$Species[postBeta.df_1$Species=="Mule deer"] <- "Mule Deer"
postBeta.df_1$Species[postBeta.df_1$Species=="Grizzly bear"] <- "Grizzly Bear"
postBeta.df_1$Species[postBeta.df_1$Species=="Elk wapiti"] <- "Elk"
postBeta.df_1$Species[postBeta.df_1$Species=="Ground squirrel"] <- "Ground Squirrel"


postBeta.df_1$Species <-  factor(postBeta.df_1$Species, levels = c("Ground Squirrel","Red Squirrel","Marten","Snowshoe Hare","Mule Deer","White-tailed Deer","Moose","Elk","Red Fox","Grizzly Bear","Black Bear","Canada Lynx","Coyote","Cougar","Gray Wolf"))


postBeta.df_1$panel <- NA
postBeta.df_1$panel[str_detect(postBeta.df_1$Fixed, "All trail")] <- "AllTrails"
postBeta.df_1$panel[str_detect(postBeta.df_1$Fixed, "Strava")] <- "Strava"
postBeta.df_1$panel[str_detect(postBeta.df_1$Fixed, "Distance")] <- "Distance"
postBeta.df_1$panel[str_detect(postBeta.df_1$Fixed, "Density")] <- "Density"

postBeta.df_1$panel[is.na(postBeta.df_1$panel)] <- "Modulators"

postBeta.df_1$panel <- factor(postBeta.df_1$panel, levels = c("AllTrails","Strava", "Distance", "Density"))


## general plot #########
colors <- c(rep("red",7),
            rep("blue",4),
            rep("darkgreen",4))


postBeta.df_1$fill <- NA

for (i in 1:nrow(postBeta.df_1)){
  if (postBeta.df_1$Value[i]<(-0.95) | postBeta.df_1$Value[i]>0.95 ) {postBeta.df_1$fill[i] <- postBeta.df_1$Value_thres[i]} 
}

postBeta.df_1 %>% 
  ggplot(aes(x = Species, y = Fixed, fill = fill))+
  geom_tile()+
  geom_text(aes(label = Value_thres_95),  size=10) +
  scale_fill_distiller(palette = "RdBu", name = "Effects size", na.value = "lightgrey")+
  coord_flip()+
  ylab("")+
  xlab("")+
  geom_hline(yintercept=c(1.5,4.5,7.5,31.5,41.5,42.5), linetype="dashed", color = "black")+
  geom_vline(xintercept=c(4.5,8.5,15.5), linetype="dashed", color = c("darkgreen","blue","red"), size=1.2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1, size = 16),
        axis.text.y = element_text( size = 16, color = rev(colors)),
        strip.text.x = element_text(size = 20),
        text = element_text(size = 16),
        strip.background = element_blank(),
        legend.position="right")

## type of activity ############
postBeta.df_1 %>% 
  filter(Fixed %in% c("Non-Motorized trail","Mixed-Use trail","Motorized trail",
                      "Strava","Strava: Non-Motorized","Strava:Mixed-Use","Strava: Motorized",
                      "Distance to trail","Distance: Non-Motorized","Distance:Mixed-Use","Distance: Motorized",
                      "Density trail","Density: Non-Motorized","Density:Mixed-Use","Density: Motorized")) %>% 
  mutate(Fixed = factor(Fixed, levels=c(
                            "Non-Motorized trail","Mixed-Use trail","Motorized trail",
                      "Strava","Strava: Non-Motorized","Strava:Mixed-Use","Strava: Motorized",
                      "Distance to trail","Distance: Non-Motorized","Distance:Mixed-Use","Distance: Motorized",
                      "Density trail","Density: Non-Motorized","Density:Mixed-Use","Density: Motorized"))) %>%
  ggplot(aes(x = Species, y = Fixed, fill = fill))+
  geom_tile()+
  geom_text(aes(label = Value_thres_95),  size=10) +
  scale_fill_distiller(palette = "RdBu", name = "Effects size", na.value = "grey")+
  coord_flip()+
  ylab("")+
  xlab("")+
  geom_hline(yintercept=c(3.5,7.5,11.5), linetype="dashed", color = "black")+
  geom_vline(xintercept=c(4.5,8.5,15.5), linetype="dashed", color = c("darkgreen","blue","red"), size=1.2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1, size = 16),
        axis.text.y = element_text( size = 16, color = rev(colors)),
        strip.text.x = element_text(size = 20),
        text = element_text(size = 16),
        strip.background = element_blank(),
        legend.position="right")



## management ###########

postBeta.df_1 %>% 
  filter(Fixed %in% c("AllTrails","All trail: Management","Strava","Strava: Management",
                      "Distance to trail","Distance: Management","Density trail","Density: Management","Management"
  ))  %>% 
  mutate(Fixed = factor(Fixed, levels=c("Management","AllTrails","All trail: Management","Strava","Strava: Management",
                                        "Distance to trail","Distance: Management","Density trail","Density: Management"
  )))  %>% 
  ggplot(aes(x = Species, y = Fixed, fill = fill))+
  geom_tile()+
  geom_text(aes(label = Value_thres_95),  size=10
            ) +
  scale_fill_distiller(palette = "RdBu", name = "Effects size", na.value = "grey")+
  #facet_wrap(~panel, scales = "free")+
  coord_flip()+
  ylab("")+
  xlab("")+
  geom_hline(yintercept=c(1.5,3.5,5.5), linetype="dashed", color = "black")+
  geom_vline(xintercept=c(4.5,8.5,15.5), linetype="dashed", color = c("darkgreen","blue","red"), size=1.2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1, size = 16),
        axis.text.y = element_text( size = 16, color = rev(colors)),
        strip.text.x = element_text(size = 16))+
  theme(strip.background = element_blank(), legend.position="right")


## Prediction #### 
mpost = convertToCodaObject(m)
mpostBeta <- mpost$Beta

int.mcmc <- as.mcmc.list(mpostBeta)

int.mcmc.mat <- as.matrix(int.mcmc)
int.mcmc.dat <- as.data.frame(int.mcmc.mat)

# which proxy 
dt <- select(int.mcmc.dat, contains("distance"))

# which variable
dt <- select(dt, contains("TYPE_n"))

# create base of management
cov <- m$XData

x2.sim <- seq(min(cov$TYPE_n), max(cov$TYPE_n), # modulator 
              by = 0.1)

#select variable and species of interest
species <- c("Gray Wolf","Coyote","Canada Lynx","Black Bear","Grizzly Bear","Red Fox","Moose","White-tailed Deer","Mule Deer","Marten") 

#empty df
dtplot <- data.frame()

for (i in species){
  dt_sub <- select(dt, contains(i))
  
  int.sim <- matrix(rep(NA, 
                        nrow(dt)*length(x2.sim)), 
                    nrow = nrow(dt_sub))
  
  for(j in 1:length(x2.sim)){
    int.sim[, j] <- dt_sub[,1] + 
      dt_sub[,1] * x2.sim[j]
  }
  
  bayes.c.eff.mean <- apply(int.sim, 2, mean)
  bayes.c.eff.lower <- apply(int.sim, 2, function(x) quantile(x, probs = c(0.025)))
  bayes.c.eff.upper <- apply(int.sim, 2, function(x) quantile(x, probs = c(0.975)))
  
  df_int <- data.frame(x2.sim, bayes.c.eff.mean, bayes.c.eff.lower, bayes.c.eff.upper)
  df_int$Species <- i
  
  dtplot <- rbind(dtplot,df_int)
}


#plot
(ggplot(data=dtplot,aes(x=x2.sim, y=bayes.c.eff.mean, color=Species)) +
    geom_ribbon(data=dtplot,aes(ymin =bayes.c.eff.lower, ymax =bayes.c.eff.upper, fill=Species), alpha = 0.0, size=0.1, linetype="dashed",show.legend=F )+
    geom_line(alpha = 1, size = 1.2)+
    xlab("Management") + ylab("Conditional effect of Distance") +
    geom_hline(yintercept=0, linetype="dashed", color = "black", size = 1.5) +
    scale_colour_manual(breaks=species,
                        values=c("#470803","#700a02","#ad1105","#d11506","#ed5a4e","#f5c5c4",
                                 "#48245c","#130899","#827bdb",
                                 "#14eb10")) +
    theme(axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20))+
    theme(strip.background = element_blank()) + theme_bw()
)


## for categorical variables


# which proxy 
dt <- select(int.mcmc.dat, contains("usrt_strv_mean"))

# which variable
dt <- select(dt, contains("Use_Type"))

# create base of management
x2.sim <- c("Non-Motorized trail")

#select variable and species of interest
species <- c("Mule Deer","Black Bear","Grizzly Bear") 

#empty df
dtplot <- data.frame()

for (i in species){
  dt_sub <- select(dt, contains(i))
  
  int.sim <- matrix(rep(NA, 
                        nrow(dt)*length(x2.sim)), 
                    nrow = nrow(dt_sub))
  


  for(j in 1:length(x2.sim)){
    int.sim[, j] <- dt_sub[,1] + dt_sub[,1] 
    
    
  }
  
  bayes.c.eff.mean <- apply(int.sim, 2, mean)
  bayes.c.eff.lower <- apply(int.sim, 2, function(x) quantile(x, probs = c(0.025)))
  bayes.c.eff.upper <- apply(int.sim, 2, function(x) quantile(x, probs = c(0.975)))
  
  df_int <- data.frame(x2.sim, bayes.c.eff.mean, bayes.c.eff.lower, bayes.c.eff.upper)
  df_int$Species <- i
  
  dtplot <- rbind(dtplot,df_int)
}


# create base of management
x2.sim <- c("Mixed-use")

#select variable and species of interest
species <- c("Mule Deer","Grizzly Bear","Canada Lynx","Coyote","Gray Wolf") 

#empty df
for (i in species){
  dt_sub <- select(dt, contains(i))
  
  int.sim <- matrix(rep(NA, 
                        nrow(dt)*length(x2.sim)), 
                    nrow = nrow(dt_sub))
  
  
  
  for(j in 1:length(x2.sim)){
    int.sim[, j] <- dt_sub[,1] + dt_sub[,1] 
    
    
  }
  
  bayes.c.eff.mean <- apply(int.sim, 2, mean)
  bayes.c.eff.lower <- apply(int.sim, 2, function(x) quantile(x, probs = c(0.025)))
  bayes.c.eff.upper <- apply(int.sim, 2, function(x) quantile(x, probs = c(0.975)))
  
  df_int <- data.frame(x2.sim, bayes.c.eff.mean, bayes.c.eff.lower, bayes.c.eff.upper)
  df_int$Species <- i
  
  dtplot <- rbind(dtplot,df_int)
}


#plot
ggplot(dtplot, aes(x=x2.sim, bayes.c.eff.mean)) +
  geom_errorbar(
    aes(ymin = bayes.c.eff.lower, ymax = bayes.c.eff.upper, color = Species),
    position = position_dodge(0.3), width = 0.2
  )+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size = 1.5) +
  geom_point(aes(color = Species), position = position_dodge(0.3)) +
  theme(axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20))+
  theme(strip.background = element_blank()) + theme_bw() +
  xlab("Management")+
  ylab("Conditional effect of Strava")+
  scale_colour_manual(breaks=c("Gray Wolf","Coyote","Canada Lynx","Black Bear","Grizzly Bear",
                               "Mule Deer"),
                      values=c("#fa0511","#730208","#3d2213","#1c0202","#e3684f",
                               "#2d20a1"))


## distance vs type of activity

# which proxy 
dt <- select(int.mcmc.dat, contains("distance"))

# which variable
dt <- select(dt, contains("Use_Type"))

# create base of management
x2.sim <- c("Non-Motorized trail")

#select variable and species of interest
species <- c("Canada Lynx", "Snowshoe Hare") 

#empty df
dtplot <- data.frame()

for (i in species){
  dt_sub <- select(dt, contains(i))
  
  int.sim <- matrix(rep(NA, 
                        nrow(dt)*length(x2.sim)), 
                    nrow = nrow(dt_sub))
  
  
  
  for(j in 1:length(x2.sim)){
    int.sim[, j] <- dt_sub[,1] + dt_sub[,1] 
    
    
  }
  
  bayes.c.eff.mean <- apply(int.sim, 2, mean)
  bayes.c.eff.lower <- apply(int.sim, 2, function(x) quantile(x, probs = c(0.025)))
  bayes.c.eff.upper <- apply(int.sim, 2, function(x) quantile(x, probs = c(0.975)))
  
  df_int <- data.frame(x2.sim, bayes.c.eff.mean, bayes.c.eff.lower, bayes.c.eff.upper)
  df_int$Species <- i
  
  dtplot <- rbind(dtplot,df_int)
}



#plot

ggplot(dtplot, aes(x=x2.sim, bayes.c.eff.mean)) +
  geom_errorbar(
    aes(ymin = bayes.c.eff.lower, ymax = bayes.c.eff.upper, color = Species),
    position = position_dodge(0.3), width = 0.2
  )+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size = 1.5) +
  geom_point(aes(color = Species), position = position_dodge(0.3)) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  theme(strip.background = element_blank()) + theme_bw() +
  xlab("Management")+
  ylab("Conditional effect of Distance")+
  scale_colour_manual(breaks=c("Canada Lynx","Snowshoe Hare"),
                      values=c("#3d2213",
                               "#09e83e"))


#Density


# which proxy 
dt <- select(int.mcmc.dat, contains("density"))

# which variable
dt <- select(dt, contains("Use_Type"))

# create base of management
x2.sim <- c("Non-Motorized trail")

#select variable and species of interest
species <- c("Gray Wolf","Coyote") 

#empty df
dtplot <- data.frame()

for (i in species){
  dt_sub <- select(dt, contains(i))
  
  int.sim <- matrix(rep(NA, 
                        nrow(dt)*length(x2.sim)), 
                    nrow = nrow(dt_sub))
  
  
  
  for(j in 1:length(x2.sim)){
    int.sim[, j] <- dt_sub[,1] + dt_sub[,1] 
    
    
  }
  
  bayes.c.eff.mean <- apply(int.sim, 2, mean)
  bayes.c.eff.lower <- apply(int.sim, 2, function(x) quantile(x, probs = c(0.025)))
  bayes.c.eff.upper <- apply(int.sim, 2, function(x) quantile(x, probs = c(0.975)))
  
  df_int <- data.frame(x2.sim, bayes.c.eff.mean, bayes.c.eff.lower, bayes.c.eff.upper)
  df_int$Species <- i
  
  dtplot <- rbind(dtplot,df_int)
}


# create base of management
x2.sim <- c("Mixed-use")

#select variable and species of interest
species <- c("Cougar","Coyote","Grizzly Bear") 

#empty df
for (i in species){
  dt_sub <- select(dt, contains(i))
  
  int.sim <- matrix(rep(NA, 
                        nrow(dt)*length(x2.sim)), 
                    nrow = nrow(dt_sub))
  
  
  
  for(j in 1:length(x2.sim)){
    int.sim[, j] <- dt_sub[,1] + dt_sub[,1] 
    
    
  }
  
  bayes.c.eff.mean <- apply(int.sim, 2, mean)
  bayes.c.eff.lower <- apply(int.sim, 2, function(x) quantile(x, probs = c(0.025)))
  bayes.c.eff.upper <- apply(int.sim, 2, function(x) quantile(x, probs = c(0.975)))
  
  df_int <- data.frame(x2.sim, bayes.c.eff.mean, bayes.c.eff.lower, bayes.c.eff.upper)
  df_int$Species <- i
  
  dtplot <- rbind(dtplot,df_int)
}



# create base of management
x2.sim <- c("Motorized")

#select variable and species of interest
species <- c("White-tailed Deer","Mule Deer","Snowshoe Hare") 

#empty df
for (i in species){
  dt_sub <- select(dt, contains(i))
  
  int.sim <- matrix(rep(NA, 
                        nrow(dt)*length(x2.sim)), 
                    nrow = nrow(dt_sub))
  
  
  
  for(j in 1:length(x2.sim)){
    int.sim[, j] <- dt_sub[,1] + dt_sub[,1] 
    
    
  }
  
  bayes.c.eff.mean <- apply(int.sim, 2, mean)
  bayes.c.eff.lower <- apply(int.sim, 2, function(x) quantile(x, probs = c(0.025)))
  bayes.c.eff.upper <- apply(int.sim, 2, function(x) quantile(x, probs = c(0.975)))
  
  df_int <- data.frame(x2.sim, bayes.c.eff.mean, bayes.c.eff.lower, bayes.c.eff.upper)
  df_int$Species <- i
  
  dtplot <- rbind(dtplot,df_int)
}


#plot

ggplot(dtplot, aes(x=x2.sim, bayes.c.eff.mean)) +
  geom_errorbar(
    aes(ymin = bayes.c.eff.lower, ymax = bayes.c.eff.upper, color = Species),
    position = position_dodge(0.3), width = 0.2
  )+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size = 1.5) +
  geom_point(aes(color = Species), position = position_dodge(0.3)) +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  theme(strip.background = element_blank()) + theme_bw() +
  xlab("Management")+
  ylab("Conditional effect of Strava")+
  scale_colour_manual(breaks=c("Gray Wolf","Coyote","Cougar","Grizzly Bear",
                               "White-tailed Deer","Mule Deer",
                               "Snowshoe Hare"),
                      values=c("#fa0511","#730208","#3d2213","#ff771c",
                               "#271cff","#95a1ed",
                               "#2fff1c"))

