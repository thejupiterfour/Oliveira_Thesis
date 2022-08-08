library(stringr)  
library(plyr)
library(tidyverse)
dat      <- read.csv('Phytoplankton_San_Francisco_Bay_1992_2014.csv')
dat$Date <- as.Date(as.character(dat$Date), '%m/%d/%Y')
dat$Year <- strftime(dat$Date, format = '%Y')
dat$Month<- strftime(dat$Date, format = '%m')
dat$DOY  <- strftime(dat$Date, format = '%j')

table(dat$Taxonomic.Identification)

#Calculate ESD
Vol2ESD <- function(V)(6*V/pi)^(1/3)
dat$ESD <- Vol2ESD(dat$Cell.Volume..cubic.micrometers.cell.)
  
#Plot histogram
hist(log10(unique(dat$ESD)))

z <- SF_data3  %>%  subset(!is.na(Date) & !is.na(Depth) & !is.na(Station_No)
             ) %>%  dplyr::mutate(Year  = as.integer(Year)
             ) %>%  dplyr::mutate(Month = as.integer(Month)
             ) %>%  dplyr::mutate(DOY   = as.integer(DOY)
             ) %>%  dplyr::mutate(Station_No=as.factor(Station_No)
             ) 

save(dat, file = 'SFBayphyto.Rdata')

#Below starts to analyse Livia's dataset
load('SFBayPhyto3.Rdata')


z <- SF_data3  %>%  subset(!is.na(Date) & !is.na(Depth) & !is.na(Station_No) & Tax_ID != 0
             ) %>%  dplyr::mutate(Year  = as.integer(Year)
             ) %>%  dplyr::mutate(Month = as.integer(Month)
             ) %>%  dplyr::mutate(DOY   = as.integer(DOY)
             ) %>%  dplyr::mutate(Station_No=as.factor(Station_No)
             ) 
z <- as.data.frame(z)
z[z$Nitrate_Nitrite <= 0, 'Nitrate_Nitrite'] <- NA

#Organize the dataset into unique samples
#choose an identifier (independent samples)
S <- ddply(z, .(Station_No, Year, Month, DOY, Depth), summarize,
                     Chl = mean(C_Chl, na.rm=T),
                     Temp= mean(Temperature, na.rm=T),
                     Sal = mean(Salinity, na.rm=T),
                     Ext_Coeff=mean(Ext_Coeff, na.rm=T),
                     PO4 = mean(Phosphate, na.rm=T),
                     NO3 = mean(Nitrate_Nitrite, na.rm=T)
           )
S <- S %>% subset( Chl > 0) #Remove 0 Chl

#Nutrient data are missing!

#Add species
Spnames <- unique(z$Tax_ID)

#Construct a matrix of species abundances
Sp_abun <- matrix(0, nrow = nrow(S), ncol = length(Spnames))

for (i in 1:length(Spnames)){
  #First, find out all observations with this species
  d <- subset(z, Tax_ID == Spnames[i])
  d <- as.data.frame(d)
  
  for (j in 1:nrow(d)){
    wd <- which(as.character(S$Station_No) == as.character(d[j, 'Station_No']) &
                S$Year       == d[j, 'Year']       &
                S$Month      == d[j, 'Month']      &
                S$DOY        == d[j, 'DOY']        &
                S$Depth      == d[j, 'Depth']      
               )
    Sp_abun[wd, i] <- d[j, 'Density']
  }
  
}

#Calculate richness for each sample

S$Richness <- apply(Sp_abun, 1, function(x)length(x[x>0]))
S$LnChl <- log(S$Chl)

#Plot Richness against Chl 
png('Richness_Chl_temp.png',  height=300, width=400)
par(mfrow=c(1,2))
par(mar=c(4,4,0,0))
plot(S$Chl, S$Richness, xlab='Chl (ug/L)', ylab='Species Richness',
     log='x',
     pch=16, cex=.5)

#Add a linear  line
lm1 <- lm(Richness ~ log(Chl), S)
abline(lm1)

#Add a unimodal line
lm2 <- lm(Richness ~ log(Chl) + I(log(Chl)^2), S)
hownewx <- seq(0.1, 100, by=.1)
newy <- predict(lm2, newdata=data.frame(Chl=newx),se.fit=T)
lines(newx,newy$fit, lwd=1.5)


plot(S$Temp, S$Richness, xlab='Temperature (C)', ylab='Species Richness',
     pch=16, cex=.5)

dev.off()

anova(lm1, lm2)

