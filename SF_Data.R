#San Francisco data set

#libraries
library(tidyverse)
library(ggsci)
library(tidyselect)
library(writexl)
library(lubridate)
library(readr)
library(stringr)
library(patchwork)
library(wesanderson)
library(gridExtra)
library(vegan)

theme_set(theme_light())
  
#Preamble of analyses
rm(list=ls())
# Import Data -------------------------------------------------------------
#environmental data
environment_1969_2015 <- read.csv("SanFranciscoBayWaterQualityData1969-2015v3.csv", header = TRUE)
environment_2016 <- read.csv("2016ver.3.0SanFranciscoBayWaterQualityData.csv", header = TRUE)
environment_2017 <- read.csv("2017ver.3.0SanFranciscoBayWaterQualityData.csv", header = TRUE)
environment_2018 <- read.csv("2018SanFranciscoBayWaterQualityData.csv", header = TRUE)
environment_2019 <- read.csv("2019SanFranciscoBayWaterQualityData.csv", header = TRUE)
environment_2020 <- read_csv("environment_2020.csv")
environment_2021<- read_csv("environment_2021.csv")

#phyto dataset from 2014-2018 with abundance counts
Phyto_Apr1992_Mar2014 <- read_csv("Phytoplankton_San_Francisco_Bay_1992_2014.csv", col_types = cols(Date = col_date(format = "%m/%d/%Y")))
Phyto_Jan2014_Feb2021 <- read_delim("~/OneDrive/University of Strathclyde/SF_Dataset/Jan2014_Feb2021.csv", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE, )
#nutrient data
TotNutrients_2014_2021 <- read_excel("Chapter_2/TotNutrients_2014_2021.xlsx") #Total nutrients not just dissolved

#Fixing the dates for environmental and nutrient data ----------
environment_1969_2015$Date <- as.Date(as.character(environment_1969_2015$Date), '%m/%d/%y')
environment_1969_2015$Year <- strftime(environment_1969_2015$Date, format = '%Y')
environment_1969_2015$Month<- strftime(environment_1969_2015$Date, format = '%m')
environment_1969_2015$DOY  <- strftime(environment_1969_2015$Date, format = '%j')

environment_2019$Date <- as.Date(as.character(environment_2019$Date), '%m/%d/%y')
environment_2019$Year <- strftime(environment_2019$Date, format = '%Y')
environment_2019$Month<- strftime(environment_2019$Date, format = '%m')
environment_2019$DOY  <- strftime(environment_2019$Date, format = '%j')

environment_2018$Date <- as.Date(as.character(environment_2018$Date), '%m/%d/%y')
environment_2018$Year <- strftime(environment_2018$Date, format = '%Y')
environment_2018$Month<- strftime(environment_2018$Date, format = '%m')
environment_2018$DOY  <- strftime(environment_2018$Date, format = '%j')

environment_2017$Date <- as.Date(as.character(environment_2017$Date), '%m/%d/%y')
environment_2017$Year <- strftime(environment_2017$Date, format = '%Y')
environment_2017$Month<- strftime(environment_2017$Date, format = '%m')
environment_2017$DOY  <- strftime(environment_2017$Date, format = '%j')


environment_2016$Date <- as.Date(as.character(environment_2016$Date), '%m/%d/%y') #changing type 
environment_2016$Year <- strftime(environment_2016$Date, format = '%Y')
environment_2016$Month<- strftime(environment_2016$Date, format = '%m')
environment_2016$DOY  <- strftime(environment_2016$Date, format = '%j')

environment_2020$Date <- as.Date(as.character(environment_2020$Date)) #changing type 
environment_2020$Year <- strftime(environment_2020$Date, format = '%Y')
environment_2020$Month<- strftime(environment_2020$Date, format = '%m')
environment_2020$DOY  <- strftime(environment_2020$Date, format = '%j')

environment_2021$Date <- as.Date(as.character(environment_2021$Date)) #changing type
environment_2021$Year <- strftime(environment_2021$Date, format = '%Y')
environment_2021$Month<- strftime(environment_2021$Date, format = '%m')
environment_2021$DOY  <- strftime(environment_2021$Date, format = '%j')


TotNutrients_2014_2021$Year <- strftime(TotNutrients_2014_2021$Date, format = '%Y')
TotNutrients_2014_2021$Month<- strftime(TotNutrients_2014_2021$Date, format = '%m')
TotNutrients_2014_2021$DOY  <- strftime(TotNutrients_2014_2021$Date, format = '%j')



#dropping columns that do not match
environment_2020 <- environment_2020%>%
  select(!c(Time,"Julian Date", `Distance from Sta. 36 (km)`, `Discrete Chlorophyll-Pheopigment Ratio`, `Sigma-t`, `Oxygen % Saturation`))
environment_2021 <- environment_2021%>%
  select(!c(Time, "Julian Date", `Distance from Sta. 36 (km)`, `Discrete Chlorophyll-Pheopigment Ratio`, `Sigma-t`, `Oxygen % Saturation`))

#changing columns names before joining

environment_2020 <- environment_2020%>%
  rename(
         Calculated_Chlorophyll = `Calculated Chlorophyll-a (micrograms/L)`,
         Discrete_Chlorophyll = `Discrete Chlorophyll-a (micrograms/L)`, 
         Discrete_Oxygen = `Discrete Oxygen (mg/L)`,
         Calculated_Oxygen = `Oxygen (mg/L)`,
         Discrete_SPM = `Discrete SPM (mg/L)`,
         Calculated_SPM = `Calculated SPM (mg/L)`,
         Extinction_Coefficient = `Extinction Coefficient`,
         Temperature = `Temperature (Degrees Celsius)`,
         Nitrate_._Nitrite = `NO32 (Micromolar)`,
         Nitrite = `NO2 (Micromolar)`,
         Ammonium = `NH4 (Micromolar)`, 
         Phosphate = `PO4 (Micromolar)`,
         Silicate = `Si (Micromolar)`,
         Station_Number = Station,
         Depth = `Depth (m)`,
         )


environment_2021 <- environment_2021%>%
  rename( 
         Calculated_Chlorophyll = `Calculated Chlorophyll-a (micrograms/L)`,
         Discrete_Chlorophyll = `Discrete Chlorophyll-a (micrograms/L)`, 
         Discrete_Oxygen = `Discrete Oxygen (mg/L)`,
         Calculated_Oxygen = `Oxygen (mg/L)`,
         Discrete_SPM = `Discrete SPM (mg/L)`,
         Calculated_SPM = `Calculated SPM (mg/L)`,
         Extinction_Coefficient = `Extinction Coefficient`,
         Temperature = `Temperature (Degrees Celsius)`,
         Nitrate_._Nitrite = `NO32 (Micromolar)`,
         Nitrite = `NO2 (Micromolar)`,
         Ammonium = `NH4 (Micromolar)`, 
         Phosphate = `PO4 (Micromolar)`,
         Silicate = `Si (Micromolar)`,
         Station_Number = Station,
         Depth = `Depth (m)`)

#----------------JOIN ENVIRONMENTAL DATA SETS 1969-2021------
#combine the environmental observations into one file 1969-2021
environment_1969_2021 <- mget(str_glue("environment_{2016:2021}"))%>%
  append(list(environment_1969_2015))%>%
  reduce(full_join)

#droping columns that do not match
environment_1969_2021 <- environment_1969_2021%>%
  select(!c(Time, Oxygen))

#filtering occurrences between 1992-2014 and 2014-2021
environment_1992_2021 <- environment_1969_2021 %>%
  filter(environment_1969_2021$Year >= 1992)

Abiotic_1992_2014 <- environment_1992_2021 %>%        
  filter(Year <= 2014)

Abiotic_2014_2021 <- environment_1992_2021 %>%
  filter(Year >= 2014)


#Fixing the dates for phytoplankton data -------
Phyto_Apr1992_Mar2014$Year <- strftime(Phyto_Apr1992_Mar2014$Date, format = '%Y')
Phyto_Apr1992_Mar2014$Month<- strftime(Phyto_Apr1992_Mar2014$Date, format = '%m')
Phyto_Apr1992_Mar2014$DOY  <- strftime(Phyto_Apr1992_Mar2014$Date, format = '%j')

Phyto_Jan2014_Feb2021$Year <- strftime(Phyto_Jan2014_Feb2021$Date, format = '%Y')
Phyto_Jan2014_Feb2021$Month<- strftime(Phyto_Jan2014_Feb2021$Date, format = '%m')
Phyto_Jan2014_Feb2021$DOY  <- strftime(Phyto_Jan2014_Feb2021$Date, format = '%j')



#----------------JOIN ENVIRONMENTAL WITH PHYTOPLANKTON AND NUTRIENT DATA ------
#rename variables before joining!
Phyto_Apr1992_Mar2014 <- Phyto_Apr1992_Mar2014 %>%
  rename(
    Tax_ID = `Taxonomic Identification`, Phyl_Class = `Phylum or Class`, Station_Number = `Station Number`,
    Depth = `Depth (m)`, Density = `Density (cells/mL)`, Biovolume = `Biovolume (cubic micrometers/mL)`,
    Cell_Vol = `Cell Volume (cubic micrometers/cell)` 
  )

Phyto_Jan2014_Feb2021 <- Phyto_Jan2014_Feb2021 %>%
  rename(
    Tax_ID = `Current Species ID`, Phyl_Class = `Phylum/Class`, Station_Number = `Station ID`,
    Depth = `Depth (m)`, Density = `Density (cells/mL)`, Biovolume = `Biovolume (cubic micrometers/mL)`,
    Cell_Vol = `Cell Volume (cubic micrometers/cell)`
  ) %>%
  select(-c(`Month (1-12)`, `Actual Count`, `Original Species ID`, Genus))

#joining phyto and environmental data
SFBay_1992_2014 <- full_join(Abiotic_1992_2014, Phyto_Apr1992_Mar2014)
SFBay_2014_2021 <- full_join(Abiotic_2014_2021, Phyto_Jan2014_Feb2021)
SFBay_2014_2021_TotNut <- full_join(SFBay_2014_2021, TotNutrients_2014_2021)


#Station numbers as factors
SFBay_1992_2014 <- SFBay_1992_2014%>%
  mutate(Station_Number = as_factor(Station_Number), Dataset="SFB1")

SFBay_2014_2021 <- SFBay_2014_2021%>%
  mutate(Station_Number = as_factor(Station_Number), Dataset ="SFB2")

SFBay_2014_2021_TotNut <- SFBay_2014_2021_TotNut%>%
  mutate(Station_Number = as_factor(Station_Number))%>%
  select(-Nitrite, -Nitrate_._Nitrite, -Ammonium, -Phosphate)

#corrects character variables into numeric without (!) generating NAs
SFBay_1992_2014$Density <- as.numeric(sub(",", ".", SFBay_1992_2014$Density, fixed = TRUE))
SFBay_1992_2014$Cell_Vol <- as.numeric(sub(",", ".", SFBay_1992_2014$Cell_Vol, fixed = TRUE))

SFBay_2014_2021$Density <- as.numeric(sub(",", ".", SFBay_2014_2021$Density, fixed = TRUE))
SFBay_2014_2021$Cell_Vol <- as.numeric(sub(",", ".", SFBay_2014_2021$Cell_Vol, fixed = TRUE))

SFBay_2014_2021_TotNut$Density <- as.numeric(sub(",", ".", SFBay_2014_2021_TotNut$Density, fixed = TRUE))
SFBay_2014_2021_TotNut$Cell_Vol <- as.numeric(sub(",", ".", SFBay_2014_2021_TotNut$Cell_Vol, fixed = TRUE))

#editing the file to rename stations
lat_long <- read_csv("~/Library/CloudStorage/OneDrive-Personal/University of Strathclyde/SFBay_TableofStationLocations.csv", 
                                         col_types = cols(`Station Number` = col_character()))
#at_long <- edit(lat_long)
#adding lat and long
#----adding stations' names----
SFBay_1992_2014<- SFBay_1992_2014%>%
  mutate(Location = case_when(
    Station_Number == 657 ~ "Rio Vista",
    Station_Number == 649 ~ "Sacramento River",
    Station_Number == 2 ~ "Chain Island",
    Station_Number == 3 ~ "Pittsburg",
    Station_Number == 4 ~ "Simmons Point",
    Station_Number == 5 ~ "Middle Ground",
    Station_Number == 6 ~ "Roe Island",
    Station_Number == 7 ~ "Avon Pier",
    Station_Number == 8 ~ "Martinez",
    Station_Number == 9 ~ "Benicia",
    Station_Number == 10 ~ "Crockett",
    Station_Number == 11 ~ "Mare Island",
    Station_Number == 12 ~ "Pinole Shoal",
    Station_Number == 13 ~ "North of Pinole Point",
    Station_Number == 14 ~ "Echo Buoy",
    Station_Number == 15 ~ "Point San Pablo",
    Station_Number == 16 ~ "Charlie Buoy",
    Station_Number == 17 ~ "Raccoon Strait",
    Station_Number == 18 ~ "Point Blunt",
    Station_Number == 20 ~ "Blossom Rock",
    Station_Number == 21 ~ "Bay Bridge",
    Station_Number == 22 ~ "Potrero Point",
    Station_Number == 23 ~ "Hunters Point",
    Station_Number == 24 ~ "Candlestick Point",
    Station_Number == 25 ~ "Oyster Point",
    Station_Number == 26 ~ "San Bruno Shoal",
    Station_Number == 27 ~ "San Francisco Airport",
    Station_Number == 28 ~ "North of San Mateo Bridge",
    Station_Number == 29 ~ "South of San Mateo Bridge",
    Station_Number == 29.5 ~ "Steinberger Slough",
    Station_Number == 30 ~ "Redwood Creek",
    Station_Number == 31 ~ "Coyote Hills",
    Station_Number == 32 ~ "Ravenswood Point",
    Station_Number == 33 ~ "Dumbarton Bridge",
    Station_Number == 34 ~ "Newark Slough",
    Station_Number == 35 ~ "Mowry Slough",
    Station_Number == 36 ~ "Calaveras Point",
    Station_Number == 1 ~ "Winters Island", #historical sites from here onwards
    Station_Number == 12.5 ~ "Pinole Point",
    Station_Number == 19 ~ "Golden Gate",
    Station_Number == 24.5 ~ "South Bay1",
    Station_Number == 25.5 ~ "South Bay2",
    Station_Number == 26.5 ~ "South Bay3",
    Station_Number == 27.5 ~ "South Bay4",
    Station_Number == 28.5 ~ "Geo Probe",
    Station_Number == 30.5 ~ "South Bay5",
    Station_Number == 31.5 ~ "South Bay6",
    Station_Number == 33.5 ~ "South Bay7",
    Station_Number == 405 ~ "Reserve Fleet 2",
    Station_Number == 407 ~ "Reserve Fleet 4",
    Station_Number == 650 ~ "Sacramento River",
    Station_Number == 651 ~ "Sacramento River",
    Station_Number == 652 ~ "Towlands Landing",
    Station_Number == 653 ~ "Mid-Decker Island",
    Station_Number == 654 ~ "Sacramento River",
    Station_Number == 655 ~ "North of Three Mile Slough",
    Station_Number == 659 ~ "Old Sacramento River",
    Station_Number == 662 ~ "Prospect Island",
    Station_Number == 411 ~ "Garnet Sill",

  ))

SFBay_2014_2021<- SFBay_2014_2021%>%
  mutate(Location = case_when(
    Station_Number == 657 ~ "Rio Vista",
    Station_Number == 649 ~ "Sacramento River",
    Station_Number == 2 ~ "Chain Island",
    Station_Number == 3 ~ "Pittsburg",
    Station_Number == 4 ~ "Simmons Point",
    Station_Number == 5 ~ "Middle Ground",
    Station_Number == 6 ~ "Roe Island",
    Station_Number == 7 ~ "Avon Pier",
    Station_Number == 8 ~ "Martinez",
    Station_Number == 9 ~ "Benicia",
    Station_Number == 10 ~ "Crockett",
    Station_Number == 11 ~ "Mare Island",
    Station_Number == 12 ~ "Pinole Shoal",
    Station_Number == 13 ~ "North of Pinole Point",
    Station_Number == 14 ~ "Echo Buoy",
    Station_Number == 15 ~ "Point San Pablo",
    Station_Number == 16 ~ "Charlie Buoy",
    Station_Number == 17 ~ "Raccoon Strait",
    Station_Number == 18 ~ "Point Blunt",
    Station_Number == 20 ~ "Blossom Rock",
    Station_Number == 21 ~ "Bay Bridge",
    Station_Number == 22 ~ "Potrero Point",
    Station_Number == 23 ~ "Hunters Point",
    Station_Number == 24 ~ "Candlestick Point",
    Station_Number == 25 ~ "Oyster Point",
    Station_Number == 26 ~ "San Bruno Shoal",
    Station_Number == 27 ~ "San Francisco Airport",
    Station_Number == 28 ~ "North of San Mateo Bridge",
    Station_Number == 29 ~ "South of San Mateo Bridge",
    Station_Number == 29.5 ~ "Steinberger Slough",
    Station_Number == 30 ~ "Redwood Creek",
    Station_Number == 31 ~ "Coyote Hills",
    Station_Number == 32 ~ "Ravenswood Point",
    Station_Number == 33 ~ "Dumbarton Bridge",
    Station_Number == 34 ~ "Newark Slough",
    Station_Number == 35 ~ "Mowry Slough",
    Station_Number == 36 ~ "Calaveras Point",
    Station_Number == 1 ~ "Winters Island", #historical sites from here onwards
    Station_Number == 12.5 ~ "Pinole Point",
    Station_Number == 19 ~ "Golden Gate",
    Station_Number == 24.5 ~ "South Bay1",
    Station_Number == 25.5 ~ "South Bay2",
    Station_Number == 26.5 ~ "South Bay3",
    Station_Number == 27.5 ~ "South Bay4",
    Station_Number == 28.5 ~ "Geo Probe",
    Station_Number == 30.5 ~ "South Bay5",
    Station_Number == 31.5 ~ "South Bay6",
    Station_Number == 33.5 ~ "South Bay7",
    Station_Number == 405 ~ "Reserve Fleet 2",
    Station_Number == 407 ~ "Reserve Fleet 4",
    Station_Number == 650 ~ "Sacramento River",
    Station_Number == 651 ~ "Sacramento River",
    Station_Number == 652 ~ "Towlands Landing",
    Station_Number == 653 ~ "Mid-Decker Island",
    Station_Number == 654 ~ "Sacramento River",
    Station_Number == 655 ~ "North of Three Mile Slough",
    Station_Number == 659 ~ "Old Sacramento River",
    Station_Number == 662 ~ "Prospect Island",
    Station_Number == 411 ~ "Garnet Sill",
    
  ))

#joining lat and long based on locations
SFBay_1992_2014 <- full_join(SFBay_1992_2014, lat_long)
SFBay_2014_2021 <- full_join(SFBay_2014_2021, lat_long)

#save base datasets in csv before analyses
write_csv2(SFBay_1992_2014, file = "SFBay_1992_2014.csv")
write_csv2(SFBay_2014_2021, file = "SSFBay_2014_2021.csv")
save(SFBay_1992_2014, file = "SFBay_1992_2014.RData")
save(SFBay_2014_2021, file = "SFBay_2014_2021.RData")


SFB_All <- full_join(SFBay_1992_2014, SFBay_2014_2021)
save(SFB_All, file = "SFB_All.RData")
#------------------ANALYSES--------
#Biovolume to C Biomass in pg C cell -1
a <- 0.216                             #scaling constant for all spp according to Menden-Deuer and Lessard (2000)
a_diatoms <- 0.288                     #scaling constant for diatoms according to Menden-Deuer and Lessard (2000)  
b <- 0.939                             #same as a
b_diatoms <- 0.881                     #same as a_diatom

#Get cell carbon as estimated by cell volume, log10C = alog10V + logb
Cell_Carbon_All <- function(V) {
  a*log10(V)^b
  #excluding diatoms
}


Cell_Carbon_Diatom <- function (V){
  a_diatoms*log10(V)^b_diatoms
  #just diatoms
}

#to protect original data sets
SFBay1 <- SFBay_1992_2014
SFBay2 <- SFBay_2014_2021
SFBay2TotN <- SFBay_2014_2021_TotNut

#Calculation cell taxa specific carbon content as in a Cell (pg C cell-1)
SFBay1$Cell_Carbon <- if_else(SFBay1$Phyl_Class == "BACILLARIOPHYTA",
                                Cell_Carbon_Diatom(SFBay1$Cell_Vol),
                                Cell_Carbon_All(SFBay1$Cell_Vol))

SFBay2$Cell_Carbon <- if_else(SFBay2$Phyl_Class == "BACILLARIOPHYTA",
                              Cell_Carbon_Diatom(SFBay2$Cell_Vol),
                              Cell_Carbon_All(SFBay2$Cell_Vol))

SFBay2TotN$Cell_Carbon <- if_else(SFBay2TotN$Phyl_Class == "BACILLARIOPHYTA",
                              Cell_Carbon_Diatom(SFBay2TotN$Cell_Vol),
                              Cell_Carbon_All(SFBay2TotN$Cell_Vol))

#ESD calculation 
Vol_ESD <- function(V)(6*V/pi)^(1/3)    #cell volume to ESD in µm

SFBay1$ESD <- Vol_ESD(SFBay1$Cell_Vol) 
SFBay2$ESD <- Vol_ESD(SFBay2$Cell_Vol) 
SFBay2TotN$ESD <- Vol_ESD(SFBay2TotN$Cell_Vol) 


#log transform ESD
SFBay1 <- SFBay1%>% mutate(ESD = log(ESD))
SFBay2 <- SFBay2%>% mutate(ESD = log(ESD)) 
SFBay2TotN <- SFBay2TotN%>% mutate(ESD = log(ESD)) 


#plot hist of ESD SFBay1 and SFBay2
hist((unique(SFBay1$ESD)))
hist((unique(SFBay2$ESD)))



#From C per cell to C per sample (mL) in microgram (ug mL)
SFBay1<- SFBay1 %>%
  group_by(Station_Number, Year, Month, DOY, Depth)%>%
  mutate (Carbon_mL = (Cell_Carbon*(Density/sum(Density)))/1000000,
          Rel_Abu = (Density/sum(Density)), 
          Rel_Bio = Carbon_mL/sum(Carbon_mL))%>%
  filter(Rel_Abu > 0)

SFBay2<- SFBay2 %>%
  group_by(Station_Number, Year, Month, DOY, Depth)%>%
  mutate (Carbon_mL = (Cell_Carbon*(Density/sum(Density)))/1000000,
          Rel_Abu = (Density/sum(Density)), 
          Rel_Bio = Carbon_mL/sum(Carbon_mL))%>%
  filter(Rel_Abu > 0)

SFBay2TotN<- SFBay2TotN %>%
  group_by(Station_Number, Year, Month, DOY, Depth)%>%
  mutate (Carbon_mL = (Cell_Carbon*(Density/sum(Density)))/1000000,
          Rel_Abu = (Density/sum(Density)), 
          Rel_Bio = Carbon_mL/sum(Carbon_mL))%>%
  filter(Rel_Abu > 0)
#Carbon:Chl-----------
SFBay1 <- SFBay1%>% add_column(C_Chl = SFBay1$Cell_Carbon/SFBay1$Calculated_Chlorophyll)
SFBay2 <- SFBay2%>% add_column(C_Chl = SFBay2$Cell_Carbon/SFBay2$Calculated_Chlorophyll)

#--Species (as columns) table for diversity indices
#using just density/abundance
SFB1_Species <- SFBay1 %>%
  group_by(Station_Number, Year, Month, DOY, Depth, Tax_ID)%>%
  summarise(Density = sum(Density, na.rm = T))%>%
  drop_na(Tax_ID)%>%
  ungroup()%>%
  pivot_wider(id_cols = c(Station_Number, Year, Month, DOY, Depth), names_from = Tax_ID, values_from = Density)%>%
  select(!c(Station_Number, Year, Month, DOY, Depth))

SFB2_Species <- SFBay2 %>%
  group_by(Station_Number, Year, Month, DOY, Depth, Tax_ID)%>%
  summarise(Density = sum(Density, na.rm = T))%>%
  drop_na(Tax_ID)%>%
  ungroup()%>%
  pivot_wider(id_cols = c(Station_Number, Year, Month, DOY, Depth), names_from = Tax_ID, values_from = Density)%>%
  select(!c(Station_Number, Year, Month, DOY, Depth))

#using relative abundance
SFB1_Species_Abu <- SFBay1 %>%
  group_by(Station_Number, Year, Month, DOY, Depth, Tax_ID)%>%
  summarise(Rel_Abu = sum(Rel_Abu, na.rm = T))%>%
  drop_na(Tax_ID)%>%
  ungroup()%>%
  pivot_wider(id_cols = c(Station_Number, Year, Month, DOY, Depth), names_from = Tax_ID, values_from = Rel_Abu)%>%
  select(!c(Station_Number, Year, Month, DOY, Depth))

SFB2_Species_Abu <- SFBay2 %>%
  group_by(Station_Number, Year, Month, DOY, Depth, Tax_ID)%>%
  summarise(Rel_Abu = sum(Rel_Abu, na.rm = T))%>%
  drop_na(Tax_ID)%>%
  ungroup()%>%
  pivot_wider(id_cols = c(Station_Number, Year, Month, DOY, Depth), names_from = Tax_ID, values_from = Rel_Abu)%>%
  select(!c(Station_Number, Year, Month, DOY, Depth))

#using relative biomass
SFB1_Species_Bio <- SFBay1 %>%
  group_by(Station_Number, Year, Month, DOY, Depth, Tax_ID)%>%
  summarise(Rel_Bio= sum(Rel_Bio, na.rm = T))%>%
  drop_na(Tax_ID)%>%
  ungroup()%>%
  pivot_wider(id_cols = c(Station_Number, Year, Month, DOY, Depth), names_from = Tax_ID, values_from = Rel_Bio)%>%
  select(!c(Station_Number, Year, Month, DOY, Depth))

SFB2_Species_Bio <- SFBay2 %>%
  group_by(Station_Number, Year, Month, DOY, Depth, Tax_ID)%>%
  summarise(Rel_Bio = sum(Rel_Bio, na.rm = T))%>%
  drop_na(Tax_ID)%>%
  ungroup()%>%
  pivot_wider(id_cols = c(Station_Number, Year, Month, DOY, Depth), names_from = Tax_ID, values_from = Rel_Bio)%>%
  select(!c(Station_Number, Year, Month, DOY, Depth))


SFB1_Species[is.na(SFB1_Species)] <- 0   #vegan does not read NA values
SFB2_Species[is.na(SFB2_Species)] <- 0

SFB1_Species_Abu[is.na(SFB1_Species_Abu)] <- 0   #vegan does not read NA values
SFB2_Species_Abu[is.na(SFB2_Species_Abu)] <- 0

SFB1_Species_Bio[is.na(SFB1_Species_Bio)] <- 0   #vegan does not read NA values
SFB2_Species_Bio[is.na(SFB2_Species_Bio)] <- 0

#----CWM Size----------
#CWM calculation
SFBay1 <- SFBay1%>%
  group_by(Station_Number, Year, Month, DOY, Depth)%>%
  mutate(CWM_Size = sum(ESD*Rel_Abu), CWM_Sizeb = sum(ESD*Rel_Bio), Dataset = "SFB1")%>%
  ungroup()%>%
  group_by(Station_Number, Year, Month, DOY, Depth)%>%
  mutate(SizeVar_bio = sum((Carbon_mL/sum(Carbon_mL))*((ESD)-CWM_Sizeb)^2), 
         SizeVar_abu = sum((Density/sum(Density))*((ESD)-CWM_Size)^2))

SFBay2 <- SFBay2%>%
  group_by(Station_Number, Year, Month, DOY, Depth)%>%
  mutate(CWM_Size = sum(ESD*Rel_Abu), CWM_Sizeb = sum(ESD*Rel_Bio),Dataset = "SFB2")%>%
  ungroup()%>%
  group_by(Station_Number, Year, Month, DOY, Depth)%>%
  mutate(SizeVar_bio = sum((Carbon_mL/sum(Carbon_mL))*((ESD)-CWM_Sizeb)^2), 
         SizeVar_abu = sum((Density/sum(Density))*((ESD)-CWM_Size)^2),
         )


#joining CWM with the entire dataset for SFB1 and SFB2
SFBay <- mget(str_glue("SFBay{1:2}"))%>%
  reduce(full_join)

save(SFBay, file = "SFBay.RData")

#Defining a sample 
#Samples 1992-2014 SFB1
SFBay1_Env <- SFBay1%>%
  group_by(Station_Number, Year, Month, DOY, Depth)%>%
  summarise(Richness = n_distinct(Tax_ID), 
            Temperature = mean(Temperature, na.rm = TRUE),
            Salinity = mean(Salinity, na.rm = TRUE),
            NO3 = mean(Nitrate_._Nitrite, na.rm = TRUE),
            PO4 = mean(Phosphate, na.rm = TRUE),
            SiO4 = mean(Silicate, na.rm = TRUE),
            Chl = mean(Calculated_Chlorophyll, na.rm = TRUE),
            Ext_Coeff = mean(Extinction_Coefficient, na.rm = TRUE),
            SizeVar_bio = sum((Carbon_mL/sum(Carbon_mL))*((ESD)-CWM_Sizeb)^2), 
            SizeVar_abu = sum((Density/sum(Density))*((ESD)-CWM_Size)^2),
            Biomass = sum(Carbon_mL),
            Abundance = sum(Density),
            CWM_Size = unique(CWM_Size),
            CWM_Sizeb = unique(CWM_Sizeb))%>%
  subset(Richness >1)%>%
  add_column(expShannonD = exp(diversity(SFB1_Species, index = "shannon")),
             simpsonD = (diversity(SFB1_Species, index = "simpson")),
             invsimpsonD = (diversity(SFB1_Species, index = "invsimpson")),
             evennessD = (expShannonD/log(specnumber(SFB1_Species))),
             expShannonRA = exp(diversity(SFB1_Species_Abu, index = "shannon")),
             simpsonRA = (diversity(SFB1_Species_Abu, index = "simpson")),
             invsimpsonRA = (diversity(SFB1_Species_Abu, index = "invsimpson")),
             evennessRA = (expShannonRA/log(specnumber(SFB1_Species_Abu))),
             expShannonRB = exp(diversity(SFB1_Species_Bio, index = "shannon")),
             simpsonRB = (diversity(SFB1_Species_Bio, index = "simpson")),
             invsimpsonRB = (diversity(SFB1_Species_Bio, index = "invsimpson")),
             evennessRB = (expShannonRB/log(specnumber(SFB1_Species_Bio))),
             Dataset = "SFB1")


#Samples 2014-2021 SFB2
SFBay2_Env <- SFBay2%>%
  group_by(Station_Number, Year, Month, DOY, Depth)%>%
  summarise(Richness = n_distinct(Tax_ID), 
            Temperature = mean(Temperature, na.rm = TRUE),
            Salinity = mean(Salinity, na.rm = TRUE),
            NO3 = mean(Nitrate_._Nitrite, na.rm = TRUE),
            PO4 = mean(Phosphate, na.rm = TRUE),
            SiO4 = mean(Silicate, na.rm = TRUE),
            Chl = mean(Calculated_Chlorophyll, na.rm = TRUE),
            Ext_Coeff = mean(Extinction_Coefficient, na.rm = TRUE),
            SizeVar_bio = sum((Carbon_mL/sum(Carbon_mL))*((ESD)-CWM_Sizeb)^2), 
            SizeVar_abu = sum((Density/sum(Density))*((ESD)-CWM_Size)^2),
            Biomass = sum(Carbon_mL),
            Abundance = sum(Density),
            CWM_Size = unique(CWM_Size),
            CWM_Sizeb = unique(CWM_Sizeb))%>%
  subset(Richness >1)%>%
  add_column(expShannonD = exp(diversity(SFB2_Species, index = "shannon")),
             simpsonD = (diversity(SFB2_Species, index = "simpson")),
             invsimpsonD = (diversity(SFB2_Species, index = "invsimpson")),
             evennessD = (expShannonD/log(specnumber(SFB2_Species))),
             expShannonRA = exp(diversity(SFB2_Species_Abu, index = "shannon")),
             simpsonRA = (diversity(SFB2_Species_Abu, index = "simpson")),
             invsimpsonRA = (diversity(SFB2_Species_Abu, index = "invsimpson")),
             evennessRA = (expShannonRA/log(specnumber(SFB2_Species_Abu))),
             expShannonRB = exp(diversity(SFB2_Species_Bio, index = "shannon")),
             simpsonRB = (diversity(SFB2_Species_Bio, index = "simpson")),
             invsimpsonRB = (diversity(SFB2_Species_Bio, index = "invsimpson")),
             evennessRB = (expShannonRB/log(specnumber(SFB2_Species_Bio))),
             Dataset = "SFB2")


#joining two datasets
SFBay_Env <- full_join(SFBay1_Env, SFBay2_Env)

SFBay_Env <- SFBay_Env%>%                                    
  group_by(Station_Number, Year, Month, DOY, Depth)%>% 
  dplyr::mutate(Sample_ID = cur_group_id())   #creates a unique numerical identifier per selected variables

save(SFBay_Env, file = "SFBay_Env.Rdata")
save(SFBay1_Env, file = "SFBay1_Env.Rdata")
save(SFBay2_Env, file = "SFBay2_Env.Rdata")
#----Plots Environmental factors vc Richness --------
#Plot Richness against Chl 
library(rtist)

ChlRich <- SFBay_Env%>%
  ggplot(aes((Chl), Richness, colour = Dataset))+
  geom_point(size=2)+
  scale_color_manual(values = rtist_palette("hokusai", 4))+
  stat_smooth(aes(color = Dataset), method = "lm")+
  labs(title = "SF 1992/2021")+
  xlab(expression(paste("Chl (", mu, "g/L"^"-1",")")))+
  ylim(0,80)+scale_x_log10()+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "r", label.x.npc = "left", na.rm = T, size=5)+
  stat_smooth(color = "black", method = "lm", formula = y ~ poly(x,2))


#Plot Temp vs Rich
TempRich <- SFBay_Env%>%
  ggplot(aes((Temperature), Richness, colour = Dataset))+
  geom_point(size=2)+
  scale_color_manual(values = rtist_palette("hokusai", 4))+
  stat_smooth(aes(color = Dataset), method = "lm", formula = y ~ x)+
  labs(title = "SF 1992/2021")+
  xlab("Temperature (°C)")+
  ylim(0,80)+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "r", label.x.npc = "left", na.rm = T,size=5)+
  geom_smooth(method = "lm", color = "black", formula = y ~ x )

#Correlation and Regressions ------------ 
library(ggpubr)
library(ggfortify)

#scatterplots
#Chl ~ Size Variance
ChlSizeVarPlot <- SFBay_Env%>%
                  group_by(Dataset, Station_Number, Year, Month, DOY, Depth)%>%
                  ggplot(aes(Chl, (SizeVar_bio)))+
                  geom_point(aes(colour=Dataset))+
                  scale_color_manual(values = rtist_palette("hokusai", 4))+
                  geom_smooth(aes(colour=Dataset), method="glm")+
                  geom_smooth(color = "black", method = "glm")+
                  ylab(expression(paste("Size Variance (ln " , mu, "m"^"3",")"^"2",)))+
                  xlab("Chl")+scale_y_log10()+scale_x_log10()+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
                  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
                  stat_cor(aes(colour=Dataset),cor.coef.name = "r", label.x.npc = "left", na.rm = T,size=5)

ChlRichPlot <- SFBay_Env%>%
                    group_by(Dataset, Station_Number, Year, Month, DOY, Depth)%>%
                    ggplot(aes(Chl, Richness))+
                    geom_point(aes(colour=Dataset))+
                    scale_color_manual(values = rtist_palette("hokusai", 4))+
                    geom_smooth(aes(colour=Dataset), method = "lm")+
                    geom_smooth(color = "black", method = "lm")+
                    xlab(expression(paste("Chl (", mu,"g/L"^"-1", ")")))+
                    ylab("Richness")+scale_x_log10()+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
                    stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
                    stat_cor(aes(colour=Dataset),cor.coef.name = "r", label.x.npc = "left", na.rm = T,size=5)



#Temperature ~ Size Variance
TempSizeVarPlot <- SFBay_Env%>%
                        group_by(Dataset, Station_Number, Year, Month, DOY, Depth)%>%
                        ggplot(aes(Temperature, (SizeVar_bio)))+
                        geom_point(aes(colour=Dataset))+
                        scale_color_manual(values = rtist_palette("hokusai", 4))+
                        geom_smooth(aes(colour=Dataset))+
                        geom_smooth(color = "black")+
                        ylab(expression(paste("Size Variance (ln " , mu, "m"^"3",")"^"2",)))+
                        xlab("Temperature (°C)")+scale_y_log10()+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "r", label.x.npc = "left", na.rm = T,size=5)


#Temperature ~ CWM_Size
TempCWMSizeb <- SFBay_Env%>%
                    group_by(Dataset)%>%
                    ggplot(aes(Temperature, CWM_Sizeb))+
                    geom_point(aes(colour=Dataset))+
                    scale_color_manual(values = rtist_palette("hokusai", 4))+
                    geom_smooth(aes(colour=Dataset))+
                    geom_smooth(color = "black")+scale_y_log10()+
                    ylab(expression(paste("CWM Size (ln " , mu, "m"^"3",")")))+
                    xlab("Temperature  (°C)")+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "r", label.x.npc = "left", na.rm = T,size=5)

#Biomass ~ Size Variance
BioSizeVarb<- SFBay_Env%>%
                    group_by(Dataset)%>%
                    ggplot(aes(SizeVar_bio,Biomass))+
                    geom_point(aes(colour=Dataset))+
                    scale_color_manual(values = rtist_palette("hokusai", 4))+
                    geom_smooth(aes(colour=Dataset),method = "glm", formula = y ~ poly(x,2))+
                    geom_smooth(color = "black", method = "glm", formula = y ~ poly(x,2))+
                    scale_y_log10()+
                    ylab(expression(paste("Size Variance (ln " , mu, "m"^"3",")"^"2",)))+
                    xlab(expression(paste("log Biomass (", mu,"g/mL"^"-1", ")")))+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "r", label.x.npc = "left", na.rm = T,size=5)

#Biomass ~ CWM Size
BioCWMSizePlot<- SFBay_Env%>%
                      group_by(Dataset)%>%
                      ggplot(aes(Biomass, CWM_Sizeb))+
                      geom_point(aes(colour=Dataset))+
                      scale_color_manual(values = rtist_palette("hokusai", 4))+
                      geom_smooth(aes(colour=Dataset),method = lm, formula = y ~ poly(x,2))+
                      geom_smooth(color = "black", method = lm, formula = y ~ poly(x,2))+
                      scale_x_log10()+scale_y_log10()+
                      ylab(expression(paste("CWM Size (ln " , mu, "m"^"3",")",)))+
                      xlab(expression(paste("log Biomass (", mu,"g/mL"^"-1", ")")))+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "r", label.x.npc = "left", na.rm = T,size=5)

#Biomass ~ expShannon
BioexpShannonDPlot<- SFBay_Env%>%
                      group_by(Dataset)%>%
                      ggplot(aes(Biomass, expShannonD))+
                      geom_point(aes(colour=Dataset))+
                      scale_color_manual(values = rtist_palette("hokusai", 4))+
                      geom_smooth(aes(colour=Dataset),method = lm, formula = y ~ poly(x,2))+
                      geom_smooth(color = "black", method = lm, formula = y ~ poly(x,2))+
                      scale_x_log10()+ylab("exp H'")+xlab(expression(paste("log Biomass (", mu,"g/mL"^"-1", ")")))+
                      scale_y_log10()+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "r", label.x.npc = "left", na.rm = T,size=5)                           


BioexpShannonRBPlot<- SFBay_Env%>%
  group_by(Dataset)%>%
  ggplot(aes(Biomass, expShannonRB))+
  geom_point(aes(colour=Dataset))+
  scale_color_manual(values = rtist_palette("hokusai", 4))+
  geom_smooth(aes(colour=Dataset),method = lm, formula = y ~ poly(x,2))+
  geom_smooth(color = "black", method = lm, formula = y ~ poly(x,2))+
  scale_x_log10()+ylab("exp H' using Relative Biomass")+xlab(expression(paste("log Biomass (", mu,"g/mL"^"-1", ")")))+
  scale_y_log10()+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "r", label.x.npc = "left", na.rm = T,size=5)                           



#Biomass ~ Richness
BioRichPlot <- SFBay_Env%>%
                      group_by(Dataset)%>%
                      ggplot(aes(Richness, Biomass))+
                      geom_point(aes(colour=Dataset))+
                      scale_color_manual(values = rtist_palette("hokusai", 4))+
                      geom_smooth(aes(colour=Dataset),method = "glm", formula = y ~ poly(x,2))+
                      geom_smooth(color = "black", method = "glm", formula = y ~ poly(x,2))+
                      scale_y_log10()+ylab("Richness")+xlab(expression(paste("log Biomass (", mu,"g/mL"^"-1", ")")))+
                      theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T, size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "r", label.x.npc = "left", na.rm = T,size=5)



CWMBioRichPlot<- SFBay_Env%>%
                group_by(Dataset)%>%
                ggplot(aes(CWM_Sizeb, Richness))+
                geom_point(aes(colour=Dataset))+
                scale_color_manual(values = rtist_palette("hokusai", 4))+
                geom_smooth(aes(colour=Dataset), method = lm, formula = y ~ poly(x,2))+
                geom_smooth(color = "black", method = lm, formula = y ~ poly(x,2))+
                ylab("Richness")+ xlab(expression(paste("CWM Size (ln " , mu, "m"^"3",")",)))+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+scale_x_log10()+ylim(0,80)+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "r", label.x.npc = "left", na.rm = T,size=5)

SizeVarRich<- SFBay_Env%>%
  group_by(Dataset)%>%
  ggplot(aes(SizeVar_bio, Richness))+
  geom_point(aes(colour=Dataset))+
  scale_color_manual(values = rtist_palette("hokusai", 4))+
  geom_smooth(aes(colour=Dataset), method = lm, formula = y ~ poly(x,2))+
  geom_smooth(color = "black", method = lm, formula = y ~ poly(x,2))+
  scale_x_log10()+ylab("Richness")+ xlab(expression(paste("Size Variance (ln " , mu, "m"^"3",")"^"2",)))+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+ylim(0,80)+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "r", label.x.npc = "left", na.rm = T,size=5)


#Biomass ~ Simpson Index 
SFBay_Env%>%
  group_by(Dataset)%>%
  ggplot(aes(Biomass, expShannonRA))+
  geom_point(aes(colour=Dataset))+
  scale_color_manual(values = rtist_palette("hokusai", 4))+
  geom_smooth(aes(colour=Dataset),method = lm, formula = y ~ poly(x,2))+
  geom_smooth(color = "black", method = lm, formula = y ~ poly(x,2))+
  scale_x_log10()+ylab("Simpson Index")+xlab(expression(paste("log Biomass (", mu,"g/mL"^"-1", ")")))+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+ylim(0,1)+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "r", label.x.npc = "left", na.rm = T,size=5)


BioSimpRBPlot<- SFBay_Env%>%
  group_by(Dataset)%>%
  ggplot(aes(Biomass, simpsonRB))+
  geom_point(aes(colour=Dataset))+
  scale_color_manual(values = rtist_palette("hokusai", 4))+
  geom_smooth(aes(colour=Dataset),method = lm, formula = y ~ poly(x,2))+
  geom_smooth(color = "black", method = lm, formula = y ~ poly(x,2))+
  scale_x_log10()+ylab("Simpson Index using Relative Biomass")+xlab(expression(paste("log Biomass (", mu,"g/mL"^"-1", ")")))+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+ylim(0,1)+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "r", label.x.npc = "left", na.rm = T,size=5)

#exp Shannon ~ Size Variance
expShRBSizeVarPlot<- SFBay_Env%>%
  group_by(Dataset)%>%
  ggplot(aes(expShannonRB, SizeVar_bio))+
  geom_point(aes(colour=Dataset))+
  scale_color_manual(values = rtist_palette("hokusai", 4))+
  geom_smooth(aes(colour=Dataset))+
  geom_smooth(color = "black")+
  ylab(expression(paste("Size Variance (ln " , mu, "m"^"3",")"^"2",)))+xlab("exp H' using Relative Biomass")+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
  scale_y_log10()+scale_x_log10()+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "R", label.x.npc = "left", na.rm = T,size=5)

expShDSizeVarPlot<- SFBay_Env%>%
  group_by(Dataset)%>%
  ggplot(aes(expShannonD, SizeVar_bio))+
  geom_point(aes(colour=Dataset))+
  scale_color_manual(values = rtist_palette("hokusai", 4))+
  geom_smooth(aes(colour=Dataset))+
  geom_smooth(color = "black")+
  ylab(expression(paste("Size Variance (ln " , mu, "m"^"3",")"^"2",)))+xlab("exp H'")+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
  scale_y_log10()+scale_x_log10()+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "R", label.x.npc = "left", na.rm = T,size=5)

expShDCWMBioPlot<- SFBay_Env%>%
  group_by(Dataset)%>%
  ggplot(aes(expShannonD, CWM_Sizeb))+
  geom_point(aes(colour=Dataset))+
  scale_color_manual(values = rtist_palette("hokusai", 4))+
  geom_smooth(aes(colour=Dataset))+
  geom_smooth(color = "black")+
  ylab(expression(paste("CWM Size (ln " , mu, "m"^"3",")",)))+xlab("exp H'")+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
  scale_y_log10()+scale_x_log10()+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "R", label.x.npc = "left", na.rm = T,size=5)

expShRBCWMBioPlot<- SFBay_Env%>%
  group_by(Dataset)%>%
  ggplot(aes(expShannonRB, CWM_Sizeb))+
  geom_point(aes(colour=Dataset))+
  scale_color_manual(values = rtist_palette("hokusai", 4))+
  geom_smooth(aes(colour=Dataset))+
  geom_smooth(color = "black")+
  ylab(expression(paste("CWM Size (ln " , mu, "m"^"3",")",)))+xlab("exp H' using Relative Biomass")+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
  scale_y_log10()+scale_x_log10()+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "R", label.x.npc = "left", na.rm = T,size=5)

#Simpson ~ Size Variance
simpsonRBSizeVarPlot<- SFBay_Env%>%
  group_by(Dataset)%>%
  ggplot(aes(simpsonRB, SizeVar_bio))+
  geom_point(aes(colour=Dataset))+
  scale_color_manual(values = rtist_palette("hokusai", 4))+
  geom_smooth(aes(colour=Dataset))+
  geom_smooth(color = "black")+
  ylab(expression(paste("Size Variance (ln " , mu, "m"^"3",")"^"2",)))+xlab("Simpson Index using Relative Biomass")+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
  scale_y_log10()+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "R", label.x.npc = "left", na.rm = T,size=5)

simpsonDSizeVarPlot<- SFBay_Env%>%
  group_by(Dataset)%>%
  ggplot(aes(simpsonD, SizeVar_bio))+
  geom_point(aes(colour=Dataset))+
  scale_color_manual(values = rtist_palette("hokusai", 4))+
  geom_smooth(aes(colour=Dataset))+
  geom_smooth(color = "black")+
  ylab(expression(paste("Size Variance (ln " , mu, "m"^"3",")"^"2",)))+xlab("Simpson Index")+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
  scale_y_log10()+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "R", label.x.npc = "left", na.rm = T,size=5)

simpsonRBCWMBioPlot<- SFBay_Env%>%
  group_by(Dataset)%>%
  ggplot(aes(simpsonRB, CWM_Sizeb))+
  geom_point(aes(colour=Dataset))+
  scale_color_manual(values = rtist_palette("hokusai", 4))+
  geom_smooth(aes(colour=Dataset))+
  geom_smooth(color = "black")+
  ylab(expression(paste("CWM Size (ln " , mu, "m"^"3",")",)))+xlab("Simpson Index using Relative Biomass")+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
  scale_y_log10()+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "R", label.x.npc = "left", na.rm = T,size=5)

simpsonDCWMBioPlot<- SFBay_Env%>%
  group_by(Dataset)%>%
  ggplot(aes(simpsonD, CWM_Sizeb))+
  geom_point(aes(colour=Dataset))+
  scale_color_manual(values = rtist_palette("hokusai", 4))+
  geom_smooth(aes(colour=Dataset))+
  geom_smooth(color = "black")+
  ylab(expression(paste("CWM Size (ln " , mu, "m"^"3",")",)))+xlab("Simpson Index")+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
  scale_y_log10()+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "R", label.x.npc = "left", na.rm = T,size=5)

#Richness ~ Size Variance
RichSizeVarPlot<- SFBay_Env%>%
  group_by(Dataset)%>%
  ggplot(aes(Richness, SizeVar_bio))+
  geom_point(aes(colour=Dataset))+
  scale_color_manual(values = rtist_palette("hokusai", 4))+
  geom_smooth(aes(colour=Dataset), method = "glm")+
  geom_smooth(color = "black", method = "glm")+
  ylab(expression(paste("Size Variance (ln " , mu, "m"^"3",")"^"2",)))+xlab("Richness")+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
  scale_y_log10()+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "R", label.x.npc = "left", na.rm = T,size=5)


RichCWMBioPlot<- SFBay_Env%>%
  group_by(Dataset)%>%
  ggplot(aes(CWM_Sizeb, Richness))+
  geom_point(aes(colour=Dataset))+
  scale_color_manual(values = rtist_palette("hokusai", 4))+
  geom_smooth(aes(colour=Dataset), formula = y~x)+
  geom_smooth(color = "black")+
  xlab(expression(paste("CWM Size (ln " , mu, "m"^"3",")",)))+ylab("Richness")+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
  scale_x_log10()+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "R", label.x.npc = "left", na.rm = T,size=5)

#patchwork of plots 
(RichCWMBioPlot+RichSizeVarPlot)/(ChlRich+TempRich)/(ChlRich+ChlRich)
ggsave("RichnnessPanel.pdf", width = 18, height = 16)
ggsave("RichnnessPanel1.png", width = 18, height = 16)

(TempSizeVarPlot+BioSizeVarb)/(expShRBSizeVarPlot+expShDSizeVarPlot)/(simpsonRBSizeVarPlot+simpsonDSizeVarPlot)
ggsave("SizeVarPanel.pdf", width = 18, height = 16)
ggsave("SizeVarPanelglm.pdf", width = 18, height = 16)

(TempCWMSizeb+BioCWMSizePlot)/(simpsonRBCWMBioPlot+simpsonDCWMBioPlot)/(expShDCWMBioPlot+expShRBCWMBioPlot)
ggsave("CWMSizePanel.pdf", width = 18, height = 16)
ggsave("CWMSizePanel1.png", width = 18, height = 16)

(BioSizeVarb+BioRichPlot)/(BioexpShannonDPlot+BioexpShannonRBPlot)/(BioSimpDPlot+BioSimpRBPlot)
ggsave("BiomassPanel.pdf", width = 18, height = 16)
ggsave("BiomassPanel1.png", width = 18, height = 16)

(ChlRich+ChlSizeVarPlot)/(ChlRich+TempRich)/(ChlRich+ChlRich)
ggsave("ChlPanel.pdf", width = 18, height = 16)

(TempCWMSizeb+BioCWMSizePlot)/(expShRBSizeVarPlot+RichSizeVarPlot)/(BioexpShannonDPlot+BioexpShannonRBPlot)/(BioSizeVarb+ChlSizeVarPlot)
ggsave("FigureReport1.pdf", width = 22, height = 20)

#basic numbers in the data set
#phytoplankton amounts 
summary(SFBay1$Density) #median 7.6, mean 1332.5
summary(SFBay2$Density) #median 21.4, mean 4893.7
summary(SFBay1$Calculated_Chlorophyll) #median 7.6, mean 12.1
summary(SFBay2$Calculated_Chlorophyll) #median 3.8, mean 5.19
summary(SFBay1$Salinity) #median
summary(SFBay2$Salinity)

datasummary_skim(SFBay1$Temperature)


SFBay_Env%>%
  group_by(Dataset)%>%
  ggplot(aes(CWM_Sizeb, Chl))+
  geom_point(aes(colour=Dataset))+
  scale_color_manual(values = rtist_palette("hokusai", 4))+
  geom_smooth(aes(colour=Dataset),method = lm)+
  scale_y_log10()+xlab("NO3")+ylab(expression(paste("log Biomass (", mu,"g/mL"^"-1", ")")))+
  theme(panel.grid  = element_blank(), text = element_text(size=20), legend.position = "none")+
  stat_cor(aes(colour=Dataset), method= "spearman", cor.coef.name = "rho", label.x.npc = "middle", na.rm = T,size=5)+
  stat_cor(aes(colour=Dataset),cor.coef.name = "r", label.x.npc = "left", na.rm = T,size=5)


SFBay%>%
  group_by(Station_Number, Depth, Year, Month, DOY)%>%
  filter(!is.na(Phyl_Class))%>%
  ggplot(aes(Year, y=Rel_Abu, fill=Phyl_Class))+
  geom_bar(stat="identity", position = "fill")+
  scale_fill_manual(values = met.brewer("Renoir"))+
  ylab("Relative Contribution per Sample")+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45))+
  labs(fill="Class")
  
  


  