#libraries
library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(tidyselect)
library(writexl)
library(lubridate)
library(readr)
library(ggpubr)
theme_set(theme_light())



#Analyses
SF_data1 <- as.matrix(SF_data)
SF_data3 <- as_tibble(SF_data)


attach(SF_data3)

#Biovolume to C Biomass in pg C cell -1
a <- 0.216                             #scaling constant for all spp according to Menden-Deuer and Lessard (2000)
a_diatoms <- 0.288                     #scaling constant for diatoms according to Menden-Deuer and Lessard (2000)  
a1 <- 0.109                            #scaling constant for all spp (including diatoms) by Montagnes et al (1994)
b <- 0.939                             #same as a
b_diatoms <- 0.881                     #same as b
b1 <- 0.991                            #same as a1

#Equation to get cell carbon as estimated by cell volume 
Cell_carbon1 <- function (V)(b*log(V))+a 
Cell_carbon2 <- function (V)a_diatoms*V^b_diatoms #Unit: pgC/cell
Cell_carbon3 <- function (V)(b1*log(V))+a1

#Calculation cell carbon content all groups together as in Cell_carbon3
#SF_data3$Cell_Carbon1 <- Cell_carbon3(SF_data3$Biovolume)


#Calculation cell taxa specific carbon content as in Cell_carbon1 and 2
SF_data3$Cell_Carbon <- if_else(SF_data3$Phyl_Class == "BACILLARIOPHYTA",
                                Cell_carbon2(SF_data3$Biovolume),
                                Cell_carbon1(SF_data3$Biovolume))
#ESD calculation -----------------------------------------
SF_data3$ESD <- (6*Cell_Vol/pi)^(1/3)    #cell volume to ESD in µm

hist((unique(ESD))) 
hist(log10((unique(ESD)))) 

#Defining a sample----------------------------------------
SF_data3 <- SF_data %>%                                    
  group_by(Date, Depth, Location) %>%
  mutate(Sample_ID = cur_group_id())    #creates a unique numerical identifier per selected variables

SF_Samples <- SF_data3%>%
  group_by(Sample_ID)%>%
  summarise(Date = Date,
            Sample_ID = Sample_ID,
            Location = Location,
            Depth = Depth,
            Richness = length(unique(Tax_ID)),
            Tax_ID = Tax_ID,
            Phyl_Class = Phyl_Class,
            ESD = ESD,
            Temperature = Temperature,
            Salinity = Salinity,
            Nitrate_Nitrite = Nitrate_Nitrite,
            Phosphate = Phosphate,
            Silicate = Silicate, 
            D_Chl = D_Chl,
            C_Chl = C_Chl,
            Biovolume = Biovolume,
            C_content = Cell_Carbon,
            Density = Density,
            Year = Year,
            Month = Month, 
            Day = DOY)
  
SF_Samples <- SF_Samples%>%
  group_by(Sample_ID)%>%
  subset(Richness > 1)                # selects samples with Richness > 1

# Environmental parameters ----------------------------------------------
#Calculate parameters of interest per Year
SF_Year <- SF_Samples%>%
  group_by(Sample_ID)%>%
  group_by(Year)%>%
  summarise(AvgTemp = mean(Temperature, na.rm = TRUE),
            Tot_DChl = sum(D_Chl), Mean_DChl = mean(D_Chl),
            Tot_CChl = sum(C_Chl), Mean_CChl = mean(C_Chl),
            AvgSal = mean(Salinity, na.rm = TRUE),
            AvgNitr = mean(Nitrate_Nitrite, na.rm = TRUE),         
            TotNitr = sum(Nitrate_Nitrite, na.rm = TRUE),
            AvgSil = mean(Silicate, na.rm = TRUE),
            TotSil = sum(Silicate, na.rm = TRUE),
            AvgPhos = mean(Phosphate, na.rm = TRUE),
            TotPhos = sum(Phosphate, na.rm = TRUE),
            AvgCell_C = mean(C_content),
            TotCell_C = sum(C_content),
            AvgBiovolume = mean(Biovolume),
            TotBiovolume = sum(Biovolume),
            AvgDensity = mean(Density),
            TotDensity = sum(Density),
            Richness = length(unique(Tax_ID)),
            AvgChl_C = Mean_DChl/AvgCell_C,
            Chl_C = Tot_DChl/TotCell_C)

#Calculate parameters of interest per Month
SF_Month <- SF_Samples%>%
  group_by(Sample_ID)%>%
  group_by(Month)%>%
  summarise(AvgTemp = mean(Temperature, na.rm = TRUE),
            Tot_DChl = sum(D_Chl), Mean_DChl = mean(D_Chl),
            Tot_CChl = sum(C_Chl), Mean_CChl = mean(C_Chl),
            AvgSal = mean(Salinity, na.rm = TRUE),
            AvgNitr = mean(Nitrate_Nitrite, na.rm = TRUE),          
            TotNitr = sum(Nitrate_Nitrite, na.rm = TRUE),
            AvgSil = mean(Silicate, na.rm = TRUE),
            TotSil = sum(Silicate, na.rm = TRUE),
            AvgPhos = mean(Phosphate, na.rm = TRUE),
            TotPhos = sum(Phosphate, na.rm = TRUE),
            AvgCell_C = mean(C_content),
            TotCell_C = sum(C_content),
            AvgBiovolume = mean(Biovolume),
            TotBiovolume = sum(Biovolume),
            AvgDensity = mean(Density),
            TotDensity = sum(Density),
            Richness = length(unique(Tax_ID)),
            AvgChl_C = Mean_DChl/AvgCell_C,
            Chl_C = Tot_DChl/TotCell_C)

#Calculate parameters of interest per Sample
SF_perSample <- SF_Samples%>%
  group_by(Sample_ID)%>%
  summarise(AvgTemp = mean(Temperature, na.rm = TRUE),
            Tot_DChl = sum(D_Chl), Mean_DChl = mean(D_Chl),
            Tot_CChl = sum(C_Chl), Mean_CChl = mean(C_Chl),
            AvgSal = mean(Salinity, na.rm = TRUE),
            AvgNitr = mean(Nitrate_Nitrite, na.rm = TRUE),          
            TotNitr = sum(Nitrate_Nitrite, na.rm = TRUE),
            AvgSil = mean(Silicate, na.rm = TRUE),
            TotSil = sum(Silicate, na.rm = TRUE),
            AvgPhos = mean(Phosphate, na.rm = TRUE),
            TotPhos = sum(Phosphate, na.rm = TRUE),
            AvgCell_C = mean(C_content),
            TotCell_C = sum(C_content),
            AvgBiovolume = mean(Biovolume),
            TotBiovolume = sum(Biovolume),
            AvgDensity = mean(Density),
            TotDensity = sum(Density),
            Richness = length(unique(Tax_ID)),
            AvgChl_C = Mean_DChl/AvgCell_C,
            Chl_C = Tot_DChl/TotCell_C)

#plots
SF_perSample%>%
  ggplot(aes(log(TotCell_C), log(Tot_DChl)))+
  geom_point(aes(colour = Sample_ID))+
  geom_smooth(method = "lm")


SF_perSample%>%
  ggplot(aes(Richness, log(TotCell_C)))+
  geom_point(aes(colour = Sample_ID))+
  geom_smooth(method="loess")

SF_perSample%>%
  ggplot(aes(log(TotBiovolume), log(TotDensity)))+
  geom_point()+
  geom_smooth(method="lm")

#Size measurements-------------





