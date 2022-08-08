#San Francisco Bay 1992-2021 data set for analyses in chapter 2

library(tidyverse)
library(ggpubr)
library(rtist)
library(ggfortify)
library(broom)
library(rstatix)
library(modelsummary)
library(skimr)
library(jtools)
library(mgcv)
library(ggstance)
library(tidyr)
library(huxtable)
library(purrr)
library(stats)
library(knitr)
library(magick)


rm(list=ls())
load("SFBay1_Env.Rdata")
load("SFBay2_Env.Rdata")
theme_set(theme_light())


#Regressions of variables with biomass and chl as response--------
#BIOMASS
#M1 = y ~ x 
SFB1_M1 <- SFBay1_Env%>%
  ungroup()%>%
  select(-Biomass, -Dataset, -Month, -Year, -DOY, -Station_Number, -Depth)%>%
  map(~lm(log(SFBay1_Env$Biomass) ~ .x, data= SFBay1_Env)) 

modelsummary(SFB1_M1,  statistic = "p = {p.value}", fmt = 2, stars=T) #table with model summary

SFB2_M1 <- SFBay2_Env%>%
  ungroup()%>%
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth)%>%
  map(~lm(log(SFBay2_Env$Biomass) ~ .x, data=SFBay2_Env)) 

modelsummary(SFB2_M1,  statistic = "p = {p.value}", fmt = 2, stars=T) #table with model summary


#M2 = y ~ log(x)
SFB1_M2 <- SFBay1_Env%>%
  ungroup()%>%
  select(-Biomass, -Dataset, -Month, -Year, -DOY, -Station_Number, -Depth)%>%
  map(~lm(log(SFBay1_Env$Biomass) ~ log(.x), data=SFBay1_Env))

modelsummary(SFB1_M2,  statistic = "p = {p.value}", fmt = 2, stars=T) #table with model summary

SFB2_M2 <- SFBay2_Env%>%
  ungroup()%>%
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth)%>%
  map(~lm(log(SFBay2_Env$Biomass) ~ log(.x), data=SFBay2_Env)) 

modelsummary(SFB2_M2,  statistic = "p = {p.value}", fmt = 2, stars=T) #table with model summary

#M3 = y ~ x + x^2
SFB1_M3 <- SFBay1_Env%>%
  ungroup()%>%
  select(-Biomass, -Dataset, -Month, -Year, -DOY, -Station_Number, -Depth)%>%
  map(~lm(log(SFBay1_Env$Biomass) ~ .x + I(.x^2), data=SFBay1_Env)) 

modelsummary(SFB1_M3,  statistic = "p = {p.value}", fmt = 2, stars=T) #table with model summary

SFB2_M3 <- SFBay2_Env%>%
  ungroup()%>%
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth)%>%
  map(~lm(log(SFBay2_Env$Biomass) ~ .x + I(.x^2), data=SFBay2_Env))

modelsummary(SFB2_M3,  statistic = "p = {p.value}", fmt = 2, stars=T) #table with model summary

#M3 = y ~ log(x) + log(x^2)
SFB1_M4 <- SFBay1_Env%>%
  ungroup()%>%
  select(-Biomass, -Dataset, -Month, -Year, -DOY, -Station_Number, -Depth)%>%
  map(~lm(log(SFBay1_Env$Biomass) ~ log(.x) + I(log(.x)^2), data=SFBay1_Env)) 

modelsummary(SFB1_M4,  statistic = "p = {p.value}", fmt = 2, stars=T) #table with model summary

SFB2_M4 <- SFBay2_Env%>%
  ungroup()%>%
  select(-Biomass, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth)%>%
  map(~lm(log(SFBay2_Env$Biomass) ~ log(.x) + I(log(.x)^2), data=SFBay2_Env)) 

modelsummary(SFB2_M4,  statistic = "p = {p.value}", fmt = 2, stars=T) #table with model summary

#CHLOROPHYLL A
#M5 = y ~ x 
SFB1_M5 <- SFBay1_Env%>%
  ungroup()%>%
  select(-Chl, -Dataset, -Month, -Year, -DOY, -Station_Number, -Depth)%>%
  map(~lm(log(SFBay1_Env$Chl) ~ .x, data= SFBay1_Env)) 

modelsummary(SFB1_M5,  statistic = "p = {p.value}", fmt = 2, stars=T) #table with model summary


SFB2_M5 <- SFBay2_Env%>%
  ungroup()%>%
  select(-Chl, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth)%>%
  map(~lm(log(SFBay2_Env$Chl) ~ .x, data=SFBay2_Env)) 

modelsummary(SFB2_M5,  statistic = "p = {p.value}", fmt = 2, stars=T) #table with model summary


#M6 = y ~ log(x) 
SFB1_M6 <- SFBay1_Env%>%
  ungroup()%>%
  select(-Chl, -Dataset, -Month, -Year, -DOY, -Station_Number, -Depth)%>%
  map(~lm(log(SFBay1_Env$Chl) ~ log(.x), data= SFBay1_Env)) 

modelsummary(SFB1_M6,  statistic = "p = {p.value}", fmt = 2, stars=T) #table with model summary


SFB2_M6 <- SFBay2_Env%>%
  ungroup()%>%
  select(-Chl, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth)%>%
  map(~lm(log(SFBay2_Env$Chl) ~ log(.x), data=SFBay2_Env)) 

modelsummary(SFB2_M6,  statistic = "p = {p.value}", fmt = 2, stars=T) #table with model summary

#M7 = y ~ x + x^2
SFB1_M7 <- SFBay1_Env%>%
  ungroup()%>%
  select(-Chl, -Dataset, -Month, -Year, -DOY, -Station_Number, -Depth)%>%
  map(~lm(log(SFBay1_Env$Chl) ~ .x + I(.x^2), data= SFBay1_Env)) 

modelsummary(SFB1_M7,  statistic = "p = {p.value}", fmt = 2, stars=T) #table with model summary

SFB2_M7 <- SFBay2_Env%>%
  ungroup()%>%
  select(-Chl, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth)%>%
  map(~lm(log(SFBay2_Env$Chl) ~ .x + I(.x^2), data=SFBay2_Env))

modelsummary(SFB2_M7,  statistic = "p = {p.value}", fmt = 2, stars=T) #table with model summary

#M8 = y ~ log(x) + log(x^2)
SFB1_M8 <- SFBay1_Env%>%
  ungroup()%>%
  select(-Chl, -Dataset, -Month, -Year, -DOY, -Station_Number, -Depth)%>%
  map(~lm(log(SFBay1_Env$Chl) ~ log(.x) + I(log(.x)^2), data= SFBay1_Env)) 

modelsummary(SFB2_M3,  statistic = "p = {p.value}", fmt = 2, stars=T) #table with model summary


SFB2_M8 <- SFBay2_Env%>%
  ungroup()%>%
  select(-Chl, -Dataset, -Month,-Year,-DOY,-Station_Number,-Depth)%>%
  map(~lm(log(SFBay2_Env$Chl) ~ log(.x) + I(log(.x)^2), data=SFBay2_Env))

modelsummary(SFB2_M8,  statistic = "p = {p.value}", fmt = 2, stars=T) #table with model summary


#---Tables with selected models------
SFB1_Biomass_Models <- list(
   "Temperature" = SFB1_M4$Temperature, "Salinity" = SFB1_M3$Salinity, "PO4" = SFB1_M4$PO4, 
   "NO3" = SFB1_M4$NO3, "SiO4" = SFB1_M3$SiO4, "Abundance" = SFB1_M4$Abundance, "Richness" = SFB1_M2$Richness,
   "exp H' RA" = SFB1_M4$expShannonRA, "exp H' RB" = SFB1_M4$expShannonRB, "Simpson RA" = SFB1_M1$simpsonRA,
   "Simpson RB" = SFB1_M3$simpsonRB, "Inv Simpson RA" = SFB1_M4$invsimpsonRA, "Inv Simpson RB" = SFB1_M4$invsimpsonRB,
   "Evenness RA" = SFB1_M2$evennessRA, "Evenness RB" = SFB1_M4$evennessRB, "CWM Size" = SFB1_M1$CWM_Size, 
   "CWM Size b" = SFB1_M4$CWM_Sizeb, "Chl" = SFB1_M4$Chl, "Ext Coeff" = SFB1_M2$Ext_Coeff,
   "Size Var RA" = SFB1_M3$SizeVar_abu, "Size Var RB" = SFB1_M4$SizeVar_bio
)

SFB1_Chl_Models <- list(
  "Temperature" = SFB1_M8$Temperature, "Salinity" = SFB1_M7$Salinity, "PO4" = SFB1_M7$PO4, 
  "NO3" = SFB1_M7$NO3, "SiO4" = SFB1_M7$SiO4, "Abundance" = SFB1_M8$Abundance, "Richness" = SFB1_M7$Richness,
  "exp H' RA" = SFB1_M7$expShannonRA, "exp H' RB" = SFB1_M8$expShannonRB, "Simpson RA" = SFB1_M5$simpsonRA,
  "Simpson RB" = SFB1_M7$simpsonRB, "Inv Simpson RA" = SFB1_M7$invsimpsonRA, "Inv Simpson RB" = SFB1_M8$invsimpsonRB,
  "Evenness RA" = SFB1_M8$evennessRA, "Evenness RB" = SFB1_M8$evennessRB, "CWM Size" = SFB1_M5$CWM_Size, 
  "CWM Size b" = SFB1_M5$CWM_Sizeb, "Biomass" = SFB1_M8$Biomass, "Ext Coeff" = SFB1_M5$Ext_Coeff,
  "Size Var RA" = SFB1_M7$SizeVar_abu, "Size Var RB" = SFB1_M7$SizeVar_bio
)

SFB2_Biomass_Models <- list(
  "Temperature" = SFB2_M4$Temperature, "Salinity" = SFB2_M3$Salinity, "PO4" = SFB2_M4$PO4,
  "NO3" = SFB2_M3$NO3, "SiO4" = SFB2_M3$SiO4, "Abundance" = SFB2_M4$Abundance, "Richness" = SFB2_M4$Richness,
  "exp H' RA" = SFB2_M4$expShannonRA, "exp H' RB" = SFB2_M4$expShannonRB, "Simpson RA" = SFB2_M3$simpsonRA, 
  "Simpson RB" = SFB2_M4$expShannonRB, "Inv Simpson RA" = SFB2_M4$invsimpsonRA, "Inv Simpson RB" = SFB2_M4$invsimpsonRB,
  "Evenness RA" = SFB2_M4$evennessRA, "Evenness RB" = SFB2_M4$evennessRB, "CWM Size" = SFB2_M3$CWM_Size, 
  "CWM Size b" = SFB2_M1$CWM_Sizeb, "Chl" = SFB2_M4$Chl, "Ext Coeff" = SFB2_M2$Ext_Coeff,
  "Size Var RA" = SFB2_M3$SizeVar_abu, "Size Var RB" = SFB2_M3$SizeVar_bio
  )

SFB2_Chl_Models <- list(
  "Temperature" = SFB2_M7$Temperature, "Salinity" = SFB2_M7$Salinity, "PO4" = SFB2_M7$PO4,
  "NO3" = SFB2_M8$NO3, "SiO4" = SFB2_M7$SiO4, "Abundance" = SFB2_M8$Abundance, "Richness" = SFB2_M8$Richness,
  "exp H' RA" = SFB2_M8$expShannonRA, "exp H' RB" = SFB2_M8$expShannonRB, "Simpson RA" = SFB2_M7$simpsonRA, 
  "Simpson RB" = SFB2_M5$expShannonRB, "Inv Simpson RA" = SFB2_M8$invsimpsonRA, "Inv Simpson RB" = SFB2_M8$invsimpsonRB,
  "Evenness RA" = SFB2_M8$evennessRA, "Evenness RB" = SFB2_M8$evennessRB, "CWM Size" = SFB2_M7$CWM_Size, 
  "CWM Size b" = SFB2_M5$CWM_Sizeb, "Biomass" = SFB2_M8$Biomass, "Ext Coeff" = SFB2_M5$Ext_Coeff,
  "Size Var RA" = SFB2_M7$SizeVar_abu, "Size Var RB" = SFB2_M7$SizeVar_bio
)

#SFB1 Biomass and Chl models
modelsummary(SFB1_Biomass_Models, statistic = "p = {p.value}", fmt = 2, stars=T) #table in Viewer
modelsummary(SFB1_Chl_Models, statistic = "p = {p.value}", fmt = 2, stars=T)
#SFB2 Biomass and Chl models
modelsummary(SFB2_Biomass_Models, statistic = "p = {p.value}", fmt = 2, stars=T)
modelsummary(SFB2_Chl_Models, statistic = "p = {p.value}", fmt = 2, stars=T)

#Model plots---------- 
#SFB1 function to plot models for Biomass and Chl
SFB1plot_fitBio <- function(x, labelx) {
  SFBay1_Env%>%
    ggplot(aes(x=x, y= log(Biomass)))+
    ylab("Biomass")+xlab(labelx)+
    geom_point()+
    geom_smooth(method='lm', formula = y ~ log(x) + I(log(x)^2))+
    stat_regline_equation(formula =  y ~ log(x) + I(log(x)^2), 
                          aes(label = paste(..adj.rr.label..,..AIC.label..,sep = "~~~")),color = "blue", label.y = -5.5)+
    geom_smooth(method='lm', formula = y ~ x + I(x^2), color="green")+
    stat_regline_equation(formula =  y ~ x + I(x^2), 
                          aes(label = paste(..adj.rr.label..,..AIC.label..,sep = "~~~")), label.y = -6.5,color = "green")+
    geom_smooth(method='lm', formula = y ~ log(x) , color="orange")+
    stat_regline_equation(formula =  y ~ log(x), 
                          aes(label = paste(..adj.rr.label..,..AIC.label..,sep = "~~~")), label.y = -7.5,color = "orange")+
    geom_smooth(method='lm', formula = y ~ x , color="red")+
    stat_regline_equation(formula =  y ~ x, 
                          aes(label = paste(..adj.rr.label..,..AIC.label..,sep = "~~~")), label.y = -8.5,color = "red")
}

SFB1plot_fitChl <- function(x, labelx) {
  SFBay1_Env%>%
    ggplot(aes(x=x, y= log(Chl)))+
    ylab("Chl")+xlab(labelx)+
    geom_point()+
    geom_smooth(method='lm', formula = y ~ log(x) + I(log(x)^2))+
    stat_regline_equation(formula =  y ~ log(x) + I(log(x)^2), 
                          aes(label = paste(..adj.rr.label..,..AIC.label..,sep = "~~~")),color = "blue", label.y = -5.5)+
    geom_smooth(method='lm', formula = y ~ x + I(x^2), color="green")+
    stat_regline_equation(formula =  y ~ x + I(x^2), 
                          aes(label = paste(..adj.rr.label..,..AIC.label..,sep = "~~~")), label.y = -6.5,color = "green")+
    geom_smooth(method='lm', formula = y ~ log(x) , color="orange")+
    stat_regline_equation(formula =  y ~ log(x), 
                          aes(label = paste(..adj.rr.label..,..AIC.label..,sep = "~~~")), label.y = -7.5,color = "orange")+
    geom_smooth(method='lm', formula = y ~ x , color="red")+
    stat_regline_equation(formula =  y ~ x, 
                          aes(label = paste(..adj.rr.label..,..AIC.label..,sep = "~~~")), label.y = -8.5,color = "red")
}

#SFB2 function to plot models for Biomass and Chl
SFB2plot_fitBio <- function(x, labelx) {
  SFBay2_Env%>%
    ggplot(aes(x=x, y= log(Biomass)))+
    ylab("Biomass")+xlab(labelx)+
    geom_point()+
    geom_smooth(method='lm', formula = y ~ log(x) + I(log(x)^2))+
    stat_regline_equation(formula =  y ~ log(x) + I(log(x)^2), 
                          aes(label = paste(..adj.rr.label..,..AIC.label..,sep = "~~~")),color = "blue", label.y = -5.5)+
    geom_smooth(method='lm', formula = y ~ x + I(x^2), color="green")+
    stat_regline_equation(formula =  y ~ x + I(x^2), 
                          aes(label = paste(..adj.rr.label..,..AIC.label..,sep = "~~~")), label.y = -6.5,color = "green")+
    geom_smooth(method='lm', formula = y ~ log(x) , color="orange")+
    stat_regline_equation(formula =  y ~ log(x), 
                          aes(label = paste(..adj.rr.label..,..AIC.label..,sep = "~~~")), label.y = -7.5,color = "orange")+
    geom_smooth(method='lm', formula = y ~ x , color="red")+
    stat_regline_equation(formula =  y ~ x, 
                          aes(label = paste(..adj.rr.label..,..AIC.label..,sep = "~~~")), label.y = -8.5,color = "red")
}

SFB2plot_fitChl <- function(x, labelx) {
  SFBay2_Env%>%
    ggplot(aes(x=x, y= log(Chl)))+
    ylab("Chl")+xlab(labelx)+
    geom_point()+
    geom_smooth(method='lm', formula = y ~ log(x) + I(log(x)^2))+
    stat_regline_equation(formula =  y ~ log(x) + I(log(x)^2), 
                          aes(label = paste(..adj.rr.label..,..AIC.label..,sep = "~~~")),color = "blue", label.y = -5.5)+
    geom_smooth(method='lm', formula = y ~ x + I(x^2), color="green")+
    stat_regline_equation(formula =  y ~ x + I(x^2), 
                          aes(label = paste(..adj.rr.label..,..AIC.label..,sep = "~~~")), label.y = -6.5,color = "green")+
    geom_smooth(method='lm', formula = y ~ log(x) , color="orange")+
    stat_regline_equation(formula =  y ~ log(x), 
                          aes(label = paste(..adj.rr.label..,..AIC.label..,sep = "~~~")), label.y = -7.5,color = "orange")+
    geom_smooth(method='lm', formula = y ~ x , color="red")+
    stat_regline_equation(formula =  y ~ x, 
                          aes(label = paste(..adj.rr.label..,..AIC.label..,sep = "~~~")), label.y = -8.5,color = "red")
}

#Model plots SFB1 Biomass
#red fit = y ~ x, orange fit = y ~ log(x), green fit = y ~ x + x^2, blue fit = y ~ log(x) + log(x)^2
#the color fit indication after the plot represents the best performing model 
SFB1plot_fitBio(SFBay1_Env$Temperature, labelx = "Temperature") #Best Model: blue fit
SFB1plot_fitBio(SFBay1_Env$Salinity, labelx = "Salinity") #green fit
SFB1plot_fitBio(SFBay1_Env$PO4, labelx= "PO4") #blue fit
SFB1plot_fitBio(SFBay1_Env$NO3, labelx="NO3") #blue fit
SFB1plot_fitBio(SFBay1_Env$SiO4, labelx="SiO4") #green fit
SFB1plot_fitBio(SFBay1_Env$Richness, labelx="Richness") #orange fit
SFB1plot_fitBio(log(SFBay1_Env$Abundance), labelx="Abundance") #blue fit
SFB1plot_fitBio((SFBay1_Env$Chl), labelx="Chl") #blue fit
SFB1plot_fitBio(SFBay1_Env$SizeVar_bio, labelx="Size Variance b") #blue fit
SFB1plot_fitBio(SFBay1_Env$SizeVar_abu, labelx="Size Variance") #green fit
SFB1plot_fitBio(SFBay1_Env$Ext_Coeff, labelx="Extinction Coefficient") #orange fit
SFB1plot_fitBio(SFBay1_Env$CWM_Size, labelx="CWM Size") #red fit
SFB1plot_fitBio(SFBay1_Env$CWM_Sizeb, labelx="CWM Size b") #blue fit
SFB1plot_fitBio(SFBay1_Env$expShannonRA, labelx="exp H' RA") #blue fit
SFB1plot_fitBio(SFBay1_Env$expShannonRB, labelx="exp H' RB") #blue fit
SFB1plot_fitBio(SFBay1_Env$simpsonRA, labelx="Simpson RA") #red fit
SFB1plot_fitBio(SFBay1_Env$simpsonRB, labelx="Simpson RB") #green fit
SFB1plot_fitBio(SFBay1_Env$invsimpsonRA, labelx="Inv Simpson RA") #blue fit
SFB1plot_fitBio(SFBay1_Env$invsimpsonRB, labelx="Inv Simpson RB") #blue fit
SFB1plot_fitBio(SFBay1_Env$evennessRA, labelx="Evenness RA") #orange fit
SFB1plot_fitBio(SFBay1_Env$evennessRB, labelx="Evenness RB") #blue fit

#Model plots SFB1 Chl
SFB1plot_fitChl(SFBay1_Env$Temperature, labelx = "Temperature") #blue fit
SFB1plot_fitChl(SFBay1_Env$Salinity, labelx = "Salinity") #green fit
SFB1plot_fitChl(SFBay1_Env$PO4, labelx= "PO4") #green fit
SFB1plot_fitChl(SFBay1_Env$NO3, labelx="NO3") #green fit
SFB1plot_fitChl(SFBay1_Env$SiO4, labelx="SiO4") #green fit
SFB1plot_fitChl(SFBay1_Env$Richness, labelx="Richness") #green fit
SFB1plot_fitChl(log(SFBay1_Env$Abundance), labelx="Abundance") #blue fit
SFB1plot_fitChl(SFBay1_Env$Biomass, labelx="Biomass") #blue fit
SFB1plot_fitChl(SFBay1_Env$SizeVar_bio, labelx="Size Variance b") #green fit
SFB1plot_fitChl(SFBay1_Env$SizeVar_abu, labelx="Size Variance") #green fit
SFB1plot_fitChl(SFBay1_Env$Ext_Coeff, labelx="Extinction Coefficient") #red fit
SFB1plot_fitChl(SFBay1_Env$CWM_Size, labelx="CWM Size") #red fit
SFB1plot_fitChl(SFBay1_Env$CWM_Sizeb, labelx="CWM Size b") #red fit
SFB1plot_fitChl(SFBay1_Env$expShannonRA, labelx="exp H' RA") #green fit
SFB1plot_fitChl(SFBay1_Env$expShannonRB, labelx="exp H' RB") #blue fit
SFB1plot_fitChl(SFBay1_Env$simpsonRA, labelx="Simpson RA") #red fit
SFB1plot_fitChl(SFBay1_Env$simpsonRB, labelx="Simpson RB") #green fit
SFB1plot_fitChl(SFBay1_Env$invsimpsonRA, labelx="Inv Simpson RA") #green fit
SFB1plot_fitChl(SFBay1_Env$invsimpsonRB, labelx="Inv Simpson RB") #blue fit
SFB1plot_fitChl(SFBay1_Env$evennessRA, labelx="Evenness RA") #blue fit
SFB1plot_fitChl(SFBay1_Env$evennessRB, labelx="Evenness RB") #blue fit

#Model plots SFB2 Biomass
SFB2plot_fitBio(SFBay2_Env$Temperature, labelx = "Temperature") #blue fit
SFB2plot_fitBio(SFBay2_Env$Salinity, labelx = "Salinity") #green fit
SFB2plot_fitBio(SFBay2_Env$PO4, labelx= "PO4") #blue fit
SFB2plot_fitBio(SFBay2_Env$NO3, labelx="NO3") #green fit
SFB2plot_fitBio(SFBay2_Env$SiO4, labelx="SiO4") #green fit
SFB2plot_fitBio(SFBay2_Env$Richness, labelx="Richness") #blue fit
SFB2plot_fitBio(log(SFBay2_Env$Abundance), labelx="Abundance") #blue fit
SFB2plot_fitBio((SFBay2_Env$Chl), labelx="Chl") #blue fit
SFB2plot_fitBio(SFBay2_Env$SizeVar_bio, labelx="Size Variance b") #green fit
SFB2plot_fitBio(SFBay2_Env$SizeVar_abu, labelx="Size Variance") #green fit
SFB2plot_fitBio(SFBay2_Env$Ext_Coeff, labelx="Extinction Coefficient") #orange fit
SFB2plot_fitBio(SFBay2_Env$CWM_Size, labelx="CWM Size") #green ft
SFB2plot_fitBio(SFBay2_Env$CWM_Sizeb, labelx="CWM Size b") #red fit
SFB2plot_fitBio(SFBay2_Env$expShannonRA, labelx="exp H' RA") #blue fit
SFB2plot_fitBio(SFBay2_Env$expShannonRB, labelx="exp H' RB") #blue fit
SFB2plot_fitBio(SFBay2_Env$simpsonRA, labelx="Simpson RA") #green fit
SFB2plot_fitBio(SFBay2_Env$simpsonRB, labelx="Simpson RB") #blue fit
SFB2plot_fitBio(SFBay2_Env$invsimpsonRA, labelx="Inv Simpson RA") #blue fit
SFB2plot_fitBio(SFBay2_Env$invsimpsonRB, labelx="Inv Simpson RB") #blue fit
SFB2plot_fitBio(SFBay2_Env$evennessRA, labelx="Evenness RA") #blue fit
SFB2plot_fitBio(SFBay2_Env$evennessRB, labelx="Evenness RB") #blue fit

#Model plots SFB2 Chl
SFB2plot_fitChl(SFBay2_Env$Temperature, labelx = "Temperature") #green fit
SFB2plot_fitChl(SFBay2_Env$Salinity, labelx = "Salinity") #green fit
SFB2plot_fitChl(SFBay2_Env$PO4, labelx= "PO4") #green fit
SFB2plot_fitChl(SFBay2_Env$NO3, labelx="NO3") #blue fit
SFB2plot_fitChl(SFBay2_Env$SiO4, labelx="SiO4") #green fit
SFB2plot_fitChl(SFBay2_Env$Richness, labelx="Richness") #blue fit
SFB2plot_fitChl(log(SFBay2_Env$Abundance), labelx="Abundance") #blue fit
SFB2plot_fitChl((SFBay2_Env$Biomass), labelx="Biomass") #blue fit
SFB2plot_fitChl(SFBay2_Env$SizeVar_bio, labelx="Size Variance b") #green fit
SFB2plot_fitChl(SFBay2_Env$SizeVar_abu, labelx="Size Variance") #green fit
SFB2plot_fitChl(SFBay2_Env$Ext_Coeff, labelx="Extinction Coefficient") #red fit
SFB2plot_fitChl(SFBay2_Env$CWM_Size, labelx="CWM Size") #green fit
SFB2plot_fitChl(SFBay2_Env$CWM_Sizeb, labelx="CWM Size b") #red fit
SFB2plot_fitChl(SFBay2_Env$expShannonRA, labelx="exp H' RA") #blue fit
SFB2plot_fitChl(SFBay2_Env$expShannonRB, labelx="exp H' RB") #blue fit
SFB2plot_fitChl(SFBay2_Env$simpsonRA, labelx="Simpson RA") #green fit
SFB2plot_fitChl(SFBay2_Env$simpsonRB, labelx="Simpson RB") #red fit
SFB2plot_fitChl(SFBay2_Env$invsimpsonRA, labelx="Inv Simpson RA") #blue fit
SFB2plot_fitChl(SFBay2_Env$invsimpsonRB, labelx="Inv Simpson RB") #blue fit
SFB2plot_fitChl(SFBay2_Env$evennessRA, labelx="Evenness RA") #blue fit
SFB2plot_fitChl(SFBay2_Env$evennessRB, labelx="Evenness RB") #blue fit

