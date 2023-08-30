#Title: "Calculation of Biomass (AFDW) (mg/cm^2)"
#Author: "Serena Hackerott"
#Date: "08/30/2023"

##Load Data
Bio<-read.csv("Data/Biomass.csv", header=TRUE)
SampData<-read.csv("Data/SampleData.csv", header=TRUE)

##Calculate Ash Free Dry Weight (AFDW)

##Calculate Change in Weight after Burning (g)
Bio$DeltaBurn_g<-Bio$Dried_g-Bio$Burned_g

##Calculate Ash Free Dry Weight (AFDW) (g) per Volume Input of Tissue (ml) 
Bio$AFDW_g.ml<-Bio$DeltaBurn_g/Bio$InVol_ml

##Merge with Sample Meta Data to Calculate Biomass per Surface Area
#Merges by Random Number (RandN) column
#Adds necessary Slurry Volume (Vol_ml) and Surface Area (SA_cm2) columns
Bio<-merge(Bio, SampData, all.x=TRUE, all.y=FALSE)

##Calculate Total AFDW (g) 
Bio$AFDW_g<-Bio$AFDW_g.ml*Bio$Vol_ml

##Calculate AFDW per Surface Area (mg/cm^2)
Bio$AFDW_g.cm2<-Bio$AFDW_g/Bio$SA_cm2
Bio$AFDW_mg.cm2 <- Bio$AFDW_g.cm2 * 1000
