#Title: "Calculation of Chlorophyll Concentration (ug/cm^2)"
#Author: "Serena Hackerott"
#Date: "08/22/2023"

##Equations for Dinos from Jeffrey and Humphrey 1975 in 100% acetone
#chla = 11.43*A663 - 0.64*A630
#chlc2 = 27.09*A630 - 3.63*A663

##Load Data
Chl<-read.csv("Data/Chlorophyll.csv",  header=TRUE)
SampleData<-read.csv("Data/SampleData.csv",  header=TRUE)

##Subtract Background A750 from A630 and A663
Chl$A630.c<-Chl$A630-Chl$A750
Chl$A663.c<-Chl$A663-Chl$A750

##Divide by Pathlength (0.5cm pathlength for 175ul sample in UVStar Plate) 
Chl$A630.c<-c(Chl$A630.c/0.5)
Chl$A663.c<-c(Chl$A663.c/0.5)

##Calculate Chl-a and Chl-c2 in µg/ml
Chl$Chl.a_ug.ml<-11.43*Chl$A663.c- 0.64*Chl$A630.c
Chl$Chl.c2_ug.ml <- 27.09*Chl$A630.c - 3.63*Chl$A663.c

##Merge with Sample Data to Calculate Chlorophyll per Surface Area
#Merges by Random Number (RandN) column
#Adds necessary Slurry Volume (Vol_ml) and Surface Area (SA_cm2) columns
Chl<-merge(Chl, SampleData,  all=TRUE)

##Calculate Total Chlorophyll-a and c2 (ug) 
Chl$Chl.a_ug<-Chl$Chl.a_ug.ml*Chl$Vol_ml
Chl$Chl.c2_ug<-Chl$Chl.c2_ug.ml*Chl$Vol_ml

##Calculate Chlorophyll-a and c2 per Surface Area (ug/cm^2)
Chl$Chl.a_ug.cm2<-Chl$Chl.a_ug/Chl$SA_cm2
Chl$Chl.c2_ug.cm2<-Chl$Chl.c2_ug/Chl$SA_cm2
