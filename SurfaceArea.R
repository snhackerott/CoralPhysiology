#Title: "Calculation of Surface Area (cm^2)"
#Author: "Serena Hackerott"
#Date: "08/30/2023"

##Load Data
SampData<-read.csv("Data/SampleData.csv", header=TRUE)
Wax.Stand<-read.csv("Data/WaxStandards.csv", header=TRUE)

##Create Standard Curve

##Calculate Surface Area of Cylinders from Standards
Wax.Stand$SA_cm2<-2*pi*(Wax.Stand$Dm_cm/2)*Wax.Stand$Ht_cm+2*pi*(Wax.Stand$Dm_cm/2)^2

##Calculate Difference in Wax Weight
Wax.Stand$Wax.D_g<- Wax.Stand$Wax.F_g-Wax.Stand$Wax.I_g

##Linear Model of SA as a Function of Wax Weight
wax.SA.lm  <- lm(SA_cm2~Wax.D_g, data=Wax.Stand)
summary(wax.SA.lm)
coef(wax.SA.lm)

SA.mod <- function(wax.weight) {
  coefs <- coef(wax.SA.lm)
  #y = mx + b
  SA <- ((coefs[2] * wax.weight) + coefs[1])
  return(SA)}

##Calculate Surface Area of Samples

##Calculate Difference in Wax Weight (g) in Sample Data
SampData$Wax.D_g<- SampData$Wax.F_g-SampData$Wax.I_g

##Calculate Surface Area (cm^2) by Applying Linear Model Function
SampData$SA_cm2<-SA.mod(SampData$Wax.D_g)
