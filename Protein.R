#Title: "Calculation of Protein Concentration (ug/cm^2)"
#Author: "Serena Hackerott"
#Date: "08/29/2023"

##Load Data
Prot<-read.csv("Data/Protein.csv", header=TRUE)
BCA.Stand<-read.csv("Data/BCAStandards.csv", header=TRUE)
SampData<-read.csv("Data/SampleData.csv", header=TRUE)

##Create Standard Curve

##Subset Standards Data
Prot.St<- subset(Prot, Input=="Standard")

##Merge Standard Meta Data with Absorbance data
#Merges by Random Number (RandN) column
#Adds necessary Absorbance at 562nm (A562) column
BCAStands<-merge(BCA.Stand, Prot.St, all.x=TRUE)

##Plot Protein Concentration as a function of Absorbance 
plot(BCAStands$A562, BCAStands$Protein_ug.ml, pch=19)

##Fit Models of Protein Concentration as a function of Absorbance 

#First degree polynomial equation:
BCA.lm  <- lm(Protein_ug.ml~A562, data=BCAStands)
#Second degree polynomial equation:
BCA.ploy2 <- lm(Protein_ug.ml~poly(A562,2,raw=TRUE), data=BCAStands)
#Third degree polynomial equation:
BCA.ploy3 <- lm(Protein_ug.ml~poly(A562,3,raw=TRUE), data=BCAStands)

##Add Models to Plot
xx <- seq(0,3, length=50)

lines(xx, predict(BCA.lm, newdata=data.frame(A562=xx)), col="red")
lines(xx, predict(BCA.ploy2, newdata=data.frame(A562=xx)), col="green")
lines(xx, predict(BCA.ploy3, newdata=data.frame(A562=xx)), col="blue")

##Compare Models for Best Fit
anova(BCA.lm, BCA.ploy2)
#Poly2 is a significantly better fit than linear

anova(BCA.ploy2, BCA.ploy3)
#Poly3 is significantly better fit than Ploy2

##Create a function with the Poly3 model Equation
summary(BCA.ploy3)
coef(BCA.ploy3)

TP.mod <- function(absorbance) {
  coefs <- coef(BCA.ploy3)
  #y = d + cx + bx^2 + ax^3
  protein <- coefs[1] + (coefs[2] * absorbance) + (coefs[3] * absorbance^2) + (coefs[4] * absorbance^3)
  return(protein)}

##Calculate Protein Concentration

##Subset Sample Data (Unknown Sample)
Prot.Un<- subset(Prot, Input=="Un")

##Calculate Protein Concentration (ug/ml)
Prot.Un$TP_ug.ml<-TP.mod(Prot.Un$A562)

##Merge with Sample Meta Data to Calculate Protein per Surface Area
#Merges by Random Number (RandN) column
#Adds necessary Slurry Volume (Vol_ml) and Surface Area (SA_cm2) columns
Prot.Un<-merge(Prot.Un, SampData, all.x=TRUE, all.y=FALSE)

##Calculate Total Protein (ug) 
Prot.Un$TP_ug<-Prot.Un$TP_ug.ml*Prot.Un$Vol_ml

##Calculate Protein per Surface Area (ug/cm^2)
Prot.Un$TP_ug.cm2<-Prot.Un$TP_ug/Prot.Un$SA_cm2
