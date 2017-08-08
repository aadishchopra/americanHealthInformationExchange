library(readxl)
AHA_Cleaned_Data.OE<- read_excel("D:/UIC/Healthcare Information Management/AHA/AHA_Cleaned Data.xlsx", 
                               sheet = "Reduced Variables")
View(AHA_Cleaned_Data)


str(AHA_Cleaned_Data.OE)
AHA_Cleaned_Data$`Number of Years having EMR`<-as.numeric(AHA_Cleaned_Data$`Number of Years having EMR`)
str(AHA_Cleaned_Data)






AHA<-AHA_Cleaned_Data
AHA$`Number of Years having EMR`<-as.numeric(AHA$`Number of Years having EMR`)
str(AHA)
AHA$`Bed category`<-as.factor(AHA$`Bed category`)
AHA$`Redesigned workflows to make optimal use of EHR`<-as.numeric(AHA$`Redesigned workflows to make optimal use of EHR`)
AHA$`Is it Top Inpatient EHR Vendor`<-as.factor(AHA$`Is it Top Inpatient EHR Vendor`)
attach(AHA)
detach(AHA)
# Model 1 is without HIEout or its components

AHA_Model1<-lm(AHA$`Sum of HIE in`~`Bed category`+`Sum of ECD`+`Sum of Results Viewing`+`Sum of Decision support`+`Sum of CLMT and other functionalities`+`Sum of Ambulatory in `+`Is it Top Inpatient EHR Vendor`+`Capital Barriers`+`Organizational Barriers`+`Uncertainty of HIE`+`Redesigned workflows to make optimal use of EHR`)
summary(AHA_Model1)
# without results viewing .Best Model
AHA_Model1.RV<-lm(AHA$`Sum of HIE in`~`Bed category`+`Sum of ECD`+`Sum of Decision support`+`Sum of CLMT and other functionalities`+`Sum of Ambulatory in `+`Is it Top Inpatient EHR Vendor`+`Capital Barriers`+`Organizational Barriers`+`Uncertainty of HIE`+`Redesigned workflows to make optimal use of EHR`)
summary(AHA_Model1.RV)
lm.beta(AHA_Model1.RV)



install.packages("QuantPsyc")
library("QuantPsyc")
lm.beta(AHA_Model1)

# Model 2 is with Number of Years

AHA_Model2<-lm(AHA$`Sum of HIE in`~ `Sum of Ambulatory in `+`Number of Years having EMR`+`Bed category`+`Sum of ECD`+`Sum of Results Viewing`+`Sum of Decision support`+`Sum of CLMT and other functionalities`+`Capital Barriers`+`Organizational Barriers`+`Uncertainty of HIE`+`Redesigned workflows to make optimal use of EHR`)
summary(AHA_Model2)


# Model 3 is with Ambulatory in

AHA_Model3<-lm(AHA$`Sum of Ambulatory in `~`Bed category`+`Sum of ECD`+`Sum of Results Viewing`+`Sum of CLMT and other functionalities`+`Sum of Decision support`+`Is it Top Outpatient EHR Vendor`+`Capital Barriers`+`Organizational Barriers`+`Uncertainty of HIE`+`Redesigned workflows to make optimal use of EHR`)
summary(AHA_Model3)


# Ambulatory in without Results viewing 

AHA_Model3.RV<-lm(AHA$`Sum of Ambulatory in `~`Bed category`+`Sum of ECD`+`Sum of CLMT and other functionalities`+`Sum of Decision support`+`Is it Top Outpatient EHR Vendor`+`Capital Barriers`+`Organizational Barriers`+`Uncertainty of HIE`+`Redesigned workflows to make optimal use of EHR`)
summary(AHA_Model3.RV)


# Ambulatory in with HIE in without Results Viewing
AHA_Model3.H<-lm(AHA$`Sum of Ambulatory in `~ `Sum of HIE in`+`Bed category`+`Sum of ECD`+`Sum of CLMT and other functionalities`+`Sum of Decision support`+`Is it Top Outpatient EHR Vendor`+`Capital Barriers`+`Organizational Barriers`+`Uncertainty of HIE`+`Redesigned workflows to make optimal use of EHR`)
summary(AHA_Model3.H)
lm.beta(AHA_Model3.H)




#model 4 is without CLMT for ambulatory in 
AHA_Model4<-lm(AHA$`Sum of Ambulatory in `~`Bed category`+`Sum of ECD`+`Sum of Results Viewing`+`Sum of Decision support`++`Capital Barriers`+`Organizational Barriers`+`Uncertainty of HIE`+`Redesigned workflows to make optimal use of EHR`)
summary(AHA_Model4)



AHA.Small <- AHA[AHA$`Bed category` == "Small",]
AHA.Medium <- AHA[AHA$`Bed category` == "Medium",]
AHA.Large <- AHA[AHA$`Bed category` == "Large",]

#Running similar parameters for small

AHA_Model1.Small<-lm(AHA.Small$`Sum of HIE in`~`Sum of ECD`+`Sum of Results Viewing`+`Sum of Decision support`+`Sum of CLMT and other functionalities`+`Sum of Ambulatory in `+`Is it Top Inpatient EHR Vendor`+`Capital Barriers`+`Organizational Barriers`+`Uncertainty of HIE`+`Redesigned workflows to make optimal use of EHR`,data=AHA.Small)
summary(AHA_Model1.Small)

# without results viewing
AHA_Model1.Small.RV<-lm(AHA.Small$`Sum of HIE in`~`Sum of ECD`+`Sum of Decision support`+`Sum of CLMT and other functionalities`+`Sum of Ambulatory in `+`Is it Top Inpatient EHR Vendor`+`Capital Barriers`+`Organizational Barriers`+`Uncertainty of HIE`+`Redesigned workflows to make optimal use of EHR`,data=AHA.Small)
summary(AHA_Model1.Small.RV)
# without ECD best results
AHA_Model1.Small.ECD<-lm(AHA.Small$`Sum of HIE in`~`Sum of Results Viewing`+`Sum of Decision support`+`Sum of CLMT and other functionalities`+`Sum of Ambulatory in `+`Is it Top Inpatient EHR Vendor`+`Capital Barriers`+`Organizational Barriers`+`Uncertainty of HIE`+`Redesigned workflows to make optimal use of EHR`,data=AHA.Small)
summary(AHA_Model1.Small.ECD)
lm.beta(AHA_Model1.Small.ECD)


AHA_Model1.Medium<-lm(AHA.Medium$`Sum of HIE in`~`Sum of ECD`+`Sum of Results Viewing`+`Sum of Decision support`+`Sum of CLMT and other functionalities`+`Sum of Ambulatory in `+`Is it Top Inpatient EHR Vendor`+`Capital Barriers`+`Organizational Barriers`+`Uncertainty of HIE`+`Redesigned workflows to make optimal use of EHR`,data=AHA.Medium)
summary(AHA_Model1.Medium)

# without Results Viewing best results
AHA_Model1.Medium.RV<-lm(AHA.Medium$`Sum of HIE in`~`Sum of ECD`+`Sum of Decision support`+`Sum of CLMT and other functionalities`+`Sum of Ambulatory in `+`Is it Top Inpatient EHR Vendor`+`Capital Barriers`+`Organizational Barriers`+`Uncertainty of HIE`+`Redesigned workflows to make optimal use of EHR`,data=AHA.Medium)
summary(AHA_Model1.Medium.RV)
lm.beta(AHA_Model1.Medium.RV)

# best results. 
AHA_Model1.Large<-lm(AHA.Large$`Sum of HIE in`~`Sum of ECD`+`Sum of Results Viewing`+`Sum of Decision support`+`Sum of CLMT and other functionalities`+`Sum of Ambulatory in `+`Is it Top Inpatient EHR Vendor`+`Capital Barriers`+`Organizational Barriers`+`Uncertainty of HIE`+`Redesigned workflows to make optimal use of EHR`,data=AHA.Large)
summary(AHA_Model1.Large)
lm.beta(AHA_Model1.Large)


# without RV
AHA_Model1.Large.RV<-lm(AHA.Large$`Sum of HIE in`~`Sum of ECD`+`Sum of Decision support`+`Sum of CLMT and other functionalities`+`Sum of Ambulatory in `+`Is it Top Inpatient EHR Vendor`+`Capital Barriers`+`Organizational Barriers`+`Uncertainty of HIE`+`Redesigned workflows to make optimal use of EHR`,data=AHA.Large)
summary(AHA_Model1.Large.RV)



#Running similar parameters for small for AIin best results

AHA_Model3.Small.AI<-lm(AHA.Small$`Sum of Ambulatory in `~`Sum of HIE in`+`Sum of ECD`+`Sum of Results Viewing`+`Sum of CLMT and other functionalities`+`Sum of Decision support`+`Is it Top Outpatient EHR Vendor`+`Capital Barriers`+`Organizational Barriers`+`Uncertainty of HIE`+`Redesigned workflows to make optimal use of EHR`,data=AHA.Small)
summary(AHA_Model3.Small.AI)
lm.beta(AHA_Model3.Small.AI)


#Running similar parameters for medium for AIin best results

AHA_Model3.Medium.AI<-lm(AHA.Medium$`Sum of Ambulatory in `~`Sum of HIE in`+`Sum of ECD`+`Sum of Results Viewing`+`Sum of CLMT and other functionalities`+`Sum of Decision support`+`Is it Top Outpatient EHR Vendor`+`Capital Barriers`+`Organizational Barriers`+`Uncertainty of HIE`+`Redesigned workflows to make optimal use of EHR`,data=AHA.Medium)
str(AHA.Medium)
AHA.Medium$`Is it Top Outpatient EHR Vendor`<-as.factor(AHA.Medium$`Is it Top Outpatient EHR Vendor`)
summary(AHA_Model3.Medium.AI)
lm.beta(AHA_Model3.Medium.AI)

#Running similar parameters for large for AIin best results

AHA_Model3.Large.AI<-lm(AHA.Large$`Sum of Ambulatory in `~`Sum of HIE in`+`Sum of ECD`+`Sum of Results Viewing`+`Sum of CLMT and other functionalities`+`Sum of Decision support`+`Is it Top Outpatient EHR Vendor`+`Capital Barriers`+`Organizational Barriers`+`Uncertainty of HIE`+`Redesigned workflows to make optimal use of EHR`,data=AHA.Large)
summary(AHA_Model3.Large.AI)
lm.beta(AHA_Model3.Large.AI)
str(AHA.Large)
AHA.Large$`Is it Top Outpatient EHR Vendor`<-as.factor(AHA.Large$`Is it Top Outpatient EHR Vendor`)
# based on above 3 models 









