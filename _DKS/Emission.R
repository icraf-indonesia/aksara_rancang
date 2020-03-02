library(dplyr)
library(reshape2)
library(plyr)



#Defining input:
#1. Land use 2000
#Land use T2: Tin
#2. Land use class
#3. Carbon stock (ton/CO2)

#Processes:

#Setting working directory
setwd("D:/MRV/Aksara/R6/Code")

#Loading the required data
LC_2011<-read.csv("LU_2011.csv")
LC_2015<-read.csv("LU_2015.csv")
LC_2030<-read.csv("LC_2030.csv")
LC_class<-read.csv("LC_class.csv")
c_stock<-read.csv("carbon_Stock.csv")
CARBON_t1<-read.csv("CARBON_t1.csv")
CARBON_t2<-read.csv("CARBON_t2.csv")



LUTMDatabase<-read.csv("LUTM_database.csv")


#Creating land use transition matrix
#perhaps calculate the probability of transition matrix

total<- sum(LUTMDatabase$COUNT)

LUTMDatabaseMelt <- melt(data = LUTMDatabase, id.vars=c('ID_LC1','ID_LC2'), measure.vars=c('COUNT'))

areaLandCover1 <- dcast(data = LUTMDatabaseMelt, formula = ID_LC1 ~ ., fun.aggregate = sum)
colnames(areaLandCover1)[2] ="COUNT"

areaLandCover2 <- dcast(data = LUTMDatabaseMelt, formula = ID_LC2 ~ ., fun.aggregate = sum)
colnames(areaLandCover2)[2] ="COUNT"



#Calculating transition probability (1st iteration)
LUTMDatabase$ID_LC1_FR <- LUTMDatabase$ID_LC1
LUTMDatabase$ID_LC2_TO <- LUTMDatabase$ID_LC2
LUTMDatabase$ID_LC2_FR <- LUTMDatabase$ID_LC1

colnames(LUTMDatabase)[4] = "ID_LC2"
colnames(areaLandCover1)[1] ="ID_LC1_FR"
colnames(areaLandCover1)[2] ="OVCOUNT_T1FR"
colnames(areaLandCover2)[1] ="ID_LC2_TO"
colnames(areaLandCover2)[2] ="OVCOUNT_T2TO"


LUTMDatabase <- as.data.frame(merge(LUTMDatabase,areaLandCover1,by="ID_LC1_FR"))
LUTMDatabase <- as.data.frame(merge(LUTMDatabase,areaLandCover2,by="ID_LC2_TO"))

colnames(areaLandCover2)[1] ="ID_LC2_FR"
colnames(areaLandCover2)[2] ="OVCOUNT_T2FR"


LUTMDatabase <- as.data.frame(merge(LUTMDatabase,areaLandCover2,by="ID_LC2_FR"))
LUTMDatabase$TPM <- LUTMDatabase$COUNT / LUTMDatabase$OVCOUNT_T1FR     #hitung proporsi for all zone: tpm = count of class1to2/sumOfClass1
LUTMDatabase$COUNT2_3 <- LUTMDatabase$TPM * LUTMDatabase$OVCOUNT_T2FR     #hitung nilai perubahan baru: newCount = tpm*sumOfClass2
LUTMDatabase$LUTMLandscape <- LUTMDatabase$COUNT2_3 / total



#Calculating carbon stocks

LUTMDatabase <- as.data.frame(merge(LUTMDatabase,CARBON_t1,by="ID_LC1"))
LUTMDatabase <- as.data.frame(merge(LUTMDatabase,CARBON_t2,by="ID_LC2"))

LUTMDatabase$CEK_EM <- LUTMDatabase$CARBON_t1 > LUTMDatabase$CARBON_t2
LUTMDatabase$CEK_SQ <- LUTMDatabase$CARBON_t1 < LUTMDatabase$CARBON_t2

LUTMDatabase$EM0 <- (LUTMDatabase$CARBON_t1 - LUTMDatabase$CARBON_t2) * LUTMDatabase$CEK_EM * LUTMDatabase$COUNT * 3.67
LUTMDatabase$SQ0 <- (LUTMDatabase$CARBON_t2 - LUTMDatabase$CARBON_t1) * LUTMDatabase$CEK_SQ * LUTMDatabase$COUNT * 3.67

LUTMDatabase$EM1 <- (LUTMDatabase$CARBON_t1 - LUTMDatabase$CARBON_t2) * LUTMDatabase$CEK_EM * LUTMDatabase$COUNT * 3.67
LUTMDatabase$SQ1 <- (LUTMDatabase$CARBON_t2 - LUTMDatabase$CARBON_t1) * LUTMDatabase$CEK_SQ * LUTMDatabase$COUNT2_3 * 3.67

LUTM1<-LUTMDatabase



#Creating matrix tables
LUTMOverallMelt <- melt(data = LUTMDatabase, id.vars=c('ID_LC1','ID_LC2'), measure.vars=c('COUNT'))
TPM <- dcast(data = LUTMOverallMelt, formula = ID_LC1 ~ ID_LC2, fun.aggregate = sum)

LUTMOverallMeltEm <- melt(data = LUTMDatabase, id.vars=c('ID_LC1','ID_LC2'), measure.vars=c('EM0'))
EM <- dcast(data = LUTMOverallMeltEm, formula = ID_LC1 ~ ID_LC2, fun.aggregate = sum)



