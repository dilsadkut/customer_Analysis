#RFM SKORLAMA

#RFM Skorlama, musteri analitiginde kullanilan bir musteri segmentasyonu, musteriyi tanima yontemidir.

# Recency - 7 (Musterinin en son ne zaman alisveris yaptigi)
# Frequency - 2 (Musterinin alisveris yapma sikligi)
# Monetary - 500(Musterinin alisverislerinde yaptigi harcamalar toplami)

#1 yil 1 hafta
#1 hafta

# R - 7
# F - 1
# M - 400

#son bir hafta icinde alisveris yapanlar 5
#bir hafta-bir ay 4
#bir ay-uc ay 3
#uc ay-alti ay 2
#alti ay 1

# 553

#################################
#veriyi alma

getwd()

setwd("C:/Users/lenovo/Desktop/CDNOW_sample")

cdnow <- read.table("CDNOW_sample.txt")

cdnow$V1 <- NULL

cdnow[1,2]

cdnow[1,]

#cdnow[,1] <- NULL

colnames(cdnow)

colnames(cdnow) <- c("ID","TARIH","ADET","FIYAT")

str(cdnow)

# id factor olmasi gerek.
# tarih'in de tarih olmasý gerek.

##ID

cdnow$ID <- as.factor(as.character(cdnow$ID))

rm(x)

str(cdnow)

##TARIH

cdnow$TARIH <- as.Date(as.character(cdnow$TARIH),"%Y%m%d")

str(cdnow)


as.numeric(Sys.Date() - cdnow$TARIH[1])

refDay <- max(cdnow$TARIH)
class(refDay)

as.numeric(refDay - cdnow$TARIH)

#Recency  degeri hesaplama

library(dplyr)

rfm_recency <- cdnow %>% group_by(ID) %>% summarise(Recency = as.numeric(refDay) - as.numeric(max(TARIH)))


#Frequency degeri hesaplama

count = n()

rfm_frequency <- cdnow %>% group_by(ID) %>% summarise(Frequency=n())

#Monetary degeri hesaplama

rfm_monetary <- cdnow %>% group_by(ID) %>% summarise(Monetary = sum(FIYAT))



#Degerleri birlestirme


?merge

rfm <- merge(rfm_recency, rfm_frequency, by="ID")

rfm <- merge(rfm, rfm_monetary, by="ID")



# RFM degerlerini Skorlama


quantile(rfm$Monetary)

2357*0.25 #yuzde 25 quantile
2357*0.5  #yuzde 50 quantile
2357*0.75 #yuzde 75 quantile

rankM = cut(rfm$Monetary, breaks = c(0,20,45,105,1000,6600))
levels(rankM)
levels(rankM) <- c(1,2,3,4,5)
levels(rankM)
head(rankM)


quantile(rfm$Recency)

rankR <- cut(rfm$Recency, breaks = c(0,60,217,473,506,550))
levels(rankR)
levels(rankR) <- c(5,4,3,2,1)
levels(rankR)


quantile(rfm$Frequency)

sum(rfm$Frequency==1)

sum(rfm$Frequency==2)

sum(rfm$Frequency==3)

sum(rfm$Frequency==4)

sum(rfm$Frequency==5)

sum(rfm$Frequency==6)

sum(rfm$Frequency==7)

sum(rfm$Frequency==8)


rankF <- cut(rfm$Frequency, breaks = c(0,1,2,3,7,60))
levels(rankF)
levels(rankF) <- c(1,2,3,4,5)


# Skor degerlerini birlestirme
rfmScores = data.frame(cbind(rfm$ID, rankR, rankF, rankM))

colnames(rfmScores) = c("ID","R","F","M")

############################

