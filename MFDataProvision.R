library(hash)

#TOCOM related data provision
TOCOM_LISTING=hash()
TOCOM_LISTING[11]<-"GOLD"
TOCOM_LISTING[12]<-"SILVER"
TOCOM_LISTING[13]<-"PLATINM"
TOCOM_LISTING[14]<-"PALLADIUM"
TOCOM_LISTING[16]<-"MiniGOLD"
TOCOM_LISTING[17]<-"MiniPLATINM"
TOCOM_LISTING[18]<-"TOKYOGOLDSpot100"
TOCOM_LISTING[21]<-"ALUMINIUM"

#TOCOM_LISTING[["11"]]
#TOCOM_LISTING[["12"]]

tocomRaw<-read.table(".\\MarketData\\excel\\Futures\\tocom\\2016-01.csv",header=F,sep=",",stringsAsFactors=F,nrows=10000)
#TOCOM_LISTING[[as.character(tocomRaw$V3[1])]]
