library(hash)
library(dplyr)

#TOCOM related data provision
#Product
TOCOM_LISTING=hash()
TOCOM_LISTING[11]<-"GOLD"
TOCOM_LISTING[12]<-"SILVER"
TOCOM_LISTING[13]<-"PLATINM"
TOCOM_LISTING[14]<-"PALLADIUM"
TOCOM_LISTING[16]<-"MiniGOLD"
TOCOM_LISTING[17]<-"MiniPLATINM"
TOCOM_LISTING[18]<-"TOKYOGOLDSpot100"
TOCOM_LISTING[21]<-"ALUMINIUM"
TOCOM_LISTING[31]<-"GASOLINE"
TOCOM_LISTING[32]<-"KEROSENE"
TOCOM_LISTING[33]<-"CRUDEOIL"
TOCOM_LISTING[34]<-"DIESELOIL"
TOCOM_LISTING[37]<-"CHUKYOGASOLINE"
TOCOM_LISTING[38]<-"CHUKYODIESEL"
TOCOM_LISTING[62]<-"COTTONTHREAD"
TOCOM_LISTING[65]<-"WOOL"
TOCOM_LISTING[81]<-"Rubber"
TOCOM_LISTING[201]<-"Corn"
TOCOM_LISTING[202]<-"Soybean"
TOCOM_LISTING[204]<-"Azuki"
TOCOM_LISTING[211]<-"RawSugar"
TOCOM_LISTING[100]<-"NikkeiTOCOMCommodityIndexCD"
TOCOM_LISTING[102]<-"NikkeiTOCOMCommodityIndexCM"

ProductName<-c("GOLD","SILVER")
TOCOM_PRODUCT<-vector("list",length(ProductName))
TOCOM_PRODUCT[[11]]<-"GOLD"
TOCOM_PRODUCT[[12]]<-"SILVER"
names(TOCOM_PRODUCT)<-ProductName

#TYPE
TOCOM_TYPE=hash()
TOCOM_TYPE[1]<-"FUTURE"
TOCOM_TYPE[2]<-"CallOption"
TOCOM_TYPE[3]<-"PutOption" #Until 20090501
TOCOM_TYPE[21]<-"CallOption"
TOCOM_TYPE[22]<-"PutOption" #After 20090507
TOCOM_TYPE[11]<-"PDFuture"
TOCOM_TYPE[12]<-"CSFuture"

#TOCOM_LISTING[["11"]]
#TOCOM_LISTING[["12"]]

tocomRaw<-read.table(".\\MarketData\\excel\\Futures\\tocom\\2016-01.csv",header=F,sep=",",stringsAsFactors=F,nrows=10000)
colnames(tocomRaw) <- c("Date","TYPE","Product","ContactMonth","Strike","Open","High","Low","Close","SettlementPrice","Volume","OpenInt")
#TOCOM_LISTING[[as.character(tocomRaw$Product[1])]]

tocomRaw  %>% filter(TYPE==11) ->tmp
tocomRaw  %>% filter(TYPE==11) ->tmp
tocomRaw %>% filter(Product==11 )-> tmp
TOCOM_PRODUCT[[1]]

tocomRaw  %>% rowwise() 
