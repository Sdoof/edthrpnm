library(ggplot2)
library(RQuantLib)
library(dplyr)
library(readxl)
library(openxlsx)

#Config File
ConfigFileName_G="ConfigParameters.csv"
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"

ConfigParameters<-read.table(paste(DataFiles_Path_G,ConfigFileName_G,sep=""),
                             row.names=1, comment.char="#",header=T,stringsAsFactors=F,sep=",")

#File
Underying_Symbol_G=ConfigParameters["Underying_Symbol_G",1]
ResultFiles_Path_G=ConfigParameters["ResultFiles_Path_G",1]

Tickers<-read.xlsx(paste(DataFiles_Path_G,"TwsDde.xlsm",sep=""),sheet="Tickers",
                      colNames=F,detectDates=F,skipEmptyRows=T,rows=rep(1:100))

Tickers %>% slice(7:nrow(Tickers)) -> Tickers
colnames(Tickers)<-unlist(Tickers[1,])
Tickers<-Tickers[-1,]
Tickers$Type<-ifelse(is.na(Tickers$Type),"none", Tickers$Type)
OpTickers<-Tickers[grep("OPT",Tickers$Type),]
OpTickers<-OpTickers[grep(Underying_Symbol_G,OpTickers$Symbol),] 
rm(Underying_Symbol_G,ResultFiles_Path_G,ConfigParameters,ConfigFileName_G,DataFiles_Path_G)
rm(histPrice,Tradesheet)


#readxl
#histPrice <- read_excel(paste(DataFiles_Path_G,Underying_Symbol_G,".xlsx",sep=""),sheet="HistPrice",col_names=T)
#histPrice$Date<-format(histPrice$Date,"%Y/%b/%d")

#openxlsx
#histPrice <-read.xlsx(paste(DataFiles_Path_G,Underying_Symbol_G,".xlsx",sep=""),sheet="HistPrice",
#                      colNames=T,detectDates=T,skipEmptyRows=T,rows=rep(1:200))
#histPrice$Date<-format(histPrice$Date,"%Y/%b/%d")

