library(RQuantLib)
library(ggplot2)
library(plyr)
library(dplyr)
library(TTR)
library(xts)
library(dygraphs)
rm(list=ls())
source('./ESourceRCode.R',encoding = 'UTF-8')

#Total TS Data Num
TS_DATA_NUM=2000

#moving average day
MV_AVRAGE_DAYNUM=c(20)

#read data file
datafile_name=paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="")
TSdata<-read.table(datafile_name,header=T,sep=",",nrows=TS_DATA_NUM)

###
##  Close to Open Spike
length(TSdata[,c("Close")])

tmp<-replace(TSdata$Close,rep(1:length(TSdata$Close)-1),TSdata$Close[2:length(TSdata$Close)])
tmp<-replace(TSdata[,c("Close")],rep(1:length(TSdata[,c("Close")])-1),TSdata[,c("Close")][2:length(TSdata[,c("Close")])])

#length(tmp)
tmp[1:length(TSdata$Close)-1]->tmp
#length(tmp)
#tmp
#tmp[1:20]
#tail(tmp)

# CLose to Open SPike %
ivCtOSpikePct=TSdata$Open[1:length(tmp)]/tmp
length(ivCtOSpikePct)
ivCtOSpikePct
mean(ivCtOSpikePct)
sd(ivCtOSpikePct)

# CLose to Open SPike % as multiple of SD
ivCtOSpikeNSd=(ivCtOSpikePct-1)/sd(ivCtOSpikePct)
ivCtOSpikeNSd[!is.na(ivCtOSpikeNSd)]

###
##  Technical Indicator
daynum=MV_AVRAGE_DAYNUM[1]

#Bolinger Bands
bbands = matrix(rep(0,(TS_DATA_NUM-MV_AVRAGE_DAYNUM[1])*3), nrow = (TS_DATA_NUM-MV_AVRAGE_DAYNUM[1]), ncol = 3)
colnames(bbands) <- c("dn","mavg","up")

for(i in 1:(TS_DATA_NUM-MV_AVRAGE_DAYNUM[1])){
  #cat(mean(TSdata[,c("Close")][i:(MV_AVRAGE_DAYNUM+i-1)]),"\n")
  bbands[i,"mavg"]=mean(TSdata[,c("Close")][i:(MV_AVRAGE_DAYNUM+i-1)])
  #cat(sd(TSdata[,c("Close")][i:(MV_AVRAGE_DAYNUM+i-1)]),"\n")
  bbands[i,"dn"]=bbands[i,"mavg"]-2*sd(TSdata[,c("Close")][i:(MV_AVRAGE_DAYNUM+i-1)])
  bbands[i,"up"]=bbands[i,"mavg"]+2*sd(TSdata[,c("Close")][i:(MV_AVRAGE_DAYNUM+i-1)])
}

#Data Frame 
nrow_tech=min(nrow(TSdata[,c("Close")]),nrow(bbands[,c("dn","up","mavg")]),
    length(ivCtOSpikePct),length(ivCtOSpikeNSd))

techAnalyDf=data.frame(TSdata[,c("Date","Open","High","Low","Close")][1:nrow_tech,],bbands[,c("dn","up","mavg")][1:nrow_tech,],
                       ivCtOSpikePct=ivCtOSpikePct[1:nrow_tech],
                       ivCtOSpikeNSd=ivCtOSpikeNSd[1:nrow_tech])

rownames(techAnalyDf) <- c(1:nrow(techAnalyDf))
str(techAnalyDf)
#delte Factor
techAnalyDf$Date <- as.character(techAnalyDf$Date)
str(techAnalyDf)

ifelse(abs(techAnalyDf$ivCtOSpikeNSd)>2,techAnalyDf$ivCtOSpikeNSd,0)-> techAnalyDf$ivCtOSpikeNSd_thresh

##dygraph
date_s = as.POSIXct(strptime(techAnalyDf$Date, 
                            format="%Y/%m/%d",tz="UTC"))

close_xts<- xts(techAnalyDf$Close,order.by=date_s,frequency=252)
mavg_xts<- xts(techAnalyDf$mavg,order.by=date_s,frequency=252)
bbUp_xts<- xts(techAnalyDf$up,order.by=date_s,frequency=252)
bbDn_xts<- xts(techAnalyDf$dn,order.by=date_s,frequency=252)
ivCtOSpikeNSd_xts<-xts(techAnalyDf$ivCtOSpikeNSd_thresh,order.by=date_s,frequency=252)

#chart_xts <- cbind(close_xts,mavg_xts,bbUp_xts,bbDn_xts,ivCtOSpikeNSd_xts)
chart_xts <- cbind(close_xts,mavg_xts,bbUp_xts,bbDn_xts)

dygraph(chart_xts,ylab="Value", 
        main="TSData Tech Analysis Chart")  %>%
  dySeries("..1",label="Close") %>%
  #dySeries("..3",label="UpB") %>%
  #dySeries("..2",label="MAVG") %>%
  #dySeries("..4",label="LowB") %>%
  dySeries(c("..3","..2","..4"), label = "BB") %>%
  #dySeries("..5",label="SpikeNSd") %>%
  dyAxis("y", label = "Value") %>%
  #dyAxis("y2", label = "SpikeNSd") %>%
  #dyOptions(colors = c("blue","brown")) %>%
  dyRangeSelector()

## pctB  and BWidth
pctB=(techAnalyDf$Close-bbands[,"dn"])/(bbands[,"up"]-bbands[,"dn"])
BWdth=(bbands[,"up"]-bbands[,"dn"])/(bbands[,"mavg"])

pctB_xts<- xts(pctB,order.by=date_s,frequency=252)
BWdth_xts<- xts(BWdth,order.by=date_s,frequency=252)

bbind_xts<- cbind(pctB_xts,BWdth_xts)

dygraph(bbind_xts,ylab="pctB and BWdth", 
        main="BB Indicator")  %>%
  dySeries("..1",label="pctB") %>%
  dySeries("..2",label="BWdth",axis = 'y2') %>%
  dyOptions(colors = c("blue","brown")) %>%
  dyRangeSelector()

### Comparison of HV and IV
#read data file
datafile_name=paste(DataFiles_Path_G,Underying_Symbol_G,"_Hist.csv",sep="")
TSdata2<-read.table(datafile_name,header=T,sep=",",nrows=TS_DATA_NUM)

udlCloseChg=TSdata2$PC1dCtC

histVol = matrix(rep(0,(TS_DATA_NUM-MV_AVRAGE_DAYNUM[1])), nrow = (TS_DATA_NUM-MV_AVRAGE_DAYNUM[1]), ncol = 1)
colnames(histVol) <- c("HV")

for(i in 1:(TS_DATA_NUM-MV_AVRAGE_DAYNUM[1])){
  vol_tmp=annuual.daily.volatility(udlClose[i:(MV_AVRAGE_DAYNUM+i-1)])$anlzd*100
  sd_tmp=sd(udlCloseChg[i:(MV_AVRAGE_DAYNUM+i-1)])*sqrt(252)*100
  cat(i," ",vol_tmp," sd",sd_tmp,"\n")
  histVol[i,"HV"]=vol_tmp
}

HV_xts<- xts(histVol,order.by=date_s,frequency=252)
volcomp_xts<- cbind(close_xts,HV_xts)

dygraph(volcomp_xts,ylab="IV and HV", 
        main="Volatility Comparison")  %>%
  dySeries("..1",label="IV") %>%
  dySeries("HV",label="HV") %>%
  #dyOptions(colors = c("blue","brown")) %>%
  dyRangeSelector()
