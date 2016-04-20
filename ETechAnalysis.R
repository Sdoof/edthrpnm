library(RQuantLib)
library(ggplot2)
library(plyr)
library(dplyr)
library(TTR)
rm(list=ls())
source('./ESourceRCode.R',encoding = 'UTF-8')

#read data file
datafile_name=paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="")
TSdata<-read.table(datafile_name,header=T,sep=",",nrows=2000)

###
##  Close to Open Spike
length(TSdata[,c("Close")])

tmp<-replace(TSdata$Close,rep(1:length(TSdata$Close)-1),TSdata$Close[2:length(TSdata$Close)])
tmp<-replace(TSdata[,c("Close")],rep(1:length(TSdata[,c("Close")])-1),TSdata[,c("Close")][2:length(TSdata[,c("Close")])])

length(tmp)
tmp[1:length(TSdata$Close)-1]->tmp
length(tmp)
tmp
tmp[1:20]
tail(tmp)

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
##   Open/Close technical Indicator
daynum=30
#Moving Average
sma<-SMA(TSdata[,c("Close")], n=daynum)
sma<-sma[!is.na(sma)]

#Bolinger Bands
bbands <- BBands( TSdata[,c("High","Low","Close")],n=daynum )
bbands <- na.omit(bbands)

#Data Frame 
nrow_tech=min(nrow(TSdata[,c("Close")]),nrow(bbands[,c("dn","up","mavg")]),
    length(ivCtOSpikePct),length(ivCtOSpikeNSd))

techAnalyDf=data.frame(TSdata[,c("Date","Open","High","Low","Close")][1:nrow_tech,],bbands[,c("dn","up","mavg")][1:nrow_tech,],
                       sma=sma[1:nrow_tech],
                       ivCtOSpikePct=ivCtOSpikePct[1:nrow_tech],
                       ivCtOSpikeNSd=ivCtOSpikeNSd[1:nrow_tech])

rownames(techAnalyDf) <- c(1:nrow(techAnalyDf))



