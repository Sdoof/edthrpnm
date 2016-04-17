library(RQuantLib)
library(ggplot2)
library(plyr)
library(dplyr)
library(TTR)
rm(list=ls())
source('./ESourceRCode.R',encoding = 'UTF-8')

#read data file
histIV<-read.table(paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep=""),header=T,sep=",",nrows=2000)

###
##  Close to Open Spike
length(histIV[,c("Close")])

tmp<-replace(histIV$Close,rep(1:length(histIV$Close)-1),histIV$Close[2:length(histIV$Close)])
tmp<-replace(histIV[,c("Close")],rep(1:length(histIV[,c("Close")])-1),histIV[,c("Close")][2:length(histIV[,c("Close")])])

length(tmp)
tmp[1:length(histIV$Close)-1]->tmp
length(tmp)
tmp
tmp[1:20]
tail(tmp)

ivCtOSpikePct=histIV$Open[1:length(tmp)]/tmp
length(ivCtOSpikePct)
ivCtOSpikePct
mean(ivCtOSpikePct)
sd(ivCtOSpikePct)

# CLose to Open SPike % as multiple of SD
ivCtOSpikeNSd=(ivCtOSpikePct-1)/sd(ivCtOSpikePct)

###
##   Open/Close technical Indicator 

daynum=30
#Moving Average
sma<-SMA(histIV[,c("Close")], n=daynum)

#Bolinger Bands
bbands <- BBands( histIV[,c("High","Low","Close")],n=daynum )





