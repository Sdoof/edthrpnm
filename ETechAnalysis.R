library(RQuantLib)
library(ggplot2)
library(plyr)
library(dplyr)
library(TTR)
rm(list=ls())
source('./ESourceRCode.R',encoding = 'UTF-8')

#read data file
histPrc<-read.table(paste(DataFiles_Path_G,Underying_Symbol_G,"_Hist.csv",sep=""),header=T,sep=",",nrows=2000)
histIV<-read.table(paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep=""),header=T,sep=",",nrows=2000)
#show Realized and Impled Volatility
cat("Realized Vol(30d anlzd)",annuual.daily.volatility(histPrc$Close[1:30])$anlzd*100,"IV",histIV$Close[1],"\n")
cat("Realized Vol(60d anlzd)",annuual.daily.volatility(histPrc$Close[1:60])$anlzd*100,"IV",histIV$Close[1],"\n")
cat("Realized Vol(120d anlzd)",annuual.daily.volatility(histPrc$Close[1:120])$anlzd*100,"IV",histIV$Close[1],"\n")
cat("Realized Vol(200d anlzd)",annuual.daily.volatility(histPrc$Close[1:200])$anlzd*100,"IV",histIV$Close[1],"\n")

###
##   IV Close to Open Spike
length(histIV$Close)
histIV$Close
histIV$Close[1:20]
tail(histIV$Close)

tmp<-replace(histIV$Close,rep(1:length(histIV$Close)-1),histIV$Close[2:length(histIV$Close)])
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

#over/under 1sd vector
ifelse(ivCtOSpikePct > mean(ivCtOSpikePct)+sd(ivCtOSpikePct),TRUE,FALSE) -> ivCtOSpikePct_over_1SD
ifelse(ivCtOSpikePct < mean(ivCtOSpikePct)-sd(ivCtOSpikePct),TRUE,FALSE) -> ivCtOSpikePct_under_1SD

###
##   IV Open/Close bolinger band

length(histIV$Close)
histIV$Close
daynum=30
annuual.daily.volatility(histIV$Close[1:daynum])$daily*sqrt(daynum)

#moving average
SMA(histIV$Close, daynum)->tmp
tmp[!is.na(tmp)] -> IVCloseMva

