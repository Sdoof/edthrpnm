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
TS_DATA_NUM=4100

#read data file
TSdata<-read.table(paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep=""),
                   header=T,sep=",",nrows=TS_DATA_NUM)

TSdata2<-read.table(paste(DataFiles_Path_G,Underying_Symbol_G,"_Hist.csv",sep="")
                    ,header=T,sep=",",nrows=TS_DATA_NUM)

##
### Begin Of Function Definitions

##
#  Close to Open Spike Vector
createCloseToOpenSpikeVec <- function(Data){
  length(Data[,c("Close")])
  
  tmp<-replace(Data$Close,rep(1:length(Data$Close)-1),Data$Close[2:length(Data$Close)])
  tmp<-replace(Data[,c("Close")],rep(1:length(Data[,c("Close")])-1),Data[,c("Close")][2:length(Data[,c("Close")])])
  
  tmp[1:length(Data$Close)-1]->tmp
  
  # CLose to Open SPike %
  ivCtOSpikePct=Data$Open[1:length(tmp)]/tmp
  length(ivCtOSpikePct)
  ivCtOSpikePct
  mean(ivCtOSpikePct)
  sd(ivCtOSpikePct)
  
  # CLose to Open SPike % as multiple of SD
  ivCtOSpikeNSd=(ivCtOSpikePct-1)/sd(ivCtOSpikePct)
  ivCtOSpikeNSd[!is.na(ivCtOSpikeNSd)]
  
  ret_names<-c("ivCtOSpikePct","ivCtOSpikeNSd")
  ret_ <- vector("list",length(ret_names))
  ret_[[1]]<-ivCtOSpikePct
  ret_[[2]]<-ivCtOSpikeNSd
  names(ret_)<-ret_names
  ret_
  
  return(ret_)
}

##
# retrun BB elements as a Matrix in place of Df
createBB <- function(Data,TS_DATA_NUM,MV_AVRAGE_DAYNUM){
  mtrxCol=c("BBmavg","BBdn","BBup")
  mtrxColN=length(mtrxCol)
  bbands = matrix(rep(0,(TS_DATA_NUM-MV_AVRAGE_DAYNUM[1])*mtrxColN), nrow = (TS_DATA_NUM-MV_AVRAGE_DAYNUM[1]), ncol = mtrxColN)
  colnames(bbands) <- mtrxCol
  
  for(i in 1:(TS_DATA_NUM-MV_AVRAGE_DAYNUM[1])){
    bbands[i,mtrxCol[1]]=mean(Data[i:(MV_AVRAGE_DAYNUM+i-1)])
    #cat(sd(Data[,c("Close")][i:(MV_AVRAGE_DAYNUM+i-1)]),"\n")
    bbands[i,mtrxCol[2]]=bbands[i,mtrxCol[1]]-2*sd(Data[i:(MV_AVRAGE_DAYNUM+i-1)])
    bbands[i,mtrxCol[3]]=bbands[i,mtrxCol[1]]+2*sd(Data[i:(MV_AVRAGE_DAYNUM+i-1)])
  }
  return(bbands)
}

##
# return TR(True Range) elemets as a Matrix
createTR <- function(DataClose,DataHigh,DataLow,TS_DATA_NUM){
  mtrxCol=c("TR")
  mtrxColN=length(mtrxCol)
  retMtrx = matrix(rep(0,(TS_DATA_NUM)*mtrxColN), nrow = (TS_DATA_NUM), ncol = mtrxColN)
  colnames(retMtrx) <- mtrxCol
  
  #get ascending ordered vector
  AcdC=rev(DataClose)
  AcdH=rev(DataHigh)
  AcdL=rev(DataLow)
  
  for(i in 2:TS_DATA_NUM){
    retMtrx[i,mtrxCol[1]]=max(AcdH[i],AcdC[i-1])-min(AcdL[i],AcdC[i-1])
  }
  
  retMtrx[,mtrxCol[1]]=rev(retMtrx[,mtrxCol[1]])
  retMtrx=matrix(retMtrx[-length(retMtrx[,mtrxCol[1]]),],nrow=length(retMtrx[,mtrxCol[1]])-1)
  colnames(retMtrx) <- mtrxCol
  return(retMtrx)
}

##
# return (Xday Simple) MAVG as a Matrix
createSMA <- function(Data,TS_DATA_NUM,MV_AVRAGE_DAYNUM){
  mtrxCol=c("SMA")
  mtrxColN=length(mtrxCol)
  retMtrx = matrix(rep(0,(TS_DATA_NUM-MV_AVRAGE_DAYNUM[1])*mtrxColN), nrow = (TS_DATA_NUM-MV_AVRAGE_DAYNUM[1]), ncol = mtrxColN)
  colnames(retMtrx) <- mtrxCol
  
  for(i in 1:(TS_DATA_NUM-MV_AVRAGE_DAYNUM[1])){
    retMtrx[i,mtrxCol[1]]=mean(Data[i:(MV_AVRAGE_DAYNUM+i-1)])
  }
  return(retMtrx)
}

##
# create 1st order Both Sides(+,-) difference as a Matrix
createBothSidesDiff <- function(Data,TS_DATA_NUM){
  mtrxCol=c("BSDiff")
  mtrxColN=length(mtrxCol)
  retMtrx=matrix(rep(0,(TS_DATA_NUM)*mtrxColN), nrow = (TS_DATA_NUM), ncol = mtrxColN)
  colnames(retMtrx) <- mtrxCol
  
  retMtrx[1,mtrxCol[1]]=Data[1]-Data[2]
  for(i in 2:(TS_DATA_NUM-1)){
    retMtrx[i,mtrxCol[1]]=(Data[i]-Data[i+1])/2+(Data[i-1]-Data[i])/2
  }
  retMtrx[TS_DATA_NUM,mtrxCol[1]]=Data[TS_DATA_NUM]-Data[TS_DATA_NUM-1]
  
  return(retMtrx)
}

##
# create normal 1st order difference as a Matrix
createDiff <- function(Data,TS_DATA_NUM){
  mtrxCol=c("Diff")
  mtrxColN=length(mtrxCol)
  retMtrx=matrix(rep(0,(TS_DATA_NUM-1)*mtrxColN), nrow = (TS_DATA_NUM-1), ncol = mtrxColN)
  colnames(retMtrx) <- mtrxCol
  
  for(i in 1:(TS_DATA_NUM-1)){
    retMtrx[i,mtrxCol[1]]=Data[i]-Data[i+1]
  }
  
  return(retMtrx)
}

##
# create Log Geometric Sequence as a Matrix
createLogGeomSeq <- function(Data,TS_DATA_NUM){
  mtrxCol=c("LGRatio","ExpRatio")
  mtrxColN=length(mtrxCol)
  retMtrx=matrix(rep(0,(TS_DATA_NUM-1)*mtrxColN), nrow = (TS_DATA_NUM-1), ncol = mtrxColN)
  colnames(retMtrx) <- mtrxCol
  
  for(i in 1:(TS_DATA_NUM-1)){
    retMtrx[i,mtrxCol[1]]=log(Data[i])-log(Data[i+1])#log(Data[i]/Data[i+1])
    retMtrx[i,mtrxCol[2]]=exp(retMtrx[i,mtrxCol[1]])
  }
  
  return(retMtrx)
}

##
# create Exponential Regression Slope(annualized) as a Matrix
createExpReg <- function(Data,TS_DATA_NUM,DAY_NUM){
  mtrxCol=c("ExpRegSlope","ExpRegSlopeAnlzd","Intercept","RSq")
  mtrxColN=length(mtrxCol)
  retMtrx=matrix(rep(0,(TS_DATA_NUM-DAY_NUM)*mtrxColN), nrow = (TS_DATA_NUM-DAY_NUM), ncol = mtrxColN)
  colnames(retMtrx) <- mtrxCol
  
  tmp=log(Data)
  tmp=rev(tmp)
  for(i in 1:(length(tmp)-DAY_NUM)){
    tmp.lm<-lm(y~x, data=data.frame(x=(i:(i+DAY_NUM-1)), y=tmp[i:(i+DAY_NUM-1)]))
    intercept=tmp.lm$coefficient[1]
    slope=tmp.lm$coefficient[2]
    slope_anlzd=(exp(slope)^252)-1
    r_squared=summary(tmp.lm)$r.squared
    retMtrx[i,mtrxCol[1]]=slope
    retMtrx[i,mtrxCol[2]]=slope_anlzd
    retMtrx[i,mtrxCol[3]]=intercept
    retMtrx[i,mtrxCol[4]]=r_squared
  }
  tmp=c(rev(retMtrx[,mtrxCol[1]]),rev(retMtrx[,mtrxCol[2]]),rev(retMtrx[,mtrxCol[3]]),rev(retMtrx[,mtrxCol[4]]))
  retMtrx=matrix(tmp,nrow=(TS_DATA_NUM-DAY_NUM), ncol = mtrxColN)
  colnames(retMtrx) <- mtrxCol
  
  return(retMtrx)
}

###
##  create Historical Volatility Matrix
createHistVol<-function(Close,TS_DATA_NUM,MV_AVRAGE_DAYNUM #,CloseChg
){
  mtrxCol=c("HV")
  histVol = matrix(rep(0,(TS_DATA_NUM-MV_AVRAGE_DAYNUM[1])), nrow = (TS_DATA_NUM-MV_AVRAGE_DAYNUM[1]), ncol = 1)
  colnames(histVol) <- mtrxCol
  for(i in 1:(TS_DATA_NUM-MV_AVRAGE_DAYNUM[1])){
    vol_tmp=annuual.daily.volatility(Close[i:(MV_AVRAGE_DAYNUM+i-1)])$anlzd*100
    #sd_tmp=sd(CloseChg[i:(MV_AVRAGE_DAYNUM+i-1)])*sqrt(252)*100
    #cat(i," ",vol_tmp," sd",sd_tmp,"\n")
    histVol[i,"HV"]=vol_tmp
  }
  return(histVol)
}

##
### End Of Function Definitions

##
#  Price analysis example

### SMA ###
sma200=createSMA(TSdata2[,"Close"],nrow(TSdata2),200)

#time
date_s = as.POSIXct(strptime(TSdata2[,"Date"], 
                             format="%Y/%m/%d",tz="UTC"))
date_s=date_s[1:length(sma200)]
close_sma200=TSdata2[1:length(sma200),"Close"]
##dygraph 
close_xts<- xts(close_sma200,order.by=date_s,frequency=252)
sma200_xts<- xts(sma200[,"SMA200"],order.by=date_s,frequency=252)
chart_xts <- cbind(close_xts,sma200_xts)

dygraph(chart_xts,ylab="Value", main="200SMA")  %>%
  dySeries("..1",label="Close") %>%
  dySeries("..2",label="200SMA") %>%
  dyAxis("y", label = "Value") %>%
  dyRangeSelector()

### TR. ATR ###
tr_s=createTR(TSdata2[,"Close"],TSdata2[,"High"],TSdata2[,"Low"],nrow(TSdata2))
atr100_s=createSMA(tr_s[,"TR"],length(tr_s[,"TR"]),100)
#time  
date_s = as.POSIXct(strptime(TSdata2[,"Date"], 
                             format="%Y/%m/%d",tz="UTC"))
tr_s=tr_s[1:length(atr100_s)]
date_s=date_s[1:length(atr100_s)]
atr100_s=atr100_s[1:length(atr100_s)]
##dygraph
tr_xts=xts(tr_s,order.by=date_s,frequency=252)
atr100_xts=xts(atr100_s,order.by=date_s,frequency=252)
chart_xts <- cbind(tr_xts,atr100_xts)

dygraph(chart_xts,ylab="Value", main="TR ATR")  %>%
  dySeries("..1",label="TR") %>%
  dySeries("..2",label="ATR") %>%
  dyAxis("y", label = "Value") %>%
  dyRangeSelector()

#VOlatility analysis example

# Close To Open SPike vector
tmp<-createCloseToOpenSpikeVec(TSdata)
ivCtOSpikePct<-tmp$ivCtOSpikePct
ivCtOSpikeNSd<-tmp$ivCtOSpikeNSd

# Bolinger Bands Dataframe
#Matrix version BB should be used
bolbands=createBB(TSdata[,"Close"],nrow(TSdata),20)

# pctB  and BWidth
pctB=(TSdata[,"Close"][1:length(bolbands[,"BBdn"])]-bolbands[,"BBdn"])/(bolbands[,"BBup"]-bolbands[,"BBdn"])
BWdth=(bolbands[,"BBup"]-bolbands[,"BBdn"])/(bolbands[,"BBmavg"])

# Data format standarizee dor dygraph
date_s = as.POSIXct(strptime(TSdata[,"Date"], 
                             format="%Y/%m/%d",tz="UTC"))
date_s=date_s=date_s[1:nrow(bolbands)]

##dygraph BB
close_xts<- xts(TSdata[,"Close"][1:nrow(histVol)],order.by=date_s,frequency=252)
mavg_xts<- xts(bolbands[,"BBmavg"],order.by=date_s,frequency=252)
bbUp_xts<- xts(bolbands[,"BBup"],order.by=date_s,frequency=252)
bbDn_xts<- xts(bolbands[,"BBdn"],order.by=date_s,frequency=252)
chart_xts <- cbind(close_xts,mavg_xts,bbUp_xts,bbDn_xts)

dygraph(chart_xts,ylab="Value", main="IV BB")  %>%
  dySeries("..1",label="Close") %>%
  dySeries(c("..3","..2","..4"), label = "BB") %>%
  dyAxis("y", label = "Value") %>%
  dyRangeSelector()


##dygraph pctB and BWidth
pctB_xts<- xts(pctB,order.by=date_s,frequency=252)
BWdth_xts<- xts(BWdth,order.by=date_s,frequency=252)

bbind_xts<- cbind(pctB_xts,BWdth_xts)

dygraph(bbind_xts,ylab="pctB and BWdth", 
        main=paste(Underying_Symbol_G," BB Indicator",sep=""))  %>%
  dySeries("..1",label="pctB") %>%
  #### y2 axix
  dySeries("..2",label="BWdth",axis = 'y2') %>%
  dyRangeSelector()

#graphing Comparison of HV and IV

# historical Volatilty vector
histVol=createHistVol(Close=TSdata2[,"Close"],
                      TS_DATA_NUM=nrow(TSdata2),
                      MV_AVRAGE_DAYNUM=30)

date_s = as.POSIXct(strptime(TSdata[,"Date"], 
                             format="%Y/%m/%d",tz="UTC"))
date_s=date_s[1:nrow(histVol)]

#time series
HV_xts<- xts(histVol,order.by=date_s,frequency=252)
close_xts<- xts(TSdata[,"Close"][1:nrow(histVol)],order.by=date_s,frequency=252)
volcomp_xts<- cbind(close_xts,HV_xts)

dygraph(volcomp_xts,ylab="IV and HV", 
        main=paste(Underying_Symbol_G," Volatility Comparison",sep=""))  %>%
  dySeries("..1",label="IV") %>%
  dySeries("HV",label="HV") %>%
  #dyOptions(colors = c("blue","brown")) %>%
  dyRangeSelector()

#HVIVR
HVIVR=histVol/TSdata[,"Close"][1:nrow(histVol)]
colnames(HVIVR)=c("HVIVR")
AHVIVR=createSMA(HVIVR[,"HVIVR"],nrow(HVIVR),30)
colnames(AHVIVR)=c("AHVIVR")

date_s = as.POSIXct(strptime(TSdata[,"Date"], 
                             format="%Y/%m/%d",tz="UTC"))
date_s=date_s[1:nrow(AHVIVR)]

HVIVR_xts<- xts(HVIVR[,"HVIVR"][1:nrow(AHVIVR)],order.by=date_s,frequency=252)
AHVIVR_xts<- xts(AHVIVR,order.by=date_s,frequency=252)
volcomp_xts<- cbind(HVIVR_xts,AHVIVR_xts)

dygraph(volcomp_xts,ylab="HVIVR and AHVIVR", main="HVIVR and AHVIVR")  %>%
  dySeries("..1",label="HVIVR") %>%
  dySeries("AHVIVR",label="AHVIVR") %>%
  #dyOptions(colors = c("blue","brown")) %>%
  dyRangeSelector()

#graphing HV only
dygraph(HV_xts,ylab="IV", 
        main=paste(Underying_Symbol_G," Historical Volatility",sep=""))  %>%
  dySeries("HV",label="HV") %>%
  #dyOptions(colors = c("blue","brown")) %>%
  dyRangeSelector()

