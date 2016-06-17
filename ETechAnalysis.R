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
TSdata<-read.table(paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep=""),
                   header=T,sep=",",nrows=TS_DATA_NUM)

TSdata2<-read.table(paste(DataFiles_Path_G,Underying_Symbol_G,"_Hist.csv",sep="")
                    ,header=T,sep=",",nrows=TS_DATA_NUM)

##
### Begin Of Function Definitions

###
##  Close to Open Spike Vector
createCloseToOpenSpikeVec <- function(TSdata){
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
  
  ret_names<-c("ivCtOSpikePct","ivCtOSpikeNSd")
  ret_ <- vector("list",length(ret_names))
  ret_[[1]]<-ivCtOSpikePct
  ret_[[2]]<-ivCtOSpikeNSd
  names(ret_)<-ret_names
  ret_
  
  return(ret_)
}

###
## create Bolinger Bands Dataframe

createBBandsDf <- function(TSdata,TS_DATA_NUM,MV_AVRAGE_DAYNUM){
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
  nrow_tech=min(nrow(TSdata[,c("Close")]),nrow(bbands[,c("dn","up","mavg")]))
  
  bbandsDf=data.frame(TSdata[,c("Date","Open","High","Low","Close")][1:nrow_tech,],bbands[,c("dn","up","mavg")][1:nrow_tech,])
  
  rownames(bbandsDf) <- c(1:nrow(bbandsDf))
  
  #delte Factor
  bbandsDf$Date <- as.character(bbandsDf$Date)
  
  return(bbandsDf)
}

##
# retrun BB elements as a Matrix in place of Df
createBB <- function(TSdata,TS_DATA_NUM,MV_AVRAGE_DAYNUM){
  mtrxCol=c(paste("BBmavg",MV_AVRAGE_DAYNUM,sep=""),"BBdn","BBup")
  mtrxColN=length(mtrxCol)
  bbands = matrix(rep(0,(TS_DATA_NUM-MV_AVRAGE_DAYNUM[1])*mtrxColN), nrow = (TS_DATA_NUM-MV_AVRAGE_DAYNUM[1]), ncol = mtrxColN)
  colnames(bbands) <- mtrxCol
  
  for(i in 1:(TS_DATA_NUM-MV_AVRAGE_DAYNUM[1])){
    bbands[i,mtrxCol[1]]=mean(TSdata[,c("Close")][i:(MV_AVRAGE_DAYNUM+i-1)])
    #cat(sd(TSdata[,c("Close")][i:(MV_AVRAGE_DAYNUM+i-1)]),"\n")
    bbands[i,mtrxCol[2]]=bbands[i,mtrxCol[1]]-2*sd(TSdata[,c("Close")][i:(MV_AVRAGE_DAYNUM+i-1)])
    bbands[i,mtrxCol[3]]=bbands[i,mtrxCol[1]]+2*sd(TSdata[,c("Close")][i:(MV_AVRAGE_DAYNUM+i-1)])
  }
  return(bbands)
}

##
# return TR elemets as a Matrix
createTR <- function(TSdata,TS_DATA_NUM){
  mtrxCol=c("TR")
  mtrxColN=length(mtrxCol)
  retMtrx = matrix(rep(0,(TS_DATA_NUM)*mtrxColN), nrow = (TS_DATA_NUM), ncol = mtrxColN)
  colnames(retMtrx) <- mtrxCol
  
  #get ascending ordered vector
  AcdC=rev(TSdata[,c("Close")])
  AcdH=rev(TSdata[,c("High")])
  AcdL=rev(TSdata[,c("Low")])
  
  for(i in 2:TS_DATA_NUM){
    retMtrx[i,mtrxCol[1]]=max(AcdH[i],AcdC[i-1])-min(AcdL[i],AcdC[i-1])
  }
  
  retMtrx[,mtrxCol[1]]=rev(retMtrx[,mtrxCol[1]])
  retMtrx=matrix(retMtrx[-length(retMtrx[,mtrxCol[1]]),],nrow=length(retMtrx[,mtrxCol[1]])-1)
  return(retMtrx)
}

##
# return (Xday)MAVG as a Matrix
createSMA <- function(Data,TS_DATA_NUM,MV_AVRAGE_DAYNUM){
  #Data=TSdata[,c("Close")]
  mtrxCol=c(paste("SMA",MV_AVRAGE_DAYNUM,sep=""))
  mtrxColN=length(mtrxCol)
  retMtrx = matrix(rep(0,(TS_DATA_NUM-MV_AVRAGE_DAYNUM[1])*mtrxColN), nrow = (TS_DATA_NUM-MV_AVRAGE_DAYNUM[1]), ncol = mtrxColN)
  colnames(retMtrx) <- mtrxCol
  
  for(i in 1:(TS_DATA_NUM-MV_AVRAGE_DAYNUM[1])){
    retMtrx[i,mtrxCol[1]]=mean(Data[i:(MV_AVRAGE_DAYNUM+i-1)])
  }
  return(retMtrx)
}

##
# create 1st order difference as a Matrix
createDiff <- function(Data,TS_DATA_NUM){
  #Data=TSdata[,c("Close")]
  mtrxCol=c("Diff")
  mtrxColN=length(mtrxCol)
  retMtrx=matrix(rep(0,(TS_DATA_NUM)*mtrxColN), nrow = (TS_DATA_NUM), ncol = mtrxColN)
  colnames(retMtrx) <- mtrxCol
  
  retMtrx[1,mtrxCol[1]]=Data[2]-Data[1]
  for(i in 2:(TS_DATA_NUM-1)){
    retMtrx[i,mtrxCol[1]]=(Data[i]-Data[i-1])/2+(Data[i+1]-Data[i])/2
  }
  retMtrx[TS_DATA_NUM,mtrxCol[1]]=Data[TS_DATA_NUM]-Data[TS_DATA_NUM-1]
  
  return(retMtrix)
}

###
##  create Historical Volatility Matrix
createHistVol<-function(Close,TS_DATA_NUM,MV_AVRAGE_DAYNUM #,CloseChg
){
  histVol = matrix(rep(0,(TS_DATA_NUM-MV_AVRAGE_DAYNUM[1])), nrow = (TS_DATA_NUM-MV_AVRAGE_DAYNUM[1]), ncol = 1)
  colnames(histVol) <- c("HV")
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

## Main routine
#

# Close To Open SPike vector
tmp<-createCloseToOpenSpikeVec(TSdata)
ivCtOSpikePct<-tmp$ivCtOSpikePct
ivCtOSpikeNSd<-tmp$ivCtOSpikeNSd

# Bolinger Bands Dataframe
bbDf=createBBandsDf(TSdata,TS_DATA_NUM,MV_AVRAGE_DAYNUM)

# pctB  and BWidth
pctB=(bbDf$Close-bbDf$dn)/(bbDf$up-bbDf$dn)
BWdth=(bbDf$up-bbDf$dn)/(bbDf$mavg)

# historical Volatilty vector
histVol=createHistVol(Close=TSdata2$Close,TS_DATA_NUM=TS_DATA_NUM,MV_AVRAGE_DAYNUM=MV_AVRAGE_DAYNUM#,TSdata2$PC1dCtC
)

# Data format standarizee dor dygraph
date_s = as.POSIXct(strptime(bbDf$Date, 
                             format="%Y/%m/%d",tz="UTC"))

##dygraph BB
close_xts<- xts(bbDf$Close,order.by=date_s,frequency=252)
mavg_xts<- xts(bbDf$mavg,order.by=date_s,frequency=252)
bbUp_xts<- xts(bbDf$up,order.by=date_s,frequency=252)
bbDn_xts<- xts(bbDf$dn,order.by=date_s,frequency=252)
chart_xts <- cbind(close_xts,mavg_xts,bbUp_xts,bbDn_xts)

dygraph(chart_xts,ylab="Value", 
        main=paste(Underying_Symbol_G," IV Analysis",sep=""))  %>%
  dySeries("..1",label="Close") %>%
  #dySeries("..3",label="UpB") %>%
  #dySeries("..2",label="MAVG") %>%
  #dySeries("..4",label="LowB") %>%
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
  dySeries("..2",label="BWdth",axis = 'y2') %>%
  #dyOptions(colors = c("blue","brown")) %>%
  dyRangeSelector()

#graphing Comparison of HV and IV
HV_xts<- xts(histVol,order.by=date_s,frequency=252)
close_xts<- xts(bbDf$Close,order.by=date_s,frequency=252)
volcomp_xts<- cbind(close_xts,HV_xts)

dygraph(volcomp_xts,ylab="IV and HV", 
        main=paste(Underying_Symbol_G," Volatility Comparison",sep=""))  %>%
  dySeries("..1",label="IV") %>%
  dySeries("HV",label="HV") %>%
  #dyOptions(colors = c("blue","brown")) %>%
  dyRangeSelector()

#graphing HV only
dygraph(HV_xts,ylab="IV", 
        main=paste(Underying_Symbol_G," Historical Volatility",sep=""))  %>%
  dySeries("HV",label="HV") %>%
  #dyOptions(colors = c("blue","brown")) %>%
  dyRangeSelector()

