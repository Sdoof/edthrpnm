library(RQuantLib)
library(ggplot2)
library(dplyr)
rm(list=ls())
source('./ESourceRCode.R',encoding = 'UTF-8')
source('./EDataProvisionLib.R',encoding = 'UTF-8')

#switch: only for today or multiple days for skew calculation
ProcessFileName=paste("_OPChain_PreForSkew.csv",sep="")
isSkewCalc=T
#set TRUE if this opchain is for Future Option 
isFOP=F
#set TRUE if this is today's new position, or (already holding position) set FALSE,
isNewPosition=T
#set TRUE if you filter the position,otherwise set FALSE
isFiltered=T
#Price is Interpolated or not
isInterpolatedPrice=F
#Saved File Name
TargetFileName=paste("_Positions_Pre_",format(Sys.time(),"%Y%b%d_%H%M%S"),".csv",sep="")
#for recording the ATMIV adjusted opchain, set isSkewCalc=F AND isFiltered=F
if(isSkewCalc){
  TargetFileName=paste("_OPChain_Pos_",format(Sys.time(),"%Y%b%d_%H%M%S"),".csv",sep="")
}else if(isFiltered==F)
  TargetFileName=paste("_OPChain_RECORD_",format(Sys.time(),"%Y%b%d_%H%M%S"),".csv",sep="")

#create Option Chain Container
if(!isFOP){
  opchain<-makeOpchainContainer()
}else{
  opchain <- makeFOPChainContainer()
  cat("(:divYld_G)",divYld_G,"\n")
}

na.omit(opchain)->opchain

#inconsistent data purged
opchain %>% filter(Price!=0) -> opchain
opchain$Strike=as.numeric(opchain$Strike)
opchain$TYPE=as.numeric(opchain$TYPE)
opchain %>% filter((Strike-UDLY)*TYPE<Price) -> opchain

#spread<ASK*k
k<-0.4
opchain$Ask=as.numeric(opchain$Ask)
opchain$Bid=as.numeric(opchain$Bid)
opchain %>% filter(!((Ask-Bid)>(Ask*k))) -> opchain
rm(k)

#Implied Volatility Set
delete<-c(-1)
N<-nrow(opchain)
for(i in 1:N){
  tryCatch(a<-set.IVOrig(xT=opchain[i,]),
           error=function(e){
             message(e)
             print(i)
             delete<<-c(delete,-i)
           })
}
#要素を削除
(delete)
delete<-delete[-1]
(delete)
nrow(opchain)
nrow(opchain[delete,])
if(length(delete)>0)
  opchain<-opchain[delete,]
nrow(opchain)
rm(delete,N,i,a)

#IVの計算とOption PriceとGreeksの設定
opchain$OrigIV<-set.IVOrig(xT=opchain)
tmp<-set.EuropeanOptionValueGreeks(opchain)
opchain$Price<-tmp$Price
opchain$Delta<-tmp$Delta
opchain$Gamma<-tmp$Gamma
opchain$Vega<-tmp$Vega
opchain$Theta<-tmp$Theta
opchain$Rho<-tmp$Rho

#opchain$Vomma<-get.EuropeanOptionVomma(opchain)
rm(tmp)
#renumber row names
rownames(opchain) <- c(1:nrow(opchain))


#not adjusted atmiv
# atmiv_na=makeNotAdjustedBySkewATMiv(opchain)

## opchain is created
opchain<-makePosition(opchain,isSkewCalc=isSkewCalc)

#just check the result before going throught the rest
if(isFiltered)
  opchain_check<-filterPosition(opchain,TARGET_EXPDATE=TARGET_EXPDATE,TARGET_EXPDATE_FRONT=TARGET_EXPDATE_FRONT,TARGET_EXPDATE_BACK=TARGET_EXPDATE_BACK)

if(!isSkewCalc){
  if(isNewPosition)
    if(isFiltered)
      if(!isFOP)
        opchain<-filterPosition(opchain,TARGET_EXPDATE=TARGET_EXPDATE,TARGET_EXPDATE_FRONT=TARGET_EXPDATE_FRONT,TARGET_EXPDATE_BACK=TARGET_EXPDATE_BACK)
      else
        opchain<-filterPosition(opchain,TARGET_EXPDATE=TARGET_EXPDATE,TARGET_EXPDATE_FRONT=TARGET_EXPDATE_FRONT,TARGET_EXPDATE_BACK=TARGET_EXPDATE_BACK)
}

#select and sort
opchain %>% dplyr::select(Date,ExpDate,TYPE,Strike,ContactName,Position,UDLY,Price,
                          Delta,Gamma,Vega,Theta,Rho,OrigIV,ATMIV,IVIDX,
                          HowfarOOM,TimeToExpDate,Moneyness.Nm) %>% 
  dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) -> opchain
opchain$Position<-ifelse(is.na(opchain$Position), 0, opchain$Position)


#Write to a file (RUT_Positions_Pre)
wf_<-paste(DataFiles_Path_G,Underying_Symbol_G,TargetFileName,sep="")
write.table(opchain,wf_,quote=T,row.names=F,sep=",")

rm(list=ls())
