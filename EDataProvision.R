library(RQuantLib)
library(ggplot2)
library(dplyr)

#Config File
ConfigFileName_G="ConfigParameters.csv"
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"

ConfigParameters<-read.table(paste(DataFiles_Path_G,ConfigFileName_G,sep=""),
                             row.names=1, comment.char="#",header=T,stringsAsFactors=F,sep=",")

###Global 変数及び定数.
#Calendar
CALENDAR_G=ConfigParameters["CALENDAR_G",1]

# Possibly read from File
riskFreeRate_G=as.numeric(ConfigParameters["riskFreeRate_G",1])
divYld_G=as.numeric(ConfigParameters["divYld_G",1])

#Definition
OpType_Put_G=as.numeric(ConfigParameters["OpType_Put_G",1])
OpType_Call_G=as.numeric(ConfigParameters["OpType_Call_G",1])
#Skewness Calculation

TimeToExp_Limit_Closeness_G=as.numeric(ConfigParameters["TimeToExp_Limit_Closeness_G",1])
#File
Underying_Symbol_G=ConfigParameters["Underying_Symbol_G",1]
ResultFiles_Path_G=ConfigParameters["ResultFiles_Path_G",1]

#switch: only for today or multiple days for skew calculation
ProcessFileName=paste("_OPChain_Pre.csv",sep="")
isSkewCalc=TRUE
#set TRUE if this is today's new position, or (already holding position) set FALSE,
isNewPosition=TRUE
TargetFileName=paste("_Positions_Pre_",Sys.Date(),".csv",sep="")
#when isSkewCalc==TRUE, just comment out below
if(isSkewCalc)
  TargetFileName=paste("_OPChain_Pos_",Sys.Date(),".csv",sep="")

makeOpchainContainer<-function(){  
  #read data file
  rf_<-paste(DataFiles_Path_G,Underying_Symbol_G,ProcessFileName,sep="")
  opch_pr_<-read.table(rf_,header=T,sep=",")
  opch_pr_$X.Change<-NULL
  opch_pr_$Position<-0
  opch_pr_$Price<-(opch_pr_$Bid+opch_pr_$Ask)/2
  
  opch_pr_$Theta<-opch_pr_$Vega<-opch_pr_$Gamma<-opch_pr_$Delta<-0
  opch_pr_$IV<-opch_pr_$OrigIV<-opch_pr_$Rho<-0
  
  #read historical price
  rf_<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Hist.csv",sep="")
  histPrc_<-read.table(rf_,header=T,sep=",",nrows=1000)
  histPrc_<-data.frame(Date=histPrc_$Date,UDLY=histPrc_$Close)
  
  #remove not used row
  #opch_pr_<-subset(opch_pr_,Volume!=0)
  opch_pr2<-merge(opch_pr_,histPrc_,all.x=T)
  opch_pr_<-opch_pr2;
  
  return(opch_pr_)
}

opchain<-makeOpchainContainer()

#inconsistent data purged
opchain %>% filter(Price!=0) -> opchain
as.numeric(as.character(opchain$UDLY))
opchain %>% filter((Strike-UDLY)*TYPE<Price) -> opchain

#spread<ASK*k
k<-0.4
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
rm(tmp)

rownames(opchain) <- c(1:nrow(opchain))

#Option_Chain_Pos file type has been created.

#using SkewModel, adjust ATMIV to reflect the differnce between Strike and UDLY
adjustATMIV <- function(atmiv){
  
  displacement<-log(atmiv$Moneyness.Frac)/atmiv$ATMIV/sqrt(atmiv$TimeToExpDate)
  smileCurve<-get.predicted.spline.skew(SkewModel,displacement)
  ATMIV_adjst<-atmiv$ATMIV/smileCurve
  
  return(ATMIV_adjst)
}

##
#  ATMIV IVIDX Moneyness etc. calculation for the opchain to be completed.

makePosition <- function(opch){
  rf_<-paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="")
  histIV<-read.table(rf_,header=T,sep=",",nrows=1000)
  rm(rf_)
  
  #
  # volatility and moneyness calculation complete
  #opch<-opchain
  
  opch$Last<-opch$Bid<-opch$Ask<-opch$Ask<-opch$Volume<-opch$OI<-NULL
  #Histrical Volatility(Index). VOlatility % to DN. Column name Close to IVIDX
  histIV<-data.frame(Date=histIV$Date,IVIDX=(histIV$Close/100))
  ##Historical Implied Volatility(index) merge
  opch<-merge(opch,histIV,all.x=T)
  rm(histIV)
  
  #if >1.0 Strike is right(bigger) than UDLY, else(<1.0) left(smaller)
  opch$Moneyness.Frac<-opch$Strike/opch$UDLY
  # As fraction of UDLY. positive indicates OOM, negative ITM.
  opch$HowfarOOM<-(1-opch$Moneyness.Frac)*opch$TYPE
  #As fraction of Implied SD. >0 OOM, <0 ITM
  opch$Moneyness.SDFrac<-(opch$UDLY-opch$Strike)*opch$TYPE/(opch$UDLY*opch$IVIDX)
  
  #construct Time axis for regression. In this case expressed as Month.
  get.busdays.between(start=opch$Date,end=opch$ExpDate)
  bdays_per_month<-252/12
  opch$TimeToExpDate<-get.busdays.between(start=opch$Date,end=opch$ExpDate)/bdays_per_month
  rm(bdays_per_month)
  
  #sort
  opch %>% dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) -> opch
  
  ##END Got complete opch
  
  #Get ATM Implied Volatilities for each option types.
  ##START Create atmiv
  opch %>% dplyr::group_by(Date,ExpDate,TYPE) %>% dplyr::filter(HowfarOOM>0) %>% 
    #Get the OrigIV where HowfarOOM is minimum, ie. IV at ATM.
    dplyr::summarise(num=n(),OOM=min(abs(HowfarOOM)),min.i=which.min(abs(HowfarOOM)),ATMIV=OrigIV[which.min(abs(HowfarOOM))],
                     TimeToExpDate=TimeToExpDate[which.min(abs(HowfarOOM))],IVIDX=IVIDX[which.min(abs(HowfarOOM))],
                     UDLY=UDLY[which.min(abs(HowfarOOM))],Strike=Strike[which.min(abs(HowfarOOM))],
                     Moneyness.Frac=Moneyness.Frac[which.min(abs(HowfarOOM))]) %>% 
    as.data.frame() -> atmiv
  atmiv %>% dplyr::select(Date,ExpDate,TYPE,ATMIV,Strike,UDLY,IVIDX,TimeToExpDate,Moneyness.Frac) %>% as.data.frame() -> atmiv
  
  #sorting
  atmiv %>% dplyr::arrange(desc(TYPE),as.Date(ExpDate,format="%Y/%m/%d"),as.Date(Date,format="%Y/%m/%d")) -> atmiv
  opch %>% dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) -> opch
  
  #adjust AMTIV
  if(isSkewCalc==FALSE){
    load.Skew()
    SkewModel
    #load.Skew("_Put")
    #SkewModel_Put
    #load.Skew(pattern="_Call")
    #SkewModel_Call
    ATMIV_adj<-adjustATMIV(atmiv)
    atmiv$ATMIV<-ATMIV_adj
  }
  
  opch <- merge(opch,
                atmiv %>% dplyr::select(Date,ExpDate,TYPE,ATMIV) %>% as.data.frame(),
                by.x=c("Date","ExpDate","TYPE"),by.y=c("Date","ExpDate","TYPE"),all.x=T)

  #sorting
  atmiv %>% dplyr::arrange(desc(TYPE),as.Date(ExpDate,format="%Y/%m/%d"),as.Date(Date,format="%Y/%m/%d")) -> atmiv
  opch %>% dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) -> opch
  
  #Calculate Moneyness.Nm using just merged ATMIV
  opch$Moneyness.Nm<-log(opch$Moneyness.Frac)/opch$ATMIV/sqrt(opch$TimeToExpDate)
  
  opchain<-opch ; rm(opch)
  rm(atmiv)
  return(opchain)
}

filterPosition <- function(opchain,HowfarOOM_MIN=-0.5,OOM_Limit_V=c(0.08,0.08)){
  ##
  #  Filter Target Ranges
  
  #Only OOM
  opchain %>% dplyr::filter(HowfarOOM>=HowfarOOM_MIN) -> opchain
  
  #Calender Spread 
  OOM_Limit<-(OOM_Limit_V[1])
  opchain %>%  dplyr::filter(ExpDate=="2015/10/15") %>% dplyr::filter(HowfarOOM<OOM_Limit)  %>% dplyr::filter((Strike%%10)==0) -> opchain_cal1
  OOM_Limit<-(OOM_Limit_V[2])
  opchain %>%  dplyr::filter(ExpDate=="2015/11/19") %>% dplyr::filter(HowfarOOM<OOM_Limit)  %>% dplyr::filter((Strike%%10)==0) -> opchain_cal2
  
  #Join
  opchain_cal1 %>%  dplyr::full_join(opchain_cal2) %>% 
    dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) -> opchain
  
  return(opchain)
  
}

opchain<-makePosition(opchain)

#opchain %>% dplyr::select(-(contains('Frac',ignore.case=TRUE)),-(IV),-(Change)) -> opchain

if(!isSkewCalc){
  if(isNewPosition)
    opchain<-filterPosition(opchain)
  else {
    opchain<-filterPosition(opchain,HowfarOOM_MIN=-0.3)
  }
}

#select and sort
opchain %>% select(Date,ExpDate,TYPE,Strike,ContactName,Position,UDLY,Price,
                   Delta,Gamma,Vega,Theta,Rho,OrigIV,ATMIV,IVIDX,
                   HowfarOOM,TimeToExpDate,Moneyness.Nm) %>% 
  arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) -> opchain
opchain$Position<-ifelse(is.na(opchain$Position), 0, opchain$Position)

#Write to a file (RUT_Positions_Pre)
wf_<-paste(DataFiles_Path_G,Underying_Symbol_G,TargetFileName,sep="")
write.table(opchain,wf_,quote=T,row.names=F,sep=",")
rm(wf_)

rm(opchain)

#Last Cleaning
rm(makeOpchainContainer,makePosition,filterPosition)
rm(ConfigFileName_G,ConfigParameters)
rm(CALENDAR_G,riskFreeRate_G,divYld_G,OpType_Put_G,OpType_Call_G,TimeToExp_Limit_Closeness_G)
rm(Underying_Symbol_G,DataFiles_Path_G,ResultFiles_Path_G,ProcessFileName,TargetFileName,isSkewCalc,isNewPosition)
rm(SkewModel)
