library(RQuantLib)
library(ggplot2)
library(plyr)
library(dplyr)

###Global 変数及び定数.
#Calendar
CALENDAR_G="UnitedStates/NYSE"

# Possibly read from File
riskFreeRate_G=0.01
divYld_G=0.0

#Definition
OpType_Put_G=1
OpType_Call_G=-1
#Skewness Calculation

TimeToExp_Limit_Closeness_G=0.3
#File
Underying_Symbol_G="RUT"
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"
ResultFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\ResultData\\"

#switch: only for today or multiple days for skew calculation
ProcessFileName=paste("_OPChain_Pre.csv",sep="")
isSkewCalc=TRUE
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
  histPrc_<-read.table(rf_,header=T,sep=",",nrows=1999)
  histPrc_<-data.frame(Date=histPrc_$Date,UDLY=histPrc_$Close)
  
  #remove not used row
  opch_pr_<-subset(opch_pr_,Volume!=0)
  opch_pr2<-merge(opch_pr_,histPrc_,all.x=T)
  opch_pr_<-opch_pr2;
  
  return(opch_pr_)
}

opchain<-makeOpchainContainer()

#inconsistent data purged
opchain %>% filter(Price!=0) -> opchain
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

##
#  ATMIV IVIDX Moneyness etc. calculation for the opchain to be completed.

makePosition <- function(opch=opchain){
  rf_<-paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="")
  histIV<-read.table(rf_,header=T,sep=",",nrows=1999)
  rm(rf_)
  
  #
  # volatility and moneyness calculation complete
  opch<-opchain
  
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
    dplyr::summarise(num=n(),OOM=min(abs(HowfarOOM)),min.i=which.min(abs(HowfarOOM)),ATMIV=OrigIV[which.min(abs(HowfarOOM))]
                     ,TimeToExpDate=TimeToExpDate[which.min(abs(HowfarOOM))],IVIDX=IVIDX[which.min(abs(HowfarOOM))]) %>% 
    as.data.frame() -> atmiv
  atmiv %>% dplyr::select(Date,ExpDate,TYPE,ATMIV,IVIDX,TimeToExpDate) %>% as.data.frame() -> atmiv
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

filterPosition <- function(opchain){
  ##
  #  Filter Target Ranges
  
  #Only OOM
  opchain %>% dplyr::filter(HowfarOOM>=0) -> opchain
  
  #Calender Spread 
  OOM_Limit<-(0.07)
  opchain %>%  dplyr::filter(ExpDate=="2015/7/17") %>% dplyr::filter(HowfarOOM<0.07)  %>% dplyr::filter((Strike%%10)==0) -> opchain_cal1
  OOM_Limit<-(0.05)
  opchain %>%  dplyr::filter(ExpDate=="2015/8/21") %>% dplyr::filter(HowfarOOM<0.05)  %>% dplyr::filter((Strike%%10)==0) -> opchain_cal2
  
  #Join
  opchain_cal1 %>%  dplyr::full_join(opchain_cal2) %>% 
    dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) -> opchain
  
  return(opchain)
  
}

opchain<-makePosition(opchain)

if(!isSkewCalc){
  opchain<-filterPosition(opchain)
}

#Write to a file (RUT_Positions_Pre)
wf_<-paste(DataFiles_Path_G,Underying_Symbol_G,TargetFileName,sep="")
write.table(opchain,wf_,quote=T,row.names=F,sep=",")
rm(wf_)

rm(opchain)

#Last Cleaning
rm(makeOpchainContainer,makePosition,filterPosition)
rm(CALENDAR_G,riskFreeRate_G,divYld_G,OpType_Put_G,OpType_Call_G,TimeToExp_Limit_Closeness_G)
rm(Underying_Symbol_G,DataFiles_Path_G,ResultFiles_Path_G,ProcessFileName,TargetFileName,isSkewCalc)


