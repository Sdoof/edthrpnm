library(RQuantLib)
library(ggplot2)
library(plyr)
library(dplyr)

#Config File
ConfigFileName_G="ConfigParameters.csv"
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"

ConfigParameters<-read.table(paste(DataFiles_Path_G,ConfigFileName_G,sep=""),
                             row.names=1, comment.char="#",header=T,stringsAsFactors=F,sep=",")

#MAX ExpToDate for Skew Regression
SkewRegressionTimeToExpDateMin<-0.9
SkewRegressionTimeToExpDateMax<-2.2

#We get regression only past this day. Currently reflected on Skew only.
#should apply Vcone, etc.
RegressionDateBackDaysMax<-10

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

#option chain Output File
OpchainOutFileName=paste("_OPChain_Skew_",Sys.Date(),".csv",sep="")

#read data file
rf_<-paste(DataFiles_Path_G,Underying_Symbol_G,"_OPChain_Pos.csv",sep="")
opch<-read.table(rf_,header=T,sep=",",nrows=50000)
rf_<-paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="")
histIV<-read.table(rf_,header=T,sep=",",nrows=1000)
rm(rf_)

##START Create complete opch
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
#opch %>% dplyr::group_by(Date,ExpDate,TYPE) %>% dplyr::filter(HowfarOOM>0) %>% 
  #Get the OrigIV where HowfarOOM is minimum, ie. IV at ATM.
#  dplyr::summarise(num=n(),OOM=min(abs(HowfarOOM)),min.i=which.min(abs(HowfarOOM)),ATMIV=OrigIV[which.min(abs(HowfarOOM))]
#                   ,TimeToExpDate=TimeToExpDate[which.min(abs(HowfarOOM))],IVIDX=IVIDX[which.min(abs(HowfarOOM))]) %>% 
#  as.data.frame() -> atmiv
#atmiv %>% dplyr::select(Date,ExpDate,TYPE,ATMIV,IVIDX,TimeToExpDate) %>% as.data.frame() -> atmiv
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

#Calculate Moneyness.Nm using just merged ATMIV
displace<-log(atmiv$Moneyness.Frac)/atmiv$ATMIV/sqrt(atmiv$TimeToExpDate)
atmiv$displace<-displace

#merge
opch <- merge(opch,
              atmiv %>% dplyr::select(Date,ExpDate,TYPE,ATMIV,displace) %>% as.data.frame(),
#              by.x=c("Date","ExpDate","TYPE"),by.y=c("Date","ExpDate","TYPE"),all.x=T)
              by.x=c("Date","ExpDate","TYPE","ATMIV"),by.y=c("Date","ExpDate","TYPE","ATMIV"),all=T)

#sorting
atmiv %>% dplyr::arrange(desc(TYPE),as.Date(ExpDate,format="%Y/%m/%d"),as.Date(Date,format="%Y/%m/%d")) -> atmiv
opch %>% dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) -> opch

#
opch$Moneyness.Nm<-log(opch$Moneyness.Frac)/opch$ATMIV/sqrt(opch$TimeToExpDate)-opch$displace

##
# Volatility Skew Regression 

# Nmlzd Skew
getNmlzdSkewVplot<-function(){
  #Complete Opchain. Using OOM options. By Call-Put parity, ITM IV is supposed to be the same as OOM IV.
  opch %>% dplyr::filter(OrigIV/ATMIV<5.0) %>% dplyr::filter(OrigIV/ATMIV>0.1) %>%
    dplyr::filter(HowfarOOM>=0) %>% dplyr::filter(TimeToExpDate>TimeToExp_Limit_Closeness_G) -> vplot
  #filter by TimeToExpDate
  vplot %>% dplyr::filter(TimeToExpDate<=SkewRegressionTimeToExpDateMax) %>% dplyr::filter(TimeToExpDate>=SkewRegressionTimeToExpDateMin) -> vplot
  #filter by recent data. Take only past RegressionDateBackDaysMax dats
  vplot %>% filter(as.Date(Date,format="%Y/%m/%d")>= max(as.Date(Date,format="%Y/%m/%d"))-RegressionDateBackDaysMax) -> vplot
  
  return(vplot)
}

vplot<-getNmlzdSkewVplot()
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV),size=TimeToExpDate/2,colour=Date))+geom_point(alpha=0.2))
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV)))+geom_point(alpha=0.2))
models <- (get.skew.regression.Models(vplot,regtype=5,df=7))

#5. (regtype) smooth splines
get.predicted.skew(models,regtype=5,xmin=-1,x_by=0)
(predict.c<-get.predicted.skew(models,regtype=5,xmin=-2.0,xmax=1.5))
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV)))+geom_point(alpha=0.2)+
   geom_line(data=data.frame(Moneyness.Nm=predict.c$x,IV2ATMIV=predict.c$y),aes(Moneyness.Nm,IV2ATMIV),color="red"))

#Optimize Moneyness.Nm using just merged ATMIV
save.Skew(models)
#load test
load.Skew()
SkewModel

#using SkewModel, adjust ATMIV to reflect the differnce between Strike and UDLY
adjustATMIV <- function(atmiv){
  displacement<-log(atmiv$Moneyness.Frac)/atmiv$ATMIV/sqrt(atmiv$TimeToExpDate)
  smileCurve<-get.predicted.spline.skew(SkewModel,displacement)
  ATMIV_adjst<-atmiv$ATMIV/smileCurve
  
  return(ATMIV_adjst)
}

#ATMIV adjusted to continue following regressions.
ATMIV_adj<-adjustATMIV(atmiv)
atmiv$ATMIV<-ATMIV_adj

#rm(SkewModel)
rm(models,vplot,predict.c)


##
# Volatility Cone and ATMIV%Chg/IVIDX%Chg Analysis and Regression.  --------------

#
# Getting and Creating atmiv.vcone.anal to analyze vcone and to model ATMIV%Chg/IVIDX%Chg

# inner function
# called from do above. This function is called for each grouped data frame.
makeVconAnalDF<- function(atmiv){
  atmiv %>% dplyr::mutate(ATMIV.s=dplyr::lead(atmiv$ATMIV,1)) -> atmiv
  atmiv %>% dplyr::mutate(IVIDX.s=dplyr::lead(atmiv$IVIDX,1)) -> atmiv
  atmiv %>% dplyr::mutate(TimeToExpDate.s=dplyr::lead(atmiv$TimeToExpDate,1)) -> atmiv
  atmiv %>% dplyr::mutate(ATMIV.f=ATMIV.s/ATMIV
                          ,IVIDX.f=IVIDX.s/IVIDX
                          ,TimeToExpDate.d=TimeToExpDate-TimeToExpDate.s) -> atmiv
  atmiv$ATMIV.s<-atmiv$IVIDX.s<-atmiv$TimeToExpDate.s<-NULL
  
  #filter out data whose busuness days interval are more than time_max days
  # if you want to exclude whose intervals are more than 6 days, 
  #  then set time_max<- 6.2 etc to avoid unncessary boundary misconfigurations.
  time_max <- 7.2
  timeIntervalExclude_max=(1/(252/12))*time_max
  atmiv %>% dplyr::filter(TimeToExpDate.d<=timeIntervalExclude_max) -> atmiv
  atmiv
}

#Row "EachDF" 's members are dataframes. Composite Object Types such as Dataframe, List.
#Thay can also be set as a data frame member. 
#     note . referes to each grouped partial data frame.
atmiv %>% group_by(ExpDate,TYPE) %>% do(EachDF=makeVconAnalDF(.)) -> atmiv.vcone.anal
atmiv.vcone.anal %>% dplyr::arrange(desc(TYPE),ExpDate) -> atmiv.vcone.anal
atmiv.vcone.eachDF<-atmiv.vcone.anal$EachDF
atmiv.vcone.bind<-NULL
for(i in 1:length(atmiv.vcone.eachDF)){
  if(i==1){
    atmiv.vcone.bind <- as.data.frame(atmiv.vcone.eachDF[i])
  }
  else{
    atmiv.vcone.bind<-rbind(atmiv.vcone.bind,as.data.frame(atmiv.vcone.eachDF[i]))
  }
  
}

#atmiv.vcone.anal: from nested data.frame to data.frame: type changed.
atmiv.vcone.anal<-NULL
atmiv.vcone.anal<-atmiv.vcone.bind
rm(i,atmiv.vcone.eachDF,atmiv.vcone.bind)

##
# Vcone Regression

#  Put Vcone IV is normalized 
#Creating vcone.
vcone<-make.vcone.df(atmiv=atmiv,type=1)
(ggplot(vcone,aes(x=Month,y=IV2IDX.nm,colour=TYPE))+geom_point())
# Regression of PUT vcone
#   5.smooth spline
#(predict(smooth.spline(vcone$Month,vcone$IV2IDX.nm,df=3),x=2))
model.ss<-smooth.spline(vcone$Month,vcone$IV2IDX.nm,df=3)
(predict.c <- predict(model.ss,x=seq(0,max(vcone$Month),by=0.1)))
(ggplot(vcone,aes(x=Month,y=IV2IDX.nm,colour=TYPE))+geom_point()+
   geom_line(data=data.frame(Month=predict.c$x,IV2IDX.nm=predict.c$y,TYPE=OpType_Put_G),aes(Month,IV2IDX.nm)))

save.VCone(model=model.ss,optype=OpType_Put_G)

rm(vcone,predict.c,model.ss)

#  Call VCone IV is normalized 
#Creating vcone.
vcone<-make.vcone.df(atmiv=atmiv,type=-1)
(ggplot(vcone,aes(x=Month,y=IV2IDX.nm,colour=TYPE))+geom_point())
# Regression of Call vcone
#   5.smooth spline
#(predict(smooth.spline(vcone$Month,vcone$IV2IDX.nm,df=3),x=2))
model.ss<-smooth.spline(vcone$Month,vcone$IV2IDX.nm,df=3)
(predict.c <- predict(model.ss,x=seq(0,max(vcone$Month),by=0.1)))
(ggplot(vcone,aes(x=Month,y=IV2IDX.nm,colour=TYPE))+geom_point()+
   geom_line(data=data.frame(Month=predict.c$x,IV2IDX.nm=predict.c$y,TYPE=-1),aes(Month,IV2IDX.nm)))

save.VCone(model=model.ss,optype=OpType_Call_G)
#loat test
load.VCone(optype=OpType_Put_G)
PutVCone
load.VCone(optype=OpType_Call_G)
CallVCone
rm(PutVCone,CallVCone)

rm(vcone,predict.c,model.ss)

##
#  Regression of ATM IV Volatility Change to IVIDX as to Time -------

#  Put IV Change to IVIDX Up and Down
#Up and Down change
vchg<-make.vchg.df(vcone=atmiv.vcone.anal,type=1)
(ggplot(vchg,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point())

vchg %>% filter(IVIDX.f>=1.0) -> vchg_plus
(ggplot(vchg_plus,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point())

vchg %>% filter(IVIDX.f<1.0) -> vchg_mns
#filter outlier
vchg_mns %>% dplyr::filter(VC.f>0.90) -> vchg_mns
(ggplot(vchg_mns,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point())

#Regression
#    5.smooth spline
vchg_t<-vchg_plus
model.ss<-smooth.spline(vchg_t$TimeToExpDate,vchg_t$VC.f,df=3)
(predict.c <- predict(model.ss,x=seq(0,max(vchg_t$TimeToExpDate),by=0.1)))
(ggplot(vchg_t,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point()+
   geom_line(data=data.frame(TimeToExpDate=predict.c$x,VC.f=predict.c$y,TYPE=1),aes(TimeToExpDate,VC.f)))
rm(vchg_t)
save.IVChg(model.ss,OpType_Put_G,10)

vchg_t<-vchg_mns
model.ss<-smooth.spline(vchg_t$TimeToExpDate,vchg_t$VC.f,df=3)
(predict.c <- predict(model.ss,x=seq(0,max(vchg_t$TimeToExpDate),by=0.1)))
(ggplot(vchg_t,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point()+
   geom_line(data=data.frame(TimeToExpDate=predict.c$x,VC.f=predict.c$y,TYPE=1),aes(TimeToExpDate,VC.f)))
rm(vchg_t)
save.IVChg(model.ss,OpType_Put_G,-10)

rm(vchg,vchg_mns,vchg_plus,model.ss,predict.c)

#  Call IV Change to IVIDX Up and Down 
#Up and Down Changes
vchg<-make.vchg.df(vcone=atmiv.vcone.anal,type=-1)
(ggplot(vchg,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point())
vchg %>% filter(IVIDX.f>=1.0) -> vchg_plus
(ggplot(vchg_plus,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point())
vchg %>% filter(IVIDX.f<1.0) -> vchg_mns
(ggplot(vchg_mns,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point())

#regresson
#    5.smooth spline
vchg_t<-vchg_plus
model.ss<-smooth.spline(vchg_t$TimeToExpDate,vchg_t$VC.f,df=3)
(predict.c <- predict(model.ss,x=seq(0,max(vchg_t$TimeToExpDate),by=0.1)))
(ggplot(vchg_t,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point()+
   geom_line(data=data.frame(TimeToExpDate=predict.c$x,VC.f=predict.c$y,TYPE=-1),aes(TimeToExpDate,VC.f)))
save.IVChg(model.ss,OpType_Call_G,10)

vchg_t<-vchg_mns
model.ss<-smooth.spline(vchg_t$TimeToExpDate,vchg_t$VC.f,df=3)
(predict.c <- predict(model.ss,x=seq(0,max(vchg_t$TimeToExpDate),by=0.1)))
(ggplot(vchg_t,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point()+
   geom_line(data=data.frame(TimeToExpDate=predict.c$x,VC.f=predict.c$y,TYPE=-1),aes(TimeToExpDate,VC.f)))
rm(vchg_t)
save.IVChg(model.ss,OpType_Call_G,-10)

#load test
load.IVChg(OpType_Put_G,10)
PutIVChgUp
load.IVChg(OpType_Put_G,-10)
PutIVChgDown
load.IVChg(OpType_Call_G,10)
CallIVChgUp
load.IVChg(OpType_Call_G,-10)
CallIVChgDown
rm(PutIVChgUp,PutIVChgDown,CallIVChgUp,CallIVChgDown)
rm(vchg,vchg_mns,vchg_plus,model.ss,predict.c)

##
# Post Process ------

#Writing to a file
wf_<-paste(DataFiles_Path_G,Underying_Symbol_G,OpchainOutFileName,sep="")
write.table(opch,wf_,quote=T,row.names=F,sep=",")
rm(wf_)

rm(atmiv.vcone.anal,atmiv,displace,opch,getNmlzdSkewVplot)
rm(ConfigFileName_G,ConfigParameters,SkewRegressionTimeToExpDateMax,SkewRegressionTimeToExpDateMin)
rm(CALENDAR_G,DataFiles_Path_G,OpType_Call_G,OpType_Put_G,OpchainOutFileName)
rm(TimeToExp_Limit_Closeness_G,Underying_Symbol_G,divYld_G,riskFreeRate_G)
rm(SkewModel,RegressionDateBackDaysMax)
