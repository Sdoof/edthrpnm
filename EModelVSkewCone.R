library(RQuantLib)
library(ggplot2)
library(plyr)
library(dplyr)
rm(list=ls())
source('./ESourceRCode.R',encoding = 'UTF-8')

#MAX ExpToDate for Skew Regression
SkewRegressionTimeToExpDateMin<-1.6
SkewRegressionTimeToExpDateMax<-3.5

#We get regression only past this day. Currently reflected on Skew only.
#should apply Vcone, etc.
RegressionDateBackDaysMax<-0

#option chain Output File
OpchainOutFileName=paste("_OPChain_Skew_",Sys.Date(),".csv",sep="")

#read data file
rf_<-paste(DataFiles_Path_G,Underying_Symbol_G,"_OPChain_Pos.csv",sep="")
opch<-read.table(rf_,header=T,skipNul=T,stringsAsFactors=F,sep=",")
rf_<-paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="")
histIV<-read.table(rf_,header=T,stringsAsFactors=F,sep=",")
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
##START to Create atmiv
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
getNmlzdSkewMoneynessVplot<-function(oom_lim){
  #Complete Opchain. Using OOM options. By Call-Put parity, ITM IV is supposed to be the same as OOM IV.
  opch %>% dplyr::filter(OrigIV/ATMIV<5.0) %>% dplyr::filter(OrigIV/ATMIV>0.1) %>%
    dplyr::filter(HowfarOOM>=oom_lim) %>% dplyr::filter(TimeToExpDate>TimeToExp_Limit_Closeness_G) -> vplot
  #filter by TimeToExpDate
  vplot %>% dplyr::filter(TimeToExpDate<=SkewRegressionTimeToExpDateMax) %>% dplyr::filter(TimeToExpDate>=SkewRegressionTimeToExpDateMin) -> vplot
  #filter by recent data. Take only past RegressionDateBackDaysMax dats
  vplot %>% dplyr::filter(as.Date(Date,format="%Y/%m/%d")>= max(as.Date(Date,format="%Y/%m/%d"))-RegressionDateBackDaysMax) -> vplot
  
  return(vplot)
}

getNmlzdSkewTypEVplot<-function(op_right){
  #Complete Opchain. Using OOM options. By Call-Put parity, ITM IV is supposed to be the same as OOM IV.
  opch %>% dplyr::filter(OrigIV/ATMIV<5.0) %>% dplyr::filter(OrigIV/ATMIV>0.1) %>%
    dplyr::filter(TYPE==op_right) %>% dplyr::filter(TimeToExpDate>TimeToExp_Limit_Closeness_G) -> vplot
  #filter by TimeToExpDate
  vplot %>% dplyr::filter(TimeToExpDate<=SkewRegressionTimeToExpDateMax) %>% dplyr::filter(TimeToExpDate>=SkewRegressionTimeToExpDateMin) -> vplot
  #filter by recent data. Take only past RegressionDateBackDaysMax dats
  vplot %>% dplyr::filter(as.Date(Date,format="%Y/%m/%d")>= max(as.Date(Date,format="%Y/%m/%d"))-RegressionDateBackDaysMax) -> vplot
  
  return(vplot)
}

#Only OOM for Put/Call
vplot<-getNmlzdSkewMoneynessVplot(0)
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV),colour=TimeToExpDate))+geom_point(alpha=0.2))
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV)))+geom_point(alpha=0.2))
#smooth splines
models <- (get.skew.regression.Models(vplot,regtype=5,df=7))
get.predicted.skew(models,regtype=5,xmin=-1,x_by=0)
(predict.c<-get.predicted.skew(models,regtype=5,xmin=-3.0,xmax=1.5))
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV),colour=TimeToExpDate))+geom_point(alpha=0.2)+
    geom_line(data=data.frame(Moneyness.Nm=predict.c$x,IV2ATMIV=predict.c$y),aes(Moneyness.Nm,IV2ATMIV),color="red"))
#save and load model
save.Skew(models)
#load test
load.Skew()
SkewModel

#All moneyness Put/Call combined. just for info
vplot<-getNmlzdSkewMoneynessVplot(-3)
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV),size=TimeToExpDate/2,colour=TYPE))+geom_point(alpha=0.2))
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV)))+geom_point(alpha=0.2))
models <- (get.skew.regression.Models(vplot,regtype=5,df=7))
#5. (regtype) smooth splines
get.predicted.skew(models,regtype=5,xmin=-1,x_by=0)
(predict.c<-get.predicted.skew(models,regtype=5,xmin=-3.0,xmax=1.5))
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV)))+geom_point(alpha=0.2)+
    geom_line(data=data.frame(Moneyness.Nm=predict.c$x,IV2ATMIV=predict.c$y),aes(Moneyness.Nm,IV2ATMIV),color="red"))
#save and load model
save.Skew(models,"_Combined")
#load test
load.Skew(pattern="_Combined")
SkewModel_Combined

#used for later for Put Skew
predict_combined=predict.c
vplot %>% dplyr::filter(!(TYPE==OpType_Call_G&Moneyness.Nm<0)) -> vplot_combined_putout

#Only Put
vplot<-getNmlzdSkewTypEVplot(op_right=OpType_Put_G)
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV),size=TimeToExpDate/2,colour=Date))+geom_point(alpha=0.2))
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV)))+geom_point(alpha=0.2))
models <- (get.skew.regression.Models(vplot,regtype=5,df=7))
#smooth spline
get.predicted.skew(models,regtype=5,xmin=-1,x_by=0)
(predict.c<-get.predicted.skew(models,regtype=5,xmin=-3.0,xmax=1.5))
if(min(predict.c$y)<=0){
  #Deep In the Money 
  #minimum value continues
  #tmp=vplot$OrigIV/vplot$ATMIV
  #min(tmp)
  #predict.c$y[which(predict.c$y<=min(tmp))]=min(tmp)
  
  #In the Money, predict_combined model is selected
  #predict.c$y=
  #  (predict_combined$x>0)*predict_combined$y+
  #  (predict.c$x<=0)*predict.c$y
  vplot=vplot_combined_putout
}
# once again
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV),size=TimeToExpDate/2,colour=Date))+geom_point(alpha=0.2))
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV)))+geom_point(alpha=0.2))
models <- (get.skew.regression.Models(vplot,regtype=5,df=7))
#smooth spline
get.predicted.skew(models,regtype=5,xmin=-1,x_by=0)
(predict.c<-get.predicted.skew(models,regtype=5,xmin=-3.0,xmax=1.5))
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV),colour=TimeToExpDate))+geom_point(alpha=0.2)+
    geom_line(data=data.frame(Moneyness.Nm=predict.c$x,IV2ATMIV=predict.c$y),aes(Moneyness.Nm,IV2ATMIV),color="red"))
#save and load model
save.Skew(models,"_Put")
#load test
load.Skew(pattern="_Put")
SkewModel_Put

#Only Call
vplot<-getNmlzdSkewTypEVplot(op_right=OpType_Call_G)
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV),size=TimeToExpDate/2,colour=Date))+geom_point(alpha=0.2))
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV)))+geom_point(alpha=0.2))
#smooth spline
models <- (get.skew.regression.Models(vplot,regtype=5,df=7))
get.predicted.skew(models,regtype=5,xmin=-1,x_by=0)
(predict.c<-get.predicted.skew(models,regtype=5,xmin=-2.0,xmax=1.5))
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV),colour=TimeToExpDate))+geom_point(alpha=0.2)+
    geom_line(data=data.frame(Moneyness.Nm=predict.c$x,IV2ATMIV=predict.c$y),aes(Moneyness.Nm,IV2ATMIV),color="red"))
#save and load model
save.Skew(models,"_Call")
#load test
load.Skew(pattern="_Call")
SkewModel_Call

#rm(SkewModel)
rm(models,vplot,predict.c)

#using SkewModel, adjust ATMIV to reflect the differnce between Strike and UDLY
adjustATMIV <- function(atmiv){
  displacement<-log(atmiv$Moneyness.Frac)/atmiv$ATMIV/sqrt(atmiv$TimeToExpDate)
  smileCurve<-get.predicted.spline.skew(SkewModel,displacement)
  ATMIV_adjst<-atmiv$ATMIV/smileCurve
  return(ATMIV_adjst)
}

##
#    ATMIV adjusted to continue following regressions.
ATMIV_adj<-adjustATMIV(atmiv)
atmiv$ATMIV<-ATMIV_adj

##
#  Regression of ATM behavior

##
# creating ATMIDXIV.f

#atmiv filtering

atmiv->atmiv.org #atmiv.org used later for IV Change to IVIDX Up and Down # atmiv=atmiv.org
tmp=format(as.Date(Sys.time())-1,"%Y/%b/%d");tmp
atmiv %>% dplyr::filter(as.Date(Date,format="%Y/%m/%d")>=as.Date(tmp,format="%Y/%m/%d")) -> atmiv
#create another column
atmiv$ATMIDXIV.f=atmiv$ATMIV/atmiv$IVIDX
#Minimus TimeToExpDate filtering
atmiv %>% dplyr::filter(TimeToExpDate>=1.0) -> atmiv
#separate to PUT and CALL atmiv
atmiv %>% dplyr::filter(TYPE==OpType_Put_G) -> atmiv.put
atmiv %>% dplyr::filter(TYPE==OpType_Call_G) -> atmiv.call
#show each
(ggplot(atmiv,aes(x=TimeToExpDate,y=ATMIDXIV.f,colour=TYPE))+geom_point(size=3))
(ggplot(atmiv.put,aes(x=TimeToExpDate,y=ATMIDXIV.f,colour=Date))+geom_point(size=3))
(ggplot(atmiv.call,aes(x=TimeToExpDate,y=ATMIDXIV.f,colour=Date))+geom_point(size=3))

##
# PUT regression
#smooth spline
model.ss<-smooth.spline(atmiv.put$TimeToExpDate,atmiv.put$ATMIDXIV.f,df=3)
(predict.c <- predict(model.ss,x=seq(0,max(atmiv.put$TimeToExpDate),by=0.1)))
(ggplot(atmiv.put,aes(x=TimeToExpDate,y=ATMIDXIV.f,colour=TYPE))+geom_point(size=3)+
    geom_line(data=data.frame(TimeToExpDate=predict.c$x,ATMIDXIV.f=predict.c$y,TYPE=1),aes(TimeToExpDate,ATMIDXIV.f)))

#linear regression
model.lm<-lm(ATMIDXIV.f~TimeToExpDate, data=atmiv.put)
(model.lm)
(ggplot(atmiv.put,aes(x=TimeToExpDate,y=ATMIDXIV.f,colour=TYPE))+geom_point(size=3)+
    geom_line(data=data.frame(TimeToExpDate=predict.c$x,ATMIDXIV.f=predict.c$y,TYPE=1),aes(TimeToExpDate,ATMIDXIV.f))+
    geom_abline(intercept=model.lm$coefficients[1],slope=model.lm$coefficient[2],color="orange")+
    aes(xmin=TimeToExp_Limit_Closeness_G,ymin=min(model.lm$coefficients[1],min(predict.c$y)))
)

#save model
save.ATMIDXIV.f(model.ss,OpType_Put_G)
load.ATMIDXIV.f(OpType_Put_G)

##
# CALL regression
#smooth spline
model.ss<-smooth.spline(atmiv.call$TimeToExpDate,atmiv.call$ATMIDXIV.f,df=3)
(predict.c <- predict(model.ss,x=seq(0,max(atmiv.call$TimeToExpDate),by=0.1)))
(ggplot(atmiv.call,aes(x=TimeToExpDate,y=ATMIDXIV.f,colour=TYPE))+geom_point(size=3)+
    geom_line(data=data.frame(TimeToExpDate=predict.c$x,ATMIDXIV.f=predict.c$y,TYPE=-1),aes(TimeToExpDate,ATMIDXIV.f)))

#linear regression
model.lm<-lm(ATMIDXIV.f~TimeToExpDate, data=atmiv.call)
(model.lm)
(ggplot(atmiv.call,aes(x=TimeToExpDate,y=ATMIDXIV.f,colour=TYPE))+geom_point(size=3)+
    geom_line(data=data.frame(TimeToExpDate=predict.c$x,ATMIDXIV.f=predict.c$y,TYPE=1),aes(TimeToExpDate,ATMIDXIV.f))+
    geom_abline(intercept=model.lm$coefficients[1],slope=model.lm$coefficient[2],color="orange")+
    aes(xmin=TimeToExp_Limit_Closeness_G,ymin=min(model.lm$coefficients[1],min(predict.c$y)))
)

#save regressed data
save.ATMIDXIV.f(model.ss,OpType_Call_G)
load.ATMIDXIV.f(OpType_Call_G)

#save atmiv.org for another analysis
write.table(atmiv.org,paste(DataFiles_Path_G,Underying_Symbol_G,"-ATMIV-VCONE-ANAL.csv",sep=""),row.names = F,col.names=T,sep=",",append=T)

#read, join atmiv and re-write ATMIV-VCONE-ANAL_Hist
atmiv_hist<-read.table(paste(DataFiles_Path_G,Underying_Symbol_G,"-ATMIV-VCONE-ANAL_Hist.csv",sep=""),header=T,sep=",")
atmiv_hist %>%
  dplyr::arrange(desc(TYPE),as.Date(ExpDate,format="%Y/%m/%d"),as.Date(Date,format="%Y/%m/%d")) %>%
  dplyr::distinct() -> atmiv_hist

atmiv_hist %>% dplyr::full_join(atmiv.org) %>% 
  dplyr::arrange(desc(TYPE),as.Date(ExpDate,format="%Y/%m/%d"),as.Date(Date,format="%Y/%m/%d")) %>%
  dplyr::group_by(Date,ExpDate,TYPE,Strike,UDLY) %>% 
  dplyr::summarise(ATMIV=mean(ATMIV),TimeToExpDate=mean(TimeToExpDate),IVIDX=mean(IVIDX),Moneyness.Frac=mean(Moneyness.Frac),displace=mean(displace)) %>% 
  as.data.frame() %>% 
  dplyr::arrange(desc(TYPE),as.Date(ExpDate,format="%Y/%m/%d"),as.Date(Date,format="%Y/%m/%d")) %>% 
  dplyr::select(Date,ExpDate,TYPE,ATMIV,Strike,UDLY,IVIDX,TimeToExpDate,Moneyness.Frac,displace) -> atmiv_hist

tail(atmiv_hist,n=100)

write.table(atmiv_hist,paste(DataFiles_Path_G,Underying_Symbol_G,"-ATMIV-VCONE-ANAL_Hist.csv",sep=""),row.names = F,col.names=T,sep=",",append=F)

##
# Vcone Regression

#  Put Vcone IV is normalized 
#Creating vcone.
vcone<-make.vcone.df(atmiv=atmiv,type=1)
vcone %>% dplyr::arrange(Month) -> vcone
(ggplot(vcone,aes(x=Month,y=IV2IDX.nm,colour=Month))+geom_point())
# Regression of PUT vcone
#   5.smooth spline
#(predict(smooth.spline(vcone$Month,vcone$IV2IDX.nm,df=3),x=2))
model.ss<-smooth.spline(vcone$Month,vcone$IV2IDX.nm,df=3)
(predict.c <- predict(model.ss,x=seq(0,max(vcone$Month),by=0.1)))
(ggplot(vcone,aes(x=Month,y=IV2IDX.nm,colour=Month))+geom_point()+
    geom_line(data=data.frame(Month=predict.c$x,IV2IDX.nm=predict.c$y,TYPE=OpType_Put_G),aes(Month,IV2IDX.nm)))

save.VCone(model=model.ss,optype=OpType_Put_G)
rm(vcone,predict.c,model.ss)
#  Call VCone IV is normalized 
#Creating vcone.
vcone<-make.vcone.df(atmiv=atmiv,type=-1)
(ggplot(vcone,aes(x=Month,y=IV2IDX.nm,colour=Month))+geom_point())
# Regression of Call vcone
#   5.smooth spline
#(predict(smooth.spline(vcone$Month,vcone$IV2IDX.nm,df=3),x=2))
model.ss<-smooth.spline(vcone$Month,vcone$IV2IDX.nm,df=3)
(predict.c <- predict(model.ss,x=seq(0,max(vcone$Month),by=0.1)))
(ggplot(vcone,aes(x=Month,y=IV2IDX.nm,colour=Month))+geom_point()+
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
# IV Change to IVIDX Up and Down
#
# Getting and Creating atmiv.vcone.anal to analyze vcone and to model ATMIV behavior

# inner function
# called from do above. This function is called for each grouped data frame.
makeVconAnalDF<- function(atmiv){
  atmiv %>% dplyr::mutate(ATMIV.s=dplyr::lead(atmiv$ATMIV,1)) -> atmiv
  atmiv %>% dplyr::mutate(IVIDX.s=dplyr::lead(atmiv$IVIDX,1)) -> atmiv
  atmiv %>% dplyr::mutate(ATMIDXIV.r.s=dplyr::lead(atmiv$ATMIDXIV.r,1)) -> atmiv
  atmiv %>% dplyr::mutate(TimeToExpDate.s=dplyr::lead(atmiv$TimeToExpDate,1)) -> atmiv
  atmiv %>% dplyr::mutate(ATMIV.f=ATMIV.s/ATMIV,
                          IVIDX.f=IVIDX.s/IVIDX,
                          ATMIDXIV.r.f=ATMIDXIV.r.s/ATMIDXIV.r,
                          TimeToExpDate.d=TimeToExpDate-TimeToExpDate.s) -> atmiv
  atmiv$ATMIV.s<-atmiv$IVIDX.s<-atmiv$ATMIDXIV.r.s<-atmiv$TimeToExpDate.s<-NULL
  
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

#Creating atmiv.vcone.anal
#atmiv = atmiv.org
atmiv = atmiv_hist
#create new column
atmiv %>% dplyr::mutate(ATMIDXIV.r=ATMIV/IVIDX) -> atmiv

atmiv %>% dplyr::group_by(ExpDate,TYPE) %>% dplyr::do(EachDF=makeVconAnalDF(.)) -> atmiv.vcone.anal
atmiv.vcone.anal %>% dplyr::arrange(desc(TYPE),as.Date(ExpDate,format="%Y/%m/%d")) -> atmiv.vcone.anal
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

###
# finally atmiv.vcone.anal is created.
#atmiv.vcone.anal: from nested data.frame to data.frame: type changed.
atmiv.vcone.anal<-NULL
atmiv.vcone.anal<-atmiv.vcone.bind
rm(i,atmiv.vcone.eachDF,atmiv.vcone.bind)
atmiv.vcone.anal %>% dplyr::select(Date,ExpDate,TYPE,ATMIV,Strike,UDLY,IVIDX,ATMIDXIV.r,TimeToExpDate,ATMIV.f,IVIDX.f,ATMIDXIV.r.f) %>% 
  dplyr::distinct()-> atmiv.vcone.anal

###
## creating ATMIV_GmChg_{Put/Call}_{Up/Down}

# Put
ATMIV_GmChg_Put<-make.vchg.df(vcone=atmiv.vcone.anal,type=OpType_Put_G)
ATMIV_GmChg_Put$DaysToMaxDate<-as.numeric(max(as.Date(ATMIV_GmChg_Put$Date,format="%Y/%m/%d"))-as.Date(ATMIV_GmChg_Put$Date,format="%Y/%m/%d"))
ATMIV_GmChg_Put %>% dplyr::filter(IVIDX.f>=1.0) -> ATMIV_GmChg_Put_Up
ATMIV_GmChg_Put %>% dplyr::filter(IVIDX.f<1.0) -> ATMIV_GmChg_Put_Down

# VC.f.r = VC.f.r=ATMIV.f/IVIDX.f
# relative ATMIV %change(.f means so) to IVIDX %change(also .f)
# for IVIDX up, VC.f.r > 1 means ATMIV moved bigger than IVIDX
# for IVIDX down, VC.f.r < 1 means ATMIV moved bigger than IVIDX
(ggplot(ATMIV_GmChg_Put,aes(x=TimeToExpDate,y=VC.f.r,colour=DaysToMaxDate))+geom_point())
(ggplot(ATMIV_GmChg_Put_Up,aes(x=TimeToExpDate,y=VC.f.r,colour=DaysToMaxDate))+geom_point())
(ggplot(ATMIV_GmChg_Put_Down,aes(x=TimeToExpDate,y=VC.f.r,colour=DaysToMaxDate))+geom_point())

# Call
ATMIV_GmChg_Call<-make.vchg.df(vcone=atmiv.vcone.anal,type=OpType_Call_G)
ATMIV_GmChg_Call$DaysToMaxDate<-as.numeric(max(as.Date(ATMIV_GmChg_Call$Date,format="%Y/%m/%d"))-as.Date(ATMIV_GmChg_Call$Date,format="%Y/%m/%d"))
ATMIV_GmChg_Call %>% dplyr::filter(IVIDX.f>=1.0) -> ATMIV_GmChg_Call_Up
ATMIV_GmChg_Call %>% dplyr::filter(IVIDX.f<1.0) -> ATMIV_GmChg_Call_Down
#plotting
(ggplot(ATMIV_GmChg_Call,aes(x=TimeToExpDate,y=VC.f.r,colour=DaysToMaxDate))+geom_point())
(ggplot(ATMIV_GmChg_Call_Up,aes(x=TimeToExpDate,y=VC.f.r,colour=DaysToMaxDate))+geom_point())
(ggplot(ATMIV_GmChg_Call_Down,aes(x=TimeToExpDate,y=VC.f.r,colour=DaysToMaxDate))+geom_point())

##
# VC.f.r Regression

# Put
model.ss<-smooth.spline(ATMIV_GmChg_Put_Up$TimeToExpDate,ATMIV_GmChg_Put_Up$VC.f.r,df=3)
(predict.c <- predict(model.ss,x=seq(0,max(ATMIV_GmChg_Put_Up$TimeToExpDate),by=0.1)))
(ggplot(ATMIV_GmChg_Put_Up,aes(x=TimeToExpDate,y=VC.f.r,colour=TYPE))+geom_point()+
    geom_line(data=data.frame(TimeToExpDate=predict.c$x,VC.f.r=predict.c$y,TYPE=1),aes(TimeToExpDate,VC.f.r)))
save.IVChg(model.ss,OpType_Put_G,10)

model.ss<-smooth.spline(ATMIV_GmChg_Put_Down$TimeToExpDate,ATMIV_GmChg_Put_Down$VC.f.r,df=3)
(predict.c <- predict(model.ss,x=seq(0,max(ATMIV_GmChg_Put_Down$TimeToExpDate),by=0.1)))
(ggplot(ATMIV_GmChg_Put_Down,aes(x=TimeToExpDate,y=VC.f.r,colour=TYPE))+geom_point()+
    geom_line(data=data.frame(TimeToExpDate=predict.c$x,VC.f.r=predict.c$y,TYPE=1),aes(TimeToExpDate,VC.f.r)))
save.IVChg(model.ss,OpType_Put_G,-10)

# Call
model.ss<-smooth.spline(ATMIV_GmChg_Call_Up$TimeToExpDate,ATMIV_GmChg_Call_Up$VC.f.r,df=3)
(predict.c <- predict(model.ss,x=seq(0,max(ATMIV_GmChg_Call_Up$TimeToExpDate),by=0.1)))
(ggplot(ATMIV_GmChg_Call_Up,aes(x=TimeToExpDate,y=VC.f.r,colour=TYPE))+geom_point()+
    geom_line(data=data.frame(TimeToExpDate=predict.c$x,VC.f.r=predict.c$y,TYPE=-1),aes(TimeToExpDate,VC.f.r)))
save.IVChg(model.ss,OpType_Call_G,10)

model.ss<-smooth.spline(ATMIV_GmChg_Call_Down$TimeToExpDate,ATMIV_GmChg_Call_Down$VC.f.r,df=3)
(predict.c <- predict(model.ss,x=seq(0,max(ATMIV_GmChg_Call_Down$TimeToExpDate),by=0.1)))
(ggplot(ATMIV_GmChg_Call_Down,aes(x=TimeToExpDate,y=VC.f.r,colour=TYPE))+geom_point()+
    geom_line(data=data.frame(TimeToExpDate=predict.c$x,VC.f.r=predict.c$y,TYPE=-1),aes(TimeToExpDate,VC.f.r)))
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

##
# Analysis

ATMIV_GmChg_Put_Up
ATMIV_GmChg_Put_Down
ATMIV_GmChg_Call_Up
ATMIV_GmChg_Call_Down

#ATMIV.f/(IVIDX.f)^(x_idx)/TimeToExpDate^(y_idx) plotting
x_idx=0.5
#y_idx=(-1)*x_idx
y_idx=(-0.5)
#Put
(ggplot(ATMIV_GmChg_Put_Up,aes(x=TimeToExpDate,y=(ATMIV.f/((IVIDX.f)^(x_idx))/(TimeToExpDate^(y_idx))),colour=DaysToMaxDate))+geom_point(alpha=0.5))
(ggplot(ATMIV_GmChg_Put_Down,aes(x=TimeToExpDate,y=(ATMIV.f/((IVIDX.f)^(x_idx))/(TimeToExpDate^(y_idx))),colour=DaysToMaxDate))+geom_point(alpha=0.5))
#Call
(ggplot(ATMIV_GmChg_Call_Up,aes(x=TimeToExpDate,y=(ATMIV.f/((IVIDX.f)^(x_idx))/(TimeToExpDate^(y_idx))),colour=DaysToMaxDate))+geom_point(alpha=0.5))
(ggplot(ATMIV_GmChg_Call_Down,aes(x=TimeToExpDate,y=(ATMIV.f/((IVIDX.f)^(x_idx))/(TimeToExpDate^(y_idx))),colour=DaysToMaxDate))+geom_point(alpha=0.5))

##
# Regression

###  Put Up
ATMIV_GmChg_Regressed=ATMIV_GmChg_Put_Up
model.ss<-smooth.spline(ATMIV_GmChg_Regressed$TimeToExpDate,
                        (ATMIV_GmChg_Regressed$ATMIV.f/((ATMIV_GmChg_Regressed$IVIDX.f)^(x_idx))/(ATMIV_GmChg_Regressed$TimeToExpDate^(y_idx))),
                        df=3)
(predict.c <- predict(model.ss,x=seq(0,max(ATMIV_GmChg_Regressed$TimeToExpDate),by=0.1)))
(ggplot(ATMIV_GmChg_Regressed,aes(x=TimeToExpDate,
                                  y=(ATMIV.f/((IVIDX.f)^(x_idx))/(TimeToExpDate^(y_idx))),
                                  colour=DaysToMaxDate))+geom_point(size=2,alpha=0.5)+
    geom_line(data=data.frame(TimeToExpDate=predict.c$x,Regressed=predict.c$y,TYPE=OpType_Put_G),aes(TimeToExpDate,Regressed),color='steelblue'))

save.ATMIV.f.IVIDX.f(model.ss,optype=OpType_Put_G,up_dn=10,day=1,x_idx=x_idx,y_idx=y_idx)
load.ATMIV.f.IVIDX.f(optype=OpType_Put_G,up_dn=10,days=1)
PutIVUp_ATMIV.f.IVIDX.f_1D$model
#test
get.ATMIV.f.VolChg(model=PutIVUp_ATMIV.f.IVIDX.f_1D$model,
                   days=1,
                   hdd=holdDays,
                   ividx.f=1.20,
                   x_idx=PutIVUp_ATMIV.f.IVIDX.f_1D$x,
                   y_idx=PutIVUp_ATMIV.f.IVIDX.f_1D$y,
                   month=2.5)

###  Put Down
ATMIV_GmChg_Regressed=ATMIV_GmChg_Put_Down
model.ss<-smooth.spline(ATMIV_GmChg_Regressed$TimeToExpDate,
                        (ATMIV_GmChg_Regressed$ATMIV.f/((ATMIV_GmChg_Regressed$IVIDX.f)^(x_idx))/(ATMIV_GmChg_Regressed$TimeToExpDate^(y_idx))),
                        df=3)
(predict.c <- predict(model.ss,x=seq(0,max(ATMIV_GmChg_Regressed$TimeToExpDate),by=0.1)))
(ggplot(ATMIV_GmChg_Regressed,aes(x=TimeToExpDate,
                                  y=(ATMIV.f/((IVIDX.f)^(x_idx))/(TimeToExpDate^(y_idx))),
                                  colour=DaysToMaxDate))+geom_point(size=2,alpha=0.5)+
    geom_line(data=data.frame(TimeToExpDate=predict.c$x,Regressed=predict.c$y,TYPE=OpType_Put_G),aes(TimeToExpDate,Regressed),color='steelblue'))

save.ATMIV.f.IVIDX.f(model.ss,optype=OpType_Put_G,up_dn=(-10),day=1,x_idx=x_idx,y_idx=y_idx)
load.ATMIV.f.IVIDX.f(optype=OpType_Put_G,up_dn=-10,days=1)
PutIVDown_ATMIV.f.IVIDX.f_1D$model
#test
get.ATMIV.f.VolChg(model=PutIVDown_ATMIV.f.IVIDX.f_1D$model,
                   days=1,
                   hdd=holdDays,
                   ividx.f=0.8,
                   x_idx=PutIVDown_ATMIV.f.IVIDX.f_1D$x,
                   y_idx=PutIVDown_ATMIV.f.IVIDX.f_1D$y,
                   month=2.5)

###  Call Up
ATMIV_GmChg_Regressed=ATMIV_GmChg_Call_Up
model.ss<-smooth.spline(ATMIV_GmChg_Regressed$TimeToExpDate,
                        (ATMIV_GmChg_Regressed$ATMIV.f/((ATMIV_GmChg_Regressed$IVIDX.f)^(x_idx))/(ATMIV_GmChg_Regressed$TimeToExpDate^(y_idx))),
                        df=3)
(predict.c <- predict(model.ss,x=seq(0,max(ATMIV_GmChg_Regressed$TimeToExpDate),by=0.1)))
(ggplot(ATMIV_GmChg_Regressed,aes(x=TimeToExpDate,
                                  y=(ATMIV.f/((IVIDX.f)^(x_idx))/(TimeToExpDate^(y_idx))),
                                  colour=DaysToMaxDate))+geom_point(size=2,alpha=0.5)+
    geom_line(data=data.frame(TimeToExpDate=predict.c$x,Regressed=predict.c$y,TYPE=OpType_Put_G),aes(TimeToExpDate,Regressed),color='steelblue'))

save.ATMIV.f.IVIDX.f(model.ss,optype=OpType_Call_G,up_dn=10,day=1,x_idx=x_idx,y_idx=y_idx)
load.ATMIV.f.IVIDX.f(optype=OpType_Call_G,up_dn=10,days=1) 
CallIVUp_ATMIV.f.IVIDX.f_1D$model
#test
get.ATMIV.f.VolChg(model=CallIVUp_ATMIV.f.IVIDX.f_1D$model,
                   days=1,
                   hdd=holdDays,
                   ividx.f=1.20,
                   x_idx=CallIVUp_ATMIV.f.IVIDX.f_1D$x,
                   y_idx=CallIVUp_ATMIV.f.IVIDX.f_1D$y,
                   month=2.5)

###  Call Down
ATMIV_GmChg_Regressed=ATMIV_GmChg_Call_Down
model.ss<-smooth.spline(ATMIV_GmChg_Regressed$TimeToExpDate,
                        (ATMIV_GmChg_Regressed$ATMIV.f/((ATMIV_GmChg_Regressed$IVIDX.f)^(x_idx))/(ATMIV_GmChg_Regressed$TimeToExpDate^(y_idx))),
                        df=3)
(predict.c <- predict(model.ss,x=seq(0,max(ATMIV_GmChg_Regressed$TimeToExpDate),by=0.1)))
(ggplot(ATMIV_GmChg_Regressed,aes(x=TimeToExpDate,
                                  y=(ATMIV.f/((IVIDX.f)^(x_idx))/(TimeToExpDate^(y_idx))),
                                  colour=DaysToMaxDate))+geom_point(size=2,alpha=0.5)+
    geom_line(data=data.frame(TimeToExpDate=predict.c$x,Regressed=predict.c$y,TYPE=OpType_Put_G),aes(TimeToExpDate,Regressed),color='steelblue'))

save.ATMIV.f.IVIDX.f(model.ss,optype=OpType_Call_G,up_dn=(-10),day=1,x_idx=x_idx,y_idx=y_idx)
load.ATMIV.f.IVIDX.f(optype=OpType_Call_G,up_dn=(-10),days=1)
CallIVDown_ATMIV.f.IVIDX.f_1D$model
#test
get.ATMIV.f.VolChg(model=CallIVDown_ATMIV.f.IVIDX.f_1D$model,
                   days=1,
                   hdd=holdDays,
                   ividx.f=0.8,
                   x_idx=CallIVDown_ATMIV.f.IVIDX.f_1D$x,
                   y_idx=CallIVDown_ATMIV.f.IVIDX.f_1D$y,
                   month=2.5)


