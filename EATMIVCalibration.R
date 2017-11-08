library(RQuantLib)
library(ggplot2)
library(plyr)
library(dplyr)
rm(list=ls())
source('./ESourceRCode.R',encoding = 'UTF-8')

#Load Original ATM History file
atmiv_hist<-read.table(paste(DataFiles_Path_G,Underying_Symbol_G,"-ATMIV-VCONE-ANAL_Hist.csv",sep=""),
                       header=T,skipNul=T,stringsAsFactors=F,sep=",")
atmiv_hist %>%
  dplyr::arrange(desc(TYPE),as.Date(ExpDate,format="%Y/%m/%d"),as.Date(Date,format="%Y/%m/%d")) %>%
  dplyr::distinct() -> atmiv_hist

head(atmiv_hist,n=100)
tail(atmiv_hist,n=100)

atmiv_hist %>%
  dplyr::arrange(as.Date(ExpDate,format="%Y/%m/%d"),as.Date(Date,format="%Y/%m/%d"),UDLY,desc(TYPE)) %>%
  dplyr::distinct() -> atmiv_hist

head(atmiv_hist,n=100)
tail(atmiv_hist,n=100)

#Create atmiv_calib which then become a new calibrated ATM History file

atmiv_hist %>% dplyr::group_by(Date,ExpDate)  %>% 
  dplyr::summarise(TYPE=dplyr::first(TYPE),ATMIV=mean(ATMIV),UDLY=mean(UDLY),IVIDX=mean(IVIDX),TimeToExpDate=mean(TimeToExpDate)) %>%
  as.data.frame() -> atmiv_calib

atmiv_calib %>% 
  dplyr::arrange(as.Date(ExpDate,format="%Y/%m/%d"),as.Date(Date,format="%Y/%m/%d"),UDLY,desc(TYPE)) %>%
  dplyr::distinct() -> atmiv_calib

tail(atmiv_calib,n=100)

atmiv_calib %>% dplyr::bind_rows(atmiv_calib %>% dplyr::mutate(TYPE=TYPE*(-1))) %>% 
  dplyr::arrange(desc(TYPE),as.Date(ExpDate,format="%Y/%m/%d"),as.Date(Date,format="%Y/%m/%d")) %>%
  dplyr::distinct() -> atmiv_calib

head(atmiv_calib,n=100)
tail(atmiv_calib,n=100)

write.table(atmiv_calib,paste(DataFiles_Path_G,Underying_Symbol_G,"-ATMIV-VCONE-ANAL_Hist_Calib.csv",sep=""),
            row.names = F,col.names=T,sep=",",append=F)

##
# Term Structure regression

atmiv=atmiv_calib
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


##
# ATMIV.f.IVIDX.f regression

atmiv=atmiv_calib

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
atmiv.vcone.anal %>% dplyr::select(Date,ExpDate,TYPE,ATMIV,UDLY,IVIDX,ATMIDXIV.r,TimeToExpDate,ATMIV.f,IVIDX.f,ATMIDXIV.r.f) %>% 
  dplyr::distinct()-> atmiv.vcone.anal

ATMIV_GmChg_Put<-make.vchg.df(vcone=atmiv.vcone.anal,type=OpType_Put_G)
ATMIV_GmChg_Put$DaysToMaxDate<-as.numeric(max(as.Date(ATMIV_GmChg_Put$Date,format="%Y/%m/%d"))-as.Date(ATMIV_GmChg_Put$Date,format="%Y/%m/%d"))
ATMIV_GmChg_Put %>% dplyr::filter(IVIDX.f>=1.0) -> ATMIV_GmChg_Put_Up
ATMIV_GmChg_Put %>% dplyr::filter(IVIDX.f<1.0) -> ATMIV_GmChg_Put_Down

# Call
ATMIV_GmChg_Call<-make.vchg.df(vcone=atmiv.vcone.anal,type=OpType_Call_G)
ATMIV_GmChg_Call$DaysToMaxDate<-as.numeric(max(as.Date(ATMIV_GmChg_Call$Date,format="%Y/%m/%d"))-as.Date(ATMIV_GmChg_Call$Date,format="%Y/%m/%d"))
ATMIV_GmChg_Call %>% dplyr::filter(IVIDX.f>=1.0) -> ATMIV_GmChg_Call_Up
ATMIV_GmChg_Call %>% dplyr::filter(IVIDX.f<1.0) -> ATMIV_GmChg_Call_Down

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

###
# Positions calibration

#Saved File Name

fnames=list.files(path = DataFiles_Path_G
           ,pattern=paste(Underying_Symbol_G,paste("_Positions_Pre*",sep=""),sep="")
           #,pattern="SPX_Positions_Pre*"
           )
#load skew
load.Skew(pattern="_Put")
SkewModel_Put

load.Skew(pattern="_Call")
SkewModel_Call

for(i in 1:length(fnames)){
  opch<-read.table(paste(DataFiles_Path_G,fnames[i],sep=""),
                   header=T,skipNul=T,stringsAsFactors=F,sep=",")
  
  opch$ATMIV=NULL
  opch %>%
    dplyr::inner_join(atmiv_calib %>% dplyr::select(Date,ExpDate,ATMIV,TYPE)) %>%
    dplyr::select(Date,ExpDate,TYPE,Strike,ContactName,Position,UDLY,ATMIV,IVIDX,TimeToExpDate,Moneyness.Nm) -> opch
  
  (opch)
  
  #calculate IV_pos(OrigIV) using SkewModel based on model definition formula.
  spskew<-(opch$TYPE==OpType_Put_G)*get.predicted.spline.skew(SkewModel_Put,opch$Moneyness.Nm)+
    (opch$TYPE==OpType_Call_G)*get.predicted.spline.skew(SkewModel_Call,opch$Moneyness.Nm)
  opch$ATMIV*spskew
  opch$OrigIV<-opch$ATMIV*spskew
  
  #calculate Price and Grrks
  vgreeks<-set.EuropeanOptionValueGreeks(opch)
  opch$Price<-vgreeks$Price
  opch$Delta<-vgreeks$Delta
  opch$Gamma<-vgreeks$Gamma
  opch$Vega<-vgreeks$Vega
  opch$Theta<-vgreeks$Theta
  opch$Rho<-vgreeks$Rho
  
  #select again
  opch %>%
    dplyr::select(Date,ExpDate,TYPE,Strike,ContactName,Position,UDLY,
                  Price,Delta,Gamma,Vega,Theta,Rho,OrigIV,
                  ATMIV,IVIDX,TimeToExpDate,Moneyness.Nm) -> opch
  
  #writing to a file
  write.table(opch,
              paste(DataFiles_Path_G,fnames[i],"_Calib",format(Sys.time(),"%Y%b%d_%H%M%S"),".csv",sep=""),
              row.names = F,col.names=T,sep=",",append=F)
  
}
