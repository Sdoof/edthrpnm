library(RQuantLib)
library(ggplot2)
library(plyr)
library(dplyr)
library(rgl)

#getting annual and daili volatility
annuual.daily.volatility <- function(p){
  p_ <- replace(p, rep(1:(length(p)-1)), p[2:length(p)])
  #sum(log(p_/p))
  #sum(log(p_/p)^2)
  #sum(log(p_/p)^2)/(length(p)-1-1)
  #sum(log(p_/p))^2/(length(p)-1)/(length(p)-1-1)
  daily_<-sqrt(sum(log(p_/p)^2)/(length(p)-1-1)-sum(log(p_/p))^2/(length(p)-1)/(length(p)-1-1))
  vol_<-daily_*sqrt(252)
  ret_<-list(vol_)
  ret_<-c(ret_,list(daily_))
  ret_
}

#Volatility Level correlaiton and regression functions
#PCIVndCtC
PCIVndCtC <- function(hist,iv,n){
  p_ <- replace(hist, rep(1:(length(hist)-n)), hist[(1+n):length(hist)])
  cp_n_<- replace(p_,rep((length(p_)-(n-1)):length(p_)),NA)
  q_ <- replace(iv, rep(1:(length(iv)-n)),iv[(1+n):length(iv)])
  civ_n_ <- replace(q_,rep((length(q_)-(n-1)):length(q_)),NA)
  ret_<-(hist-cp_n_)/(civ_n_/100*cp_n_)
  ret_
}
#PCndCtC
PCndCtC <- function(hist,n){
  q_ <- replace(hist, rep(1:(length(hist)-n)),hist[(1+n):length(hist)])
  cp_n_ <- replace(q_,rep((length(q_)-(n-1)):length(q_)),NA)
  ret_ <- (hist-cp_n_)/cp_n_
  ret_
}
#IVCFndCTC
IVCFndCTC <- function(iv,n){
  q_ <- replace(iv, rep(1:(length(iv)-n)),iv[(1+n):length(iv)])
  civ_n_ <- replace(q_,rep((length(q_)-(n-1)):length(q_)),NA)
  ret_ <- (iv-civ_n_)/civ_n_
  ret_
}

###
## AUDUSD volatility examples
###

#simple sample
p<-c(20,20.1,19.9,20,20.5,20.25,20.9,20.9,20.9,20.75,20.75,21,21.1,20.9,20.9,21.25,21.4,21.4,21.25,21.75,22)
pv_<-annuual.daily.volatility(p)

#AUD uSD
p<-read.table("AUDUSD.csv",header=T,sep=",")
p<-p$AUDUSD[1:100]
pv_<-annuual.daily.volatility(p)


###
## Volatility Level correlaiton and regression
###

#read data file
rf_<-paste(DataFiles_Path_G,Underying_Synbol_G,"_Hist.csv",sep="")
histPrc_<-read.table(rf_,header=T,sep=",",nrows=1999)
rf_<-paste(DataFiles_Path_G,Underying_Synbol_G,"_IV.csv",sep="")
histIV_<-read.table(rf_,header=T,sep=",",nrows=1999)

#data construct
PC5dCtC  <- PCndCtC(hist=histPrc_$Close,n=5)
PCIV5dCtC<-PCIVndCtC(hist=histPrc_$Close,iv=histIV_$Close,n=5)
IVCF5dCTC<-IVCFndCTC(iv=histIV_$Close,n=5)

##Regression : n day price move(price chg% plus SD(IV)) to IV change fcation(%)
#Regression from start_day_ ago to num_day_ business days
start_day_<-1;num_day_<-400
##3d
PCIV3dCtC<-histPrc_$PCIV3dCtC[start_day_:(start_day_+num_day_)]
IVCF3dCtC<-histIV_$IVCF3dCtC[start_day_:(start_day_+num_day_)]
P2IV3d <- data.frame(PCIV3dCtC=PCIV3dCtC, IVCF3dCtC=IVCF3dCtC)
(gg_<-ggplot(P2IV3d,aes(x=PCIV3dCtC,y=IVCF3dCtC))+geom_point())
cor(PCIV3dCtC,IVCF3dCtC)
#linear regression
norns.lm<-lm(IVCF3dCtC~PCIV3dCtC, data=P2IV3d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")

PC3dCtC<-histPrc_$PC3dCtC[start_day_:(start_day_+num_day_)]
P2IV3d <- data.frame(PC3dCtC=PC3dCtC, IVCF3dCtC=IVCF3dCtC)
(gg_<-ggplot(P2IV3d,aes(x=PC3dCtC,y=IVCF3dCtC))+geom_point())
cor(PC3dCtC,IVCF3dCtC)
#linear regression
norns.lm<-lm(IVCF3dCtC~PC3dCtC, data=P2IV3d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")

##5d
P2IV5d <- data.frame(PCIV5dCtC=PCIV5dCtC[start_day_:(start_day_+num_day_)], IVCF5dCTC=IVCF5dCTC[start_day_:(start_day_+num_day_)])
(gg_<-ggplot(P2IV5d,aes(x=PCIV5dCtC,y=IVCF5dCTC))+geom_point())
cor(PCIV5dCtC[start_day_:(start_day_+num_day_)],IVCF5dCTC[start_day_:(start_day_+num_day_)])
#linear regression
norns.lm<-lm(IVCF5dCTC~PCIV5dCtC, data=P2IV5d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")

P2IV5d <- data.frame(PC5dCtC=PC5dCtC[start_day_:(start_day_+num_day_)], IVCF5dCTC=IVCF5dCTC[start_day_:(start_day_+num_day_)])
(gg_<-ggplot(P2IV5d,aes(x=PC5dCtC,y=IVCF5dCTC))+geom_point())
cor(PC5dCtC[start_day_:(start_day_+num_day_)],IVCF5dCTC[start_day_:(start_day_+num_day_)])
#linear regression
norns.lm<-lm(IVCF5dCTC~PC5dCtC, data=P2IV5d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")

##7d
PCIV7dCtC<-histPrc_$PCIV7dCtC[start_day_:(start_day_+num_day_)]
IVCF7dCtC<-histIV_$IVCF7dCtC[start_day_:(start_day_+num_day_)]
P2IV7d <- data.frame(PCIV7dCtC=PCIV7dCtC, IVCF7dCtC=IVCF7dCtC)
(gg_<-ggplot(P2IV7d,aes(x=PCIV7dCtC,y=IVCF7dCtC))+geom_point())
cor(PCIV7dCtC,IVCF7dCtC)
#linear regression
norns.lm<-lm(IVCF7dCtC~PCIV7dCtC, data=P2IV7d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")

PC7dCtC<-histPrc_$PC7dCtC[start_day_:(start_day_+num_day_)]
P2IV7d <- data.frame(PC7dCtC=PC7dCtC, IVCF7dCtC=IVCF7dCtC)
(gg_<-ggplot(P2IV7d,aes(x=PC7dCtC,y=IVCF7dCtC))+geom_point())
cor(PC7dCtC,IVCF7dCtC)
#linear regression
norns.lm<-lm(IVCF7dCtC~PC7dCtC, data=P2IV7d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")

##1d
PCIV1dCtC<-histPrc_$PCIV1dCtC[start_day_:(start_day_+num_day_)]
IVCF1dCtC<-histIV_$IVCF1dCtC[start_day_:(start_day_+num_day_)]
P2IV1d <- data.frame(PCIV1dCtC=PCIV1dCtC, IVCF1dCtC=IVCF1dCtC)
(gg_<-ggplot(P2IV1d,aes(x=PCIV1dCtC,y=IVCF1dCtC))+geom_point())
cor(PCIV1dCtC,IVCF1dCtC)
#linear regression
norns.lm<-lm(IVCF1dCtC~PCIV1dCtC, data=P2IV1d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")

PC1dCtC<-histPrc_$PC1dCtC[start_day_:(start_day_+num_day_)]
P2IV1d <- data.frame(PC1dCtC=PC1dCtC, IVCF1dCtC=IVCF1dCtC)
(gg_<-ggplot(P2IV1d,aes(x=PC1dCtC,y=IVCF1dCtC))+geom_point())
cor(PC1dCtC,IVCF1dCtC)
#linear regression
norns.lm<-lm(IVCF1dCtC~PC1dCtC, data=P2IV1d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")

##1d Close to Open
PCIV1dCtO<-histPrc_$PCIV1dCtO[start_day_:(start_day_+num_day_)]
IVCF1dCtO<-histIV_$IVCF1dCtO[start_day_:(start_day_+num_day_)]
P2IV1d <- data.frame(PCIV1dCtO=PCIV1dCtO, IVCF1dCtO=IVCF1dCtO)
(gg_<-ggplot(P2IV1d,aes(x=PCIV1dCtO,y=IVCF1dCtO))+geom_point())
cor(PCIV1dCtO,IVCF1dCtO)
#linear regression
norns.lm<-lm(IVCF1dCtO~PCIV1dCtO, data=P2IV1d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")

PC1dCtO<-histPrc_$PC1dCtO[start_day_:(start_day_+num_day_)]
P2IV1d <- data.frame(PC1dCtO=PC1dCtO, IVCF1dCtO=IVCF1dCtO)
(gg_<-ggplot(P2IV1d,aes(x=PC1dCtO,y=IVCF1dCtO))+geom_point())
cor(PC1dCtO,IVCF1dCtO)
#linear regression
norns.lm<-lm(IVCF1dCtO~PC1dCtO, data=P2IV1d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")

###
## Volatility Skew analyzation
##
#read data file
rf_<-paste(DataFiles_Path_G,Underying_Synbol_G,"_OPChain_Pos.csv",sep="")
opch<-read.table(rf_,header=T,sep=",",nrows=50000)
rf_<-paste(DataFiles_Path_G,Underying_Synbol_G,"_IV.csv",sep="")
histIV<-read.table(rf_,header=T,sep=",",nrows=1999)
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
#only OOM Option's IV is used and..
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

##END Got complete atmiv

###
# Volatility Cone and ATMIV%Chg/IVIDX%Chg Analysis, Regression.  
###

#
# Getting and Creating atmiv.vcone.anal to analyze vcone and to model ATMIV%Chg/IVIDX%Chg

#START
# called from do below. This function is called for each grouped data frame.
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

# Now We've got atmiv.vcone.anal data.dframe
#END

##
# Option Chain Analysis
#

## option chain 3D volatility plot

#Without Normalization
#Put OOM
opch %>%  dplyr::filter(TYPE==1) %>% 
  dplyr::filter(OrigIV/ATMIV<3.0)  %>% dplyr::filter(OrigIV/ATMIV>0.2) %>%
  dplyr::filter(HowfarOOM>=0) %>% dplyr::filter(TimeToExpDate>0.3) -> vplot
rgl::plot3d(vplot$Moneyness.Frac,vplot$TimeToExpDate,vplot$OrigIV,col=rainbow(100))
rgl::clear3d()

#Put ITM
opch %>%  dplyr::filter(TYPE==1) %>% 
  dplyr::filter(OrigIV/ATMIV<3.0)  %>% dplyr::filter(OrigIV/ATMIV>0.2) %>%
  dplyr::filter(HowfarOOM<0) %>% dplyr::filter(TimeToExpDate>0.3) -> vplot
rgl::plot3d(vplot$Moneyness.Frac,vplot$TimeToExpDate,vplot$OrigIV,col=rainbow(100))
rgl::clear3d()

#Call OOM
opch %>%  dplyr::filter(TYPE==-1) %>% 
  dplyr::filter(OrigIV/ATMIV<3.0)  %>% dplyr::filter(OrigIV/ATMIV>0.2) %>%
  dplyr::filter(HowfarOOM>=0) %>% dplyr::filter(TimeToExpDate>0.3) -> vplot
rgl::plot3d(vplot$Moneyness.Frac,vplot$TimeToExpDate,vplot$OrigIV,col=rainbow(100))
rgl::clear3d()

#Call ITM
opch %>%  dplyr::filter(TYPE==-1) %>% 
  dplyr::filter(OrigIV/ATMIV<3.0)  %>% dplyr::filter(OrigIV/ATMIV>0.2) %>%
  dplyr::filter(HowfarOOM<0) %>% dplyr::filter(TimeToExpDate>0.3) -> vplot
rgl::plot3d(vplot$Moneyness.Frac,vplot$TimeToExpDate,vplot$OrigIV,col=rainbow(100))
rgl::clear3d()

rm(vplot)

#Normalized Skew
#Put
opch %>%  dplyr::filter(TYPE==1) %>% 
  dplyr::filter(OrigIV/ATMIV<3.0)  %>% dplyr::filter(OrigIV/ATMIV>0.2) %>%
  dplyr::filter(HowfarOOM>=-100) %>% dplyr::filter(TimeToExpDate>0.3) -> vplot
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV),size=TimeToExpDate/2,colour=Date))+geom_point(alpha=0.2))

#Call
opch %>%  dplyr::filter(TYPE==-1) %>% 
  dplyr::filter(OrigIV/ATMIV<3.0)  %>% dplyr::filter(OrigIV/ATMIV>0.2) %>%
  dplyr::filter(HowfarOOM>=-100) %>% dplyr::filter(TimeToExpDate>0.3) -> vplot
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV),size=TimeToExpDate/2,colour=Date))+geom_point(alpha=0.2))

#Complete Opchain. Using OOM/ATM options.
opch %>% dplyr::filter(OrigIV/ATMIV<5.0)  %>% dplyr::filter(OrigIV/ATMIV>0.1) %>%
  dplyr::filter(TimeToExpDate>0.3) -> vplot
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV),size=TimeToExpDate/2,colour=TYPE))+geom_point(alpha=0.2))

#Complete Opchain. Using OOM options. By Call-Put parity, ITM IV is same as OOM IV.
opch %>% dplyr::filter(OrigIV/ATMIV<5.0)  %>% dplyr::filter(OrigIV/ATMIV>0.1) %>%
  dplyr::filter(HowfarOOM>=0) %>% dplyr::filter(TimeToExpDate>0.3) -> vplot
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV),size=TimeToExpDate/2,colour=Date))+geom_point(alpha=0.2))

#Regression
get.skew.regression.Results<-function(vplot,moneyness_adjust=0.1,atm_adjust=0.0){
  data.frame(Moneyness=vplot$Moneyness.Nm,
             Month=vplot$TimeToExpDate,
             IV2ATMIV=vplot$OrigIV/vplot$ATMIV)->vplot
  vplot %>% dplyr::filter(Moneyness<=moneyness_adjust) %>% 
    dplyr::filter(abs(IV2ATMIV-1.0)>=atm_adjust) -> vplot_mns
  vplot %>% dplyr::filter(Moneyness>(-1*moneyness_adjust)) %>% 
    dplyr::filter(abs(IV2ATMIV-1.0)>=atm_adjust) -> vplot_pls
  model.m<-lm(IV2ATMIV~1+Moneyness+I(Moneyness^2),data=vplot_mns)
  predict.m <- predict(model.m)
  model.p<-lm(IV2ATMIV~1+Moneyness+I(Moneyness^2),data=vplot_pls)
  predict.p <- predict(model.p)
  
  data.frame(vplot_mns,fit=predict.m) %>% dplyr::filter(Moneyness<=0) %>%
    dplyr::full_join(data.frame(vplot_pls,fit=predict.p) %>% dplyr::filter(Moneyness>0)) -> vplot
  
  
  vplot_exp<-data.frame(Moneyness=seq(-2,0,by=0.01))
  predict.m <- predict(model.m,newdata=vplot_exp)
  vplot_exp<-data.frame(vplot_exp,fit=predict.m)
  #predict.m
  #vplot_exp
}

#data.frame(Moneyness=vplot$Moneyness.Nm,
#           Month=vplot$TimeToExpDate,
#           IV2ATMIV=vplot$OrigIV/vplot$ATMIV)->vplot
get.skew.regression.Results(vplot)->vplot_exp
(gg_<-ggplot(vplot_exp,aes(x=Moneyness,y=IV2ATMIV))+geom_line(data=vplot_exp,aes(Moneyness,fit)))

#get.skew.regression.Results(vplot)->vplot
#(gg_<-ggplot(vplot,aes(x=Moneyness,y=IV2ATMIV,colour=Month))+geom_point(alpha=0.3)+
#   geom_line(data=vplot,aes(Moneyness,fit)))

rm(gg_,model.pm,vplot)

#3D Plot and Plane Fitting test

model.c<-lm(IV2ATMIV~Moneyness+Month+Moneyness:Month,data=vplot)

vplot$pred_IV2ATMIV<-predict(model.c)

mgrid_df<-predictgrid(model.c,"Moneyness","Month","IV2ATMIV")
mgrid_list<-df2mat(mgrid_df)

rgl::plot3d(vplot$Moneyness,vplot$Month,vplot$IV2ATMIV,
            xlab="",ylab="",zlab="",axes=FALSE,lit=FALSE)

#spheres3d(vplot$Moneyness,vplot$Month,vplot$pred_IV2ATMIV,
#          alpha=0.4,col="blue",lit=FALSE)

#segments3d(interleave(vplot$Moneyness,vplot$Moneyness),
#           interleave(vplot$Month,vplot$Month),
#           interleave(vplot$IV2ATMIV,vplot$pred_IV2ATMIV),
#           alpha=0.4,col="red")

surface3d(mgrid_list$Moneyness,mgrid_list$Month,mgrid_list$IV2ATMIV,
          alpha=0.4,front="lines",back="lines")

rgl.bbox(color="grey50",
         emission="grey50",
         xlen=0,ylen=0,zlen=0)
rgl.material(color="black")
axes3d(edges=c("x--","y+-","z--"),
       ntick=6,
       cex=0.75)
mtext3d("Moneyness",edge="x--",line=2)
mtext3d("Month",edge="y+-",line=3)
mtext3d("IV2ATMIV",edge="z--",line=3)
rm(vplot,model.c,mgrid_df,mgrid_list)

#after creating regression model, we should persp the estimated volatility surface.

##
#Vcone Analysis
#

#functions
make.vcone.df<-function(atmiv,type=0){
  if(type==0){
    vcone<- data.frame(Month=atmiv$TimeToExpDate, IV=atmiv$ATMIV,TYPE=atmiv$TYPE) 
  }else{
    atmiv %>% filter(TYPE==type) -> atmiv
    vcone<-data.frame(Month=atmiv$TimeToExpDate, IV=atmiv$ATMIV,IVIDX=atmiv$IVIDX,TYPE=atmiv$TYPE)
    #volatility normalize
    iv_mean_p<-mean(vcone$IV)
    ividx_mean_p<-mean(vcone$IVIDX)
    vcone %>% dplyr::mutate(IV.nm=IV/iv_mean_p) -> vcone
    #vcone %>% dplyr::mutate(IV2IDX.nm=IV/ividx_mean_p) -> vcone
    vcone %>% dplyr::mutate(IV2IDX.nm=IV/IVIDX) -> vcone
  }
  #Time filtering, because when IV is not stable when Time is very close to ExpDate .
  vcone %>% dplyr::filter(Month>=0.30) -> vcone
  vcone
}

make.vchg.df<-function(vcone,type=0){
  if(type!=0){
    vcone %>% filter(TYPE==type) -> vcone
  }
  vcone %>% dplyr::mutate(VC.f=ATMIV.f/IVIDX.f) -> vcone
  #Time filtering, because when IV is not stable when Time is very close to ExpDate .
  vcone %>% dplyr::filter(TimeToExpDate>=0.25) -> vcone
  vcone
}

# regression functions

vcone_regression<-function(vcone,regtype=1,ret=1){
  if(regtype==1){
    nls.m<-lm(IV2IDX.nm~Month,data=vcone)
  }
  if(ret==2){
    nls.m
  }else{
    predict.m <- predict(nls.m)
    predict.m
  }
}

vchg_regression<-function(vchg,type=2,start=NULL,ret=1){
  if(type==1){
    nls.m<-nls(VC.f~a*TimeToExpDate^b+c,data=vchg,start=start)
  }else if(type==2){
    nls.m<-lm(VC.f~TimeToExpDate,data=vchg)
  }
  if(ret==2){
    nls.m
  }else{
    predict.m <- predict(nls.m)
    predict.m
  }
}

#Plotting all type
vcone<-make.vcone.df(atmiv=atmiv,type=0)
(gg_<-ggplot(vcone,aes(x=Month,y=IV,colour=TYPE))+geom_point())
rm(gg_,vcone)

#Plotting Put. IV is normalized.
vcone<-make.vcone.df(atmiv=atmiv,type=1)
#(gg_<-ggplot(vcone,aes(x=Month,y=IV.nm,colour=TYPE))+geom_point())
(gg_<-ggplot(vcone,aes(x=Month,y=IV2IDX.nm,colour=TYPE))+geom_point())
# Regression of PUT vcone
#   1.linear
predict.c <- vcone_regression(vcone=vcone,regtype=1)
(gg_<-ggplot(vcone,aes(x=Month,y=IV2IDX.nm,colour=TYPE))+geom_point()+
   geom_line(data=data.frame(vcone,fit=predict.c),aes(Month,fit)))
rm(gg_,vcone,predict.c)

#Plotting Call. IV is normalized.
vcone<-make.vcone.df(atmiv=atmiv,type=-1)
#(gg_<-ggplot(vcone,aes(x=Month,y=IV.nm,colour=TYPE))+geom_point())
(gg_<-ggplot(vcone,aes(x=Month,y=IV2IDX.nm,colour=TYPE))+geom_point())
# Regression of PUT vcone
#   1.linear
predict.c <- vcone_regression(vcone=vcone,regtype=1)
(gg_<-ggplot(vcone,aes(x=Month,y=IV2IDX.nm,colour=TYPE))+geom_point()+
   geom_line(data=data.frame(vcone,fit=predict.c),aes(Month,fit)))
rm(gg_,vcone,predict.c)

##
# ATM IV Volatility Change to IV Index for TimeToExpDate
#

#all
vchg<-make.vchg.df(vcone=atmiv.vcone.anal,type=0)
(gg_<-ggplot(vchg,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point())
#(gg_<-ggplot(vchg,aes(x=TimeToExpDate,y=VC.f.AbSdf,colour=TYPE))+geom_point())
rm(gg_,vchg)

##Put

vchg<-make.vchg.df(vcone=atmiv.vcone.anal,type=1)
(gg_<-ggplot(vchg,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point())

vchg %>% filter(IVIDX.f>=1.0) -> vchg_plus
(gg_<-ggplot(vchg_plus,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point())

vchg %>% filter(IVIDX.f<1.0) -> vchg_mns
#filter outlier
vchg_mns %>% dplyr::filter(VC.f>0.90) -> vchg_mns
(gg_<-ggplot(vchg_mns,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point())

#    1.exponent function
vchg_t<-vchg_plus
predict.c <- vchg_regression(vchg=vchg_t,type=1,start=c(a=-0.02,b=0.6,c=0.01))
(gg_<-ggplot(vchg_t,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point()+
   geom_line(data=data.frame(vchg_t,fit=predict.c),aes(TimeToExpDate,fit)))
vchg_regression(vchg=vchg_t,type=1,start=c(a=-0.02,b=0.6,c=0.01),ret=2)
rm(vchg_t)

vchg_t<-vchg_mns
predict.c <- vchg_regression(vchg=vchg_t,type=1,start=c(a=0.2,b=0.2,c=-0.2))
(gg_<-ggplot(vchg_t,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point()+
   geom_line(data=data.frame(vchg_t,fit=predict.c),aes(TimeToExpDate,fit)))
vchg_regression(vchg=vchg_t,type=1,start=c(a=0.2,b=0.2,c=-0.2),ret=2)
rm(vchg_t)
#     2.linear
vchg_t<-vchg_plus
predict.c <- vchg_regression(vchg=vchg_t,type=2)
(gg_<-ggplot(vchg_t,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point()+
   geom_line(data=data.frame(vchg_t,fit=predict.c),aes(TimeToExpDate,fit)))
rm(vchg_t)

vchg_t<-vchg_mns
predict.c <- vchg_regression(vchg=vchg_t,type=2)
(gg_<-ggplot(vchg_t,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point()+
   geom_line(data=data.frame(vchg_t,fit=predict.c),aes(TimeToExpDate,fit)))
rm(vchg_t)

rm(gg_,vchg,vchg_mns,vchg_plus,predict.c)

##Call
vchg<-make.vchg.df(vcone=atmiv.vcone.anal,type=-1)
(gg_<-ggplot(vchg,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point())
vchg %>% filter(IVIDX.f>=1.0) -> vchg_plus
(gg_<-ggplot(vchg_plus,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point())
vchg %>% filter(IVIDX.f<1.0) -> vchg_mns
(gg_<-ggplot(vchg_mns,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point())

#regresson
#    1.exponent function
vchg_t<-vchg_plus
predict.c <- vchg_regression(vchg=vchg_t,type=1,start=c(a=0.1,b=-0.1,c=-0.03))
(gg_<-ggplot(vchg_t,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point()+
   geom_line(data=data.frame(vchg_t,fit=predict.c),aes(TimeToExpDate,fit)))
vchg_regression(vchg=vchg_t,type=1,start=c(a=0.1,b=-0.1,c=-0.03),ret=2)
rm(vchg_t)
vchg_t<-vchg_mns
predict.c <- vchg_regression(vchg=vchg_t,type=1,start=c(a=0.2,b=0.2,c=-0.2))
(gg_<-ggplot(vchg_t,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point()+
   geom_line(data=data.frame(vchg_t,fit=predict.c),aes(TimeToExpDate,fit)))
rm(vchg_t)
#     2.linear 
vchg_t<-vchg_plus
predict.c <- vchg_regression(vchg=vchg_t,type=2)
(gg_<-ggplot(vchg_t,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point()+
   geom_line(data=data.frame(vchg_t,fit=predict.c),aes(TimeToExpDate,fit)))
vchg_t<-vchg_mns
predict.c <- vchg_regression(vchg=vchg_t,type=2)
(gg_<-ggplot(vchg_t,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point()+
   geom_line(data=data.frame(vchg_t,fit=predict.c),aes(TimeToExpDate,fit)))
rm(vchg_t)

rm(gg_,vchg,vchg_mns,vchg_plus,predict.c)

#Writing to a file
wf_<-paste(DataFiles_Path_G,Underying_Synbol_G,"_OPChain_Skew.csv",sep="")
write.table(opch,wf_,quote=T,row.names=F,sep=",")
rm(wf_)

rm(makeVconAnalDF)
rm(atmiv.vcone.anal,atmiv,opch)
