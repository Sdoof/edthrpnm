library(ggplot2)

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
###
#read data file
rf_<-paste(DataFiles_Path_G,Underying_Synbol_G,"_OPChain_Pos.csv",sep="")
opch<-read.table(rf_,header=T,sep=",",nrows=50000)
rf_<-paste(DataFiles_Path_G,Underying_Synbol_G,"_IV.csv",sep="")
histIV<-read.table(rf_,header=T,sep=",",nrows=1999)
rm(rf_)

#NULL unneeded columns.
#Option Chain
opch$Last<-opch$Bid<-opch$Ask<-opch$Ask<-opch$Volume<-opch$OI<-NULL
#Histrical Volatility(Index). VOlatility % to DN. Column name Close to IVIDX
histIV<-data.frame(Date=histIV$Date,IVIDX=(histIV$Close/100))
##Historical Implied Volatility(index) merge
#merge test
#opch_tmp<-merge(opch,histIV,all.x=T)
#subset(opch_tmp,Date=="2014/12/18")
#subset(opch_tmp,Date=="2014/12/23")
#rm(opch_tmp)
#mrege
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

#Normalized Moneyness
#log(opch$Moneyness.Frac)
#opch$IVIDX
#sqrt(opch$TimeToExpDate)
opch$Moneyness.Nm<-log(opch$Moneyness.Frac)/opch$IVIDX/sqrt(opch$TimeToExpDate)

#Writing to a file
wf_<-paste(DataFiles_Path_G,Underying_Synbol_G,"_OPChain_Skew.csv",sep="")
write.table(opch,wf_,quote=T,row.names=F,sep=",")
rm(wf_)

rm(opch)
