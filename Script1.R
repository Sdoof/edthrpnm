library(RQuantLib)
library(ggplot2)
library(plyr)
library(dplyr)

##
#  From EOptionOprEurop -----------

###Global 変数及び定数.
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

##
#  From EVolatility -----------

#read data file
rf_<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Hist.csv",sep="")
histPrc_<-read.table(rf_,header=T,sep=",",nrows=550)
rf_<-paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="")
histIV_<-read.table(rf_,header=T,sep=",",nrows=550)
rm(rf_)

#data construct
PC5dCtC  <- PCndCtC(hist=histPrc_$Close,n=5)
PCIV5dCtC<-PCIVndCtC(hist=histPrc_$Close,iv=histIV_$Close,n=5)
IVCF5dCtC<-IVCFndCtC(iv=histIV_$Close,n=5)

##Regression : n day price move(price chg% plus SD(IV)) to IV change fcation(%)
#Regression from start_day_ ago to num_day_ business days
start_day_<-1;num_day_<-400

##3d
PC3dCtC<-histPrc_$PC3dCtC[start_day_:(start_day_+num_day_)]
IVCF3dCtC<-histIV_$IVCF3dCtC[start_day_:(start_day_+num_day_)]
P2IV3d <- data.frame(PC3dCtC=PC3dCtC, IVCF3dCtC=IVCF3dCtC)
(gg_<-ggplot(P2IV3d,aes(x=PC3dCtC,y=IVCF3dCtC))+geom_point())
co<-cor(PC3dCtC,IVCF3dCtC)
#linear regression
norns.lm<-lm(IVCF3dCtC~PC3dCtC, data=P2IV3d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")

#Regression File save
#mean(PC3dCtC,na.rm=TRUE)
mean(PC3dCtC)
sd(PC3dCtC)
mean(IVCF3dCtC)
sd(IVCF3dCtC)
save.PC2IV(model=norns.lm,PC=getvarname(PC3dCtC),IVC=getvarname(IVCF3dCtC),cor=co,
           pcstat=c(mean(PC3dCtC),sd(PC3dCtC)),
           ivstat=c(mean(IVCF3dCtC),sd(IVCF3dCtC)))
#Load test
load.PC2IV(PC=getvarname(PC3dCtC),IVC=getvarname(IVCF3dCtC))
PC3dCtC_IVCF3dCtC
rm(PC3dCtC_IVCF3dCtC)

##5d
PC5dCtC=PC5dCtC[start_day_:(start_day_+num_day_)]
IVCF5dCtC=IVCF5dCtC[start_day_:(start_day_+num_day_)]
P2IV5d <- data.frame(PCIV5dCtC=PC5dCtC, IVCF5dCtC=IVCF5dCtC)
(gg_<-ggplot(P2IV5d,aes(x=PCIV5dCtC,y=IVCF5dCtC))+geom_point())
co<-cor(PCIV5dCtC[start_day_:(start_day_+num_day_)],IVCF5dCtC[start_day_:(start_day_+num_day_)])
#linear regression
norns.lm<-lm(IVCF5dCtC~PCIV5dCtC, data=P2IV5d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")

P2IV5d <- data.frame(PC5dCtC=PC5dCtC, IVCF5dCtC=IVCF5dCtC)
(gg_<-ggplot(P2IV5d,aes(x=PC5dCtC,y=IVCF5dCtC))+geom_point())
co<-cor(PC5dCtC[start_day_:(start_day_+num_day_)],IVCF5dCtC[start_day_:(start_day_+num_day_)])
#linear regression
norns.lm<-lm(IVCF5dCtC~PC5dCtC, data=P2IV5d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")

#Regression File save
#mean(PC5dCtC,na.rm=TRUE)
mean(PC5dCtC)
sd(PC5dCtC)
mean(IVCF5dCtC)
sd(IVCF5dCtC)
save.PC2IV(model=norns.lm,PC=getvarname(PC5dCtC),IVC=getvarname(IVCF5dCtC),cor=co,
           pcstat=c(mean(PC5dCtC),sd(PC5dCtC)),
           ivstat=c(mean(IVCF5dCtC),sd(IVCF5dCtC)))
#Load test
load.PC2IV(PC=getvarname(PC5dCtC),IVC=getvarname(IVCF5dCtC))
PC5dCtC_IVCF5dCtC
rm(PC5dCtC_IVCF5dCtC)

##7d
PC7dCtC<-histPrc_$PC7dCtC[start_day_:(start_day_+num_day_)]
IVCF7dCtC<-histIV_$IVCF7dCtC[start_day_:(start_day_+num_day_)]
P2IV7d <- data.frame(PC7dCtC=PC7dCtC, IVCF7dCtC=IVCF7dCtC)
(gg_<-ggplot(P2IV7d,aes(x=PC7dCtC,y=IVCF7dCtC))+geom_point())
co<-cor(PC7dCtC,IVCF7dCtC)
#linear regression
norns.lm<-lm(IVCF7dCtC~PC7dCtC, data=P2IV7d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")

#Regression File save
#mean(PC5dCtC,na.rm=TRUE)
mean(PC7dCtC)
sd(PC7dCtC)
mean(IVCF7dCtC)
sd(IVCF7dCtC)
save.PC2IV(model=norns.lm,PC=getvarname(PC7dCtC),IVC=getvarname(IVCF7dCtC),cor=co,
           pcstat=c(mean(PC7dCtC),sd(PC7dCtC)),
           ivstat=c(mean(IVCF7dCtC),sd(IVCF7dCtC)))
#Load test
load.PC2IV(PC=getvarname(PC7dCtC),IVC=getvarname(IVCF7dCtC))
PC7dCtC_IVCF7dCtC
rm(PC7dCtC_IVCF7dCtC)

##1d
PC1dCtC<-histPrc_$PC1dCtC[start_day_:(start_day_+num_day_)]
IVCF1dCtC<-histIV_$IVCF1dCtC[start_day_:(start_day_+num_day_)]
P2IV1d <- data.frame(PC1dCtC=PC1dCtC, IVCF1dCtC=IVCF1dCtC)
(gg_<-ggplot(P2IV1d,aes(x=PC1dCtC,y=IVCF1dCtC))+geom_point())
co<-cor(PC1dCtC,IVCF1dCtC)
#linear regression
norns.lm<-lm(IVCF1dCtC~PC1dCtC, data=P2IV1d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")
#Regression File save
mean(PC1dCtC)
sd(PC1dCtC)
mean(IVCF1dCtC)
sd(IVCF1dCtC)
save.PC2IV(model=norns.lm,PC=getvarname(PC1dCtC),IVC=getvarname(IVCF1dCtC),cor=co,
           pcstat=c(mean(PC1dCtC),sd(PC1dCtC)),
           ivstat=c(mean(IVCF1dCtC),sd(IVCF1dCtC)))
#Load test
load.PC2IV(PC=getvarname(PC1dCtC),IVC=getvarname(IVCF1dCtC))
PC1dCtC_IVCF1dCtC
rm(PC1dCtC_IVCF1dCtC)

rm(gg_,co,histIV_,histPrc_)
rm(PC1dCtC,PC1dCtO,PC3dCtC,PC5dCtC,PC7dCtC,PCIV1dCtC,PCIV1dCtO,PCIV3dCtC,PCIV5dCtC,PCIV7dCtC)
rm(IVCF1dCtC,IVCF1dCtO,IVCF3dCtC,IVCF5dCtC,IVCF7dCtC)
rm(norns.lm,start_day_,num_day_)
rm(P2IV1d,P2IV3d,P2IV5d,P2IV7d)
