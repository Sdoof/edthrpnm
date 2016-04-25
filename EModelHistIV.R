library(RQuantLib)
library(ggplot2)
library(plyr)
library(dplyr)
rm(list=ls())
source('./ESourceRCode.R',encoding = 'UTF-8')

#data row num
DATA_NUM=2000

#read data file
rf_<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Hist.csv",sep="")
histPrc_<-read.table(rf_,header=T,sep=",",nrows=DATA_NUM)
rf_<-paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="")
histIV_<-read.table(rf_,header=T,sep=",",nrows=DATA_NUM)
rm(rf_)
#show Realized and Impled Volatility
cat("Realized Vol(30d anlzd)",annuual.daily.volatility(histPrc_$Close[1:30])$anlzd*100,"IV",histIV_$Close[1])
cat("Realized Vol(60d anlzd)",annuual.daily.volatility(histPrc_$Close[1:60])$anlzd*100,"IV",histIV_$Close[1])
cat("Realized Vol(120d anlzd)",annuual.daily.volatility(histPrc_$Close[1:120])$anlzd*100,"IV",histIV_$Close[1])
cat("Realized Vol(200d anlzd)",annuual.daily.volatility(histPrc_$Close[1:200])$anlzd*100,"IV",histIV_$Close[1])

##Regression : n day price move(price chg% plus SD(IV)) to IV change fcation(%)
#Regression from start_day_ ago to num_day_ business days
start_day_<-1;num_day_<-400

#####
##   3d
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

#####
##  5d
#data construct by function
PC5dCtC  <- PCndCtC(hist=histPrc_$Close,n=5)
PCIV5dCtC<-PCIVndCtC(hist=histPrc_$Close,iv=histIV_$Close,n=5)
IVCF5dCtC<-IVCFndCtC(iv=histIV_$Close,n=5)

#regression
PC5dCtC=PC5dCtC[start_day_:(start_day_+num_day_)]
na.omit(PC5dCtC)  %>% as.vector() -> PC5dCtC
IVCF5dCtC=IVCF5dCtC[start_day_:(start_day_+num_day_)]
na.omit(IVCF5dCtC)  %>% as.vector() -> IVCF5dCtC
PC5dCtC=PC5dCtC[1:min(length(PC5dCtC),length(IVCF5dCtC))]
IVCF5dCtC=IVCF5dCtC[1:min(length(PC5dCtC),length(IVCF5dCtC))]
P2IV5d <- data.frame(PCIV5dCtC=PC5dCtC, IVCF5dCtC=IVCF5dCtC)
(gg_<-ggplot(P2IV5d,aes(x=PCIV5dCtC,y=IVCF5dCtC))+geom_point())

P2IV5d <- data.frame(PC5dCtC=PC5dCtC, IVCF5dCtC=IVCF5dCtC)
(gg_<-ggplot(P2IV5d,aes(x=PC5dCtC,y=IVCF5dCtC))+geom_point())
co<-cor(PC5dCtC[start_day_:min((start_day_+num_day_),length(PC5dCtC))],IVCF5dCtC[start_day_:min((start_day_+num_day_),length(IVCF5dCtC))])
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

#####
##   7d
PC7dCtC<-histPrc_$PC7dCtC[start_day_:(start_day_+num_day_)]
IVCF7dCtC<-histIV_$IVCF7dCtC[start_day_:(start_day_+num_day_)]
P2IV7d <- data.frame(PC7dCtC=PC7dCtC, IVCF7dCtC=IVCF7dCtC)
(gg_<-ggplot(P2IV7d,aes(x=PC7dCtC,y=IVCF7dCtC))+geom_point())
co<-cor(PC7dCtC,IVCF7dCtC)
#linear regression
norns.lm<-lm(IVCF7dCtC~PC7dCtC, data=P2IV7d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")
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

#####
##  12d
#data construct by function
PC12dCtC  <- PCndCtC(hist=histPrc_$Close,n=12)
PCIV12dCtC<-PCIVndCtC(hist=histPrc_$Close,iv=histIV_$Close,n=12)
IVCF12dCtC<-IVCFndCtC(iv=histIV_$Close,n=12)

#regression
PC12dCtC=PC12dCtC[start_day_:(start_day_+num_day_)]
na.omit(PC12dCtC)  %>% as.vector() -> PC12dCtC
IVCF12dCtC=IVCF12dCtC[start_day_:(start_day_+num_day_)]
na.omit(IVCF12dCtC)  %>% as.vector() -> IVCF12dCtC
PC12dCtC=PC12dCtC[1:min(length(PC12dCtC),length(IVCF12dCtC))]
IVCF12dCtC=IVCF12dCtC[1:min(length(PC12dCtC),length(IVCF12dCtC))]
P2IV12d <- data.frame(PCIV12dCtC=PC12dCtC, IVCF12dCtC=IVCF12dCtC)
(gg_<-ggplot(P2IV12d,aes(x=PCIV12dCtC,y=IVCF12dCtC))+geom_point())

P2IV12d <- data.frame(PC12dCtC=PC12dCtC, IVCF12dCtC=IVCF12dCtC)
(gg_<-ggplot(P2IV12d,aes(x=PC12dCtC,y=IVCF12dCtC))+geom_point())
co<-cor(PC12dCtC[start_day_:min((start_day_+num_day_),length(PC12dCtC))],IVCF12dCtC[start_day_:min((start_day_+num_day_),length(IVCF12dCtC))])
#linear regression
norns.lm<-lm(IVCF12dCtC~PC12dCtC, data=P2IV12d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")

#Regression File save
#mean(PC5dCtC,na.rm=TRUE)
mean(PC12dCtC)
sd(PC12dCtC)
mean(IVCF12dCtC)
sd(IVCF12dCtC)
save.PC2IV(model=norns.lm,PC=getvarname(PC12dCtC),IVC=getvarname(IVCF12dCtC),cor=co,
           pcstat=c(mean(PC12dCtC),sd(PC12dCtC)),
           ivstat=c(mean(IVCF12dCtC),sd(IVCF12dCtC)))
#Load test
load.PC2IV(PC=getvarname(PC12dCtC),IVC=getvarname(IVCF12dCtC))
PC12dCtC_IVCF12dCtC
rm(PC12dCtC_IVCF12dCtC)

#####
##  18d
#data construct by function
PC18dCtC  <- PCndCtC(hist=histPrc_$Close,n=18)
PCIV18dCtC<-PCIVndCtC(hist=histPrc_$Close,iv=histIV_$Close,n=18)
IVCF18dCtC<-IVCFndCtC(iv=histIV_$Close,n=18)

#regression
PC18dCtC=PC18dCtC[start_day_:(start_day_+num_day_)]
na.omit(PC18dCtC)  %>% as.vector() -> PC18dCtC
IVCF18dCtC=IVCF18dCtC[start_day_:(start_day_+num_day_)]
na.omit(IVCF18dCtC)  %>% as.vector() -> IVCF18dCtC
PC18dCtC=PC18dCtC[1:min(length(PC18dCtC),length(IVCF18dCtC))]
IVCF18dCtC=IVCF18dCtC[1:min(length(PC18dCtC),length(IVCF18dCtC))]
P2IV18d <- data.frame(PCIV18dCtC=PC18dCtC, IVCF18dCtC=IVCF18dCtC)
(gg_<-ggplot(P2IV18d,aes(x=PCIV18dCtC,y=IVCF18dCtC))+geom_point())

P2IV18d <- data.frame(PC18dCtC=PC18dCtC, IVCF18dCtC=IVCF18dCtC)
(gg_<-ggplot(P2IV18d,aes(x=PC18dCtC,y=IVCF18dCtC))+geom_point())
co<-cor(PC18dCtC[start_day_:min((start_day_+num_day_),length(PC18dCtC))],IVCF18dCtC[start_day_:min((start_day_+num_day_),length(IVCF18dCtC))])
#linear regression
norns.lm<-lm(IVCF18dCtC~PC18dCtC, data=P2IV18d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")

#Regression File save
#mean(PC5dCtC,na.rm=TRUE)
mean(PC18dCtC)
sd(PC18dCtC)
mean(IVCF18dCtC)
sd(IVCF18dCtC)
save.PC2IV(model=norns.lm,PC=getvarname(PC18dCtC),IVC=getvarname(IVCF18dCtC),cor=co,
           pcstat=c(mean(PC18dCtC),sd(PC18dCtC)),
           ivstat=c(mean(IVCF18dCtC),sd(IVCF18dCtC)))
#Load test
load.PC2IV(PC=getvarname(PC18dCtC),IVC=getvarname(IVCF18dCtC))
PC18dCtC_IVCF18dCtC
rm(PC18dCtC_IVCF18dCtC)

#####
##   1d
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

rm(list=ls())
