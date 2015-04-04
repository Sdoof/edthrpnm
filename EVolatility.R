library(RQuantLib)
library(ggplot2)
library(plyr)
library(dplyr)
library(rgl)

###
# Volatility Level correlaiton and regression  -----------

#read data file
rf_<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Hist.csv",sep="")
histPrc_<-read.table(rf_,header=T,sep=",",nrows=1999)
rf_<-paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="")
histIV_<-read.table(rf_,header=T,sep=",",nrows=1999)
rm(rf_)

#data construct
PC5dCtC  <- PCndCtC(hist=histPrc_$Close,n=5)
PCIV5dCtC<-PCIVndCtC(hist=histPrc_$Close,iv=histIV_$Close,n=5)
IVCF5dCtC<-IVCFndCtC(iv=histIV_$Close,n=5)

##Regression : n day price move(price chg% plus SD(IV)) to IV change fcation(%)
#Regression from start_day_ ago to num_day_ business days
start_day_<-1;num_day_<-400

##3d
PCIV3dCtC<-histPrc_$PCIV3dCtC[start_day_:(start_day_+num_day_)]
IVCF3dCtC<-histIV_$IVCF3dCtC[start_day_:(start_day_+num_day_)]
P2IV3d <- data.frame(PCIV3dCtC=PCIV3dCtC, IVCF3dCtC=IVCF3dCtC)
(gg_<-ggplot(P2IV3d,aes(x=PCIV3dCtC,y=IVCF3dCtC))+geom_point())
co<-cor(PCIV3dCtC,IVCF3dCtC)
#linear regression
norns.lm<-lm(IVCF3dCtC~PCIV3dCtC, data=P2IV3d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")

PC3dCtC<-histPrc_$PC3dCtC[start_day_:(start_day_+num_day_)]
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
PCIV7dCtC<-histPrc_$PCIV7dCtC[start_day_:(start_day_+num_day_)]
IVCF7dCtC<-histIV_$IVCF7dCtC[start_day_:(start_day_+num_day_)]
P2IV7d <- data.frame(PCIV7dCtC=PCIV7dCtC, IVCF7dCtC=IVCF7dCtC)
(gg_<-ggplot(P2IV7d,aes(x=PCIV7dCtC,y=IVCF7dCtC))+geom_point())
co<-cor(PCIV7dCtC,IVCF7dCtC)

#linear regression
norns.lm<-lm(IVCF7dCtC~PCIV7dCtC, data=P2IV7d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")

PC7dCtC<-histPrc_$PC7dCtC[start_day_:(start_day_+num_day_)]
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
PCIV1dCtC<-histPrc_$PCIV1dCtC[start_day_:(start_day_+num_day_)]
IVCF1dCtC<-histIV_$IVCF1dCtC[start_day_:(start_day_+num_day_)]
P2IV1d <- data.frame(PCIV1dCtC=PCIV1dCtC, IVCF1dCtC=IVCF1dCtC)
(gg_<-ggplot(P2IV1d,aes(x=PCIV1dCtC,y=IVCF1dCtC))+geom_point())
co<-cor(PCIV1dCtC,IVCF1dCtC)
#linear regression
norns.lm<-lm(IVCF1dCtC~PCIV1dCtC, data=P2IV1d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")

PC1dCtC<-histPrc_$PC1dCtC[start_day_:(start_day_+num_day_)]
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


##1d Close to Open
PCIV1dCtO<-histPrc_$PCIV1dCtO[start_day_:(start_day_+num_day_)]
IVCF1dCtO<-histIV_$IVCF1dCtO[start_day_:(start_day_+num_day_)]
P2IV1d <- data.frame(PCIV1dCtO=PCIV1dCtO, IVCF1dCtO=IVCF1dCtO)
(gg_<-ggplot(P2IV1d,aes(x=PCIV1dCtO,y=IVCF1dCtO))+geom_point())
co<-cor(PCIV1dCtO,IVCF1dCtO)
#linear regression
norns.lm<-lm(IVCF1dCtO~PCIV1dCtO, data=P2IV1d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")

PC1dCtO<-histPrc_$PC1dCtO[start_day_:(start_day_+num_day_)]
P2IV1d <- data.frame(PC1dCtO=PC1dCtO, IVCF1dCtO=IVCF1dCtO)
(gg_<-ggplot(P2IV1d,aes(x=PC1dCtO,y=IVCF1dCtO))+geom_point())
co<-cor(PC1dCtO,IVCF1dCtO)
#linear regression
norns.lm<-lm(IVCF1dCtO~PC1dCtO, data=P2IV1d)
summary(norns.lm)
gg_+geom_abline(intercept=norns.lm$coefficient[1],slope=norns.lm$coefficient[2],color="orange")

#Regression File save
mean(PC1dCtO)
sd(PC1dCtO)
mean(IVCF1dCtO)
sd(IVCF1dCtO)
save.PC2IV(model=norns.lm,PC=getvarname(PC1dCtO),IVC=getvarname(IVCF1dCtO),cor=co,
           pcstat=c(mean(PC1dCtO),sd(PC1dCtO)),
           ivstat=c(mean(IVCF1dCtO),sd(IVCF1dCtO)))

#Load test
load.PC2IV(PC=getvarname(PC1dCtO),IVC=getvarname(IVCF1dCtO))
PC1dCtO_IVCF1dCtO
rm(PC1dCtO_IVCF1dCtO)

rm(gg_,co,histIV_,histPrc_)
rm(PC1dCtC,PC1dCtO,PC3dCtC,PC5dCtC,PC7dCtC,PCIV1dCtC,PCIV1dCtO,PCIV3dCtC,PCIV5dCtC,PCIV7dCtC)
rm(IVCF1dCtC,IVCF1dCtO,IVCF3dCtC,IVCF5dCtC,IVCF7dCtC)
rm(norns.lm,start_day_,num_day_)
rm(P2IV1d,P2IV3d,P2IV5d,P2IV7d)

###
# Volatility Analyzation --------

# Creating Option Chain(opch) and ATM IV(atmiv) data frames 

#read data file
rf_<-paste(DataFiles_Path_G,Underying_Symbol_G,"_OPChain_Pos.csv",sep="")
opch<-read.table(rf_,header=T,sep=",",nrows=50000)
rf_<-paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="")
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

# Now We've got atmiv.vcone.anal data.dframe
##END Got complete atmiv.vcone.anal

# Option Chain Analysis Without Nmlz **Just For Info --------
## option chain 3D volatility plot
#Put OOM
#opch %>%  dplyr::filter(TYPE==1) %>% 
#   dplyr::filter(OrigIV/ATMIV<3.0)  %>% dplyr::filter(OrigIV/ATMIV>0.2) %>%
#   dplyr::filter(HowfarOOM>=0) %>% dplyr::filter(TimeToExpDate>TimeToExp_Limit_Closeness_G) -> vplot
# # dplyr::filter(HowfarOOM>=0) %>% dplyr::filter(TimeToExpDate>0.3) -> vplot
# rgl::plot3d(vplot$Moneyness.Frac,vplot$TimeToExpDate,vplot$OrigIV,col=rainbow(100))
# rgl::clear3d()
# 
# #Put ITM
# opch %>% dplyr::filter(TYPE==1) %>%
#   dplyr::filter(OrigIV/ATMIV<3.0) %>% dplyr::filter(OrigIV/ATMIV>0.2) %>%
#   dplyr::filter(HowfarOOM<0) %>% dplyr::filter(TimeToExpDate>0.3) -> vplot
# rgl::plot3d(vplot$Moneyness.Frac,vplot$TimeToExpDate,vplot$OrigIV,col=rainbow(100))
# rgl::clear3d()
# 
# #Call OOM
# opch %>% dplyr::filter(TYPE==-1) %>%
#   dplyr::filter(OrigIV/ATMIV<3.0) %>% dplyr::filter(OrigIV/ATMIV>0.2) %>%
#   dplyr::filter(HowfarOOM>=0) %>% dplyr::filter(TimeToExpDate>0.3) -> vplot
# rgl::plot3d(vplot$Moneyness.Frac,vplot$TimeToExpDate,vplot$OrigIV,col=rainbow(100))
# rgl::clear3d()
# 
# #Call ITM
# opch %>% dplyr::filter(TYPE==-1) %>%
#   dplyr::filter(OrigIV/ATMIV<3.0) %>% dplyr::filter(OrigIV/ATMIV>0.2) %>%
#   dplyr::filter(HowfarOOM<0) %>% dplyr::filter(TimeToExpDate>0.3) -> vplot
# rgl::plot3d(vplot$Moneyness.Frac,vplot$TimeToExpDate,vplot$OrigIV,col=rainbow(100))
# rgl::clear3d()
#rm(vplot)

# Nmlzd Skew Analysis Variations **Just for Info -----------
#Normalized Skew
#Put
# opch %>% dplyr::filter(TYPE==1) %>%
#   dplyr::filter(OrigIV/ATMIV<3.0) %>% dplyr::filter(OrigIV/ATMIV>0.2) %>%
#   dplyr::filter(HowfarOOM>=-100) %>% dplyr::filter(TimeToExpDate>0.3) -> vplot
# (ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV),size=TimeToExpDate/2,colour=Date))+geom_point(alpha=0.2))
# 
# #Call
# opch %>% dplyr::filter(TYPE==-1) %>%
#   dplyr::filter(OrigIV/ATMIV<3.0) %>% dplyr::filter(OrigIV/ATMIV>0.2) %>%
#   dplyr::filter(HowfarOOM>=-100) %>% dplyr::filter(TimeToExpDate>0.3) -> vplot
# (ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV),size=TimeToExpDate/2,colour=Date))+geom_point(alpha=0.2))
# 
# #Complete Opchain. Using OOM/ATM options.
# opch %>% dplyr::filter(OrigIV/ATMIV<5.0) %>% dplyr::filter(OrigIV/ATMIV>0.1) %>%
#   dplyr::filter(TimeToExpDate>0.3) -> vplot
# (ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV),size=TimeToExpDate/2,colour=TYPE))+geom_point(alpha=0.2))

##
# Volatility Skew Regression  -----------

# Nmlzd Skew
#Complete Opchain. Using OOM options. By Call-Put parity, ITM IV is supposed to be the same as OOM IV.
opch %>% dplyr::filter(OrigIV/ATMIV<5.0) %>% dplyr::filter(OrigIV/ATMIV>0.1) %>%
  dplyr::filter(HowfarOOM>=0) %>% dplyr::filter(TimeToExpDate>TimeToExp_Limit_Closeness_G) -> vplot
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV),size=TimeToExpDate/2,colour=Date))+geom_point(alpha=0.2))


#1. Poly
# models <- (get.skew.regression.Models(vplot))
# get.predicted.skew(models,xmin=-1,x_by=0)
# vplot_exp <- get.predicted.skew(models)
# (ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV)))+geom_point(alpha=0.2)+
#    geom_line(data=vplot_exp,aes(Moneyness,fit),color="red",size=0.73))

#5. smooth splines
models <- (get.skew.regression.Models(vplot,regtype=5,df=7))
#just get one predicted value. wrapped by get.predicted.skew()
get.predicted.skew(models,regtype=5,xmin=-1,x_by=0)
(predict.c<-get.predicted.skew(models,regtype=5,xmin=-2.0,xmax=1.5))
(ggplot(vplot,aes(x=Moneyness.Nm,y=(OrigIV/ATMIV)))+geom_point(alpha=0.2)+
   geom_line(data=data.frame(Moneyness.Nm=predict.c$x,IV2ATMIV=predict.c$y),aes(Moneyness.Nm,IV2ATMIV),color="red"))

save.Skew(models)
#load test
load.Skew()
SkewModel
rm(SkewModel)
rm(models,vplot,vplot_exp,predict.c)

##
# Vcone Regression ------------------------

# All Type Vcone *Just for Info -----
#Plotting all type. IV is normalized
#vcone created
vcone<-make.vcone.df(atmiv=atmiv,type=0)
#(gg_<-ggplot(vcone,aes(x=Month,y=IV,colour=TYPE))+geom_point())
(gg_<-ggplot(vcone,aes(x=Month,y=IV2IDX.nm,colour=TYPE))+geom_point())
# Regression of PUT vcone
#   1.linear
predict.c <- vcone_regression(vcone=vcone,regtype=1,ret=2)
(gg_<-ggplot(vcone,aes(x=Month,y=IV2IDX.nm,colour=TYPE))+geom_point()+
   geom_line(data=data.frame(vcone,fit=predict.c),aes(Month,fit)))
#   3.poly
predict.c <- vcone_regression(vcone=vcone,regtype=3,ret=2)
(gg_<-ggplot(vcone,aes(x=Month,y=IV2IDX.nm,colour=TYPE))+geom_point()+
   geom_line(data=data.frame(vcone,fit=predict.c),aes(Month,fit)))
#   5.smooth spline
# predict() S3 method for class 'smooth.spline'
# predict(object, x, deriv = 0, ...)
#   Arguments
#    object  a fit from smooth.spline.
#    x	the new values of x.
#    deriv	integer; the order of the derivative required.
#  Value
#   A list with components x(The input x),y(The fitted values or derivatives at x)
#(predict(smooth.spline(vcone$Month,vcone$IV2IDX.nm,df=3),x=2))
(predict.c <- predict(smooth.spline(vcone$Month,vcone$IV2IDX.nm,df=3),x=seq(0,max(vcone$Month),by=0.1)))
(gg_<-ggplot(vcone,aes(x=Month,y=IV2IDX.nm))+geom_point()+
   geom_line(data=data.frame(Month=predict.c$x,IV2IDX.nm=predict.c$y),aes(Month,IV2IDX.nm)))
rm(gg_,vcone,predict.c)

#  Put Vcone IV is normalized 
#Creating vcone.
vcone<-make.vcone.df(atmiv=atmiv,type=1)
(ggplot(vcone,aes(x=Month,y=IV2IDX.nm,colour=TYPE))+geom_point())
# Regression of PUT vcone
#   1.linear
# predict.c <- vcone_regression(vcone=vcone,regtype=1,ret=2)
# (ggplot(vcone,aes(x=Month,y=IV2IDX.nm,colour=TYPE))+geom_point()+
#    geom_line(data=data.frame(vcone,fit=predict.c),aes(Month,fit)))
#   3.poly
# predict.c <- vcone_regression(vcone=vcone,regtype=3,ret=2)
# (ggplot(vcone,aes(x=Month,y=IV2IDX.nm,colour=TYPE))+geom_point()+
#    geom_line(data=data.frame(vcone,fit=predict.c),aes(Month,fit)))

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
#   1.linear
# predict.c <- vcone_regression(vcone=vcone,regtype=1,ret=2)
# (ggplot(vcone,aes(x=Month,y=IV2IDX.nm,colour=TYPE))+geom_point()+
#    geom_line(data=data.frame(vcone,fit=predict.c),aes(Month,fit)))
#   3.poly
# predict.c <- vcone_regression(vcone=vcone,regtype=3,ret=2)
# (ggplot(vcone,aes(x=Month,y=IV2IDX.nm,colour=TYPE))+geom_point()+
#    geom_line(data=data.frame(vcone,fit=predict.c),aes(Month,fit)))
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

#  All Type *Just for Info ----
vchg<-make.vchg.df(vcone=atmiv.vcone.anal,type=0)
(ggplot(vchg,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point())
#(gg_<-ggplot(vchg,aes(x=TimeToExpDate,y=VC.f.AbSdf,colour=TYPE))+geom_point())
rm(gg_,vchg)

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
#     1.linear
# vchg_t<-vchg_plus
# predict.c <- vchg_regression(vchg=vchg_t,regtype=1,ret=2)
# (ggplot(vchg_t,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point()+
#    geom_line(data=data.frame(vchg_t,fit=predict.c),aes(TimeToExpDate,fit)))
# rm(vchg_t)
# 
# vchg_t<-vchg_mns
# predict.c <- vchg_regression(vchg=vchg_t,regtype=1,ret=2)
# (ggplot(vchg_t,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point()+
#    geom_line(data=data.frame(vchg_t,fit=predict.c),aes(TimeToExpDate,fit)))
#rm(vchg_t)
#    2.exponent function
# vchg_t<-vchg_plus
# predict.c <- vchg_regression(vchg=vchg_t,regtype=2,ret=2,start=c(a=-0.02,b=0.6,c=0.01))
# (ggplot(vchg_t,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point()+
#    geom_line(data=data.frame(vchg_t,fit=predict.c),aes(TimeToExpDate,fit)))
# rm(vchg_t)
# 
# vchg_t<-vchg_mns
# predict.c <- vchg_regression(vchg=vchg_t,regtype=2,ret=2,start=c(a=0.2,b=0.2,c=-0.2))
# (ggplot(vchg_t,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point()+
#    geom_line(data=data.frame(vchg_t,fit=predict.c),aes(TimeToExpDate,fit)))
# rm(vchg_t)

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
#    2.exponent function
# vchg_t<-vchg_plus
# predict.c <- vchg_regression(vchg=vchg_t,regtype=2,ret=2,start=c(a=0.1,b=-0.1,c=-0.03))
# (ggplot(vchg_t,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point()+
#    geom_line(data=data.frame(vchg_t,fit=predict.c),aes(TimeToExpDate,fit)))
# rm(vchg_t)

# vchg_t<-vchg_mns
# predict.c <- vchg_regression(vchg=vchg_t,regtype=2,ret=2,start=c(a=0.2,b=0.2,c=-0.2))
# (ggplot(vchg_t,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point()+
#    geom_line(data=data.frame(vchg_t,fit=predict.c),aes(TimeToExpDate,fit)))
# rm(vchg_t)
#     1.linear 
# vchg_t<-vchg_plus
# predict.c <- vchg_regression(vchg=vchg_t,regtype=1,ret=2)
# (ggplot(vchg_t,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point()+
#    geom_line(data=data.frame(vchg_t,fit=predict.c),aes(TimeToExpDate,fit)))
# vchg_t<-vchg_mns
# predict.c <- vchg_regression(vchg=vchg_t,regtype=1,ret=2)
# (ggplot(vchg_t,aes(x=TimeToExpDate,y=VC.f,colour=TYPE))+geom_point()+
#    geom_line(data=data.frame(vchg_t,fit=predict.c),aes(TimeToExpDate,fit)))
# rm(vchg_t)
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
wf_<-paste(DataFiles_Path_G,Underying_Symbol_G,"_OPChain_Skew.csv",sep="")
write.table(opch,wf_,quote=T,row.names=F,sep=",")
rm(wf_)

rm(atmiv.vcone.anal,atmiv,opch)

##
#Function to be loaded ------------------

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
#IVCFndCtC
IVCFndCtC <- function(iv,n){
  q_ <- replace(iv, rep(1:(length(iv)-n)),iv[(1+n):length(iv)])
  civ_n_ <- replace(q_,rep((length(q_)-(n-1)):length(q_)),NA)
  ret_ <- (iv-civ_n_)/civ_n_
  ret_
}

#Save volatility Level correlaiton and regression function
save.PC2IV <- function (model, PC, IVC,cor,pcstat,ivstat) {
  reg_saved<-list(model)
  reg_saved<-c(reg_saved,list(cor))
  data.frame(Avr=pcstat[1],SD=pcstat[2]) %>% 
    full_join(data.frame(Avr=ivstat[1],SD=ivstat[2])) -> stat
  rownames(stat)<-c("PC","IVC") 
  reg_saved<-c(reg_saved,list(stat))
  names(reg_saved)<-c("model","cor","stat")
  
  #saved file name.
  reg_saved_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_",PC,"_",IVC,sep="")
  save(reg_saved,file=reg_saved_fn)  
}

#Load volatility Level correlaiton and regression function
load.PC2IV <- function (PC,IVC) {
  #load file name.
  reg_load_fn <- paste(DataFiles_Path_G,Underying_Symbol_G,"_",PC,"_",IVC,sep="")
  load(reg_load_fn)
  assign(paste(PC,"_",IVC,sep=""), reg_saved,env=.GlobalEnv)
}

#Just like get.predected.Skew. The differnce is here we only use linier regression
get.predicted.IVIDXChange<-function(model,xmin=-0.03,xmax=0.03,x_by=0.01){  
  intercept=model$coefficient[1]
  slope=model$coefficient[2]
  names(intercept)<-c("1"); names(slope)<-c("1")
  if(x_by==0){
    x<-c(xmin)
    y<-intercept+slope*xmin
  }else{
    x<-seq(xmin,xmax,by=x_by)
    y<-intercept+slope*x   
  }
  ivchg<-data.frame(PC=x,IVIDXC=y)
  ivchg
}

#Return the new data frame that contains the predicted value (fit column)
#from xmin to xmax by x_by data interval. If x_by is given as 0, then
#single predicted value of Moneyness=xmin is returned as a one data vector.
get.predicted.skew<-function(models,regtype=1,xmin=-2.0,xmax=1.5,x_by=0.01){  
  if(x_by==0){
    if(regtype==1){
      if(xmin<=0){
        vplot<-predict(models$model.m,newdata=data.frame(Moneyness=xmin))
      }else{
        vplot<-predict(models$model.p,newdata=data.frame(Moneyness=xmin))
      }
    }else if(regtype==5){
      if(xmin<=0){
        vplot<-predict(models$model.m,x=xmin)
      }else{
        vplot<-predict(models$model.p,x=xmin)
      }
      vplot<-vplot$y
    }
    return(vplot)
  }
  if(regtype==1){
    vplot_m<-data.frame(Moneyness=seq(xmin,0,by=x_by))  
    predict.m <- predict(models$model.m,newdata=vplot_m)
    vplot_m<-data.frame(vplot_m,fit=predict.m)
    
    vplot_p<-data.frame(Moneyness=seq(x_by,xmax,by=x_by))  
    predict.p <- predict(models$model.p,newdata=vplot_p)
    vplot_p<-data.frame(vplot_p,fit=predict.p)
    vplot_m %>% dplyr::full_join(vplot_p) -> vplot
  }else if(regtype==5){
    vplot_m<-predict(models$model.m,x=seq(xmin,0,by=x_by))
    vplot_p<-predict(models$model.p,x=seq(x_by,xmax,by=x_by))
    vplot<-list(append(vplot_m$x,vplot_p$x))
    vplot<-c(vplot,list(append(vplot_m$y,vplot_p$y)))
    names(vplot)<-c("x","y")
  }
  vplot
}

# This is the get.predicted.skew() dedicated to SmoothSpline regression.
# monesness and retuned values are Vectorized.
get.predicted.spline.skew<-function(models,moneyness){
  skew_nm<-as.numeric(moneyness<=0)*predict(models$model.m,x=moneyness)$y
  skew_nm<-skew_nm + as.numeric(moneyness>0)*predict(models$model.m,x=moneyness)$y
  skew_nm
}


#return list that includes model.m(Moneyness<0) and model.p(Moneyness>0)
#if the same model is employed regardless of Moneyness's value, then
# model.p and model.p is identical.
get.skew.regression.Models<-function(vplot,regtype=1,moneyness_adjust=0.1,atm_adjust=0.0,df=5){
  data.frame(Moneyness=vplot$Moneyness.Nm,
             Month=vplot$TimeToExpDate,
             IV2ATMIV=vplot$OrigIV/vplot$ATMIV)->vplot
  vplot %>% dplyr::filter(Moneyness<=moneyness_adjust) %>% 
    dplyr::filter(abs(IV2ATMIV-1.0)>=atm_adjust) -> vplot_mns
  vplot %>% dplyr::filter(Moneyness>(-1*moneyness_adjust)) %>% 
    dplyr::filter(abs(IV2ATMIV-1.0)>=atm_adjust) -> vplot_pls
  
  if(regtype==1){
    model.m<-lm(IV2ATMIV~1+Moneyness+I(Moneyness^2),data=vplot_mns)
    #predict.m <- predict(model.m)
    model.p<-lm(IV2ATMIV~1+Moneyness+I(Moneyness^2),data=vplot_pls)
    #predict.p <- predict(model.p)
  }else if(regtype==5){
    model.m<-smooth.spline(vplot_mns$Moneyness,vplot_mns$IV2ATMIV,df=df)
    model.p<-smooth.spline(vplot_pls$Moneyness,vplot_pls$IV2ATMIV,df=df)
  }
  
  models<-list(model.m)
  models<-c(models,list(model.p))
  names(models)<-c("model.m","model.p")
  
  models
}

save.Skew<- function(models){
  reg_saved_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Skew",sep="")
  save(models,file=reg_saved_fn)
}

load.Skew<- function() {
  #load file name.
  reg_load_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Skew",sep="")
  load(reg_load_fn)
  assign("SkewModel",models,env=.GlobalEnv)
}

# ATM IV Volatility Change to IVIDX as to Time 

save.IVChg<- function(model,optype,up_dn){
  if(optype==OpType_Put_G){
    if(up_dn>=0){
      reg_saved_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_PutIVChgUp",sep="")
    }else if(up_dn<0){
      reg_saved_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_PutIVChgDown",sep="")
    }
  }else if(optype==OpType_Call_G){
    if(up_dn>=0){
      reg_saved_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_CallIVChgUp",sep="")
    }else if(up_dn<0){
      reg_saved_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_CallIVChgDown",sep="")
    }
  }
  save(model,file=reg_saved_fn)
}

load.IVChg<- function(optype,up_dn) {
  if(optype==OpType_Put_G){
    if(up_dn>=0){
      reg_load_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_PutIVChgUp",sep="")
      load(reg_load_fn)
      assign("PutIVChgUp",model,env=.GlobalEnv)
    }else if(up_dn<0){
      reg_load_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_PutIVChgDown",sep="")
      load(reg_load_fn)
      assign("PutIVChgDown",model,env=.GlobalEnv)
    }
  }else if(optype==OpType_Call_G){
    if(up_dn>=0){
      reg_load_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_CallIVChgUp",sep="")
      load(reg_load_fn)
      assign("CallIVChgUp",model,env=.GlobalEnv)
    }else if(up_dn<0){
      reg_load_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_CallIVChgDown",sep="")
      load(reg_load_fn)
      assign("CallIVChgDown",model,env=.GlobalEnv)
    }
  }
}

# Vcone Analysis Functions

#Making Volatility Cone data frame
make.vcone.df<-function(atmiv,type=0){
  if(type!=0){
    atmiv %>% filter(TYPE==type) -> atmiv
  }
  vcone<-data.frame(Month=atmiv$TimeToExpDate, IV=atmiv$ATMIV,IVIDX=atmiv$IVIDX,TYPE=atmiv$TYPE)  
  #volatility normalize
  iv_mean_p<-mean(vcone$IV)
  ividx_mean_p<-mean(vcone$IVIDX)
  vcone %>% dplyr::mutate(IV.nm=IV/iv_mean_p) -> vcone
  #vcone %>% dplyr::mutate(IV2IDX.nm=IV/ividx_mean_p) -> vcone
  vcone %>% dplyr::mutate(IV2IDX.nm=IV/IVIDX) -> vcone
  #Time filtering, because when IV is not stable when Time is very close to ExpDate .
  vcone %>% dplyr::filter(Month>=0.30) -> vcone
  vcone
}

#Making Volatility Change data frame
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
  }else if(regtype==3){
    nls.m<-lm(IV2IDX.nm~1+Month+I(Month^2),data=vcone)
  }
  if(ret==1){
    nls.m
  }else{
    predict.m <- predict(nls.m)
    predict.m
  }
}

vchg_regression<-function(vchg,regtype=2,start=NULL,ret=1){
  if(regtype==2){
    nls.m<-nls(VC.f~a*TimeToExpDate^b+c,data=vchg,start=start)
  }else if(regtype==1){
    nls.m<-lm(VC.f~TimeToExpDate,data=vchg)
  }
  if(ret==1){
    nls.m
  }else{
    predict.m <- predict(nls.m)
    predict.m
  }
}

save.VCone<- function(model,optype){
  if(optype==OpType_Put_G){
    reg_saved_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_PutVCone",sep="")
  }else if(optype==OpType_Call_G){
    reg_saved_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_CallVCone",sep="")
  }
  save(model,file=reg_saved_fn)
}

load.VCone<- function(optype) {
  if(optype==OpType_Put_G){
    reg_load_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_PutVCone",sep="")
    load(reg_load_fn)
    assign("PutVCone",model,env=.GlobalEnv)
  }else if(optype==OpType_Call_G){
    reg_load_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_CallVCone",sep="")
    load(reg_load_fn)
    assign("CallVCone",model,env=.GlobalEnv)
  }
}


# Linear 3D Fitting **Just a test 
#Functions
#モデルオブジェエクとを引数として、xvarとyvarからzvarを予測する
#デフォルトでは指定されたxとy変数の範囲で、16x16グリッドを計算
predictgrid<-function(model,xvar,yvar,zvar,res=16,type=NULL){
  #モデルオブジェクトから予測面のxとy変数の範囲を決める
  #lmとglmなどで使用可能だが、他のモデルではカスタマイズが必要
  xrange<-range(model$model[[xvar]])
  yrange<-range(model$model[[yvar]])
  
  newdata<-expand.grid(x=seq(xrange[1],xrange[2],length.out=res),
                       y=seq(yrange[1],yrange[2],length.out=res))
  names(newdata)<-c(xvar,yvar)
  newdata[[zvar]]<-predict(model,newdata,type=type)
  newdata
}

#x,y,zの値を格納したlong形式のデータフレームを、xとyのベクトル行列zを
#含むリストに変換する
df2mat<-function(p,xvar=NULL,yvar=NULL,zvar=NULL){
  if(is.null(xvar)) xvar <- names(p)[1]
  if(is.null(yvar)) yvar <- names(p)[2]
  if(is.null(zvar)) zvar <- names(p)[3]
  
  x<-unique(p[[xvar]])
  y<-unique(p[[yvar]])
  z<-matrix(p[[zvar]],nrow=length(y),ncol=length(x))
  
  m<-list(x,y,z)
  names(m)<-c(xvar,yvar,zvar)
  m
}
