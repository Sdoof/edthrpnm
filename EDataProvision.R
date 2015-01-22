##
# Data preproceccing, preparing, bridge between Excel and R
##

#Read Excel CSV file and reformat as "OptionVariables format", saves as a CSV file.
#OptionVariables format is 
#  Position  ContactName	Date	ExpDate	TYPE	UDLY	Strike	Price	..(next line)
#  Delta	Gamma	Vega	Theta	Rho	OrigIV	IV

# We must read both (UDL)_HIST.csv and (UDLOPChain)_Pre.csv
# UDLY row must be merged. Following example must be referred.

#OPtionOpr/EOptionOprEuro 's functions should be pre-read
#When setting IV and Greeks
#EOPtionStimulations set.EuropeanOptionValueGreeks, etc should be also read.

#id     <- c("A","A","C","E")
#height <- c(158,152,177,166)
#D1     <- data.frame(ID=id, H=height)
#id     <- c("A","B","c","D","E")
#weight <- c(51, 55,56,57, 55)
#D2     <- data.frame(ID=id, W=weight)
#merge(D1, D2, all.x=T)
#以下で置き換えて考える
#D1<-OPChainPre
#D2<-data.frame(HIST$Date,HIST$Close)

#read data file
rf_<-paste(DataFiles_Path_G,Underying_Synbol_G,"_OPChain_Pre.csv",sep="")
opch_pr_<-read.table(rf_,header=T,sep=",")
opch_pr_$X.Change<-NULL
opch_pr_$Position<-0
opch_pr_$Price<-(opch_pr_$Bid+opch_pr_$Ask)/2

opch_pr_$Theta<-opch_pr_$Vega<-opch_pr_$Gamma<-opch_pr_$Delta<-0
opch_pr_$IV<-opch_pr_$OrigIV<-opch_pr_$Rho<-0

rf_<-paste(DataFiles_Path_G,Underying_Synbol_G,"_IV.csv",sep="")
histIV_<-read.table(rf_,header=T,sep=",",nrows=1999)
histIV_<-data.frame(Date=histIV_$Date,IVIDX=histIV_$Close)

rf_<-paste(DataFiles_Path_G,Underying_Synbol_G,"_Hist.csv",sep="")
histPrc_<-read.table(rf_,header=T,sep=",",nrows=1999)
histPrc_<-data.frame(Date=histPrc_$Date,UDLY=histPrc_$Close)

#remove not used row
opch_pr_<-subset(opch_pr_,Volume!=0)
oprch_pr2<-merge(opch_pr_,histPrc_,all.x=T)
oprch_pr_<-oprch_pr2;
oprch_pr2<-NULL
#subset(oprch_pr_,Date=="2014/12/31")

#subset(oprch_pr_,Price==0)
oprch_pr_<-subset(oprch_pr_,Price!=0)
oprch_pr_<-subset(oprch_pr_,(Strike-UDLY)*TYPE<Price)
oprch_pr_<-subset(oprch_pr_,!(TYPE==-1 & Strike<900))

tryCatch(a<-set.IVOrig(xT=oprch_pr_),
         error=function(e){
           message(e)
           print(e)
         })

#> oprch_pr_[978,]
#Date Strike        ContactName   Last   Bid   Ask Change Volume OI IV   ExpDate TYPE Position
#1026 2014/12/16   1010 RUT150117C01010000 145.52 128.2 131.6      0      5  5  0 2015/1/17   -1        0
#Price Delta Gamma Vega Theta Rho OrigIV    UDLY
#1026 129.9     0     0    0     0   0      0 1139.37
#oprch_pr_<-oprch_pr_[-978,]
set.IVOrig(xT=oprch_pr_[1:1100,])
# oprch_pr_[1170,]
#Date Strike        ContactName Last Bid  Ask Change Volume OI IV    ExpDate TYPE Position
#1255 2014/12/16    300 RUT141220P00300000 0.04   0 0.75      0      1 50  0 2014/12/20    1        0
#Price Delta Gamma Vega Theta Rho OrigIV    UDLY
#1255 0.375     0     0    0     0   0      0 1139.37
#oprch_pr_<-oprch_pr_[-1170,]
#oprch_pr_[1642,]
#           Date Strike        ContactName   Last Bid Ask Change Volume OI IV   ExpDate TYPE Position
#1739 2014/12/17    960 RUT150117C00960000 217.05 213 217      0     16 30  0 2015/1/17   -1        0
#Price Delta Gamma Vega Theta Rho OrigIV    UDLY
#1739   215     0     0    0     0   0      0 1174.84
#oprch_pr_<-oprch_pr_[-1642,]
#Date Strike        ContactName Last Bid  Ask Change Volume OI IV    ExpDate TYPE Position
#1916 2014/12/17    300 RUT141220P00300000 0.04   0 0.65      0      1 50  0 2014/12/20    1        0
#Price Delta Gamma Vega Theta Rho OrigIV    UDLY

#> oprch_pr_<-oprch_pr_[-1804,]
