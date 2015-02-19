##
# Data preproceccing, preparing, bridge between Excel and R
##

#get variable name utility.
getvarname <- function(v) {
  deparse(substitute(v))
}

#Read Excel CSV file and reformat as "OptionVariables format", saves as a CSV file.
#OptionVariables format is 
#  Position  ContactName	Date	ExpDate	TYPE	UDLY	Strike	Price	..(next line)
#  Delta	Gamma	Vega	Theta	Rho	OrigIV	IV

# We must read both (UDL)_HIST.csv and (UDL)_OPChain_Pre.csv
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

#rf_<-paste(DataFiles_Path_G,Underying_Synbol_G,"_IV.csv",sep="")
#histIV_<-read.table(rf_,header=T,sep=",",nrows=1999)
#histIV_<-data.frame(Date=histIV_$Date,IVIDX=histIV_$Close)

rf_<-paste(DataFiles_Path_G,Underying_Synbol_G,"_Hist.csv",sep="")
histPrc_<-read.table(rf_,header=T,sep=",",nrows=1999)
histPrc_<-data.frame(Date=histPrc_$Date,UDLY=histPrc_$Close)

#remove not used row
opch_pr_<-subset(opch_pr_,Volume!=0)
opch_pr2<-merge(opch_pr_,histPrc_,all.x=T)
opch_pr_<-oprch_pr2;
opch_pr2<-NULL
rm(rf_)
rm(opch_pr2)
#subset(opch_pr_,Date=="2014/12/31")

#subset(opch_pr_,Price==0)
opch_pr_<-subset(opch_pr_,Price!=0)
opch_pr_<-subset(opch_pr_,(Strike-UDLY)*TYPE<Price)
opch_pr_<-subset(opch_pr_,!(TYPE==-1 & Strike<900))
#spread<ASK*k
k<-0.4
subset(opch_pr_,!((Ask-Bid)>(Ask*k)))
opch_pr_<-subset(opch_pr_,!((Ask-Bid)>(Ask*k)))
rm(k)

#IVの計算で不定となる要素をdelte vectorに格納
delete<-c(-1)
N<-nrow(opch_pr_)
for(i in 1:N){
  tryCatch(a<-set.IVOrig(xT=opch_pr_[i,]),
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
nrow(opch_pr_)
nrow(opch_pr_[delete,])
opch_pr_<-opch_pr_[delete,]
nrow(opch_pr_)
rm(delete)
#IVの計算
opch_pr_$OrigIV<-set.IVOrig(xT=opch_pr_)

tmp<-set.EuropeanOptionValueGreeks(opch_pr_)
opch_pr_$Price<-tmp$Price
opch_pr_$Delta<-tmp$Delta
opch_pr_$Gamma<-tmp$Gamma
opch_pr_$Vega<-tmp$Vega
opch_pr_$Theta<-tmp$Theta
opch_pr_$Rho<-tmp$Theta
rm(tmp)

wf_<-paste(DataFiles_Path_G,Underying_Synbol_G,"_OPChain_Pos.csv",sep="")
write.table(opch_pr_,wf_,quote=T,row.names=F,sep=",")
rm(wf_)
