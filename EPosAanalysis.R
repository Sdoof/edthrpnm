library(dplyr)
library(RQuantLib)
library(ggplot2)

#evaluated position or set evaPos manually by copy&paste csv value
evaPos<-c(1,0,0,0,0,0,-1,0,0,0,0,0,0,0,0)

#UDLY draw limit. given absolute % value
UDLY_DrawRange<-0.10

#Show Graph of Delta Headged Position
ShowDeltaHedge=TRUE

#Config File
ConfigFileName_G="ConfigParameters.csv"
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"

ConfigParameters<-read.table(paste(DataFiles_Path_G,ConfigFileName_G,sep=""),
                             row.names=1, comment.char="#",header=T,stringsAsFactors=F,sep=",")
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
ResultFiles_Path_G=ConfigParameters["ResultFiles_Path_G",1]

# set days interval between which the position is analyzed step by step.
stepdays=as.numeric(ConfigParameters["AnalyStepdays",1])
udlStepNum=as.numeric(ConfigParameters["AnalyStepNum",1])
udlStepPct=as.numeric(ConfigParameters["AnalyStepPct",1])
MAX_DAY=as.numeric(ConfigParameters["AnalyMaxDay",1])

#HV_IV_Adjust_Ratio
HV_IV_Adjust_Ratio=as.numeric(ConfigParameters["EvalFnc_HV_IV_Adjust_Ratio",1])

#Holding Period
holdDays=as.numeric(ConfigParameters["holdDays",1])
#Number of Days for calculating Annualized Daily Volatility of Implied Volatility (DVIV)
dviv_caldays=as.numeric(ConfigParameters["dviv_caldays",1])
#Multipler of Position
PosMultip=as.numeric(ConfigParameters["PosMultip",1])

#Load opchain if evaPos is already assigned.
#if(sum(as.numeric(evaPos!=0))==0) {   #means evaPos=0 vector
# opchain<-theAlreadyCreatedPosition
#} else {
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre.csv",sep="")
opchain<-read.table(rf,header=T,sep=",",stringsAsFactors=FALSE)
rm(rf)
#} #endOfelse

##Historical Implied Volatility Data
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="") 
histIV<-read.table(rf,header=T,sep=",",nrows=1000);rm(rf)
#filtering
histIV %>% dplyr::transmute(Date=Date,IVIDX=Close/100) -> histIV
histIV %>% dplyr::filter(as.Date(Date,format="%Y/%m/%d")<=max(as.Date(opchain$Date,format="%Y/%m/%d"))) %>%
  dplyr::arrange(desc(as.Date(Date,format="%Y/%m/%d"))) %>% head(n=dviv_caldays) -> histIV

#Load Regression and Correlation Parameters
load.PC2IV(PC="PC3dCtC",IVC="IVCF3dCtC")
PC3dCtC_IVCF3dCtC
load.PC2IV(PC="PC5dCtC",IVC="IVCF5dCtC")
PC5dCtC_IVCF5dCtC
load.PC2IV(PC="PC7dCtC",IVC="IVCF7dCtC")
PC7dCtC_IVCF7dCtC
load.PC2IV(PC="PC1dCtC",IVC="IVCF1dCtC")
PC1dCtC_IVCF1dCtC
#load.PC2IV(PC="PC1dCtO",IVC="IVCF1dCtO")
#PC1dCtO_IVCF1dCtO
load.Skew()
SkewModel
load.Skew("_Put")
SkewModel_Put
load.Skew("_Call")
SkewModel_Call
load.VCone(optype=OpType_Put_G)
PutVCone
load.VCone(optype=OpType_Call_G)
CallVCone
load.IVChg(OpType_Put_G,10)
PutIVChgUp
load.IVChg(OpType_Put_G,-10)
PutIVChgDown
load.IVChg(OpType_Call_G,10)
CallIVChgUp
load.IVChg(OpType_Call_G,-10)
CallIVChgDown

#get evaluation days vector, evaldays
opchain$Date<-as.character(opchain$Date)
opchain$ExpDate<-as.character(opchain$ExpDate)
max_days<-min(get.busdays.between(opchain$Date,opchain$ExpDate))
if(MAX_DAY<max_days)
  max_days<-MAX_DAY
totalstep=max_days%/%stepdays 
evaldays<-rep(stepdays,times=totalstep)
evaldays<- cumsum(evaldays)
#if the lastday is ExpDate, we set the lastday as (lastday-1), 
# because the ExpDate option price is unstable.
if(evaldays[length(evaldays)]==min(get.busdays.between(opchain$Date,opchain$ExpDate))){
  if(stepdays==1){
    evaldays<-evaldays[-length(evaldays)]
  }else{
    evaldays[length(evaldays)]<-evaldays[length(evaldays)]-1
  }
}else if(evaldays[length(evaldays)]<MAX_DAY) {
  evaldays[length(evaldays)+1]<-max_days
}


#read analyzed positon. here we give by copy and paste
pos_anlys<-evaPos


#note opchain is already instanciated by the proceduces of ERskRtnEval
opchain$Position<-pos_anlys
opchain %>% dplyr::filter(Position!=0) -> thePosition

#thePosition's greek df and initial price
thePositonGrks<-getPositionGreeks(thePosition,multi=PosMultip,hdd=holdDays,HV_IV_Adjust_Ratio=HV_IV_Adjust_Ratio)
iniPrice <- thePositonGrks$Price
iniCredit <- -1*iniPrice

#total data frame
posStepDays<-data.frame(days=evaldays)

#Set data frames as a row value of another data frame.
posStepDays %>% group_by(days) %>%
  do(scene=createPositionEvalTable(position=thePosition,udlStepNum=udlStepNum,udlStepPct=udlStepPct,
                                   multi=PosMultip,hdd=stepdays,HV_IV_Adjust_Ratio=HV_IV_Adjust_Ratio)) -> posStepDays
posStepDays_vc<-posStepDays
#We must adjust each position values
posStepDays %>% group_by(days) %>% rowwise() %>% do(days=.$days,scene2=adjustPosChg(.$scene,.$days-stepdays,base_vol_chg=0,multi=PosMultip,hdd=holdDays,HV_IV_Adjust_Ratio=HV_IV_Adjust_Ratio)) -> tmp
unlist(tmp$days) -> posStepDays$days ; tmp$scene2 -> posStepDays$scene ;rm(tmp)
#We've got the complete posStepDays.

#Now drawing
drawGrktbl<-createAgrregatedGreekTbl(posStepDays,thePosition,udlStepNum=udlStepNum,udlStepPct=udlStepPct,multi=PosMultip,iniCredit=iniCredit)


#Effect Aggregation
drawGrktbl %>% dplyr::mutate(TotalEffect=ThetaEffect+DeltaEffect+GammaEffect+VegaEffect) -> drawGrktbl
drawGrktbl %>% dplyr::mutate(NdEffect=ThetaEffect+GammaEffect) -> drawGrktbl
drawGrktbl %>% dplyr::mutate(DEffect=DeltaEffect+VegaEffect) -> drawGrktbl

## Drawing profit
#limit the UDLY range.
drawGrktbl %>% dplyr::filter(UDLY>mean(thePosition$UDLY)*(1-UDLY_DrawRange)) %>% 
  dplyr::filter(UDLY<mean(thePosition$UDLY)*(1+UDLY_DrawRange)) -> drawGrktbl
#Profit and Greeks Combined Graph
gg<-ggplot(drawGrktbl,aes(x=UDLY,y=profit,group=day))
(
  gg
  +geom_line(size=0.9-0.01*round(drawGrktbl$day/stepdays),linetype=round(drawGrktbl$day/stepdays))
  +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$DeltaEffect,colour="blue",size=0.9-0.01*round(drawGrktbl$day/stepdays),linetype=round(drawGrktbl$day/stepdays))
  +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$GammaEffect,colour="red",size=0.9-0.01*round(drawGrktbl$day/stepdays),group=drawGrktbl$day,linetype=round(drawGrktbl$day/stepdays))
  +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$VegaEffect,colour="green",size=0.9-0.01*round(drawGrktbl$day/stepdays),group=drawGrktbl$day,linetype=round(drawGrktbl$day/stepdays))
  +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$ThetaEffect,colour="orange",size=0.9-0.01*round(drawGrktbl$day/stepdays),group=drawGrktbl$day,linetype=round(drawGrktbl$day/stepdays))
  +geom_point(x=thePositonGrks$UDLY,y=0,size=4.0,colour="black")
  +geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$DeltaEffect,size=4.0,colour="blue")+geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$GammaEffect,size=4.0,colour="red")
  +geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$VegaEffect,size=4.0,colour="green")+geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$ThetaEffect,size=4.0,colour="orange")
  +ylim(
    min(c(min(drawGrktbl$ThetaEffect),min(drawGrktbl$DeltaEffect),min(drawGrktbl$GammaEffect),min(drawGrktbl$VegaEffect),min(drawGrktbl$profit))),
    max(c(max(drawGrktbl$ThetaEffect),max(drawGrktbl$DeltaEffect),max(drawGrktbl$GammaEffect),max(drawGrktbl$VegaEffect),max(drawGrktbl$profit))))
)

##Delta Hedge Effect

if(ShowDeltaHedge){
  drawGrktbl_DltHgd<-drawGrktbl
  #initial Delta
  iniDelta<-getPosGreeks(pos=thePosition$Position,greek=thePosition$Delta,multi=PosMultip)
  #new Profit
  drawGrktbl_DltHgd$profit<-drawGrktbl_DltHgd$profit-as.numeric(iniDelta)*(drawGrktbl_DltHgd$UDLY-mean(thePosition$UDLY))
  #new DeltaEffect
  #temporal. you shoud get IVIX and calculte #expPriceChange<-mean(UDLY*(exp(ividx_td*sqrt(hdd/365))-1))
  expPriceChange<-as.numeric(drawGrktbl_DltHgd$Delta!=0)*drawGrktbl_DltHgd$DeltaEffect/(-abs(drawGrktbl_DltHgd$Delta))
  drawGrktbl_DltHgd$DeltaEffect<-drawGrktbl_DltHgd$DeltaEffect-as.numeric(iniDelta)*expPriceChange
  rm(expPriceChange)
  #new delta
  drawGrktbl_DltHgd$Delta<-drawGrktbl_DltHgd$Delta-as.numeric(iniDelta)
  
  gg<-ggplot(drawGrktbl_DltHgd,aes(x=UDLY,y=profit,group=day))
  (
  gg
  +geom_line(size=0.9-0.01*round(drawGrktbl_DltHgd$day/stepdays),linetype=round(drawGrktbl_DltHgd$day/stepdays))
  #+geom_line(x=drawGrktbl_DltHgd$UDLY,y=drawGrktbl_DltHgd$DeltaEffect,colour="blue",size=0.9-0.01*round(drawGrktbl_DltHgd$day/stepdays),linetype=round(drawGrktbl_DltHgd$day/stepdays))
  +geom_line(x=drawGrktbl_DltHgd$UDLY,y=drawGrktbl_DltHgd$GammaEffect,colour="red",size=0.9-0.01*round(drawGrktbl_DltHgd$day/stepdays),group=drawGrktbl_DltHgd$day,linetype=round(drawGrktbl_DltHgd$day/stepdays))
  #+geom_line(x=drawGrktbl_DltHgd$UDLY,y=drawGrktbl_DltHgd$VegaEffect,colour="green",size=0.9-0.01*round(drawGrktbl_DltHgd$day/stepdays),group=drawGrktbl_DltHgd$day,linetype=round(drawGrktbl_DltHgd$day/stepdays))
  +geom_line(x=drawGrktbl_DltHgd$UDLY,y=drawGrktbl_DltHgd$ThetaEffect,colour="orange",size=0.9-0.01*round(drawGrktbl_DltHgd$day/stepdays),group=drawGrktbl_DltHgd$day,linetype=round(drawGrktbl_DltHgd$day/stepdays))
  +geom_point(x=thePositonGrks$UDLY,y=0,size=4.0,colour="black")
  #+geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$DeltaEffect,size=4.0,colour="blue")+geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$VegaEffect,size=4.0,colour="green")
  +geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$GammaEffect,size=4.0,colour="red")+geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$ThetaEffect,size=4.0,colour="orange")
  +ylim(
    min(c(min(drawGrktbl_DltHgd$ThetaEffect),min(drawGrktbl_DltHgd$DeltaEffect),min(drawGrktbl_DltHgd$GammaEffect),min(drawGrktbl_DltHgd$VegaEffect),min(drawGrktbl_DltHgd$profit))),
    max(c(max(drawGrktbl_DltHgd$ThetaEffect),max(drawGrktbl_DltHgd$DeltaEffect),max(drawGrktbl_DltHgd$GammaEffect),max(drawGrktbl_DltHgd$VegaEffect),max(drawGrktbl_DltHgd$profit))))
  )
}

rm(gg,drawGrktbl)
rm(opchain,histIV,evaPos)
rm(evaldays,stepdays,pos_anlys,totalstep,udlStepNum,udlStepPct,iniPrice,iniCredit)
rm(posStepDays,posStepDays_vc,thePosition,thePositonGrks)
rm(PC1dCtC_IVCF1dCtC,PC3dCtC_IVCF3dCtC,PC5dCtC_IVCF5dCtC,PC7dCtC_IVCF7dCtC,SkewModel)
rm(CallIVChgDown,CallIVChgUp,CallVCone,PutIVChgDown,PutIVChgUp,PutVCone)
rm(ConfigFileName_G,ConfigParameters,EvalFuncSetting,HV_IV_Adjust_Ratio)
rm(riskFreeRate_G,divYld_G,OpType_Put_G,OpType_Call_G)
rm(CALENDAR_G,TimeToExp_Limit_Closeness_G,Underying_Symbol_G,DataFiles_Path_G,ResultFiles_Path_G)
rm(holdDays,dviv_caldays,PosMultip)

#Profit + Directional + Non Didectional(Intrinsic Advantageous) + Total Effect Graph
# gg<-ggplot(drawGrktbl,aes(x=UDLY,y=profit,group=day))
# (
#   gg + geom_line(size=0.9-0.01*round(drawGrktbl$day/stepdays),linetype=round(drawGrktbl$day/stepdays),colour="black")
#   +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$TotalEffect,size=0.9-0.01*round(drawGrktbl$day/stepdays),linetype=round(drawGrktbl$day/stepdays),colour="darkgreen") 
#   +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$NdEffect,size=0.9-0.01*round(drawGrktbl$day/stepdays),linetype=round(drawGrktbl$day/stepdays),colour="red")
#   +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$DEffect,size=0.9-0.01*round(drawGrktbl$day/stepdays),linetype=round(drawGrktbl$day/stepdays),colour="blue")
#   +geom_point(x=thePositonGrks$UDLY,y=0,size=4.0,colour="black")
#   +geom_point(x=thePositonGrks$UDLY,
#               y=thePositonGrks$DeltaEffect+thePositonGrks$VegaEffect+thePositonGrks$GammaEffect+thePositonGrks$ThetaEffect,
#               size=4.0,colour="darkgreen")
#   +geom_point(x=thePositonGrks$UDLY,
#               y=thePositonGrks$GammaEffect+thePositonGrks$ThetaEffect,
#               size=4.0,colour="red")
#   +geom_point(x=thePositonGrks$UDLY,
#               y=thePositonGrks$DeltaEffect+thePositonGrks$VegaEffect,
#               size=4.0,colour="blue")
#   +ylim( 
#     min(c(min(drawGrktbl$TotalEffect),min(drawGrktbl$NdEffect),min(drawGrktbl$DEffect),min(drawGrktbl$profit))), 
#     max(c(max(drawGrktbl$TotalEffect),max(drawGrktbl$NdEffect),max(drawGrktbl$DEffect),max(drawGrktbl$profit))))
# )

#Greeks Only Graph
# gg<-ggplot(drawGrktbl,aes(x=UDLY,y=ThetaEffect,group=day))
# (
#   gg + geom_line(size=0.9-0.01*round(drawGrktbl$day/stepdays),colour="orange",linetype=round(drawGrktbl$day/stepdays))
#   +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$DeltaEffect,size=0.9-0.01*round(drawGrktbl$day/stepdays),colour="blue",group=drawGrktbl$day,linetype=round(drawGrktbl$day/stepdays))
#   +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$GammaEffect,size=0.9-0.01*round(drawGrktbl$day/stepdays),colour="red",group=drawGrktbl$day,linetype=round(drawGrktbl$day/stepdays))
#   +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$VegaEffect,size=0.9-0.01*round(drawGrktbl$day/stepdays),colour="green",group=drawGrktbl$day,linetype=round(drawGrktbl$day/stepdays))
#   +geom_point(x=thePositonGrks$UDLY,y=0,size=4.0,colour="black")
#   +geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$DeltaEffect,size=4.0,colour="blue")+geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$GammaEffect,size=4.0,colour="red")
#   +geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$VegaEffect,size=4.0,colour="green")+geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$ThetaEffect,size=4.0,colour="orange")
#   +ylim(
#     min(c(min(drawGrktbl$ThetaEffect),min(drawGrktbl$DeltaEffect),min(drawGrktbl$GammaEffect),min(drawGrktbl$VegaEffect))),
#     max(c(max(drawGrktbl$ThetaEffect),max(drawGrktbl$DeltaEffect),max(drawGrktbl$GammaEffect),max(drawGrktbl$VegaEffect))))
# )
