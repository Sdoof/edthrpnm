library(dplyr)
library(RQuantLib)
library(ggplot2)
rm(list=ls())
source('./ESourceRCode.R',encoding = 'UTF-8')

#Config File
ConfigFileName_G="ConfigParameters.csv"
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"
ConfigParameters<-read.table(paste(DataFiles_Path_G,ConfigFileName_G,sep=""),
                             row.names=1, comment.char="#",header=T,stringsAsFactors=F,sep=",")

#File
Underying_Symbol_G=ConfigParameters["Underying_Symbol_G",1]
ResultFiles_Path_G=ConfigParameters["ResultFiles_Path_G",1]

#Evaluation CSV file
EvalPos_fn=paste(ResultFiles_Path_G,Underying_Symbol_G,"_EvalPosition.csv",sep='')

#Here you spicify which position should be analyzed.
eval_pos_idx=1

#UDLY draw limit. given absolute % value
UDLY_DrawRange<-0.10

#Show Graph of Delta Headged Position
ShowDeltaHedge=TRUE

#Volatility Sensitivity Checked
VolSensitivityCheck=TRUE

#First day line mede Blank
FirstDayLineMadeBlank=TRUE

# set days interval between which the position is analyzed step by step.
stepdays=as.numeric(ConfigParameters["AnalyStepdays",1])
udlStepNum=as.numeric(ConfigParameters["AnalyStepNum",1])
udlStepPct=as.numeric(ConfigParameters["AnalyStepPct",1])
MAX_DAY=as.numeric(ConfigParameters["AnalyMaxDay",1])


##
# General Settings
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

#For calculating GreekEffect for drawing, we assume holdDays as stepdays
holdDays=stepdays

#Number of Days for calculating Annualized Daily Volatility of Implied Volatility (DVIV)
dviv_caldays=as.numeric(ConfigParameters["dviv_caldays",1])
#Multipler of Position
PosMultip=as.numeric(ConfigParameters["PosMultip",1])

#HV_IV_Adjust_Ratio
HV_IV_Adjust_Ratio=as.numeric(ConfigParameters["EvalFnc_HV_IV_Adjust_Ratio",1])

#EvalFuncSetting
EvFNames <- c("holdDays","UdlStepNum","UdlStepPct","Posnum","Tail_rate","LossLimitPrice",
              "HV_IV_Adjust_Ratio","Weight_Drift","Delta_Thresh_Minus","Delta_Thresh_Plus","Vega_Thresh_Minus","Vega_Thresh_Plus",
              "Delta_Direct_Prf","Vega_Direct_Prf","Delta_Neutral_Offset","Vega_Neutral_Offset",
              "Profit_Coef","AdvEffect_Coef","AllEffect_Coef","DrctlEffect_Coef","MaxLoss_Coef",
              "SigmoidA_Numerator","SigmoidA_Denominator","ThetaEffectPositive","EvalConvex","DeltaHedge","GreekEfctOnHldD")
EvalFuncSetting<-vector("list",length(EvFNames))

EvalFuncSetting[[1]]<-as.numeric(ConfigParameters["holdDays",1])
EvalFuncSetting[[2]]<-as.numeric(ConfigParameters["EvalFnc_UdlStepNum",1])
EvalFuncSetting[[3]]<-as.numeric(ConfigParameters["EvalFnc_UdlStepPct",1])
EvalFuncSetting[[4]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Posnum",1])))
EvalFuncSetting[[5]]<-as.numeric(ConfigParameters["EvalFnc_Tail_rate",1]) 
EvalFuncSetting[[6]]<-as.numeric(ConfigParameters["EvalFnc_LossLimitPrice",1]) 
EvalFuncSetting[[7]]<-as.numeric(ConfigParameters["EvalFnc_HV_IV_Adjust_Ratio",1])
EvalFuncSetting[[8]]<-as.numeric(ConfigParameters["EvalFnc_Weight_Drift",1])
EvalFuncSetting[[9]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Delta_Thresh_Minus",1])))
EvalFuncSetting[[10]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Delta_Thresh_Plus",1])))
EvalFuncSetting[[11]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Vega_Thresh_Minus",1])))
EvalFuncSetting[[12]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Vega_Thresh_Plus",1])))
EvalFuncSetting[[13]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Delta_Direct_Prf",1])))
EvalFuncSetting[[14]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Vega_Direct_Prf",1])))
EvalFuncSetting[[15]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Delta_Neutral_Offset",1])))
EvalFuncSetting[[16]]<-eval(parse(text=gsub("\\$",",",ConfigParameters["EvalFnc_Vega_Neutral_Offset",1])))
EvalFuncSetting[[17]]<-as.numeric(ConfigParameters["EvalFnc_Profit_Coef",1])
EvalFuncSetting[[18]]<-as.numeric(ConfigParameters["EvalFnc_AdvEffect_Coef",1])
EvalFuncSetting[[19]]<-as.numeric(ConfigParameters["EvalFnc_AllEffect_Coef",1]) 
EvalFuncSetting[[20]]<-as.numeric(ConfigParameters["EvalFnc_DrctlEffect_Coef",1])
EvalFuncSetting[[21]]<-as.numeric(ConfigParameters["EvalFnc_MaxLoss_Coef",1])
EvalFuncSetting[[22]]<-as.numeric(ConfigParameters["EvalFnc_SigmoidA_Numerator",1])
EvalFuncSetting[[23]]<-as.numeric(ConfigParameters["EvalFnc_SigmoidA_Denominator",1])
EvalFuncSetting[[24]]<-ifelse(as.numeric(ConfigParameters["EvalFnc_ThetaEffectPositive",1])==1,TRUE,FALSE)
EvalFuncSetting[[25]]<-ifelse(as.numeric(ConfigParameters["EvalFnc_EvalConvex",1])==1,TRUE,FALSE)
EvalFuncSetting[[26]]<-ifelse(as.numeric(ConfigParameters["EvalFnc_DeltaHedgeToEvalProfit",1])==1,TRUE,FALSE)
EvalFuncSetting[[27]]<-ifelse(as.numeric(ConfigParameters["EvalFnc_GreekEffectEvalOnHoldDay",1])==1,TRUE,FALSE)

names(EvalFuncSetting)<-EvFNames
rm(EvFNames)

#Load opchain if evaPos is already assigned.
opchain<-read.table(paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre.csv",sep=""),header=T,sep=",",stringsAsFactors=FALSE)

##Historical Implied Volatility Data
histIV<-read.table(paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep=""),header=T,sep=",",nrows=1000)
#filtering
histIV %>% dplyr::transmute(Date=Date,IVIDX=Close/100) -> histIV
histIV %>% dplyr::filter(as.Date(Date,format="%Y/%m/%d")<=max(as.Date(opchain$Date,format="%Y/%m/%d"))) %>%
  dplyr::arrange(desc(as.Date(Date,format="%Y/%m/%d"))) %>% head(n=dviv_caldays) -> histIV

#Load Regression and Correlation Parameters
load.PC2IV(PC="PC1dCtC",IVC="IVCF1dCtC")
load.PC2IV(PC="PC3dCtC",IVC="IVCF3dCtC")
load.PC2IV(PC="PC5dCtC",IVC="IVCF5dCtC")
load.PC2IV(PC="PC7dCtC",IVC="IVCF7dCtC")
load.PC2IV(PC="PC12dCtC",IVC="IVCF12dCtC")
load.PC2IV(PC="PC18dCtC",IVC="IVCF18dCtC")
#PC1dCtO_IVCF1dCtO
load.Skew()
load.Skew("_Put")
load.Skew("_Call")
load.VCone(optype=OpType_Put_G)
load.VCone(optype=OpType_Call_G)
load.IVChg(OpType_Put_G,10)
load.IVChg(OpType_Put_G,-10)
load.IVChg(OpType_Call_G,10)
load.IVChg(OpType_Call_G,-10)

#get evaluation days vector, evaldays
opchain$Date<-as.character(opchain$Date)
opchain$ExpDate<-as.character(opchain$ExpDate)
max_days<- min(get.busdays.between(opchain$Date,opchain$ExpDate))-1
if(MAX_DAY<max_days)
  max_days<-MAX_DAY
totalstep=max_days%/%stepdays 
evaldays<-rep(stepdays,times=totalstep)
evaldays<- cumsum(evaldays)
if(stepdays!=1)
  evaldays<-append(evaldays, c(1), after=0)
#if the lastday is ExpDate, we set the lastday as (lastday-1), 
# because the ExpDate option price is unstable.
if(evaldays[length(evaldays)]==min(get.busdays.between(opchain$Date,opchain$ExpDate))){
  if(stepdays==1){
    evaldays<-evaldays[-length(evaldays)]
  }else{
    evaldays[length(evaldays)]<-evaldays[length(evaldays)]-1
  }
}else if(evaldays[length(evaldays)]<max_days) {
  evaldays[length(evaldays)+1]<-max_days
}

#note opchain is already instanciated by the proceduces of ERskRtnEval
opchain$Position<-evaPos
opchain %>% dplyr::filter(Position!=0) -> thePosition

###If needed, change thePositon's Date,Price,UDLY and re-calc the Greeks

#thePosition's greek df and initial price
thePositonGrks<-getPositionGreeks(thePosition,multi=PosMultip,hdd=holdDays,HV_IV_Adjust_Ratio=HV_IV_Adjust_Ratio)

#total data frame
posStepDays<-data.frame(days=evaldays)

#Set data frames as a row value of another data frame.
posStepDays %>% group_by(days) %>%
  do(scene=createPositionEvalTable(position=thePosition,udlStepNum=udlStepNum,udlStepPct=udlStepPct,
                                   multi=PosMultip,hdd=EvalFuncSetting$holdDays,HV_IV_Adjust_Ratio=HV_IV_Adjust_Ratio)) -> posStepDays

iniCredit <- -1*posStepDays$scene[[1]]$Price[udlStepNum+1]

#Use for checking volatility sensitivity
posStepDays_vc<-posStepDays

#We must adjust each position values
posStepDays %>% group_by(days) %>% rowwise() %>% do(days=.$days,scene2=adjustPosChg(.$scene,.$days-1,base_vol_chg=0,multi=PosMultip,hdd=holdDays,HV_IV_Adjust_Ratio=HV_IV_Adjust_Ratio,isDebug=TRUE)) -> tmp
unlist(tmp$days) -> posStepDays$days ; tmp$scene2 -> posStepDays$scene ;rm(tmp)

#Now drawing
drawGrktbl<-createAgrregatedGreekTbl(posStepDays,thePosition,udlStepNum=udlStepNum,udlStepPct=udlStepPct,multi=PosMultip,iniCredit=iniCredit)

#Effect Aggregation
drawGrktbl %>% dplyr::mutate(TotalEffect=ThetaEffect+DeltaEffect+GammaEffect+VegaEffect) -> drawGrktbl
drawGrktbl %>% dplyr::mutate(NdEffect=ThetaEffect+GammaEffect) -> drawGrktbl
drawGrktbl %>% dplyr::mutate(DEffect=DeltaEffect+VegaEffect) -> drawGrktbl

#limit the UDLY range.
drawGrktbl %>% dplyr::filter(UDLY>mean(thePosition$UDLY)*(1-UDLY_DrawRange)) %>% 
  dplyr::filter(UDLY<mean(thePosition$UDLY)*(1+UDLY_DrawRange)) -> drawGrktbl

##
# Drawing Profit and Greeks Combined Graph
draw_line_size_max=1.4
draw_line_size_min=0
draw_line_steps=length(evaldays)
draw_line_step_size=(draw_line_size_max-draw_line_size_min)/(draw_line_steps-1)

#draw_line_size vector thin to thick
draw_line_size=draw_line_size_min + draw_line_step_size*(ceiling((drawGrktbl$day-1)/stepdays))
#draw_line_type vector is ascending ordered
draw_line_type=rev(length(evaldays)-ceiling((drawGrktbl$day-1)/stepdays))
#draw_line_type vector is descending ordered
#draw_line_type=length(evaldays)-ceiling((drawGrktbl$day-1)/stepdays)

#first day line_type made blank
if(FirstDayLineMadeBlank){
  tmp <- (draw_line_type!=draw_line_type[1])*draw_line_type
  min_line_type=min(tmp[tmp>0])
  draw_line_type= (min_line_type<=1)*(draw_line_type!=draw_line_type[1])*draw_line_type +
    (min_line_type>1)*(draw_line_type!=draw_line_type[1])*(draw_line_type-1)
}

#if draw Delta and Vega Effect with sign
#draw_DeltaE_with_sign=(drawGrktbl$Delta>=0)*abs(drawGrktbl$DeltaEffect)+(drawGrktbl$Delta<0)*(-1)*abs(drawGrktbl$DeltaEffect)
#draw_VegaE_with_sign=(drawGrktbl$Vega>=0)*abs(drawGrktbl$VegaEffect)+(drawGrktbl$Vega<0)*(-1)*abs(drawGrktbl$VegaEffect)
gg<-ggplot(drawGrktbl,aes(x=UDLY,y=profit,group=day))
(
gg
+geom_line(size=draw_line_size,linetype=draw_line_type)
#+geom_line(x=drawGrktbl$UDLY,y=draw_DeltaE_with_sign,colour="blue",size=draw_line_size,linetype=draw_line_type)
#+geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$Delta,colour="blue",size=draw_line_size,linetype=draw_line_type)
+geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$GammaEffect,colour="red",size=draw_line_size,group=drawGrktbl$day,linetype=draw_line_type)
#+geom_line(x=drawGrktbl$UDLY,y=draw_VegaE_with_sign,colour="green",size=draw_line_size,group=drawGrktbl$day,linetype=draw_line_type)
#+geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$Vega,colour="green",size=draw_line_size,group=drawGrktbl$day,linetype=draw_line_type)
+geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$ThetaEffect,colour="orange",size=draw_line_size,group=drawGrktbl$day,linetype=draw_line_type)
+geom_point(x=thePositonGrks$UDLY,y=0,size=4.0,colour="black")
#+geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$Delta,size=4.0,colour="blue")+geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$GammaEffect,size=4.0,colour="red")
#+geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$Vega,size=4.0,colour="green")+geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$ThetaEffect,size=4.0,colour="orange")
+ylim(
  #min(c(min(drawGrktbl$ThetaEffect),min(drawGrktbl$Delta),min(drawGrktbl$GammaEffect),min(drawGrktbl$Vega),min(drawGrktbl$profit))),
  min(c(min(drawGrktbl$profit)),min(drawGrktbl$ThetaEffect),min(drawGrktbl$GammaEffect)),
  max(c(max(drawGrktbl$profit)),max(drawGrktbl$ThetaEffect),max(drawGrktbl$GammaEffect)))
#max(c(max(drawGrktbl$ThetaEffect),max(drawGrktbl$Delta),max(drawGrktbl$GammaEffect),max(drawGrktbl$Vega),max(drawGrktbl$profit))))
)

if(ShowDeltaHedge){
  #data frame for delta headge
  drawGrktbl_DltHgd<-drawGrktbl
  #initial Delta
  #iniDelta<-getPosGreeks(pos=thePosition$Position,greek=thePosition$Delta,multi=PosMultip)
  iniDelta<-posStepDays$scene[[1]]$Delta[udlStepNum+1]
  #adjustment to initial delta
  Delta_Adjust = 0
  headgedDelta = iniDelta + Delta_Adjust
  deltaHedged= -as.numeric(headgedDelta)
  #new Profit
  drawGrktbl_DltHgd$profit<-drawGrktbl_DltHgd$profit-as.numeric(headgedDelta)*(drawGrktbl_DltHgd$UDLY-mean(thePosition$UDLY))
  
  #drawGrktbl_DltHgd$profit<-drawGrktbl_DltHgd$profit-as.numeric(headgedDelta)*(drawGrktbl_DltHgd$UDLY-rep(posStepDays$scene[[1]]$UDLY,times=length(evaldays)))
  #new delta
  drawGrktbl_DltHgd$Delta<-drawGrktbl_DltHgd$Delta-as.numeric(headgedDelta)
  
  gg<-ggplot(drawGrktbl_DltHgd,aes(x=UDLY,y=profit,group=day))
  (
  gg
  +geom_line(size=draw_line_size,linetype=draw_line_type)
  #+geom_line(x=drawGrktbl_DltHgd$UDLY,y=drawGrktbl_DltHgd$Delta,colour="blue",size=draw_line_size,linetype=draw_line_type)
  +geom_line(x=drawGrktbl_DltHgd$UDLY,y=drawGrktbl_DltHgd$GammaEffect,colour="red",size=draw_line_size,group=drawGrktbl_DltHgd$day,linetype=draw_line_type)
  #+geom_line(x=drawGrktbl_DltHgd$UDLY,y=drawGrktbl_DltHgd$Vega,colour="green",size=draw_line_size,group=drawGrktbl_DltHgd$day,linetype=draw_line_type)
  +geom_line(x=drawGrktbl_DltHgd$UDLY,y=drawGrktbl_DltHgd$ThetaEffect,colour="orange",size=draw_line_size,group=drawGrktbl_DltHgd$day,linetype=draw_line_type)
  +geom_point(x=thePositonGrks$UDLY,y=0,size=4.0,colour="black")
  #+geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$Delta,size=4.0,colour="blue")+geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$Vega,size=4.0,colour="green")
  #+geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$GammaEffect,size=4.0,colour="red")+geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$ThetaEffect,size=4.0,colour="orange")
  +ylim(
    #min(c(min(drawGrktbl$ThetaEffect),min(drawGrktbl$Delta),min(drawGrktbl$GammaEffect),min(drawGrktbl$Vega),min(drawGrktbl$profit))),
    min(c(min(drawGrktbl_DltHgd$profit)),min(drawGrktbl_DltHgd$ThetaEffect),min(drawGrktbl_DltHgd$GammaEffect)),
    max(c(max(drawGrktbl_DltHgd$profit)),max(drawGrktbl_DltHgd$ThetaEffect),max(drawGrktbl_DltHgd$GammaEffect)))
  )
}

#volatility change %
if(VolSensitivityCheck){
  vol_chg<-0.2
  #copy for -vol_chg%
  posStepDays_vc_orig<-posStepDays_vc
  
  posStepDays_vc %>% group_by(days) %>% rowwise() %>% do(days=.$days,scene2=adjustPosChg(.$scene,.$days-1,base_vol_chg=vol_chg,multi=PosMultip,hdd=holdDays,HV_IV_Adjust_Ratio=HV_IV_Adjust_Ratio,isDebug=TRUE)) -> tmp
  unlist(tmp$days) -> posStepDays_vc$days ; tmp$scene2 -> posStepDays_vc$scene ;rm(tmp)
  drawGrktbl_vc_plus<-createAgrregatedGreekTbl(posStepDays_vc,thePosition,udlStepNum=udlStepNum,udlStepPct=udlStepPct,multi=PosMultip,iniCredit=iniCredit)
  
  posStepDays_vc<-posStepDays_vc_orig
  posStepDays_vc %>% group_by(days) %>% rowwise() %>% do(days=.$days,scene2=adjustPosChg(.$scene,.$days-1,base_vol_chg=(-1)*vol_chg,multi=PosMultip,hdd=holdDays,HV_IV_Adjust_Ratio=HV_IV_Adjust_Ratio,isDebug=TRUE)) -> tmp
  unlist(tmp$days) -> posStepDays_vc$days ; tmp$scene2 -> posStepDays_vc$scene ;rm(tmp)
  drawGrktbl_vc_mnus<-createAgrregatedGreekTbl(posStepDays_vc,thePosition,udlStepNum=udlStepNum,udlStepPct=udlStepPct,multi=PosMultip,iniCredit=iniCredit)
  
  drawGrktbl_vc_plus %>% dplyr::filter(UDLY>mean(thePosition$UDLY)*(1-UDLY_DrawRange)) %>% 
    dplyr::filter(UDLY<mean(thePosition$UDLY)*(1+UDLY_DrawRange)) -> drawGrktbl_vc_plus
  
  drawGrktbl_vc_mnus %>% dplyr::filter(UDLY>mean(thePosition$UDLY)*(1-UDLY_DrawRange)) %>% 
    dplyr::filter(UDLY<mean(thePosition$UDLY)*(1+UDLY_DrawRange)) -> drawGrktbl_vc_mnus
}

##
# Draw Delta Hedged Spread's Implied Volaility Sensitivity
if(ShowDeltaHedge && VolSensitivityCheck){
  #data frame for delta headge
  drawGrktbl_DltHgd_vc_plus<-drawGrktbl_vc_plus
  drawGrktbl_DltHgd_vc_mnus<-drawGrktbl_vc_mnus
  
  #new Profit
  drawGrktbl_DltHgd_vc_plus$profit<-drawGrktbl_DltHgd_vc_plus$profit - as.numeric(headgedDelta)*(drawGrktbl_DltHgd_vc_plus$UDLY-mean(thePosition$UDLY))
  drawGrktbl_DltHgd_vc_mnus$profit<-drawGrktbl_DltHgd_vc_mnus$profit - as.numeric(headgedDelta)*(drawGrktbl_DltHgd_vc_mnus$UDLY-mean(thePosition$UDLY))
  
  #new delta
  drawGrktbl_DltHgd_vc_plus$Delta<-drawGrktbl_DltHgd_vc_plus$Delta-as.numeric(headgedDelta)
  drawGrktbl_DltHgd_vc_mnus$Delta<-drawGrktbl_DltHgd_vc_mnus$Delta-as.numeric(headgedDelta)
  
  #data frame for delta headge
  drawGrktbl_DltHgd_vc_plus<-drawGrktbl_vc_plus
  drawGrktbl_DltHgd_vc_mnus<-drawGrktbl_vc_mnus
  
  #new Profit
  drawGrktbl_DltHgd_vc_plus$profit<-drawGrktbl_DltHgd_vc_plus$profit - as.numeric(headgedDelta)*(drawGrktbl_DltHgd_vc_plus$UDLY-mean(thePosition$UDLY))
  drawGrktbl_DltHgd_vc_mnus$profit<-drawGrktbl_DltHgd_vc_mnus$profit - as.numeric(headgedDelta)*(drawGrktbl_DltHgd_vc_mnus$UDLY-mean(thePosition$UDLY))
  
  #new delta
  drawGrktbl_DltHgd_vc_plus$Delta<-drawGrktbl_DltHgd_vc_plus$Delta-as.numeric(headgedDelta)
  drawGrktbl_DltHgd_vc_mnus$Delta<-drawGrktbl_DltHgd_vc_mnus$Delta-as.numeric(headgedDelta)
  
  gg<-ggplot(drawGrktbl_DltHgd,aes(x=UDLY,y=profit,group=day))
  (
  gg
  +geom_line(size=draw_line_size,linetype=draw_line_type,colour="gray60")
  +geom_line(x=drawGrktbl_DltHgd_vc_plus$UDLY,y=drawGrktbl_DltHgd_vc_plus$profit,colour="blue",size=draw_line_size,group=drawGrktbl_DltHgd_vc_plus$day,linetype=draw_line_type)
  +geom_line(x=drawGrktbl_DltHgd_vc_mnus$UDLY,y=drawGrktbl_DltHgd_vc_mnus$profit,colour="red",size=draw_line_size,group=drawGrktbl_DltHgd_vc_plus$day,linetype=draw_line_type)
  +geom_point(x=thePositonGrks$UDLY,y=0,size=4.0,colour="black")
  +ylim(
    min(c(min(drawGrktbl_DltHgd$profit),min(drawGrktbl_DltHgd_vc_plus$profit),min(drawGrktbl_DltHgd_vc_mnus$profit))),
    max(c(max(drawGrktbl_DltHgd$profit),max(drawGrktbl_DltHgd_vc_plus$profit),max(drawGrktbl_DltHgd_vc_mnus$profit))))
  )
}

##
# Draw Implied Volaility Sensitivity Hedge Effect
if(VolSensitivityCheck){
  gg<-ggplot(drawGrktbl,aes(x=UDLY,y=profit,group=day))
  (
  gg
  +geom_line(size=draw_line_size,linetype=draw_line_type,colour="gray60")
  +geom_line(x=drawGrktbl_vc_plus$UDLY,y=drawGrktbl_vc_plus$profit,colour="blue",size=draw_line_size,group=drawGrktbl_vc_plus$day,linetype=draw_line_type)
  +geom_line(x=drawGrktbl_vc_mnus$UDLY,y=drawGrktbl_vc_mnus$profit,colour="red",size=draw_line_size,group=drawGrktbl_vc_plus$day,linetype=draw_line_type)
  +geom_point(x=thePositonGrks$UDLY,y=0,size=4.0,colour="black")
  +ylim(
    min(c(min(drawGrktbl$profit),min(drawGrktbl_vc_plus$profit),min(drawGrktbl_vc_mnus$profit))),
    max(c(max(drawGrktbl$profit),max(drawGrktbl_vc_plus$profit),max(drawGrktbl_vc_mnus$profit))))
  )
}

rm(list=ls())

#Profit + Directional + Non Didectional(Intrinsic Advantageous) + Total Effect Graph
# gg<-ggplot(drawGrktbl,aes(x=UDLY,y=profit,group=day))
# (
#   gg + geom_line(size=draw_line_size,linetype=draw_line_type),colour="black")
#   +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$TotalEffect,size=draw_line_size,linetype=draw_line_type),colour="darkgreen") 
#   +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$NdEffect,size=draw_line_size,linetype=draw_line_type),colour="red")
#   +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$DEffect,size=draw_line_size,linetype=draw_line_type),colour="blue")
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
#   gg + geom_line(size=draw_line_size,colour="orange",linetype=draw_line_type))
#   +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$DeltaEffect,size=draw_line_size,colour="blue",group=drawGrktbl$day,linetype=draw_line_type))
#   +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$GammaEffect,size=draw_line_size,colour="red",group=drawGrktbl$day,linetype=draw_line_type))
#   +geom_line(x=drawGrktbl$UDLY,y=drawGrktbl$VegaEffect,size=draw_line_size,colour="green",group=drawGrktbl$day,linetype=draw_line_type))
#   +geom_point(x=thePositonGrks$UDLY,y=0,size=4.0,colour="black")
#   +geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$DeltaEffect,size=4.0,colour="blue")+geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$GammaEffect,size=4.0,colour="red")
#   +geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$VegaEffect,size=4.0,colour="green")+geom_point(x=thePositonGrks$UDLY,y=thePositonGrks$ThetaEffect,size=4.0,colour="orange")
#   +ylim(
#     min(c(min(drawGrktbl$ThetaEffect),min(drawGrktbl$DeltaEffect),min(drawGrktbl$GammaEffect),min(drawGrktbl$VegaEffect))),
#     max(c(max(drawGrktbl$ThetaEffect),max(drawGrktbl$DeltaEffect),max(drawGrktbl$GammaEffect),max(drawGrktbl$VegaEffect))))
# )
