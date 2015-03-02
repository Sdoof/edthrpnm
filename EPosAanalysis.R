library(dplyr)
library(RQuantLib)

# set days interval between which the position is analyzed step by step.
stepdays<-5
udlStepNum<-2
udlStepPct=0.05

#get evaluation days vector, evaldays
opchain$Date<-as.character(opchain$Date);
opchain$ExpDate<-as.character(opchain$ExpDate)
max_days<-min(get.busdays.between(opchain$Date,opchain$ExpDate))
totalstep=max_days%/%stepdays ; rm(max_days)

evaldays<-rep(stepdays,times=totalstep)
evaldays<- cumsum(evaldays)

#read analyzed positon. here we give by copy and paste
pos_anlys<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,-3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-3,2,0,0,0,0)

#note opchain is already instanciated by the proceduces of ERskRtnEval
opchain$Position<-pos_anlys
opchain %>% dplyr::filter(Position!=0) -> thePosition

#initial price
iniPrice <- getPosGreeks(pos=thePosition$Position,greek=thePosition$Price,multi=PosMultip)
iniCredit <- -1*iniPrice

#total data frame
posStepDays<-data.frame(days=evaldays)
#Set data frames as a row value of another data frame.
posStepDays %>% group_by(days) %>%
  do(scene=createPositinEvalTable(position=thePosition,udlStepNum=udlStepNum,udlStepPct=udlStepPct,days=stepdays)) -> posStepDays
#We must adjust each position values
posStepDays %>% group_by(days) %>% rowwise() %>% do(days=.$days,scene2=adjustPosChg(.$scene,.$days-stepdays)) -> tmp
unlist(tmp$days) -> posStepDays$days ; tmp$scene2 -> posStepDays$scene ;rm(tmp)
#We've got the complete posStepDays.
posStepDays$scene[[2]];posStepDays$scene[[2]]$pos[2]

#Now drawing
#rowwise() -> dataframe(days,UDLY,Price)
rm(stepdays,pos_anlys,totalstep,udlChgPct,udlStepNum,udlStepPct)
rm(posStepDays,thePosition)

#innfer functions : position operation related.

#After each posTable is created by createPositinEvalTable(), 
#we must adjust actual Date and related conditions. Date (and TimeToExpDate), 
#Moneyness.nm,, IV(OrigIV) and time decayed Greeks.

#assuming on each day, IVIDX doesn't change at udlChgPcg==0.
#instance price change had only occured from stepdays before.

#nested. adjustPosChg() calls adjustPosChgInner()
adjustPosChgInner<-function(process_df,time_advcd){
  pos<-as.data.frame(process_df$pos[1])
  if(sum(as.numeric(time_advcd==0))!=0) return(pos)
#  print(pos)

  #Volatility Cone の影響。時間変化した分の影響を受ける。その比の分だけ比率変化
  #ATMIV_pos <- ATMIV_pos*(ATMIV_pos/IVIDX_pos)t=TimeToExpDate_pre/(ATMIV_pos/IVIDX_pos)t=TimeToExpDate_pos
   bdays_per_month<-252/12
   TimeToExpDate_pos<-(pos$TimeToExpDate*bdays_per_month-time_advcd)/bdays_per_month
   pos$ATMIV<-pos$ATMIV *
     get.Volatility.Cone.Regression.Result(pos$TYPE,TimeToExpDate_pos)/
     get.Volatility.Cone.Regression.Result(pos$TYPE,pos$TimeToExpDate)
  
  #set new TimeToExpDate
   pos$TimeToExpDate<-TimeToExpDate_pos
  
  #Date advance
   pos$Date <- format(advance("UnitedStates/NYSE",dates=as.Date(pos$Date,format="%Y/%m/%d"),
                              time_advcd,0),"%Y/%m/%d")

  #Moneyness.nm which reflects the value of TimetoExpDate
  pos$Moneyness.Frac<-pos$Strike/pos$UDLY
  eval_timeToExpDate<-as.numeric(pos$TimeToExpDate<TimeToExp_Limit_Closeness_G)*TimeToExp_Limit_Closeness_G+
    as.numeric(pos$TimeToExpDate>=TimeToExp_Limit_Closeness_G)*pos$TimeToExpDate
  pos$Moneyness.Nm<-log(pos$Moneyness.Frac)/pos$ATMIV/sqrt(eval_timeToExpDate)
  pos$Moneyness.Frac<-NULL

  
  #calculate IV_pos(OrigIV) using SkewModel based on model definition formula.
   get.predicted.spline.skew(SkewModel,pos$Moneyness.Nm)
   pos$ATMIV*get.predicted.spline.skew(SkewModel,pos$Moneyness.Nm)
   pos$OrigIV<-pos$ATMIV*get.predicted.spline.skew(SkewModel,pos$Moneyness.Nm)
  
  #calculate pption price and thier greeks
   vgreeks<-set.EuropeanOptionValueGreeks(pos)
   pos$Price<-vgreeks$Price
   pos$Delta<-vgreeks$Delta
   pos$Gamma<-vgreeks$Gamma
   pos$Vega<-vgreeks$Vega
   pos$Theta<-vgreeks$Theta
   pos$Rho<-vgreeks$Rho
  
  pos
}

adjustPosChg<-function(process_df,time_advcd){
  print(process_df)
  print(time_advcd)
  
  process_df %>% group_by(udlChgPct) %>% do(pos=adjustPosChgInner(.,time_advcd)) -> process_df
  return(process_df)
}

#innfer functions : graphical related.

