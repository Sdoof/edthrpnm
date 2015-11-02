library(RQuantLib)
library(ggplot2)
library(dplyr)
library(pracma)
## Preparation
#  Load EOptimize.R's code chunk

#Config File
#ConfigFileName_G="ConfigParameters.csv"
#DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"

#ConfigParameters<-read.table(paste(DataFiles_Path_G,ConfigFileName_G,sep=""),
#                             row.names=1, comment.char="#",header=T,stringsAsFactors=F,sep=",")

#Definition
#OpType_Put_G=as.numeric(ConfigParameters["OpType_Put_G",1])
#OpType_Call_G=as.numeric(ConfigParameters["OpType_Call_G",1])

#File
#Underying_Symbol_G=ConfigParameters["Underying_Symbol_G",1]
#ResultFiles_Path_G=ConfigParameters["ResultFiles_Path_G",1]

getPutCallLegNOfthePosition<-function(x){
  type<-opchain$TYPE
  putpos<-(type+OpType_Put_G)
  putn<-sum( as.numeric((putpos*x)!=0) )
  callpos<-(type+OpType_Call_G)
  calln<-sum( as.numeric((callpos*x)!=0) )
  return (c(putn,calln))
}

###
## Create Last Day's positiojn
##

##
# 追跡対象(Last Day)のOption Chain読み込み
#
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre_Last.csv",sep="")
opchain<-read.table(rf,header=T,sep=",") ; rm(rf)

##evaluated position
EvalPos_fn=paste(ResultFiles_Path_G,Underying_Symbol_G,"_EvalPosition_Last.csv",sep='')
#Here you spicify which position should be analyzed.
eval_pos_idx=3
##Spread Position loaded to be evaluated.
tmp<-read.table(EvalPos_fn,header=F,sep=",",colClasses="numeric")
evaPos<-unlist(tmp[eval_pos_idx,])[1:length(opchain$Position)]

##get position where opchain$Position!=0

opchain$Position=evaPos
opchain %>% dplyr::filter(Position!=0) -> position
position %>% select(ExpDate,TYPE,Strike,Position) -> position

##
# Load Whole Option Chain for today 
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre_Whole.csv",sep="")
opchain<-read.table(rf,header=T,sep=",")

#opchain %>% select(-(Position)) -> opchain ; head(opchain)
opchain$Position=NULL

#select and sort
merge(opchain,position) %>% select(Date,ExpDate,TYPE,Strike,ContactName,Position,UDLY,Price,
                                   Delta,Gamma,Vega,Theta,Rho,OrigIV,ATMIV,IVIDX,
                                   HowfarOOM,TimeToExpDate,Moneyness.Nm) %>% 
  arrange(Date,ExpDate,desc(TYPE),Strike) -> position

position$Position<-ifelse(is.na(position$Position), 0, position$Position)

#evaluation value of last day's Leg
##
# NOTICE! opchain behaves as a Global Variable for eval function(obj_Income_sgmd)
# In the future, this specification should be modified. But for now, 3 steps are needed to call obj_Income_sgmd
#  1. assign opchain (where Position!=0)
#  2. assign posnum
#  3. call obj_Income_sgmd()
opchain<-position
posnum<-sum(getPutCallLegNOfthePosition(opchain$Position))
val<-obj_Income_sgmd(opchain$Position,EvalFuncSetting,isDebug=F,isDetail=F,
                     udlStepNum=EvalFuncSetting$UdlStepNum,udlStepPct=EvalFuncSetting$UdlStepPct,
                     maxposnum=EvalFuncSetting$Maxposnum,PosMultip=PosMultip,
                     tail_rate=EvalFuncSetting$Tail_rate,lossLimitPrice=EvalFuncSetting$LossLimitPrice,
                     Delta_Direct_Prf=EvalFuncSetting$Delta_Direct_Prf[posnum],Vega_Direct_Prf=EvalFuncSetting$Vega_Direct_Prf[posnum],
                     Delta_Neutral_Offset=EvalFuncSetting$Delta_Neutral_Offset[posnum],Vega_Neutral_Offset=EvalFuncSetting$Vega_Neutral_Offset[posnum])
print(val)

###
## Create Today's Day's position and show Differnces 
##

##
# Differnce of Today's Candidate 
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre.csv",sep="")
opchain_today<-read.table(rf,header=T,sep=",") ; rm(rf)

EvalPos_fn=paste(ResultFiles_Path_G,Underying_Symbol_G,"_EvalPosition.csv",sep='')
EvalPos_table<-read.table(EvalPos_fn,header=F,sep=",",colClasses="numeric")

# Today's Spreads compared
Spreads=c(1:100)

for(eval_pos_idx in Spreads){
  #Here you spicify which position should be analyzed.
  #eval_pos_idx=27
  ##Spread Position loaded to be evaluated.
  opchain=opchain_today
  
  evaPos<-unlist(EvalPos_table[eval_pos_idx,])[1:length(opchain$Position)]
  opchain$Position<-evaPos
  
  #get position where opchain$Position!=0
  opchain %>% dplyr::filter(Position!=0) -> position_today
  position_today %>% select(ExpDate,TYPE,Strike,Position) -> position_today
  
  ##
  # Load Whole Option Chain for today 
  rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre_Whole.csv",sep="")
  opchain<-read.table(rf,header=T,sep=",")
  
  #opchain %>% select(-(Position)) -> opchain ; head(opchain)
  opchain$Position=NULL
  
  #select and sort
  merge(opchain,position_today) %>% select(Date,ExpDate,TYPE,Strike,ContactName,Position,UDLY,Price,
                                           Delta,Gamma,Vega,Theta,Rho,OrigIV,ATMIV,IVIDX,
                                           HowfarOOM,TimeToExpDate,Moneyness.Nm) %>% 
    arrange(Date,ExpDate,desc(TYPE),Strike) -> position_today
  
  ###
  ## show
  #print(position)
  #print(position_today)
  
  position$Position_Pre<-position$Position
  position$Position=NULL
  
  full_join(position,position_today) -> positon_join
  positon_join$Position<-ifelse(is.na(positon_join$Position), 0, positon_join$Position)
  positon_join$Position_Pre<-ifelse(is.na(positon_join$Position_Pre), 0, positon_join$Position_Pre)
  positon_join %>% arrange(Date,ExpDate,desc(TYPE),Strike) -> positon_join
  
  positon_join$DiffPos=positon_join$Position-positon_join$Position_Pre
  
  print(positon_join)
  cat("pos idx",eval_pos_idx,"diff ",sum(abs(positon_join$DiffPos))," ")
  
  #evaluation value of today's eval_pos_idx'th Leg
  opchain=position_today
  posnum<-sum(getPutCallLegNOfthePosition(opchain$Position))
  val<-obj_Income_sgmd(opchain$Position,EvalFuncSetting,isDebug=F,isDetail=F,
                       udlStepNum=EvalFuncSetting$UdlStepNum,udlStepPct=EvalFuncSetting$UdlStepPct,
                       maxposnum=EvalFuncSetting$Maxposnum,PosMultip=PosMultip,
                       tail_rate=EvalFuncSetting$Tail_rate,lossLimitPrice=EvalFuncSetting$LossLimitPrice,
                       Delta_Direct_Prf=EvalFuncSetting$Delta_Direct_Prf[posnum],Vega_Direct_Prf=EvalFuncSetting$Vega_Direct_Prf[posnum],
                       Delta_Neutral_Offset=EvalFuncSetting$Delta_Neutral_Offset[posnum],Vega_Neutral_Offset=EvalFuncSetting$Vega_Neutral_Offset[posnum])
  print(val)
  
}

#savefile or create object

#Config File
rm(ConfigFileName_G,DataFiles_Path_G,ConfigParameters,OpType_Put_G,OpType_Call_G)
rm(Underying_Symbol_G,ResultFiles_Path_G)
rm(evaPos,opchain)

