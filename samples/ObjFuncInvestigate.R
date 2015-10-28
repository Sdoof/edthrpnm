#Evaluation CSV file
EvalPos_fn=paste(ResultFiles_Path_G,Underying_Symbol_G,"_EvalPosition_Theta.csv",sep='')

#Here you spicify which position should be analyzed.
eval_pos_idx=3

##Spread Position loaded to be evaluated.
tmp<-read.table(EvalPos_fn,header=F,sep=",",colClasses="numeric")

evaPos<-unlist(tmp[eval_pos_idx,])[1:length(opchain$Position)]

getPutCallnOfthePosition<-function(x){
  type<-opchain$TYPE
  putpos<-(type+OpType_Put_G)
  putn<-sum( as.numeric((putpos*x)!=0) )
  callpos<-(type+OpType_Call_G)
  calln<-sum( as.numeric((callpos*x)!=0) )
  return (c(putn,calln))
}

x<-evaPos

posnum<-sum(getPutCallnOfthePosition(x))

val<-obj_Income_sgmd(x,EvalFuncSetting,isDebug=TRUE,isDetail=TRUE,
                     udlStepNum=EvalFuncSetting$UdlStepNum,udlStepPct=EvalFuncSetting$UdlStepPct,
                     maxposnum=EvalFuncSetting$Maxposnum,PosMultip=PosMultip,
                     tail_rate=EvalFuncSetting$Tail_rate,lossLimitPrice=EvalFuncSetting$LossLimitPrice,
                     Delta_Direct_Prf=EvalFuncSetting$Delta_Direct_Prf[posnum],Vega_Direct_Prf=EvalFuncSetting$Vega_Direct_Prf[posnum],
                     Delta_Neutral_Offset=EvalFuncSetting$Delta_Neutral_Offset[posnum],Vega_Neutral_Offset=EvalFuncSetting$Vega_Neutral_Offset[posnum])
(val)

