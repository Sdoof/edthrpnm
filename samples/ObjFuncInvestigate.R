getPutCallnOfthePosition<-function(x){
  type<-opchain$TYPE
  putpos<-(type+OpType_Put_G)
  putn<-sum( as.numeric((putpos*x)!=0) )
  callpos<-(type+OpType_Call_G)
  calln<-sum( as.numeric((callpos*x)!=0) )
  return (c(putn,calln))
}

x<-c(0,0,0,0,0,0,0,0,1,-1,0,0,0,2,0,0,0,0,0,0,0,-1,0,0,0,0,0,-1)

posnum<-sum(getPutCallnOfthePosition(x))

val<-obj_Income_sgmd(x,EvalFuncSetting,isDebug=TRUE,isDetail=TRUE,
                     udlStepNum=EvalFuncSetting$UdlStepNum,udlStepPct=EvalFuncSetting$UdlStepPct,
                     maxposnum=EvalFuncSetting$Maxposnum,PosMultip=PosMultip,
                     tail_rate=EvalFuncSetting$Tail_rate,lossLimitPrice=EvalFuncSetting$LossLimitPrice,
                     Delta_Direct_Prf=EvalFuncSetting$Delta_Direct_Prf[posnum],Vega_Direct_Prf=EvalFuncSetting$Vega_Direct_Prf[posnum],
                     Delta_Neutral_Offset=EvalFuncSetting$Delta_Neutral_Offset[posnum],Vega_Neutral_Offset=EvalFuncSetting$Vega_Neutral_Offset[posnum])
(val)
