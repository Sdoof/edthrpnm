library(RQuantLib)
library(ggplot2)
library(dplyr)
library(pracma)
##
# Before Running this routine, 1. Create UDLY_AdjustPosition.csv file.
# 2. Make ConfigParameters file's SimEvalPosStart and SimEvalPosEnd appropriate values to immediately continue MC Simulation.
# 3. Make ConfigParameters file's PlaybackDeltaHedgeSpreds appropriate value to immediately continue Delta Headge Evaluation.
# 4. Make ConfigParameters file's SimReportPosStart, SimReportPosEnd and SimReportScenarioMode appropriate values 
#    to immediately continue SimResult.Rmd

# first read UDLY_AdjustPosition.csv in which each line shows the difference added to the oiginal spreads.
# The file's first line must be the original spread.
adjPositions<-read.table(paste(ResultFiles_Path_G,Underying_Symbol_G,"_AdjustPosition.csv",sep=""),
                         header=F,comment.char="#",sep=",",colClasses="numeric")

if(length(adjPositions)>length(opchain$Position)){
  adjPositions %>% distinct() -> adjPositions 
}

#Get the original spread.
origPosition<-unlist(adjPositions[1,])
#origPosition should be excluded from adjPositions.
adjPositions<- adjPositions[-1,]
rownames(adjPositions) <- c(1:nrow(adjPositions))

#Handle these as a Matrix
as.matrix(adjPositions) -> adjPositions
#We also evaluate (the differnce x 2) spread, so rbind the adjPositions and adjPositions*2
adjPositions<-rbind(adjPositions,adjPositions*2)
rownames(adjPositions) <- c(1:nrow(adjPositions))

#also make origPosition Matrix to facilitate calculation
origPosition<-matrix(rep(origPosition,nrow(adjPositions)),nrow=nrow(adjPositions),ncol=ncol(adjPositions),byrow=T)

#Orig Spread + differnce Spreads are the target Spreads we should evaluate.
evalPositions<- adjPositions + origPosition

getPutCallnOfthePosition<-function(x){
  type<-opchain$TYPE
  putpos<-(type+OpType_Put_G)
  putn<-sum( as.numeric((putpos*x)!=0) )
  callpos<-(type+OpType_Call_G)
  calln<-sum( as.numeric((callpos*x)!=0) )
  return (c(putn,calln))
}

#calculate Orig Spread's evaluation score.
x<-origPosition[1,]
val<-1.0
posnum<-sum(getPutCallnOfthePosition(x))
val<-obj_Income_sgmd(x,EvalFuncSetting,isDebug=F,isDetail=F,
                     udlStepNum=EvalFuncSetting$UdlStepNum,udlStepPct=EvalFuncSetting$UdlStepPct,
                     maxposnum=EvalFuncSetting$Maxposnum,PosMultip=PosMultip,
                     tail_rate=EvalFuncSetting$Tail_rate,lossLimitPrice=EvalFuncSetting$LossLimitPrice,
                     Delta_Direct_Prf=EvalFuncSetting$Delta_Direct_Prf[posnum],Vega_Direct_Prf=EvalFuncSetting$Vega_Direct_Prf[posnum],
                     Delta_Neutral_Offset=EvalFuncSetting$Delta_Neutral_Offset[posnum],Vega_Neutral_Offset=EvalFuncSetting$Vega_Neutral_Offset[posnum])
cat("original pos",x,":score",val,"\n")
orig_score<-val

#calculate target Spreads's evaluation Score
score_v<-rep(1,nrow(evalPositions))
posnums_v<-rep(0,nrow(evalPositions))
for(ithPos in 1:nrow(evalPositions)) {
  x<-evalPositions[ithPos,]
  posnum<-sum(getPutCallnOfthePosition(x))
  val<-obj_Income_sgmd(x,EvalFuncSetting,isDebug=F,isDetail=F,
                       udlStepNum=EvalFuncSetting$UdlStepNum,udlStepPct=EvalFuncSetting$UdlStepPct,
                       maxposnum=EvalFuncSetting$Maxposnum,PosMultip=PosMultip,
                       tail_rate=EvalFuncSetting$Tail_rate,lossLimitPrice=EvalFuncSetting$LossLimitPrice,
                       Delta_Direct_Prf=EvalFuncSetting$Delta_Direct_Prf[posnum],Vega_Direct_Prf=EvalFuncSetting$Vega_Direct_Prf[posnum],
                       Delta_Neutral_Offset=EvalFuncSetting$Delta_Neutral_Offset[posnum],Vega_Neutral_Offset=EvalFuncSetting$Vega_Neutral_Offset[posnum])
  cat(ithPos,"pos",x,":score",val,"\n")
  score_v[ithPos] <- val
  posnums_v[ithPos] <- posnum
}

(score_v);(posnums_v)

#Write to the UDL-EvalPosition.csv. The table has 2 more columns.
#One Each target Spread's evaluation score. The Other Original Spread's evaluation score. 
write.table(cbind(evalPositions,score_v,orig_score,posnums_v),paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition.csv",sep=""),sep=",",
            quote=T,col.names=F,row.names=F,append=F)



