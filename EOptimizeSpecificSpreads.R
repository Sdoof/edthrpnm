
sampleVerticalSpread<-function(targetOpTyep,verticalType,targetExpDate,isDebug=FALSE,isDetail=FALSE){
  
  idxy<-as.numeric(opchain$TYPE==targetOpTyep)*
    as.numeric(opchain$ExpDate==targetExpDate)*rep(1:length(opchain$TYPE),length=length(opchain$TYPE))
  if(isDebug){ cat(" candidate pos:",idxy) }
  y<-rep(0,times=length(opchain$TYPE))
  if(sum(idxy)>0){
    idxy<-idxy[idxy!=0]
    #if(isDebug){ cat(" put pos cand :",idxy) }
    idxy<-sample(idxy,size=2,replace=FALSE,prob=NULL)
    opchain$Strike[idxy[1]]
    opchain$Strike[idxy[2]]
    (opchain$Strike[idxy[2]]>=opchain$Strike[idxy[1]])*(verticalType==BULL_VERTICAL_SPREAD_TYPE)*idxy +
      (opchain$Strike[idxy[2]]<opchain$Strike[idxy[1]])*(verticalType==BULL_VERTICAL_SPREAD_TYPE)*rev(idxy) +
      (opchain$Strike[idxy[2]]>=opchain$Strike[idxy[1]])*(verticalType==BEAR_VERTICAL_SPREAD_TYPE)*rev(idxy) +
      (opchain$Strike[idxy[2]]<opchain$Strike[idxy[1]])*(verticalType==BEAR_VERTICAL_SPREAD_TYPE)*idxy ->idxy
    if(isDebug){ cat("idxy:",idxy) }
    #y<-rep(0,times=length(iniPos))
    y_shift<-0
    n_shift<-(2)
    y[idxy[(1+y_shift):(y_shift+(n_shift/2))]]<-(1)
    y[idxy[(n_shift/2+1+y_shift):2]]<-(-1)
  }
  if(isDebug){ cat(" (:pos",y,")") }
  return(y)
}

sampleDiagonalSpread<-function(targetOpTyep,diagonalType,targetExpDate_f,targetExpDate_b,isDebug=FALSE,isDetail=FALSE){
  #Front
  idxy<-as.numeric(opchain$TYPE==targetOpTyep)*
    as.numeric(opchain$ExpDate==targetExpDate_f)*rep(1:length(opchain$TYPE),length=length(opchain$TYPE))
  y<-rep(0,times=length(opchain$TYPE))
  if(sum(idxy)>0){
    idxy<-idxy[idxy!=0]
    #if(isDebug){ cat(" put pos cand :",idxy) }
    idxy<-sample(idxy,size=1,replace=T,prob=NULL)
    #y<-rep(0,times=length(iniPos))
    y[idxy[1]]<-as.numeric(diagonalType==DIAGONAL_TYPE_LONG)*(-1) +
      as.numeric(diagonalType==DIAGONAL_TYPE_SHORT)*(1)
  }
  #Back
  idxy<-as.numeric(opchain$TYPE==targetOpTyep)*
    as.numeric(opchain$ExpDate==targetExpDate_b)*rep(1:length(opchain$TYPE),length=length(opchain$TYPE))
  z<-rep(0,times=length(iniPos))
  if(sum(idxy)>0){
    idxy<-idxy[idxy!=0]
    #if(isDebug){ cat(" put pos cand :",idxy) }
    idxy<-sample(idxy,size=1,replace=T,prob=NULL)
    #y<-rep(0,times=length(iniPos))
    z[idxy[1]]<-as.numeric(diagonalType==DIAGONAL_TYPE_LONG)*(1) +
      as.numeric(diagonalType==DIAGONAL_TYPE_SHORT)*(-1)
  }
  x<-y+z
  
  if(isDebug){ cat(" (:pos",x,")") }
  return(x)
}

##Spread Type Definition
BULL_VERTICAL_SPREAD_TYPE=1
BEAR_VERTICAL_SPREAD_TYPE=-1

#DIAGONAL Type Definition
DIAGONAL_TYPE_LONG=1
DIAGONAL_TYPE_SHORT=-1

#sample type Definition
IRON_CONDOR_SMPLING=1
DIAGONAL_SPREAD_SMPLING=2
DOUBLE_DIAGONAL_SPREAD_SMPLING=3
IRON_CONDOR_PLUS_SINGLE_DIAGONAL_SMPLING=4
IRON_CONDOR_PLUS_DOUBLE_DIAGONAL_SMPLING=5
CALL_BEAR_SPREAD_SMPLING=6
CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONAL_SMPLING=7

#Check Option
(opchain)

####
## sampling main routine
sampleSpreadType=DOUBLE_DIAGONAL_SPREAD_SMPLING
targetExpDate="2016/5/19"
targetExpDate_f=targetExpDate
targetExpDate_b="2016/6/16"
spreadRatio=c(1,1,1)
totalPopNum=1000
InitialPopThresh=2.5
isFileout=T
isDebug=F
isDetail=F
#OutFname=paste(".\\ResultData\\ironcondor",format(Sys.time(),"%Y-%b-%d"),".csv",sep="")
OutFname=paste(".\\ResultData\\spread_result-",sampleSpreadType[1],"-",spreadRatio[1],spreadRatio[2],spreadRatio[3],".csv",sep="")

added_num<-(0)
total_count<-(0)
start_t<-proc.time()
while(TRUE){
  x<-rep(0,times=length(opchain$TYPE))
  if(sampleSpreadType==IRON_CONDOR_SMPLING){
    y=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
                           verticalType=BULL_VERTICAL_SPREAD_TYPE,
                           targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
    z=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
                           verticalType=BEAR_VERTICAL_SPREAD_TYPE,
                           targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
    x<-y+z
    x<-x*spreadRatio[1]
    
  }else if(sampleSpreadType==IRON_CONDOR_PLUS_SINGLE_DIAGONAL_SMPLING){
    y=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
                           verticalType=BULL_VERTICAL_SPREAD_TYPE,
                           targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
    
    z=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
                           verticalType=BEAR_VERTICAL_SPREAD_TYPE,
                           targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
    
    w=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                           diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT),
                           targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
    x<-(y+z)*spreadRatio[1]+w*spreadRatio[2]
      
  }else if(sampleSpreadType==IRON_CONDOR_PLUS_DOUBLE_DIAGONAL_SMPLING){
    y=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
                           verticalType=BULL_VERTICAL_SPREAD_TYPE,
                           targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
    
    z=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
                           verticalType=BEAR_VERTICAL_SPREAD_TYPE,
                           targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
    
    diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
    w=sampleDiagonalSpread(targetOpTyep=OpType_Put_G,
                           diagonalType=diagonalType,
                           targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
    
    diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
    v=sampleDiagonalSpread(targetOpTyep=OpType_Call_G,
                           diagonalType=diagonalType,
                           targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
   
     x<-(y+z)*spreadRatio[1]+w*spreadRatio[2]+v*spreadRatio[3]
    
  }else if(sampleSpreadType==DOUBLE_DIAGONAL_SPREAD_SMPLING){
    diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
    y=sampleDiagonalSpread(targetOpTyep=OpType_Put_G,
                           diagonalType=diagonalType,
                           targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
    diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
    z=sampleDiagonalSpread(targetOpTyep=OpType_Call_G,
                           diagonalType=diagonalType,
                           targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
    x<-(y+z)**spreadRatio[1]
  }
  
  if(isDetail)
    print(hollowNonZeroPosition(x))
  
  posnum=sum(as.numeric((x)!=0))
  tryCatch(
    val<-obj_Income_sgmd(x,EvalFuncSetting,isDebug=F,isDetail=F,
                         #val<-obj_fixedpt_sgmd(x,EvalFuncSetting,isDebug=isDebug,isDetail=isDetail,
                         udlStepNum=EvalFuncSetting$UdlStepNum,udlStepPct=EvalFuncSetting$UdlStepPct,
                         maxposnum=EvalFuncSetting$Maxposnum,PosMultip=PosMultip,
                         tail_rate=EvalFuncSetting$Tail_rate,lossLimitPrice=EvalFuncSetting$LossLimitPrice,
                         Delta_Direct_Prf=EvalFuncSetting$Delta_Direct_Prf[posnum],Vega_Direct_Prf=EvalFuncSetting$Vega_Direct_Prf[posnum],
                         Delta_Neutral_Offset=EvalFuncSetting$Delta_Neutral_Offset[posnum],Vega_Neutral_Offset=EvalFuncSetting$Vega_Neutral_Offset[posnum]),
    error=function(e){
      message(e)
      val<-(InitialPopThresh+1.0)
    })
  
  if(val<InitialPopThresh){
    added_num<-added_num+1
    if(isFileout){
      cat(x,file=OutFname,sep=",",append=TRUE);cat(",",file=OutFname,append=TRUE)
      cat(val,file=OutFname,"\n",append=TRUE)
    }
  }
  total_count<-total_count+1
  if((added_num%%50)==0){
    cat(" added num:",added_num,"total count:",total_count,"time:",(proc.time()-start_t)[3],"\n")
    start_t<-proc.time()
  }
  if(added_num==totalPopNum)
    break
}


#Simple Test
#y=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
#                       verticalType=BULL_VERTICAL_SPREAD_TYPE,
#                       targetExpDate="2016/5/19",isDebug=T,isDetail=T)
#hollowNonZeroPosition(y)

#y=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
#                       verticalType=BEAR_VERTICAL_SPREAD_TYPE,
#                       targetExpDate="2016/5/19",isDebug=T,isDetail=T)
#hollowNonZeroPosition(y)

#z=sampleDiagonalSpread(targetOpTyep=OpType_Put_G,
#                       diagonalType=DIAGONAL_TYPE_LONG,
#                       targetExpDate_f="2016/5/19",targetExpDate_b="2016/6/16",
#                       isDebug=FALSE,isDetail=FALSE)
#hollowNonZeroPosition(z)

#y=sampleDiagonalSpread(targetOpTyep=OpType_Call_G,
#                       diagonalType=DIAGONAL_TYPE_LONG,
#                       targetExpDate_f="2016/5/19",targetExpDate_b="2016/6/16",
#                       isDebug=FALSE,isDetail=FALSE)
#hollowNonZeroPosition(y)

#y=sampleDiagonalSpread(targetOpTyep=OpType_Put_G,
#                       diagonalType=DIAGONAL_TYPE_SHORT,
#                       targetExpDate_f="2016/5/19",targetExpDate_b="2016/6/16",
#                       isDebug=FALSE,isDetail=FALSE)
#hollowNonZeroPosition(y)

#y=sampleDiagonalSpread(targetOpTyep=OpType_Call_G,
#                       diagonalType=DIAGONAL_TYPE_SHORT,
#                       targetExpDate_f="2016/5/19",targetExpDate_b="2016/6/16",
#                       isDebug=FALSE,isDetail=FALSE)
#hollowNonZeroPosition(y)

##Iron Condor Sample
#y=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
#                       verticalType=BULL_VERTICAL_SPREAD_TYPE,
#                       targetExpDate="2016/5/19",isDebug=T,isDetail=T)
#z=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
#                       verticalType=BEAR_VERTICAL_SPREAD_TYPE,
#                       targetExpDate="2016/5/19",isDebug=T,isDetail=T)
#x<-y+z
#hollowNonZeroPosition(x)



