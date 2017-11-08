library(RQuantLib)
library(ggplot2)
library(dplyr)
library(pracma)
library(digest)
library(hash)
library(gsl)
library(PearsonDS)
rm(list=ls())
source('./ESourceRCode.R',encoding = 'UTF-8')

#COMBINATION LossLimit Multipe
COMBINATION_LOSSLIMIT_MULTIPLE=2

#Cache Hash setting
FILEPLUS_HOT_START=F

#Debug, Detail Mode
IS_DEBUG_MODE=F
IS_DETAIL_MODE=F

##Spread Type Definition
BULL_VERTICAL_SPREAD_TYPE=1
BEAR_VERTICAL_SPREAD_TYPE=(-1)

#DIAGONAL Type Definition
DIAGONAL_TYPE_LONG=1
DIAGONAL_TYPE_SHORT=(-1)

#sample type Definition
IRON_CONDOR_SMPLING=1
DIAGONAL_SMPLING=2
DOUBLE_DIAGONAL_SMPLING=3
DOUBLE_DIAGONAL_CALL_SMPLING=4
DOUBLE_DIAGONAL_PUT_SMPLING=5
DOUBLE_DIAGONAL_OPTYPE_ANY_SMPLING=6
IRON_CONDOR_PLUS_SINGLE_DIAGONAL_SMPLING=7
IRON_CONDOR_PLUS_DOUBLE_DIAGONAL_SMPLING=8
CALL_BEAR_SPREAD_SMPLING=9
CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONAL_SMPLING=10
CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING=11
PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING=12
PUT_BULL_SPREAD_PLUS_SINGLE_DIAGONAL_SMPLING=13
POOL_PLUS_SINGLE_DIAGONAL_SMPLING=20
POOL_PLUS_DOUBLE_DIAGONAL_SMPLING=21
CALL_BEAR_SPREAD_SMPLING=14
PUT_BULL_SPREAD_SMPLING=15
FILE_PLUS_VERTICAL_CREDIT_SPREAD=121
FILE_PLUS_VERTICAL_DEBT_SPREAD=122
FILE_PLUS_IRON_CONDOR=123
FILE_PLUS_SINGLE_DIAGONAL=124
FILE_PLUS_FILE=125
FILE_PLUS_DOUBLE_DIAGONAL=126

SpreadTypeNames<-vector("list",9)
SpreadTypeNames[[IRON_CONDOR_SMPLING]]="IRON_CONDOR"
SpreadTypeNames[[DIAGONAL_SMPLING]]="DIAGONAL"
SpreadTypeNames[[DOUBLE_DIAGONAL_SMPLING]]="DOUBLE_DIAGONA_EACH"
SpreadTypeNames[[DOUBLE_DIAGONAL_CALL_SMPLING]]="DOUBLE_DIAGONAL_CALL"
SpreadTypeNames[[DOUBLE_DIAGONAL_PUT_SMPLING]]="DOUBLE_DIAGONAL_PUT"
SpreadTypeNames[[DOUBLE_DIAGONAL_OPTYPE_ANY_SMPLING]]="DOUBLE_DIAGONAL_ANY"
SpreadTypeNames[[IRON_CONDOR_PLUS_SINGLE_DIAGONAL_SMPLING]]="IRON_CONDOR_PLUS_SINGLE_DIAGONAL"
SpreadTypeNames[[IRON_CONDOR_PLUS_DOUBLE_DIAGONAL_SMPLING]]="IRON_CONDOR_PLUS_DOUBLE_DIAGONAL"
SpreadTypeNames[[CALL_BEAR_SPREAD_SMPLING]]="CALL_BEAR_SPREAD"
SpreadTypeNames[[CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONAL_SMPLING]]="CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONAL"
SpreadTypeNames[[CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING]]="CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL"
SpreadTypeNames[[PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING]]="PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL"
SpreadTypeNames[[PUT_BULL_SPREAD_PLUS_SINGLE_DIAGONAL_SMPLING]]="PUT_BULL_SPREAD_PLUS_SINGLE_DIAGONAL"
SpreadTypeNames[[POOL_PLUS_SINGLE_DIAGONAL_SMPLING]]="POOL_PLUS_SINGLE_DIAGONAL"
SpreadTypeNames[[POOL_PLUS_DOUBLE_DIAGONAL_SMPLING]]="POOL_PLUS_DOUBLE_DIAGONAL"
SpreadTypeNames[[CALL_BEAR_SPREAD_SMPLING]]="CALL_BEAR_SPREAD"
SpreadTypeNames[[PUT_BULL_SPREAD_SMPLING]]="PUT_BULL_SPREAD"
SpreadTypeNames[[FILE_PLUS_VERTICAL_CREDIT_SPREAD]]="FILE_PLUS_VERTICAL_CREDIT_SPREAD"
SpreadTypeNames[[FILE_PLUS_VERTICAL_DEBT_SPREAD]]="FILE_PLUS_VERTICAL_DEBT_SPREAD"
SpreadTypeNames[[FILE_PLUS_IRON_CONDOR]]="FILE_PLUS_IRON_CONDOR"
SpreadTypeNames[[FILE_PLUS_SINGLE_DIAGONAL]]="FILE_PLUS_SINGLE_DIAGONAL"
SpreadTypeNames[[FILE_PLUS_FILE]]="FILE_PLUS_FILE"
SpreadTypeNames[[FILE_PLUS_DOUBLE_DIAGONAL]]="FILE_PLUS_DOUBLE_DIAGONAL"

SpreadTypeToDir<-vector("list",length(SpreadTypeNames))
SpreadTypeToDir[[IRON_CONDOR_SMPLING]]=1
SpreadTypeToDir[[DIAGONAL_SMPLING]]=9
SpreadTypeToDir[[DOUBLE_DIAGONAL_SMPLING]]=c(4,8)
SpreadTypeToDir[[DOUBLE_DIAGONAL_CALL_SMPLING]]=c(14,18)
SpreadTypeToDir[[DOUBLE_DIAGONAL_PUT_SMPLING]]=c(24,28)
SpreadTypeToDir[[DOUBLE_DIAGONAL_OPTYPE_ANY_SMPLING]]=c(34,38)
SpreadTypeToDir[[IRON_CONDOR_PLUS_SINGLE_DIAGONAL_SMPLING]]=0
SpreadTypeToDir[[IRON_CONDOR_PLUS_DOUBLE_DIAGONAL_SMPLING]]=0
SpreadTypeToDir[[CALL_BEAR_SPREAD_SMPLING]]=0
SpreadTypeToDir[[CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONAL_SMPLING]]=6
SpreadTypeToDir[[CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING]]=3
SpreadTypeToDir[[PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING]]=2
SpreadTypeToDir[[PUT_BULL_SPREAD_PLUS_SINGLE_DIAGONAL_SMPLING]]=5
SpreadTypeToDir[[POOL_PLUS_SINGLE_DIAGONAL_SMPLING]]=c(1,4,14,24,34)
SpreadTypeToDir[[POOL_PLUS_DOUBLE_DIAGONAL_SMPLING]]=c(8,18,28,38,9)
SpreadTypeToDir[[FILE_PLUS_VERTICAL_CREDIT_SPREAD]]=121
SpreadTypeToDir[[FILE_PLUS_VERTICAL_DEBT_SPREAD]]=121
SpreadTypeToDir[[FILE_PLUS_IRON_CONDOR]]=122
SpreadTypeToDir[[FILE_PLUS_SINGLE_DIAGONAL]]=c(124,8,18,28,38)
SpreadTypeToDir[[FILE_PLUS_FILE]]=125
SpreadTypeToDir[[FILE_PLUS_DOUBLE_DIAGONAL]]=c(126,4,14,24,34)

#Check Option
(opchain)

#functions used for this script only
createOutFname<-function(targetExpDate,targetExpDate_f,targetExpDate_b,spreadRatio,EvalFuncSetting){
  outFname=paste(".\\ResultData\\",Underying_Symbol_G,"-",SpreadTypeNames[[sampleSpreadType]],"-",
                 LocalcreateSampleConditionStr(EvalFuncSetting),"-",
                 "TgD$",format(as.Date(targetExpDate,format="%Y/%m/%d"),"%m%d"),"_",format(as.Date(targetExpDate_f,format="%Y/%m/%d"),"%m%d"),"_",format(as.Date(targetExpDate_b,format="%Y/%m/%d"),"%m%d"),
                 ".csv",sep="")
  return(outFname)
}

##
# Sampling related functions

sampleVerticalSpread<-function(targetOpTyep,verticalType,targetExpDate,
                               isDebug=FALSE,isDetail=FALSE){
  
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
    #y<-rep(0,times=length(opchain$Position))
    y_shift<-0
    n_shift<-(2)
    y[idxy[(1+y_shift):(y_shift+(n_shift/2))]]<-(1)
    y[idxy[(n_shift/2+1+y_shift):2]]<-(-1)
  }
  if(isDebug){ cat(" (:pos",y,")") }
  return(y)
}

sampleDiagonalSpread<-function(targetOpTyep,diagonalType,targetExpDate_f,targetExpDate_b,
                               isDebug=FALSE,isDetail=FALSE){
  #Front
  idxy<-as.numeric(opchain$TYPE==targetOpTyep)*
    as.numeric(opchain$ExpDate==targetExpDate_f)*rep(1:length(opchain$TYPE),length=length(opchain$TYPE))
  y<-rep(0,times=length(opchain$TYPE))
  if(sum(idxy)>0){
    idxy<-idxy[idxy!=0]
    #if(isDebug){ cat(" put pos cand :",idxy) }
    idxy<-sample(idxy,size=1,replace=T,prob=NULL)
    #y<-rep(0,times=length(opchain$Position))
    y[idxy[1]]<-as.numeric(diagonalType==DIAGONAL_TYPE_LONG)*(-1) +
      as.numeric(diagonalType==DIAGONAL_TYPE_SHORT)*(1)
  }
  #Back
  idxy<-as.numeric(opchain$TYPE==targetOpTyep)*
    as.numeric(opchain$ExpDate==targetExpDate_b)*rep(1:length(opchain$TYPE),length=length(opchain$TYPE))
  z<-rep(0,times=length(opchain$Position))
  if(sum(idxy)>0){
    idxy<-idxy[idxy!=0]
    #if(isDebug){ cat(" put pos cand :",idxy) }
    idxy<-sample(idxy,size=1,replace=T,prob=NULL)
    #y<-rep(0,times=length(opchain$Position))
    z[idxy[1]]<-as.numeric(diagonalType==DIAGONAL_TYPE_LONG)*(1) +
      as.numeric(diagonalType==DIAGONAL_TYPE_SHORT)*(-1)
  }
  x<-y+z
  
  if(isDebug){ cat(" (:pos",x,")") }
  return(x)
}

## load file info into Global HashT
loadToPositionHash<-function(fname){
  tmp<-read.table(fname,header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
  tmp=tmp[,1:(length(opchain$Position)+1)]
  colnames(tmp)=c(rep(1:length(opchain$Position)),"eval")
  tmp %>% dplyr::arrange(tmp[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp
  tmp %>% dplyr::rowwise() %>%
    # x = unlist(.)[1:length(opchain$Position)]
    dplyr::do(key=paste(unlist(.)[1:length(opchain$Position)],collapse = ""),
              md5sum=digest(paste(unlist(.)[1:length(opchain$Position)],collapse = ""))) -> tmp2
  POSITION_OPTIM_HASH[ unlist(tmp2$md5sum) ]<<-tmp$eval
}

## sampling main routine
sampleMain<-function(sampleSpreadType,totalPopNum,targetExpDate,targetExpDate_f,targetExpDate_b,
                     spreadRatio,InitialPopThresh,outFname,isFileout=T,isDebug=F,isDetail=F,
                     POSITION_HASH=hash()){
  added_num=0
  total_count=0
  hash_hit_num=0
  s1_idx=0
  s2_idx=0
  start_t<-proc.time()
  #POSITION_HASH=hash()
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
    }else if(sampleSpreadType==DOUBLE_DIAGONAL_SMPLING){
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      y=sampleDiagonalSpread(targetOpTyep=OpType_Put_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      z=sampleDiagonalSpread(targetOpTyep=OpType_Call_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      x<-(y+z)*spreadRatio[1]
    }else if(sampleSpreadType==DOUBLE_DIAGONAL_CALL_SMPLING){
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      y=sampleDiagonalSpread(targetOpTyep=OpType_Call_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      z=sampleDiagonalSpread(targetOpTyep=OpType_Call_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      x<-(y+z)*spreadRatio[1]
    }else if(sampleSpreadType==DOUBLE_DIAGONAL_PUT_SMPLING){
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      y=sampleDiagonalSpread(targetOpTyep=OpType_Put_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      z=sampleDiagonalSpread(targetOpTyep=OpType_Put_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      x<-(y+z)*spreadRatio[1]
    }else if(sampleSpreadType==DOUBLE_DIAGONAL_OPTYPE_ANY_SMPLING){
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      y=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      z=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      x<-(y+z)*spreadRatio[1]
    }else if(sampleSpreadType==DIAGONAL_SMPLING){
      y=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                             diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT),
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      x<-y*spreadRatio[1]
    }else if(sampleSpreadType==POOL_PLUS_SINGLE_DIAGONAL_SMPLING){
      
      s1<-pools[[ 1 ]][[2]][ceiling(runif(1, min=1e-320, max=nrow(pools[[1]][[2]]))), ]
      y<-unlist(s1[1:length(opchain$Position)]);s1_score<-as.numeric(s1[length(s1)])
      
      z=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                             diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT),
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      x<-y*spreadRatio[1]+z*spreadRatio[2]
    }else if(sampleSpreadType==POOL_PLUS_DOUBLE_DIAGONAL_SMPLING){
      
      s1<-pools[[ 1 ]][[2]][ceiling(runif(1, min=1e-320, max=nrow(pools[[1]][[2]]))), ]
      y<-unlist(s1[1:length(opchain$Position)]);s1_score<-as.numeric(s1[length(s1)])
      
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      z=sampleDiagonalSpread(targetOpTyep=OpType_Put_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      w=sampleDiagonalSpread(targetOpTyep=OpType_Call_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      x<-y*spreadRatio[1]+(z+w)*spreadRatio[2]
    }else if(sampleSpreadType==FILE_PLUS_SINGLE_DIAGONAL){
      s1_idx=0
      s2_idx=0
      
      s1_idx=ceiling(runif(1, min=1e-320, max=nrow(pools[[1]][[2]])))
      s1<-pools[[ 1 ]][[2]][s1_idx, ]
      y<-unlist(s1[1:length(opchain$Position)]);s1_score<-as.numeric(s1[length(s1)])
      
      z=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                             diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT),
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      x<-y*spreadRatio[1]+z*spreadRatio[2]
    }else if(sampleSpreadType==FILE_PLUS_VERTICAL_CREDIT_SPREAD){
      
      s1<-pools[[ 1 ]][[2]][ceiling(runif(1, min=1e-320, max=nrow(pools[[1]][[2]]))), ]
      y<-unlist(s1[1:length(opchain$Position)]);s1_score<-as.numeric(s1[length(s1)])
      
      if(runif(1)<=0.500000){
        z=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
                               verticalType=BULL_VERTICAL_SPREAD_TYPE,
                               targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      }else {
        z=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
                               verticalType=BEAR_VERTICAL_SPREAD_TYPE,
                               targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      }
      x<-y*spreadRatio[1]+z*spreadRatio[2]
    }else if(sampleSpreadType==FILE_PLUS_VERTICAL_DEBT_SPREAD){
      
      s1<-pools[[ 1 ]][[2]][ceiling(runif(1, min=1e-320, max=nrow(pools[[1]][[2]]))), ]
      y<-unlist(s1[1:length(opchain$Position)]);s1_score<-as.numeric(s1[length(s1)])
      
      if(runif(1)<=0.500000){
        z=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
                               verticalType=BEAR_VERTICAL_SPREAD_TYPE,
                               targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      }else {
        z=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
                               verticalType=BULL_VERTICAL_SPREAD_TYPE,
                               targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      }
      x<-y*spreadRatio[1]+z*spreadRatio[2]
    }else if(sampleSpreadType==FILE_PLUS_IRON_CONDOR){
      
      s1<-pools[[ 1 ]][[2]][ceiling(runif(1, min=1e-320, max=nrow(pools[[1]][[2]]))), ]
      y<-unlist(s1[1:length(opchain$Position)]);s1_score<-as.numeric(s1[length(s1)])
      
      z=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
                             verticalType=BULL_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      w=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
                             verticalType=BEAR_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      x<-y*spreadRatio[1]+(z+w)*spreadRatio[2]
    }else if(sampleSpreadType==FILE_PLUS_FILE){
      s1_idx=0
      s2_idx=0
      
      s1_idx=ceiling(runif(1, min=1e-320, max=nrow(pools[[1]][[2]])))
      s1<-pools[[ 1 ]][[2]][s1_idx, ]
      y<-unlist(s1[1:length(opchain$Position)]);s1_score<-as.numeric(s1[length(s1)])
      
      s2_idx=ceiling(runif(1, min=1e-320, max=nrow(pools[[  2 ]][[2]])))
      s2<-pools[[ 2 ]][[2]][s2_idx, ]
      z<-unlist(s2[1:length(opchain$Position)]);s2_score<-as.numeric(s2[length(s2)])
      
      x<-y*spreadRatio[1]+z*spreadRatio[2]
    }else if(sampleSpreadType==FILE_PLUS_DOUBLE_DIAGONAL){
      s1_idx=0
      s2_idx=0
      
      s1_idx=ceiling(runif(1, min=1e-320, max=nrow(pools[[1]][[2]])))
      s1<-pools[[ 1 ]][[2]][s1_idx, ]
      y<-unlist(s1[1:length(opchain$Position)]);s1_score<-as.numeric(s1[length(s1)])
      
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      z=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      w=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      x<-y*spreadRatio[1]+(z+w)*spreadRatio[2]
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
    }else if(sampleSpreadType==CALL_BEAR_SPREAD_SMPLING){
      y=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
                             verticalType=BEAR_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      x<-y*spreadRatio[1]
      
    }else if(sampleSpreadType==PUT_BULL_SPREAD_SMPLING){
      y=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
                             verticalType=BULL_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      x<-y*spreadRatio[1]
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
    }else if(sampleSpreadType==CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONAL_SMPLING){
      
      y=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
                             verticalType=BEAR_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      
      z=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                             diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT),
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      x<-y*spreadRatio[1]+z*spreadRatio[2]
    }else if(sampleSpreadType==PUT_BULL_SPREAD_PLUS_SINGLE_DIAGONAL_SMPLING){
      y=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
                             verticalType=BULL_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      z=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                             diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT),
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      x<-y*spreadRatio[1]+z*spreadRatio[2]
    }else if(sampleSpreadType==CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING){
      y=sampleVerticalSpread(targetOpTyep=OpType_Call_G,
                             verticalType=BEAR_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      z=sampleDiagonalSpread(targetOpTyep=OpType_Put_G,
                             diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT),
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      
      w=sampleDiagonalSpread(targetOpTyep=ifelse(runif(1)<=0.500000,OpType_Put_G,OpType_Call_G),
                             diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT),
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      x<-y*spreadRatio[1]+(z+w)*spreadRatio[2]
    }else if(sampleSpreadType==PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING){
      y=sampleVerticalSpread(targetOpTyep=OpType_Put_G,
                             verticalType=BULL_VERTICAL_SPREAD_TYPE,
                             targetExpDate=targetExpDate,isDebug=isDebug,isDetail=idDetail)
      
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      z=sampleDiagonalSpread(targetOpTyep=OpType_Put_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      diagonalType=ifelse(runif(1)<=0.500000,DIAGONAL_TYPE_LONG,DIAGONAL_TYPE_SHORT)
      w=sampleDiagonalSpread(targetOpTyep=OpType_Call_G,
                             diagonalType=diagonalType,
                             targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,isDebug=isDebug,isDetail=idDetail)
      x<-y*spreadRatio[1]+(z+w)*spreadRatio[2]
    }
    
    posnum=sum(as.numeric((x)!=0))
    if(posnum==0)
      next
    
    if(isDetail)
      print(hollowNonZeroPosition(x))
    
    #cache check and evaluate
    val<-(InitialPopThresh-1)
    md5sumOfPos=digest(paste(x,collapse = ""))
    if(has.key(md5sumOfPos, POSITION_HASH)==FALSE){
      tryCatch(
        val<-obj_Income_sgmd(x,EvalFuncSetting,isDebug=isDebug,isDetail=isDetail,
                             udlStepNum=EvalFuncSetting$UdlStepNum,udlStepPct=EvalFuncSetting$UdlStepPct,
                             PosMultip=PosMultip,
                             lossLimitPrice=EvalFuncSetting$LossLimitPrice,
                             Delta_Direct_Prf=EvalFuncSetting$Delta_Direct_Prf[posnum],Vega_Direct_Prf=EvalFuncSetting$Vega_Direct_Prf[posnum],
                             Delta_Neutral_Offset=EvalFuncSetting$Delta_Neutral_Offset[posnum],Vega_Neutral_Offset=EvalFuncSetting$Vega_Neutral_Offset[posnum]),
        error=function(e){
          message(e)
          val<-(InitialPopThresh+1.0)
        })
      POSITION_HASH[md5sumOfPos]<-val
    }else{
      val<-POSITION_HASH[[md5sumOfPos]]
      hash_hit_num<-hash_hit_num+1
    }
    
    #value check
    tryCatch(
      if(val<(InitialPopThresh-1)){
        added_num<-added_num+1
      }else{
        val=(ifelse(is.na(val),InitialPopThresh,val)+total_count)
      },
      error=function(e){
        message(e)
        cat("val:",val,"InitialPopThresh",InitialPopThresh,"\n")
        val=(ifelse(is.na(val),InitialPopThresh,val)+total_count)
        cat("val:",val,"InitialPopThresh",InitialPopThresh,"\n")
      }
    )
    #write to the file
    if(isFileout){
      cat(x,file=outFname,sep=",",append=TRUE);cat(",",file=outFname,append=TRUE)
      cat(val,file=outFname,append=TRUE)
      if(s1_idx!=0)
        cat(",",s1_idx,file=outFname,append=TRUE)
      if(s2_idx!=0){
        cat(",",s2_idx,file=outFname,append=TRUE)
      }
      cat("\n",file=outFname,append=TRUE)
    }
    #add total_count and show count information
    total_count<-total_count+1
    if((added_num%%50)==0){
      cat(" added num:",added_num," hash hit:",hash_hit_num," hash length:",length(POSITION_HASH),
          "total:",total_count," time:",(proc.time()-start_t)[3],"\n")
      start_t<-proc.time()
    }
    if(added_num==totalPopNum)
      break
  }
  cat("   hash hit:",hash_hit_num," hash length:",length(POSITION_HASH)," total:",total_count,"\n")
  POSITION_HASH<-hash()
}

#### Sampling

###IRON_CONDOR Candidates for Combination
sampleSpreadType=IRON_CONDOR_SMPLING
if(max(SpreadTypeToDir[[sampleSpreadType]]==SpreadTypeSpecified)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=targetExpDate
  targetExpDate_b=targetExpDate
  spreadRatio=c(1,1,1)
  totalPopNum=PopN[1]
  
  #output file name
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  file.create(outFname)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=UNACCEPTABLEVAL,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1])  %>%
    dplyr::filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) %>%
    dplyr::distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  
  #pool setting
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% head(min(TopN[1],max(4,nrow(.)/2))) -> tmp
  pools<<-list(list(c(1,0,0),tmp))
}

### DIAGONAL Candiates for Combination
sampleSpreadType=DIAGONAL_SMPLING
if(max(SpreadTypeToDir[[sampleSpreadType]]==SpreadTypeSpecified)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  spreadRatio=c(1,1,1)
  totalPopNum=PopN[1]/2
  
  #output file name
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  file.create(outFname)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=UNACCEPTABLEVAL,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1])  %>%
    dplyr::filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) %>%
    dplyr::distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  
  #pool setting
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% head(min(TopN[1]/2,max(4,nrow(.)/2))) -> tmp
  pools<<-list(list(c(1,0,0),tmp))
}

### DOUBLE DIAGONAL Candiates for Combination
sampleSpreadType=DOUBLE_DIAGONAL_SMPLING
if(max(SpreadTypeToDir[[sampleSpreadType]]==SpreadTypeSpecified)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  spreadRatio=c(1,1,1)
  totalPopNum=PopN[1]
  
  #output file name
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  file.create(outFname)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=UNACCEPTABLEVAL,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1])  %>%
    dplyr::filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) %>%
    dplyr::distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  #pool setting
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% head(min(TopN[1],max(4,nrow(.)/2))) -> tmp
  pools<<-list(list(c(1,0,0),tmp))
  
  #copy file to READ-SPREAD
  file.copy(from=outFname, to=paste(ResultFiles_Path_G,Underying_Symbol_G,"-READ-SPREAD.csv",sep=""),overwrite=T)
}

### DOUBLE DIAGONAL Put/Call Combination not specified Candiates for Combination
sampleSpreadType=DOUBLE_DIAGONAL_OPTYPE_ANY_SMPLING
if(max(SpreadTypeToDir[[sampleSpreadType]]==SpreadTypeSpecified)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  spreadRatio=c(1,1,1)
  totalPopNum=PopN[1]
  
  #output file name
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  file.create(outFname)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=UNACCEPTABLEVAL,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1])  %>%
    dplyr::filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) %>%
    dplyr::distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  #pool setting
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% head(min(TopN[1],max(4,nrow(.)/2))) -> tmp
  pools<<-list(list(c(1,0,0),tmp))
  
  #copy file to READ-SPREAD
  file.copy(from=outFname, to=paste(ResultFiles_Path_G,Underying_Symbol_G,"-READ-SPREAD.csv",sep=""),overwrite=T)
}

### DOUBLE DIAGONAL Call Candiates for Combination
sampleSpreadType=DOUBLE_DIAGONAL_CALL_SMPLING
if(max(SpreadTypeToDir[[sampleSpreadType]]==SpreadTypeSpecified)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  spreadRatio=c(1,1,1)
  totalPopNum=PopN[1]
  
  #output file name
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  file.create(outFname)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=UNACCEPTABLEVAL,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1])  %>%
    dplyr::filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) %>%
    dplyr::distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  #pool setting
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% head(min(TopN[1],max(4,nrow(.)/2))) -> tmp
  pools<<-list(list(c(1,0,0),tmp))
  
  #copy file to READ-SPREAD
  file.copy(from=outFname, to=paste(ResultFiles_Path_G,Underying_Symbol_G,"-READ-SPREAD.csv",sep=""),overwrite=T)
}

### DOUBLE DIAGONAL Put Candiates for Combination
sampleSpreadType=DOUBLE_DIAGONAL_PUT_SMPLING
if(max(SpreadTypeToDir[[sampleSpreadType]]==SpreadTypeSpecified)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  spreadRatio=c(1,1,1)
  totalPopNum=PopN[1]
  
  #output file name
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=UNACCEPTABLEVAL,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1])  %>%
    dplyr::filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) %>%
    dplyr::distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  #pool setting
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% head(min(TopN[1],max(4,nrow(.)/2))) -> tmp
  pools<<-list(list(c(1,0,0),tmp))
  
  #copy file to READ-SPREAD
  file.copy(from=outFname, to=paste(ResultFiles_Path_G,Underying_Symbol_G,"-READ-SPREAD.csv",sep=""),overwrite=T)
}

###CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL
sampleSpreadType=CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING
if(max(SpreadTypeToDir[[sampleSpreadType]]==SpreadTypeSpecified)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  totalPopNum=PopN[1]
  
  #spread ratio 1
  spreadRatio=c(1,1,1)
  
  ##
  #  First CALL_BEAR_SPREAD_SMPLING sampling
  sampleSpreadType=CALL_BEAR_SPREAD_SMPLING
  
  #output file name
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  file.create(outFname)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=100,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=UNACCEPTABLEVAL,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1])  %>%
    dplyr::filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) %>%
    dplyr::distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  #pool setting
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% head(min(TopN[1],max(4,nrow(.)))) -> tmp
  pools<<-list(list(c(1,0,0),tmp))
  
  ##
  #  Second POOL_PLUS_DOUBLE_DIAGONAL_SMPLING sampling, then saved as CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING sampling
  
  # sampleSpreadType for output file name
  sampleSpreadType=CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  # sampleSpreadType for sampling
  sampleSpreadType=POOL_PLUS_DOUBLE_DIAGONAL_SMPLING
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*COMBINATION_LOSSLIMIT_MULTIPLE
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=UNACCEPTABLEVAL,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  # sampleSpreadType CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING again
  sampleSpreadType=CALL_BEAR_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING
  #remove pools
  rm(pools)
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1])  %>% 
    dplyr::filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) %>%
    dplyr::distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  
  #spread ratio 2
  # spreadRatio=c(2,1,1)
  # 
  # outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  # 
  #sampling
  # originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  # EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  # 
  # sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
  #            targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
  #            spreadRatio=spreadRatio,InitialPopThresh=UNACCEPTABLEVAL,outFname=outFname,isFileout=T,isDebug=F,isDetail=F)
  # 
  # EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
}

###PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING
sampleSpreadType=PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING
if(max(SpreadTypeToDir[[sampleSpreadType]]==SpreadTypeSpecified)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  totalPopNum=PopN[1]
  
  #spread ratio 1
  spreadRatio=c(1,1,1)
  
  ##
  #  First PUT_BULL_SPREAD_SMPLING sampling
  sampleSpreadType=PUT_BULL_SPREAD_SMPLING
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  file.create(outFname)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=100,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=UNACCEPTABLEVAL,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1])  %>%
    dplyr::filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) %>%
    dplyr::distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  #pool setting
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% head(min(TopN[1],max(4,nrow(.)/2))) -> tmp
  pools<<-list(list(c(1,0,0),tmp))
  
  ##
  #  Second POOL_PLUS_DOUBLE_DIAGONAL_SMPLING sampling, then saved as PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING sampling
  
  # sampleSpreadType for output file name
  sampleSpreadType=PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  # sampleSpreadType for sampling
  sampleSpreadType=POOL_PLUS_DOUBLE_DIAGONAL_SMPLING
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*COMBINATION_LOSSLIMIT_MULTIPLE
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=UNACCEPTABLEVAL,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  sampleSpreadType=PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING
  
  # sampleSpreadType PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING again
  sampleSpreadType=PUT_BULL_SPREAD_PLUS_DOUBLE_DIAGONAL_SMPLING
  #remove pools
  rm(pools)
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1])  %>%
    dplyr::filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) %>%
    dplyr::distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
}

### POOL_PLUS_SINGLE_DIAGONAL_SMPLING
sampleSpreadType=POOL_PLUS_SINGLE_DIAGONAL_SMPLING
if(max(SpreadTypeToDir[[sampleSpreadType]]==SpreadTypeSpecified)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  totalPopNum=PopN[2]
  
  #spread ratio 1
  spreadRatio=c(1,1,1)
  
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  file.create(outFname)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*COMBINATION_LOSSLIMIT_MULTIPLE
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=UNACCEPTABLEVAL,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1])  %>% 
    dplyr::filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) %>%
    dplyr::distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  
  #copy file to EvalPosition
  file.copy(from=outFname, to=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition.csv",sep=""),overwrite=T)
  #copy file to READ-SPREAD
  file.copy(from=outFname, to=paste(ResultFiles_Path_G,Underying_Symbol_G,"-READ-SPREAD.csv",sep=""),overwrite=T)
}

###POOL_PLUS_DOUBLE_DIAGONAL_SMPLING
sampleSpreadType=POOL_PLUS_DOUBLE_DIAGONAL_SMPLING
if(max(SpreadTypeToDir[[sampleSpreadType]]==SpreadTypeSpecified)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  totalPopNum=PopN[2]
  
  #spread ratio 1
  spreadRatio=c(1,1,1)
  
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  file.create(outFname)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*COMBINATION_LOSSLIMIT_MULTIPLE
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=UNACCEPTABLEVAL,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1])  %>%
    dplyr::filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) %>%
    dplyr::distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  
  #copy file to EvalPosition
  file.copy(from=outFname, to=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition.csv",sep=""),overwrite=T)
  #copy file to READ-SPREAD
  file.copy(from=outFname, to=paste(ResultFiles_Path_G,Underying_Symbol_G,"-READ-SPREAD.csv",sep=""),overwrite=T)
}

###CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONAL
sampleSpreadType=CALL_BEAR_SPREAD_PLUS_SINGLE_DIAGONAL_SMPLING
if(max(SpreadTypeToDir[[sampleSpreadType]]==SpreadTypeSpecified)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  totalPopNum=PopN[1]
  
  #spread ratio 1
  spreadRatio=c(1,1,1)
  
  #output file name
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  file.create(outFname)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=UNACCEPTABLEVAL,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1])  %>%
    dplyr::filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) %>%
    dplyr::distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  
  #copy file to EvalPosition
  file.copy(from=outFname, to=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition.csv",sep=""),overwrite=T)
}

###PUT_BULL_SPREAD_PLUS_SINGLE_DIAGONAL
sampleSpreadType=PUT_BULL_SPREAD_PLUS_SINGLE_DIAGONAL_SMPLING
if(max(SpreadTypeToDir[[sampleSpreadType]]==SpreadTypeSpecified)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  totalPopNum=PopN[1]
  
  #spread ratio 1
  spreadRatio=c(1,1,1)
  
  #output file name
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  file.create(outFname)
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*max(spreadRatio)
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=UNACCEPTABLEVAL,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1])  %>%
    dplyr::filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) %>%
    dplyr::distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  
  #copy file to EvalPosition
  file.copy(from=outFname, to=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition.csv",sep=""),overwrite=T)
}

###FILE_PLUS_SINGLE_DIAGONAL
sampleSpreadType=FILE_PLUS_SINGLE_DIAGONAL
if(max(SpreadTypeToDir[[sampleSpreadType]]==SpreadTypeSpecified)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  totalPopNum=PopN[2]
  
  #spread ratio 1
  spreadRatio=c(1,1,1)
  
  #output file name
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  if( file.exists(outFname) && FILEPLUS_HOT_START){
    hash::clear(POSITION_OPTIM_HASH)
    loadToPositionHash(fname=outFname)
    file.copy(from=outFname,to=paste(outFname,"_load.csv",sep=""),overwrite=T)
  }else{
    file.create(outFname)
  }
  
  #read file and pool setting
  readFname=paste(ResultFiles_Path_G,Underying_Symbol_G,"-READ-SPREAD.csv",sep="")
  tmp<-read.table(readFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  colnames(tmp)<-c(1:ncol(tmp))
  tmp %>% arrange(.[,length(opchain$Position)+1]) %>% head(min(TopN[2],max(4,nrow(.)/2))) -> tmp
  pools<<-list(list(c(1,0,0),tmp))
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*COMBINATION_LOSSLIMIT_MULTIPLE
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=UNACCEPTABLEVAL,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE,
             POSITION_HASH=POSITION_OPTIM_HASH)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #Merge file
  if( file.exists(outFname) && FILEPLUS_HOT_START){
    LocalMergeWriteFiles(outFname)
  }
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1])  %>% 
    dplyr::filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) %>%
    dplyr::distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  
  #copy file to EvalPosition
  file.copy(from=outFname, to=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition.csv",sep=""),overwrite=T)
}

###FILE_PLUS_VERTICAL_CREDIT_SPREAD
sampleSpreadType=FILE_PLUS_VERTICAL_CREDIT_SPREAD
if(max(SpreadTypeToDir[[sampleSpreadType]]==SpreadTypeSpecified)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  totalPopNum=PopN[2]/4
  
  #spread ratio 1
  spreadRatio=c(1,1,1)
  
  #output file name
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  if( file.exists(outFname) && FILEPLUS_HOT_START){
    hash::clear(POSITION_OPTIM_HASH)
    loadToPositionHash(fname=outFname)
    file.copy(from=outFname,to=paste(outFname,"_load.csv",sep=""),overwrite=T)
  }else{
    file.create(outFname)
  }
  
  #read file and pool setting
  readFname=paste(ResultFiles_Path_G,Underying_Symbol_G,"-READ-SPREAD.csv",sep="")
  tmp<-read.table(readFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  colnames(tmp)<-c(1:ncol(tmp))
  tmp %>% arrange(.[,length(opchain$Position)+1]) %>% head(min(TopN[2]/4,max(4,nrow(.)/2))) -> tmp
  pools<<-list(list(c(1,0,0),tmp))
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*COMBINATION_LOSSLIMIT_MULTIPLE
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=UNACCEPTABLEVAL,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE,
             POSITION_HASH=POSITION_OPTIM_HASH)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #Merge file
  if( file.exists(outFname) && FILEPLUS_HOT_START){
    LocalMergeWriteFiles(outFname)
  }
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1])  %>% 
    dplyr::filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) %>%
    dplyr::distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  
  #copy file to EvalPosition
  file.copy(from=outFname, to=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition.csv",sep=""),overwrite=T)
}

###FILE_PLUS_VERTICAL_DEBT_SPREAD
sampleSpreadType=FILE_PLUS_VERTICAL_DEBT_SPREAD
if(max(SpreadTypeToDir[[sampleSpreadType]]==SpreadTypeSpecified)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  totalPopNum=PopN[2]/4
  
  #spread ratio 1
  spreadRatio=c(1,1,1)
  
  #output file name
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  if( file.exists(outFname) && FILEPLUS_HOT_START){
    hash::clear(POSITION_OPTIM_HASH)
    loadToPositionHash(fname=outFname)
    file.copy(from=outFname,to=paste(outFname,"_load.csv",sep=""),overwrite=T)
  }else{
    file.create(outFname)
  }
  
  #read file and pool setting
  readFname=paste(ResultFiles_Path_G,Underying_Symbol_G,"-READ-SPREAD.csv",sep="")
  tmp<-read.table(readFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  colnames(tmp)<-c(1:ncol(tmp))
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% head(min(TopN[2]/4,max(4,nrow(.)/2))) -> tmp
  pools<<-list(list(c(1,0,0),tmp))
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*COMBINATION_LOSSLIMIT_MULTIPLE
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=UNACCEPTABLEVAL,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE,
             POSITION_HASH=POSITION_OPTIM_HASH)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #Merge file
  if( file.exists(outFname) && FILEPLUS_HOT_START){
    LocalMergeWriteFiles(outFname)
  }
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1])  %>%
    dplyr::filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) %>%
    dplyr::distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  
  #copy file to EvalPosition
  file.copy(from=outFname, to=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition.csv",sep=""),overwrite=T)
}

###FILE_PLUS_IRON_CONDOR
sampleSpreadType=FILE_PLUS_IRON_CONDOR
if(max(SpreadTypeToDir[[sampleSpreadType]]==SpreadTypeSpecified)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  totalPopNum=PopN[2]
  
  #spread ratio 1
  spreadRatio=c(1,1,1)
  
  #output file name
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  if( file.exists(outFname) && FILEPLUS_HOT_START){
    hash::clear(POSITION_OPTIM_HASH)
    loadToPositionHash(fname=outFname)
    file.copy(from=outFname,to=paste(outFname,"_load.csv",sep=""),overwrite=T)
  }else{
    file.create(outFname)
  }
  
  #read file and pool setting
  readFname=paste(ResultFiles_Path_G,Underying_Symbol_G,"-READ-SPREAD.csv",sep="")
  tmp<-read.table(readFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  colnames(tmp)<-c(1:ncol(tmp))
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% head(min(TopN[2],max(4,nrow(.)/2))) -> tmp
  pools<<-list(list(c(1,0,0),tmp))
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*COMBINATION_LOSSLIMIT_MULTIPLE
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=UNACCEPTABLEVAL,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE,
             POSITION_HASH=POSITION_OPTIM_HASH)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #Merge file
  if( file.exists(outFname) && FILEPLUS_HOT_START){
    LocalMergeWriteFiles(outFname)
  }
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1])  %>% 
    dplyr::filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) %>%
    dplyr::distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  
  #copy file to EvalPosition
  file.copy(from=outFname, to=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition.csv",sep=""),overwrite=T)
}

###FILE_PLUS_DOUBLE_DIAGONAL
sampleSpreadType=FILE_PLUS_DOUBLE_DIAGONAL
if(max(SpreadTypeToDir[[sampleSpreadType]]==SpreadTypeSpecified)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  totalPopNum=PopN[2]
  
  #spread ratio 1
  spreadRatio=c(1,1,1)
  
  #output file name
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  if( file.exists(outFname) && FILEPLUS_HOT_START){
    hash::clear(POSITION_OPTIM_HASH)
    loadToPositionHash(fname=outFname)
    file.copy(from=outFname,to=paste(outFname,"_load.csv",sep=""),overwrite=T)
  }else{
    file.create(outFname)
  }
  
  #read file and pool setting
  readFname=paste(ResultFiles_Path_G,Underying_Symbol_G,"-READ-SPREAD.csv",sep="")
  tmp<-read.table(readFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  colnames(tmp)<-c(1:ncol(tmp))
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% head(min(TopN[2],max(4,nrow(.)/2))) -> tmp
  pools<<-list(list(c(1,0,0),tmp))
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*COMBINATION_LOSSLIMIT_MULTIPLE
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=UNACCEPTABLEVAL,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE,
             POSITION_HASH=POSITION_OPTIM_HASH)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #Merge file
  if( file.exists(outFname) && FILEPLUS_HOT_START){
    LocalMergeWriteFiles(outFname)
  }
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1])  %>%
    dplyr::filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) %>%
    dplyr::distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  
  #copy file to EvalPosition
  evalPosFname=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition.csv",sep="")
  file.copy(from=outFname, to=evalPosFname,overwrite=T)
  #FlipEconomicValue
  LocalflipScoreWriteToFile(evalPosFname,50)
}

###FILE_PLUS_FILE
sampleSpreadType=FILE_PLUS_FILE
if(max(SpreadTypeToDir[[sampleSpreadType]]==SpreadTypeSpecified)){
  targetExpDate=TARGET_EXPDATE
  targetExpDate_f=TARGET_EXPDATE_FRONT
  targetExpDate_b=TARGET_EXPDATE_BACK
  totalPopNum=PopN[2]
  
  #spread ratio 1
  spreadRatio=c(1,1,1)
  
  #output file name
  outFname=createOutFname(targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,spreadRatio=spreadRatio,EvalFuncSetting=EvalFuncSetting)
  
  if( file.exists(outFname) && FILEPLUS_HOT_START){
    hash::clear(POSITION_OPTIM_HASH)
    loadToPositionHash(fname=outFname)
    file.copy(from=outFname,to=paste(outFname,"_load.csv",sep=""),overwrite=T)
  }else{
    file.create(outFname)
  }
  
  #read first file and pool setting
  readFname=paste(ResultFiles_Path_G,Underying_Symbol_G,"-READ-SPREAD.csv",sep="")
  tmp<-read.table(readFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  colnames(tmp)<-c(1:ncol(tmp))
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% head(min(TopN[2],max(4,nrow(.)/2))) -> tmp
  pools<-list(list(c(1,0,0),tmp))
  
  #read first file and pool setting
  readFname=paste(ResultFiles_Path_G,Underying_Symbol_G,"-READ-SPREAD2.csv",sep="")
  tmp<-read.table(readFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  colnames(tmp)<-c(1:ncol(tmp))
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% head(min(TopN[2],max(4,nrow(.)/2))) -> tmp
  pools[2]<-list(list(c(1,0,0),tmp))
  pools<<-pools
  
  #sampling
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*COMBINATION_LOSSLIMIT_MULTIPLE
  
  sampleMain(sampleSpreadType=sampleSpreadType,totalPopNum=totalPopNum,
             targetExpDate=targetExpDate,targetExpDate_f=targetExpDate_f,targetExpDate_b=targetExpDate_b,
             spreadRatio=spreadRatio,InitialPopThresh=UNACCEPTABLEVAL,outFname=outFname,isFileout=T,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE,
             POSITION_HASH=POSITION_OPTIM_HASH)
  
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  #Merge file
  if( file.exists(outFname) && FILEPLUS_HOT_START){
    LocalMergeWriteFiles(outFname)
  }
  
  #file handling
  tmp<-read.table(outFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1])  %>%
    dplyr::filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) %>%
    dplyr::distinct() -> tmp
  write.table(tmp,outFname,row.names = F,col.names=F,sep=",",append=F)
  
  #copy file to EvalPosition
  evalPosFname=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition.csv",sep="")
  file.copy(from=outFname, to=evalPosFname,overwrite=T)
  #FlipEconomicValue
  LocalflipScoreWriteToFile(evalPosFname,50)
}

