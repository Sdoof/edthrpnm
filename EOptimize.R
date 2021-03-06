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

#continue COMBINATION HOT START
COMBINATION_HOT_START=F

#speceif configuration is to be applied to first generation
SPECIFIC_FIRSTG_SETTING=F

#COMBINATION LossLimit Multipe
COMBINATION_LOSSLIMIT_MULTIPLE=2

#Debug, Detail Mode
IS_DEBUG_MODE=F
IS_DETAIL_MODE=F

#CACHE PRELOAD
IS_CACHE_PRELOAD=T

#cache for the position
HASH_HIT_NUM=0

#Combine Target Generation: 
# c(1Cb tgt,2Cb tgt,3Cb tgt ,4Cb tge,5Cb tgt) where 1Cb should be 0
# xCb tgt means: when xCb combination is created, combine (x-1)Cb with CombineTargetGeneration[xCb_tgt_idx]
#  for example: When CombineTargetGeneration=c(0,1,2,2,2),
#    3Cb target is created by combining 2Cb target with 2Cb target(=CombineTargetGeneration[3])
#    4Cb target is created by combining 3Cb target with 2Cb target(=CombineTargetGeneration[4])
EvalFuncSetting$CombineTargetGeneration
#Combinee Max posnum is calculated
CombinedMaxPosnum=rep(0,times=length(EvalFuncSetting$CombineTargetGeneration))
CombinedMaxPosnum[1]=max(EvalFuncSetting$Posnum)
for(tmp in 2:length(CombinedMaxPosnum)){
  CombinedMaxPosnum[tmp]=CombinedMaxPosnum[EvalFuncSetting$CombineTargetGeneration[tmp]]+CombinedMaxPosnum[(tmp-1)]
}

#touch the file whose name shows the configuration of this search
tmp_touchfile=paste(ResultFiles_Path_G,LocalcreateSampleConditionStr(EvalFuncSetting),".csv",sep="")
file.create(tmp_touchfile)
cat("PriceRangeDist",EvalFuncSetting$PriceRangeDist,"\n",file=tmp_touchfile,sep=",",append=TRUE)
cat("Weight_Explicit",EvalFuncSetting$Weight_Explicit,"\n",file=tmp_touchfile,sep=",",append=TRUE)
cat("Weight_Explicit_1D",EvalFuncSetting$Weight_Explicit_1D,"\n",file=tmp_touchfile,sep=",",append=TRUE)
cat("UnitMinProfit",EvalFuncSetting$UnitMinProfit,"\n",file=tmp_touchfile,sep=",",append=TRUE)
cat("ConditonalProfitEval",EvalFuncSetting$ConditonalProfitEval,"\n",file=tmp_touchfile,sep=",",append=TRUE)
cat("Posnum",EvalFuncSetting$Posnum,"\n",file=tmp_touchfile,sep=",",append=TRUE)
cat("CombineTargetGeneration",EvalFuncSetting$CombineTargetGeneration,"\n",file=tmp_touchfile,sep=",",append=TRUE)
cat("CombinedMaxPosnum",CombinedMaxPosnum,"\n",file=tmp_touchfile,sep=",",append=TRUE)
cat("TopN",TopN,"\n",file=tmp_touchfile,sep=",",append=TRUE)
cat("PopN",PopN,"\n",file=tmp_touchfile,sep=",",append=TRUE)
cat("ExpIVChange_Multiple",EvalFuncSetting$ExpIVChange_Multiple,"\n",file=tmp_touchfile,sep=",",append=TRUE)
cat("Delta_Direct_Prf",EvalFuncSetting$Delta_Direct_Prf,"\n",file=tmp_touchfile,sep=",",append=TRUE)
cat("Vega_Direct_Prf",EvalFuncSetting$Vega_Direct_Prf,"\n",file=tmp_touchfile,sep=",",append=TRUE)

##
# Creating a First Generation

#If speceif configuration is to be applied to first generation. 

#The objective to change the configuration when creating a population of first generation is
#to try to make the expected return (profit) reasonably good while preserving diversity of the population.
#Remember if X and Y is independent, E(X+Y)=E(X)+E(Y). So trying to get a population
#whose member's E(X) is as big as possible would be reasonable.
#Risk side evaluation is not so simple. RisK(X+Y) != Risk(X)+Risk(Y), expectiing risk of
#X and Y could be canceled by combination.
#This is the reason we could exploit the combinational optimization approach.

if(SPECIFIC_FIRSTG_SETTING==T){
  origEvalFuncSetting=EvalFuncSetting
  
  #Give some leeway to Directional Effect
  EvalFuncSetting$ExpIVChange_Multiple=EvalFuncSetting$ExpIVChange_Multiple*0.8
  #EvalFuncSetting$DrctlEffect_Coef["DeltaECoef"]=EvalFuncSetting$DrctlEffect_Coef["DeltaECoef"]*0.8
  EvalFuncSetting$Delta_Thresh_Minus=rep(min(EvalFuncSetting$Delta_Thresh_Minus)-1,
                                         length(EvalFuncSetting$Delta_Thresh_Minus))
  EvalFuncSetting$Delta_Thresh_Plus=rep(max(EvalFuncSetting$Delta_Thresh_Plus)+(1),
                                        length(EvalFuncSetting$Delta_Thresh_Plus))
  #EvalFuncSetting$Vega_Thresh_Minus=rep(min(EvalFuncSetting$Vega_Thresh_Minus)-30,
  #                                      length(EvalFuncSetting$Vega_Thresh_Minus))
  #EvalFuncSetting$Vega_Thresh_Plus=rep(max(EvalFuncSetting$Vega_Thresh_Plus)+30,
  #                                     length(EvalFuncSetting$Vega_Thresh_Plus))
  
  #Use economic Value or not
  #if only profit is concerned. ONLY POSSIBLE WHEN USING EconomicValue
  #EvalFuncSetting$EvalEconomicValue=ifelse(EvalFuncSetting$EvalEconomicValue,F,T)
  
  #Use SortinoRatio swtich
  #EvalFuncSetting$UseSortinoRatio=ifelse(EvalFuncSetting$UseSortinoRatio,F,T)
  
  #write to the touch file
  cat("1StGen.DrctlEfct",EvalFuncSetting$DrctlEffect_Coef,"MaxLoss_Coef",EvalFuncSetting$MaxLoss_Coef ,"\n",file=tmp_touchfile,sep=",",append=TRUE)
  cat("1StGen.Delta_Thresh",EvalFuncSetting$Delta_Thresh_Minus,"_",EvalFuncSetting$Delta_Thresh_Plus,"\n",file=tmp_touchfile,sep=",",append=TRUE)
  cat("1StGen.Vega_Thresh",EvalFuncSetting$Vega_Thresh_Minus,"_",EvalFuncSetting$Vega_Thresh_Plus,"\n",file=tmp_touchfile,sep=",",append=TRUE)
  cat("1StGen.EvalEconomicValue",EvalFuncSetting$EvalEconomicValue,"\n",file=tmp_touchfile,sep=",",append=TRUE)
  cat("1StGen.UseSortinoRatio",EvalFuncSetting$UseSortinoRatio,"\n",file=tmp_touchfile,sep=",",append=TRUE)
  cat("1StGen.ExpIVChange_Multiple",EvalFuncSetting$ExpIVChange_Multiple,"\n",file=tmp_touchfile,sep=",",append=TRUE)
}

if(COMBINATION_HOT_START==T){
  hash::clear(POSITION_OPTIM_HASH)
  
  tmp=paste(ResultFiles_Path_G,"1Cb.csv",sep="")
  if( file.exists(tmp)){
    loadToPositionHash(fname=tmp)
    file.copy(from=tmp,to=paste(tmp,"_load.csv",sep=""),overwrite=T)
  }
  
  tmp=paste(ResultFiles_Path_G,"2Cb.csv",sep="")
  if( file.exists(tmp)){
    loadToPositionHash(fname=tmp)
    file.copy(from=tmp,to=paste(tmp,"_load.csv",sep=""),overwrite=T)
  }
  
  tmp=paste(ResultFiles_Path_G,"3Cb.csv",sep="")
  if( file.exists(tmp)){
    loadToPositionHash(fname=tmp)
    file.copy(from=tmp,to=paste(tmp,"_load.csv",sep=""),overwrite=T)
  }
  
  tmp=paste(ResultFiles_Path_G,"4Cb.csv",sep="")
  if( file.exists(tmp)){
    loadToPositionHash(fname=tmp)
    file.copy(from=tmp,to=paste(tmp,"_load.csv",sep=""),overwrite=T)
  }
  
  tmp=paste(ResultFiles_Path_G,"5Cb.csv",sep="")
  if( file.exists(tmp)){
    loadToPositionHash(fname=tmp)
    file.copy(from=tmp,to=paste(tmp,"_load.csv",sep=""),overwrite=T)
  }
}

if(COMBINATION_HOT_START==F){
  if(IS_CACHE_PRELOAD){
    #create 1Cb.csv
    st <- "powershell.exe .\\shell\\cmd1.ps1"
    system(st)
    st <- "powershell.exe .\\shell\\cmd2.ps1"
    system(st)
    st <- "powershell.exe -Command \" del .\\ResultData\\1Cb-.csv \" "
    system(st) ;rm(st)
    #cache pre-load
    readFname=paste(ResultFiles_Path_G,"1Cb.csv",sep='')
    if(file.exists(readFname)){
      tryCatch(
        {
          tmp<-read.table(readFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
          tmp=tmp[,1:(length(opchain$Position)+1)]
          colnames(tmp)=c(rep(1:length(opchain$Position)),"eval")
          tmp %>% arrange(.[,length(opchain$Position)+1]) %>%
            dplyr::distinct(.keep_all=TRUE) -> tmp
          # clear hash
          hash::clear(POSITION_OPTIM_HASH)
          # create initail POSITION_OPTIM_HASH
          tmp %>% dplyr::rowwise() %>%
            # x = unlist(.)[1:length(opchain$Position)]
            # revVal = unlist(.)[length(opchain$Position)+1]
            dplyr::do(md5sum=digest(paste(unlist(.)[1:length(opchain$Position)],collapse = "")),revVal=unlist(.)[length(opchain$Position)+1]
            ) -> tmp2
          POSITION_OPTIM_HASH[ unlist(tmp2$md5sum) ]<-unlist(tmp2$revVal)
        },
        error=function(e){
          cat("no cache preloaded: ")
          #message(e)
          st <- "powershell.exe -Command \" del .\\ResultData\\1Cb.csv \" "
          system(st) ;rm(st)
        }
      )
      cat("preloaded cache: hash hit:",HASH_HIT_NUM,"hash num:",length(POSITION_OPTIM_HASH),"\n")
    }
  }
  #First Generation
  for(itr in 1:InitialPopCreateLoopNum){
    cat("itr:",itr,"\n")
    if(sum(EvalFuncSetting$Posnum==5)!=0){
      createInitialExactPutCallPopulation(popnum=100,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,
                                          putn=3,calln=2,ml=Optimize_ml,
                                          fname=paste(".\\ResultData\\inipop-05P3C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          PosMultip,
                                          isFileout=TRUE,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
      createInitialExactPutCallPopulation(popnum=70,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,
                                          putn=2,calln=3,ml=Optimize_ml,
                                          fname=paste(".\\ResultData\\inipop-05P2C3-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          PosMultip,
                                          isFileout=TRUE,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
    }
    if(sum(EvalFuncSetting$Posnum==4)!=0){
      createInitialExactPutCallPopulation(popnum=100,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,
                                          putn=4,calln=0,ml=Optimize_ml,
                                          fname=paste(".\\ResultData\\inipop-04P4C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          PosMultip,
                                          isFileout=TRUE,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
      createInitialExactPutCallPopulation(popnum=100,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,
                                          putn=2,calln=2,ml=Optimize_ml,
                                          fname=paste(".\\ResultData\\inipop-04P2C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          PosMultip,
                                          isFileout=TRUE,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
      # createInitialExactPutCallPopulation(popnum=50,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,
      #                                     putn=0,calln=4,ml=Optimize_ml,
      #                                     fname=paste(".\\ResultData\\inipop-04P0C4-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
      #                                     PosMultip,
      #                                      isFileout=TRUE,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
    }
    if(sum(EvalFuncSetting$Posnum==3)!=0){
      createInitialExactPutCallPopulation(popnum=50,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,
                                          putn=0,calln=4,ml=Optimize_ml,
                                          fname=paste(".\\ResultData\\inipop-04P0C4-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          PosMultip,
                                          isFileout=TRUE,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
      # createInitialExactPutCallPopulation(popnum=50,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,
      #                                     putn=3,calln=0,ml=Optimize_ml,
      #                                     fname=paste(".\\ResultData\\inipop-03P3C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
      #                                     PosMultip,
      #                                     isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
      
      # createInitialExactPutCallPopulation(popnum=25,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,
      #                                     putn=0,calln=3,ml=Optimize_ml,
      #                                     fname=paste(".\\ResultData\\inipop-03P0C3-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
      #                                     PosMultip,
      #                                     isFileout=TRUE,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
    }
    if(sum(EvalFuncSetting$Posnum==2)!=0){
      createInitialExactPutCallPopulation(popnum=30,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,
                                          putn=2,calln=0,ml=Optimize_ml,
                                          fname=paste(".\\ResultData\\inipop-02P2C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          PosMultip,
                                          isFileout=TRUE,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
      createInitialExactPutCallPopulation(popnum=30,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,
                                          putn=0,calln=2,ml=Optimize_ml,
                                          fname=paste(".\\ResultData\\inipop-02P0C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                          PosMultip,
                                          isFileout=TRUE,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE)
    }
  }
  #Restore original setting
  if(SPECIFIC_FIRSTG_SETTING==T){
    EvalFuncSetting=origEvalFuncSetting
    #write to the touched file
    cat("Restrd.DrctlEfct",EvalFuncSetting$DrctlEffect_Coef,"MaxLoss_Coef",EvalFuncSetting$MaxLoss_Coef ,"\n",file=tmp_touchfile,sep=",",append=TRUE)
    cat("Restrd.Delta_Thresh",EvalFuncSetting$Delta_Thresh_Minus,"_",EvalFuncSetting$Delta_Thresh_Plus,"\n",file=tmp_touchfile,sep=",",append=TRUE)
    cat("Restrd.Vega_Thresh",EvalFuncSetting$Vega_Thresh_Minus,"_",EvalFuncSetting$Vega_Thresh_Plus,"\n",file=tmp_touchfile,sep=",",append=TRUE)
    cat("Restrd.EvalEconomicValue",EvalFuncSetting$EvalEconomicValue,"\n",file=tmp_touchfile,sep=",",append=TRUE)
    cat("Restrd.UseSortinoRatio",EvalFuncSetting$UseSortinoRatio,"\n",file=tmp_touchfile,sep=",",append=TRUE)
    cat("Restrd.ExpIVChange_Multiple",EvalFuncSetting$ExpIVChange_Multiple,"\n",file=tmp_touchfile,sep=",",append=TRUE)
  }
  #1Cb.csv
  st <- "powershell.exe .\\shell\\cmd1.ps1"
  system(st)
  st <- "powershell.exe .\\shell\\cmd2.ps1"
  system(st)
  st <- "powershell.exe -Command \" del .\\ResultData\\1Cb-.csv \" "
  system(st) ;rm(st)
}

## read 1Cb.csv created above
tmp<-read.table(paste(ResultFiles_Path_G,"1Cb.csv",sep=""),header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
tmp=tmp[,1:(length(opchain$Position)+1)]
colnames(tmp)=c(rep(1:length(opchain$Position)),"eval")
tmp %>% dplyr::arrange(tmp[,(length(opchain$Position)+1)]) %>% 
  dplyr::filter(eval<UNACCEPTABLEVAL) %>%
  dplyr::distinct(eval,.keep_all=TRUE) -> tmp
#posnum put call
tmp[,1:length(opchain$Position)] %>% dplyr::rowwise() %>% dplyr::do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp2
tmp2  %>% dplyr::rowwise() %>% dplyr::do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp3
tmp$putn<-unlist(tmp3$putn);tmp$calln<-unlist(tmp3$calln);rm(tmp2);rm(tmp3)
#write to a file
write.table(tmp,paste(ResultFiles_Path_G,"1Cb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)

## clear hash to improve performance
hash::clear(POSITION_OPTIM_HASH)

## revalue the position for the value to be compatilbe with following process
if(SPECIFIC_FIRSTG_SETTING==T){
  file.copy(from=paste(ResultFiles_Path_G,"1Cb.csv",sep=""),to=paste(ResultFiles_Path_G,"1Cb_org.csv",sep=""),overwrite=T)
  tmp %>% dplyr::rowwise() %>%
    # x = unlist(.)[1:length(opchain$Position)]
    dplyr::do(revVal=obj_Income_sgmd(unlist(.)[1:length(opchain$Position)],
                                     EvalFuncSetting,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE,
                                     udlStepNum=EvalFuncSetting$UdlStepNum,udlStepPct=EvalFuncSetting$UdlStepPct,
                                     PosMultip=PosMultip,
                                     lossLimitPrice=EvalFuncSetting$LossLimitPrice,
                                     Delta_Direct_Prf=EvalFuncSetting$Delta_Direct_Prf[sum(as.numeric((unlist(.)[1:length(opchain$Position)])!=0))],
                                     Vega_Direct_Prf=EvalFuncSetting$Vega_Direct_Prf[sum(as.numeric((unlist(.)[1:length(opchain$Position)])!=0))],
                                     Delta_Neutral_Offset=EvalFuncSetting$Delta_Neutral_Offset[sum(as.numeric((unlist(.)[1:length(opchain$Position)])!=0))],
                                     Vega_Neutral_Offset=EvalFuncSetting$Vega_Neutral_Offset[sum(as.numeric((unlist(.)[1:length(opchain$Position)])!=0))])
    ) -> tmp2
  tmp[,length(opchain$Position)+1]=unlist(tmp2)
  #posnum put call
  tmp[,1:length(opchain$Position)] %>% dplyr::rowwise() %>% dplyr::do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp2
  tmp2  %>% dplyr::rowwise() %>% dplyr::do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp3
  tmp$putn<-unlist(tmp3$putn);tmp$calln<-unlist(tmp3$calln);rm(tmp2);rm(tmp3)
  #write to a file
  write.table(tmp,paste(ResultFiles_Path_G,"1Cb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
}

#recreate POSITION_OPTIM_HASH
tmp %>% dplyr::rowwise() %>%
  # x = unlist(.)[1:length(opchain$Position)]
  # revVal = unlist(.)[length(opchain$Position)+1]
  dplyr::do(md5sum=digest(paste(unlist(.)[1:length(opchain$Position)],collapse = "")),revVal=unlist(.)[length(opchain$Position)+1]
  ) -> tmp2
POSITION_OPTIM_HASH[ unlist(tmp2$md5sum) ]<-unlist(tmp2$revVal)

#combined population search
if(Combined_Spread){
  ##
  #  LossLimitPrice adjust
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*COMBINATION_LOSSLIMIT_MULTIPLE
  
  # TopN_1 selection
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% head(TopN[1]) -> tmp
  
  ###
  ##   2(exact x exact) Combinations (2Cb)
  
  ## Making pools for combinational search
  #   when all results are mixed together regardress of the number of Putn and Calln, pools[[1]] should be set as
  #   c(1,0,0) <- c(1Cb{=exact}, Putn not spicified, Calln not spicified)
  pools<-list(list(c(1,0,0),tmp)) #No.[[1]]
  rm(tmp)
  
  maxposn_tmp=length(EvalFuncSetting$Vega_Direct_Prf)
  #combinational search
  if(CombinedMaxPosnum[1] < maxposn_tmp){
    createCombinedPopulation(popnum=PopN[2],EvalFuncSetting,thresh=ThreshN[2],plelem=c(EvalFuncSetting$CombineTargetGeneration[2],1),ml=Optimize_ml,
                             fname=paste(".\\ResultData\\combine-Result-1Cb+1Cb-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                             isFileout=TRUE,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE,maxposn=maxposn_tmp,PosMultip=PosMultip)
    #creating 2Cb.csv
    st <- "powershell.exe .\\shell\\cmd3.ps1"
    system(st)
    st <- "powershell.exe .\\shell\\cmd4.ps1"
    system(st)
    st <- "powershell.exe -Command \" del .\\ResultData\\2Cb-.csv \" "
    system(st) ;rm(st)
    #read, sort and rewrite to the file
    tmp<-read.table(paste(ResultFiles_Path_G,"2Cb.csv",sep=""),header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
    tmp=tmp[,1:(length(opchain$Position)+1)]
    colnames(tmp)=c(rep(1:length(opchain$Position)),"eval")
    tmp %>% dplyr::arrange(tmp[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp
    write.table(tmp,paste(ResultFiles_Path_G,"2Cb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
  }
  
  ###
  ##   3(2Cb x 1Cb) Combinations (3Cb)
  maxposn_tmp=length(EvalFuncSetting$Vega_Direct_Prf)
  if(CombinedMaxPosnum[2] < maxposn_tmp){
    #adding pools' element
    tmp %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% head(TopN[2]) -> tmp
    pools[2]<-list(list(c(1,0,0),tmp)) #No.[[2]]
    pools<<-pools
    #combinational search
    createCombinedPopulation(popnum=PopN[3],EvalFuncSetting,thresh=ThreshN[3],plelem=c(EvalFuncSetting$CombineTargetGeneration[3],2),ml=Optimize_ml,
                             fname=paste(".\\ResultData\\combine-Result-1Cb+1Cb+1Cb-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                             isFileout=TRUE,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE,maxposn=maxposn_tmp,PosMultip=PosMultip)
    #creating 3Cb.csv
    st <- "powershell.exe .\\shell\\cmd5.ps1"
    system(st)
    st <- "powershell.exe .\\shell\\cmd6.ps1"
    system(st)
    st <- "powershell.exe -Command \" del .\\ResultData\\3Cb-.csv \" "
    system(st) ;rm(st)
    #read, sort and rewrite to the file
    tmp<-read.table(paste(ResultFiles_Path_G,"3Cb.csv",sep=""),header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
    tmp=tmp[,1:(length(opchain$Position)+1)]
    colnames(tmp)=c(rep(1:length(opchain$Position)),"eval")
    tmp %>% dplyr::arrange(tmp[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp
    write.table(tmp,paste(ResultFiles_Path_G,"3Cb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
  }
  
  ###
  ##  4(3Cb x 1Cb currently shown as 2Cb+2Cb) Combinations (4Cb)
  maxposn_tmp=length(EvalFuncSetting$Vega_Direct_Prf)
  if(CombinedMaxPosnum[3] < maxposn_tmp){
    #adding pools' element
    tmp %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% head(TopN[3]) -> tmp
    pools[3]<-list(list(c(1,0,0),tmp)) #No.[[3]]
    pools<<-pools
    #combinational search
    createCombinedPopulation(popnum=PopN[4],EvalFuncSetting,thresh=ThreshN[4],plelem=c(EvalFuncSetting$CombineTargetGeneration[4],3),ml=Optimize_ml,
                             fname=paste(".\\ResultData\\combine-Result-2Cb+2Cb-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                             isFileout=TRUE,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE,maxposn=maxposn_tmp,PosMultip=PosMultip)
    #creating 4Cb.csv
    st <- "powershell.exe .\\shell\\cmd7.ps1"
    system(st)
    st <- "powershell.exe .\\shell\\cmd8.ps1"
    system(st)
    st <- "powershell.exe -Command \" del .\\ResultData\\4Cb-.csv \" "
    system(st) ;rm(st)
    #read, sort and rewrite to the file
    tmp<-read.table(paste(ResultFiles_Path_G,"4Cb.csv",sep=""),header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
    tmp=tmp[,1:(length(opchain$Position)+1)]
    colnames(tmp)=c(rep(1:length(opchain$Position)),"eval")
    tmp %>% dplyr::arrange(tmp[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp
    write.table(tmp,paste(ResultFiles_Path_G,"4Cb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
  }
  
  ###
  ##  5(3Cb x 2Cb) Combinations (5Cb)
  maxposn_tmp=length(EvalFuncSetting$Vega_Direct_Prf)
  if(CombinedMaxPosnum[4] < maxposn_tmp){
    #adding pools' element
    tmp %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% head(TopN[4]) -> tmp
    pools[4]<-list(list(c(1,0,0),tmp)) #No.[[4]]
    pools<<-pools
    #combinational search
    createCombinedPopulation(popnum=PopN[5],EvalFuncSetting,thresh=ThreshN[5],plelem=c(EvalFuncSetting$CombineTargetGeneration[5],4),ml=Optimize_ml,
                             fname=paste(".\\ResultData\\combine-Result-3Cb+2Cb-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                             isFileout=TRUE,isDebug=IS_DEBUG_MODE,isDetail=IS_DETAIL_MODE,maxposn=maxposn_tmp,PosMultip=PosMultip)
    #creating 5Cb.csv
    st <- "powershell.exe .\\shell\\cmd9.ps1"
    system(st)
    st <- "powershell.exe .\\shell\\cmd10.ps1"
    system(st)
    st <- "powershell.exe -Command \" del .\\ResultData\\5Cb-.csv \" "
    system(st) ;rm(st)
    #read, sort and rewrite to the file
    tmp<-read.table(paste(ResultFiles_Path_G,"5Cb.csv",sep=""),header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
    tmp=tmp[,1:(length(opchain$Position)+1)]
    colnames(tmp)=c(rep(1:length(opchain$Position)),"eval")
    tmp %>% dplyr::arrange(tmp[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp
    write.table(tmp,paste(ResultFiles_Path_G,"5Cb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
  }
  
  ##
  # LossLimitPrice's original value is returned
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
}

###
##
# Result Post Proceccing

##
# Exact (1Cb)
res1<-read.table(paste(ResultFiles_Path_G,"1Cb.csv",sep=""),header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
res1=res1[,1:(length(opchain$Position)+1)]
colnames(res1)=c(rep(1:length(opchain$Position)),"eval")
res1 %>% dplyr::arrange(res1[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> res1
#over the specified socre
res1 %>% dplyr::filter(.[,length(opchain$Position)+1]<ThreshN[1]) -> res1
#posnum put call
res1[,1:length(opchain$Position)] %>% dplyr::rowwise() %>% dplyr::do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
tmp  %>% dplyr::rowwise() %>% dplyr::do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)
res1 -> total_res ; rm(res1)

if(Combined_Spread){
  ##
  # 2Cb
  if(file.exists(paste(ResultFiles_Path_G,"2Cb.csv",sep=""))){
    res1<-read.table(paste(ResultFiles_Path_G,"2Cb.csv",sep=""),header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
    res1=res1[,1:(length(opchain$Position)+1)]
    write.table(res1,paste(ResultFiles_Path_G,"2Cb.cmb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
    colnames(res1)=c(rep(1:length(opchain$Position)),"eval")
    res1 %>% dplyr::arrange(res1[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> res1
    #posnum put call
    res1[,1:length(opchain$Position)] %>% dplyr::rowwise() %>% dplyr::do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
    tmp  %>% dplyr::rowwise() %>% dplyr::do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
    res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)
    write.table(res1,paste(ResultFiles_Path_G,"2Cb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
    #over the specified socre
    res1 %>% dplyr::filter(.[,length(opchain$Position)+1]<ThreshN[2]) -> res1
    #factorと認識されたときの変換 #res1$V1<-as.numeric(as.character(res1$V1))
    
    #full join
    dplyr::full_join(total_res,res1) %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% dplyr::distinct() -> total_res
    rm(res1)
  }
  
  ##
  # 3Cb
  if(file.exists(paste(ResultFiles_Path_G,"3Cb.csv",sep=""))){
    res1<-read.table(paste(ResultFiles_Path_G,"3Cb.csv",sep=""),header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
    res1=res1[,1:(length(opchain$Position)+1)]
    write.table(res1,paste(ResultFiles_Path_G,"3Cb.cmb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
    colnames(res1)=c(rep(1:length(opchain$Position)),"eval")
    res1 %>% dplyr::arrange(res1[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> res1
    #posnum put call
    res1[,1:length(opchain$Position)] %>% dplyr::rowwise() %>% dplyr::do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
    tmp  %>% dplyr::rowwise() %>% dplyr::do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
    res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)
    write.table(res1,paste(ResultFiles_Path_G,"3Cb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
    #over the specified socre
    res1 %>% dplyr::filter(.[,length(opchain$Position)+1]<ThreshN[3]) -> res1
    #factorと認識されたときの変換 #res1$V1<-as.numeric(as.character(res1$V1))
    
    #full join
    dplyr::full_join(total_res,res1) %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% dplyr::distinct() -> total_res
    rm(res1)
  }
  
  ##
  #  4Cb
  if(file.exists(paste(ResultFiles_Path_G,"4Cb.csv",sep=""))){
    res1<-read.table(paste(ResultFiles_Path_G,"4Cb.csv",sep=""),header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
    res1=res1[,1:(length(opchain$Position)+1)]
    write.table(res1,paste(ResultFiles_Path_G,"4Cb.cmb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
    colnames(res1)=c(rep(1:length(opchain$Position)),"eval")
    res1 %>% dplyr::arrange(res1[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> res1
    #posnum put call
    res1[,1:length(opchain$Position)] %>% dplyr::rowwise() %>% dplyr::do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
    tmp  %>% dplyr::rowwise() %>% dplyr::do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
    res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)
    write.table(res1,paste(ResultFiles_Path_G,"4Cb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
    #over the specified socre
    res1 %>% dplyr::filter(.[,length(opchain$Position)+1]<ThreshN[4]) -> res1
    
    #full join
    dplyr::full_join(total_res,res1) %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% dplyr::distinct() -> total_res
    rm(res1)
  }
  
  ##
  # 5Cb
  if(file.exists(paste(ResultFiles_Path_G,"5Cb.csv",sep=""))){
    res1<-read.table(paste(ResultFiles_Path_G,"5Cb.csv",sep=""),header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
    res1=res1[,1:(length(opchain$Position)+1)]
    write.table(res1,paste(ResultFiles_Path_G,"5Cb.cmb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
    colnames(res1)=c(rep(1:length(opchain$Position)),"eval")
    res1 %>% dplyr::arrange(res1[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> res1
    #posnum put call
    res1[,1:length(opchain$Position)] %>% dplyr::rowwise() %>% dplyr::do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
    tmp  %>% dplyr::rowwise() %>% dplyr::do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
    res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)
    write.table(res1,paste(ResultFiles_Path_G,"5Cb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
    #over the specified socre
    res1 %>% dplyr::filter(.[,length(opchain$Position)+1]<ThreshN[5]) -> res1
    
    #full join
    dplyr::full_join(total_res,res1) %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% dplyr::distinct() -> total_res
    rm(res1)
  }
}

# Writing to files based on option legs total number
total_res %>% dplyr::mutate(posn=(putn+calln)) -> total_res
total_res %>%  dplyr::filter(posn>=9) -> tmp_fil 
total_res %>%  dplyr::filter(posn==7 | posn==8) -> tmp_fil2
total_res %>%  dplyr::filter(posn==5 | posn==6) -> tmp_fil3
total_res %>%  dplyr::filter(posn==3 | posn==4) -> tmp_fil4
total_res %>%  dplyr::filter(posn<=2) -> tmp_fil5

#position wiht Greeks
getPositionWithGreeks<-function(tmp_fil){
  tmp_fil[,1:length(opchain$Position)] %>% dplyr::rowwise() %>% 
    dplyr::do(theGreks=getPositionGreeks(hollowNonZeroPosition(unlist(.)),
                                         multi=PosMultip,
                                         hdd=holdDays,
                                         HV_IV_Adjust_Ratio=HV_IV_Adjust_Ratio)) -> tmp
  tmp_fil$theGreks<-tmp$theGreks
  tmp_fil %>% dplyr::rowwise() %>%
    dplyr::do(Delta=.$theGreks$Delta,Vega=.$theGreks$Vega,
              DeltaEffect=.$theGreks$DeltaEffect,VegaEffect=.$theGreks$VegaEffect,
              ThetaEffect=.$theGreks$ThetaEffect,GammaEffect=.$theGreks$GammaEffect) -> tmp2
  tmp_fil$Delta<-unlist(tmp2$Delta)
  tmp_fil$Vega<-unlist(tmp2$Vega)
  tmp_fil$DeltaEffect<-unlist(tmp2$DeltaEffect)
  tmp_fil$ThetaEffect<-unlist(tmp2$ThetaEffect)
  tmp_fil$GammaEffect<-unlist(tmp2$GammaEffect)
  tmp_fil$VegaEffect<-unlist(tmp2$VegaEffect)
  tmp_fil$theGreks<-NULL
  tmp_fil$AdvEffect<-unlist(tmp2$ThetaEffect)+unlist(tmp2$GammaEffect)
  return(tmp_fil)
}

#data for writing to files.
tmp_fil %>% 
  dplyr::arrange(tmp_fil[,(length(opchain$Position)+1)]) %>%
  dplyr::distinct(eval,.keep_all=TRUE) -> tmp_fil_w

tmp_fil2 %>% 
  dplyr::arrange(tmp_fil2[,(length(opchain$Position)+1)]) %>%
  dplyr::distinct(eval,.keep_all=TRUE) -> tmp_fil_w2

tmp_fil3 %>% 
  dplyr::arrange(tmp_fil3[,(length(opchain$Position)+1)]) %>%
  dplyr::distinct(eval,.keep_all=TRUE) -> tmp_fil_w3

tmp_fil4 %>% 
  dplyr::arrange(tmp_fil4[,(length(opchain$Position)+1)]) %>%
  dplyr::distinct(eval,.keep_all=TRUE) -> tmp_fil_w4

tmp_fil5 %>% 
  dplyr::arrange(tmp_fil5[,(length(opchain$Position)+1)]) %>%
  dplyr::distinct(eval,.keep_all=TRUE) -> tmp_fil_w5

#MERGE
if(COMBINATION_HOT_START==T){
  rf_=paste(ResultFiles_Path_G,"2Cb.csv",sep="")
  if(file.exists(rf_)){
    LocalMergeWriteFiles(rf=rf_)
  }
  
  rf_=paste(ResultFiles_Path_G,"3Cb.csv",sep="")
  if(file.exists(rf_)){
    LocalMergeWriteFiles(rf=rf_)
    
  }
  
  rf_=paste(ResultFiles_Path_G,"4Cb.csv",sep="")
  if(file.exists(rf_)){
    LocalMergeWriteFiles(rf=rf_)
  }
  
  rf_=paste(ResultFiles_Path_G,"5Cb.csv",sep="")
  if(file.exists(rf_)){
    LocalMergeWriteFiles(rf=rf_)
  }
}

## Save to a file
resultSaveFileRf=vector("list",4)
resultSaveFileRf[[1]]=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition_",
                            format(Sys.time(),"%Y%b%d_%H%M%S"),".csv",sep="")
resultSaveFileRf[[2]]=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition2_",
                            format(Sys.time(),"%Y%b%d_%H%M%S"),".csv",sep="")
resultSaveFileRf[[3]]=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition3_",
                            format(Sys.time(),"%Y%b%d_%H%M%S"),".csv",sep="")
resultSaveFileRf[[4]]=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition4_"
                            ,format(Sys.time(),"%Y%b%d_%H%M%S"),".csv",sep="")
resultSaveFileRf[[5]]=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition5_"
                            ,format(Sys.time(),"%Y%b%d_%H%M%S"),".csv",sep="")
if(nrow(tmp_fil_w)!=0)
  write.table(tmp_fil_w,resultSaveFileRf[[1]],row.names = F,col.names=F,sep=",",append=F)
if(nrow(tmp_fil_w2)!=0)
  write.table(tmp_fil_w2,resultSaveFileRf[[2]],row.names = F,col.names=F,sep=",",append=F)
if(nrow(tmp_fil_w3)!=0)
  write.table(tmp_fil_w3,resultSaveFileRf[[3]],row.names = F,col.names=F,sep=",",append=F)
if(nrow(tmp_fil_w4)!=0)
  write.table(tmp_fil_w4,resultSaveFileRf[[4]],row.names = F,col.names=F,sep=",",append=F)
if(nrow(tmp_fil_w5)!=0)
  write.table(tmp_fil_w5,resultSaveFileRf[[5]],row.names = F,col.names=F,sep=",",append=F)


## Copy to UDLY_EValPosition.csv and Evaluate Economic Value
file.copy(from=resultSaveFileRf[[4]], 
          to=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition.csv",sep=""),
          overwrite=T)
LocalflipScoreWriteToFile(resultSaveFileRf[[4]],50)

#Run greedy search
source('./EGreedySearch.R',encoding = 'UTF-8')


