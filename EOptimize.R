library(RQuantLib)
library(ggplot2)
library(dplyr)
library(pracma)
library(digest)
library(hash)
rm(list=ls())
source('./ESourceRCode.R',encoding = 'UTF-8')

#continue COMBINATION HOT START
COMBINATION_HOT_START=F

#speceif configuration is to be applied to first generation
SPECIFIC_FIRSTG_SETTING=F

#COMBINATION LossLimit Multipe
COMBINATION_LOSSLIMIT_Multipe=2

#cache for the position
POSITION_OPTIM_HASH=hash()
HASH_HIT_NUM=0

##
# Creating First Generation

#If speceif configuration is to be applied to first generation. 

#The objective to change the configuration when creating a population of first generation is
#to try to make the expected return (profit) as big as possible.
#Remember if X and Y is independent, E(X+Y)=E(X)+E(Y). So trying to get a population
#whose member's E(X) is as big as possible would be reasonable.
#Risk side evaluation is not so simple. RisK(X+Y) != Risk(X)+Risk(Y), expectiing risk of
#X and Y could be canceled by combination.
#This is the reason we could exploit the combinational optimizational approach.

if(SPECIFIC_FIRSTG_SETTING==T){
  #copy original setting
  origEvalFuncSetting=EvalFuncSetting
  #changed setting
  EvalFuncSetting$Delta_Thresh_Minus=rep(min(EvalFuncSetting$Delta_Thresh_Minus)-3,
                                         length(EvalFuncSetting$Delta_Thresh_Minus))
  EvalFuncSetting$Delta_Thresh_Plus=rep(max(EvalFuncSetting$Delta_Thresh_Plus)+3,
                                        length(EvalFuncSetting$Delta_Thresh_Plus))
  EvalFuncSetting$Vega_Thresh_Minus=rep(min(EvalFuncSetting$Vega_Thresh_Minus)-30,
                                        length(EvalFuncSetting$Vega_Thresh_Minus))
  EvalFuncSetting$Vega_Thresh_Plus=rep(max(EvalFuncSetting$Vega_Thresh_Plus)+30,
                                       length(EvalFuncSetting$Vega_Thresh_Plus))
}

if(COMBINATION_HOT_START==T){
  hash::clear(POSITION_OPTIM_HASH)
  
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
  
  loadToPositionHash(fname=paste(ResultFiles_Path_G,"1Cb.csv",sep=""))
  loadToPositionHash(fname=paste(ResultFiles_Path_G,"2Cb.csv",sep=""))
  loadToPositionHash(fname=paste(ResultFiles_Path_G,"4Cb.csv",sep=""))
}

if(COMBINATION_HOT_START==F){
  #First Generation
  for(itr in 1:InitialPopCreateLoopNum){
    cat("itr:",itr,"\n")
    if(sum(EvalFuncSetting$Posnum==5)!=0){
      create_initial_exact_PutCall_polulation(popnum=250,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,
                                              putn=3,calln=2,ml=Optimize_ml,
                                              fname=paste(".\\ResultData\\inipop-05P3C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                              PosMultip,
                                              isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
      create_initial_exact_PutCall_polulation(popnum=250,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,
                                              putn=2,calln=3,ml=Optimize_ml,
                                              fname=paste(".\\ResultData\\inipop-05P2C3-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                              PosMultip,
                                              isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
    }
    if(sum(EvalFuncSetting$Posnum==4)!=0){
      create_initial_exact_PutCall_polulation(popnum=100,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,
                                              putn=4,calln=0,ml=Optimize_ml,
                                              fname=paste(".\\ResultData\\inipop-04P4C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                              PosMultip,
                                              isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
      create_initial_exact_PutCall_polulation(popnum=200,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,
                                              putn=2,calln=2,ml=Optimize_ml,
                                              fname=paste(".\\ResultData\\inipop-04P2C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                              PosMultip,
                                              isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
      create_initial_exact_PutCall_polulation(popnum=100,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,
                                              putn=0,calln=4,ml=Optimize_ml,
                                              fname=paste(".\\ResultData\\inipop-04P0C4-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                              PosMultip,
                                              isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
    }
    if(sum(EvalFuncSetting$Posnum==3)!=0){
      create_initial_exact_PutCall_polulation(popnum=100,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,
                                              putn=3,calln=0,ml=Optimize_ml,
                                              fname=paste(".\\ResultData\\inipop-03P3C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                              PosMultip,
                                              isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
      create_initial_exact_PutCall_polulation(popnum=100,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,
                                              putn=0,calln=3,ml=Optimize_ml,
                                              fname=paste(".\\ResultData\\inipop-03P0C3-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                              PosMultip,
                                              isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
    }
    if(sum(EvalFuncSetting$Posnum==2)!=0){
      create_initial_exact_PutCall_polulation(popnum=50,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,
                                              putn=2,calln=0,ml=Optimize_ml,
                                              fname=paste(".\\ResultData\\inipop-02P2C0-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                              PosMultip,
                                              isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
      create_initial_exact_PutCall_polulation(popnum=50,opchain$TYPE,EvalFuncSetting,thresh=InitialPopThresh,
                                              putn=0,calln=2,ml=Optimize_ml,
                                              fname=paste(".\\ResultData\\inipop-02P0C2-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                                              PosMultip,
                                              isFileout=TRUE,isDebug=FALSE,isDetail=FALSE)
    }
  }
  #Restore original setting
  if(SPECIFIC_FIRSTG_SETTING==T){
    EvalFuncSetting=origEvalFuncSetting
  }
  #1Cb.csv
  st <- "powershell.exe .\\shell\\cmd1.ps1"
  system(st)
  st <- "powershell.exe .\\shell\\cmd2.ps1"
  system(st)
  st <- "powershell.exe -Command \" del .\\ResultData\\1Cb-.csv \" "
  system(st) ;rm(st)
  
}

#combined population serach
if(Combined_Spread){
  #LossLimitPrice adjust
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*COMBINATION_LOSSLIMIT_Multipe
  
  ### 2(exact x exact) Combinations (2Cb)
  tmp<-read.table(paste(ResultFiles_Path_G,"1Cb.csv",sep=""),header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
  tmp=tmp[,1:(length(opchain$Position)+1)]
  colnames(tmp)=c(rep(1:length(opchain$Position)),"eval")
  tmp %>% dplyr::arrange(tmp[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp
  write.table(tmp,paste(ResultFiles_Path_G,"1Cb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% head(TopN_1) -> tmp
  
  ##
  # revalue the position for the value to be compatilbe  with following process
  if(SPECIFIC_FIRSTG_SETTING==T){
    file.copy(from=paste(ResultFiles_Path_G,"1Cb.csv",sep=""),to=paste(ResultFiles_Path_G,"1Cb_org.csv",sep=""),overwrite=T)
    tmp %>% dplyr::rowwise() %>%
      # x = unlist(.)[1:length(opchain$Position)]
      dplyr::do(revVal=obj_Income_sgmd(unlist(.)[1:length(opchain$Position)],
                                       EvalFuncSetting,isDebug=F,isDetail=F,
                                       udlStepNum=EvalFuncSetting$UdlStepNum,udlStepPct=EvalFuncSetting$UdlStepPct,
                                       maxposnum=EvalFuncSetting$Maxposnum,PosMultip=PosMultip,
                                       tail_rate=EvalFuncSetting$Tail_rate,lossLimitPrice=EvalFuncSetting$LossLimitPrice,
                                       Delta_Direct_Prf=EvalFuncSetting$Delta_Direct_Prf[sum(as.numeric((unlist(.)[1:length(opchain$Position)])!=0))],
                                       Vega_Direct_Prf=EvalFuncSetting$Vega_Direct_Prf[sum(as.numeric((unlist(.)[1:length(opchain$Position)])!=0))],
                                       Delta_Neutral_Offset=EvalFuncSetting$Delta_Neutral_Offset[sum(as.numeric((unlist(.)[1:length(opchain$Position)])!=0))],
                                       Vega_Neutral_Offset=EvalFuncSetting$Vega_Neutral_Offset[sum(as.numeric((unlist(.)[1:length(opchain$Position)])!=0))])
      ) ->tmp2
    tmp[,length(opchain$Position)+1]=unlist(tmp2)
    write.table(tmp,paste(ResultFiles_Path_G,"1Cb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
    tmp %>% dplyr::rowwise() %>%
      # x = unlist(.)[1:length(opchain$Position)]
      # revVal = unlist(.)[length(opchain$Position)+1]
      dplyr::do(md5sum=digest(paste(unlist(.)[1:length(opchain$Position)],collapse = "")),revVal=unlist(.)[length(opchain$Position)+1]
      ) -> tmp2
    #hash::clear(POSITION_OPTIM_HASH) not necessary
    POSITION_OPTIM_HASH[ unlist(tmp2$md5sum) ]<-unlist(tmp2$revVal)
  }
  
  ## or when all results are mixed together regardress of the number of Putn and Calln, pools[[1]] should be set as
  # c(1,0,0) <- c(1Cb{=exact}, Putn not spicified, Calln not spicified)
  pools<-list(list(c(1,0,0),tmp)) #No.[[1]]
  rm(tmp)
  #combinational search
  create_combined_population(popnum=PopN_1,EvalFuncSetting,thresh=Thresh_1,plelem=c(1,1),ml=Optimize_ml,
                             fname=paste(".\\ResultData\\combine-Result-1Cb+1Cb-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                             isFileout=TRUE,isDebug=FALSE,maxposn=length(EvalFuncSetting$Delta_Direct_Prf),PosMultip=PosMultip)
  #2Cb.csv
  if(COMBINATION_HOT_START==F){
    st <- "powershell.exe .\\shell\\cmd3.ps1"
    system(st)
    st <- "powershell.exe .\\shell\\cmd4.ps1"
    system(st)
    st <- "powershell.exe -Command \" del .\\ResultData\\2Cb-.csv \" "
    system(st) ;rm(st)
  }
  ### 4(2Cb x 2Cb) Combinations (4Cb)
  tmp<-read.table(paste(ResultFiles_Path_G,"2Cb.csv",sep=""),header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
  tmp=tmp[,1:(length(opchain$Position)+1)]
  colnames(tmp)=c(rep(1:length(opchain$Position)),"eval")
  tmp %>% dplyr::arrange(tmp[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp
  write.table(tmp,paste(ResultFiles_Path_G,"2Cb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
  tmp %>% dplyr::arrange(.[,length(iniPos)+1]) %>% head(TopN_2) -> tmp
    
  pools<-list(list(c(2,0,0),tmp)) #No.[[1]]
  rm(tmp)
  #combinational search
  create_combined_population(popnum=PopN_2,EvalFuncSetting,thresh=Thresh_2,plelem=c(1,1),ml=Optimize_ml,
                             fname=paste(".\\ResultData\\combine-Result-2Cb+2Cb-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                             isFileout=TRUE,isDebug=FALSE,maxposn=length(EvalFuncSetting$Delta_Direct_Prf),PosMultip=PosMultip)
  #4Cb.csv
  if(COMBINATION_HOT_START==F){
    st <- "powershell.exe .\\shell\\cmd7.ps1"
    system(st)
    st <- "powershell.exe .\\shell\\cmd8.ps1"
    system(st)
    st <- "powershell.exe -Command \" del .\\ResultData\\4Cb-.csv \" "
    system(st) ;rm(st)
  }
  
  tmp<-read.table(paste(ResultFiles_Path_G,"4Cb.csv",sep=""),header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
  tmp=tmp[,1:(length(opchain$Position)+1)]
  colnames(tmp)=c(rep(1:length(opchain$Position)),"eval")
  tmp %>% dplyr::arrange(tmp[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp
  write.table(tmp,paste(ResultFiles_Path_G,"4Cb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
  
  ### 3(exact x exact x exact) Combinations (3Cb)
  
  #LossLimitPrice's original value is returned
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
  
  rm(pools)
}

###
##
# Result Post Proceccing


#Threas Score
Thresh_Score1=as.numeric(ConfigParameters["ResultProcess_Thresh_Score1",1])
Thresh_Score2=as.numeric(ConfigParameters["ResultProcess_Thresh_Score2",1])
Thresh_Score3=as.numeric(ConfigParameters["ResultProcess_Thresh_Score3",1])

##
# Exact (1Cb)
res1<-read.table(paste(ResultFiles_Path_G,"1Cb.csv",sep=""),header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
res1=res1[,1:(length(opchain$Position)+1)]
colnames(res1)=c(rep(1:length(opchain$Position)),"eval")
res1 %>% dplyr::arrange(res1[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> res1
#over the specified socre
res1 %>% dplyr::filter(.[,length(opchain$Position)+1]<Thresh_1) -> res1
#posnum put call
res1[,1:length(opchain$Position)] %>% dplyr::rowwise() %>% dplyr::do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
tmp  %>% dplyr::rowwise() %>% dplyr::do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)
res1 -> total_res ; rm(res1)

if(Combined_Spread){
  ##
  # 2Cb
  res1<-read.table(paste(ResultFiles_Path_G,"2Cb.csv",sep=""),header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
  write.table(res1,paste(ResultFiles_Path_G,"2Cb.cmb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
  colnames(res1)=c(rep(1:length(opchain$Position)),"eval")
  res1 %>% dplyr::arrange(res1[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> res1
  #posnum put call
  res1[,1:length(opchain$Position)] %>% dplyr::rowwise() %>% dplyr::do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
  tmp  %>% dplyr::rowwise() %>% dplyr::do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
  res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)
  write.table(res1,paste(ResultFiles_Path_G,"2Cb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
  #over the specified socre
  res1 %>% dplyr::filter(.[,length(opchain$Position)+1]<Thresh_1) -> res1
  #factorと認識されたときの変換 #res1$V1<-as.numeric(as.character(res1$V1))
  
  #full join
  dplyr::full_join(total_res,res1) %>% dplyr::arrange(.[,length(iniPos)+1]) %>% dplyr::distinct() -> total_res
  rm(res1)
  
  ##
  #  4Cb
  res1<-read.table(paste(ResultFiles_Path_G,"4Cb.csv",sep=""),header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
  write.table(res1,paste(ResultFiles_Path_G,"4Cb.cmb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
  colnames(res1)=c(rep(1:length(opchain$Position)),"eval")
  res1 %>% dplyr::arrange(res1[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> res1
  #posnum put call
  res1[,1:length(opchain$Position)] %>% dplyr::rowwise() %>% dplyr::do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp
  tmp  %>% dplyr::rowwise() %>% dplyr::do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp2
  res1$putn<-unlist(tmp2$putn);res1$calln<-unlist(tmp2$calln);rm(tmp);rm(tmp2)
  write.table(res1,paste(ResultFiles_Path_G,"4Cb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
  #over the specified socre
  res1 %>% dplyr::filter(.[,length(opchain$Position)+1]<Thresh_21) -> res1
  
  #full join
  dplyr::full_join(total_res,res1) %>% dplyr::arrange(.[,length(iniPos)+1]) %>% dplyr::distinct() -> total_res
  rm(res1)
  
  ##
  #  3Cb
  
}

# Writing to files based on option legs total number
total_res %>% dplyr::mutate(posn=(putn+calln)) -> total_res
total_res %>%  dplyr::filter(posn==9 | posn==10) -> tmp_fil 
total_res %>%  dplyr::filter(posn==7 | posn==8) -> tmp_fil2
total_res %>%  dplyr::filter(posn==5 | posn==6) -> tmp_fil3
total_res %>%  dplyr::filter(posn<=4) -> tmp_fil4

#position wiht Greeks
getPositionWithGreeks<-function(tmp_fil){
  tmp_fil[,1:length(iniPos)] %>% dplyr::rowwise() %>% 
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
#dataw with greeks
#getPositionWithGreeks(tmp_fil) -> tmp_fil
#getPositionWithGreeks(tmp_fil2) -> tmp_fil2
#getPositionWithGreeks(tmp_fil3) -> tmp_fil3
#getPositionWithGreeks(tmp_fil4) -> tmp_fil4

##Filtering 
#read again and again
# Thresh_AdvEffect=as.numeric(ConfigParameters["ResultProcess_Thresh_AdvEffect",1])
# F_Thrsh_Params_Names<-c("Score1","Score2","Score3","Delta1_Minus","Delta2_Minus","Delta3_Minus",
#                         "Delta1_Plus","Delta2_Plus","Delta3_Plus","Thrsh_VegaE1","Thrsh_VegaE2","Thrsh_VegaE3",
#                         "F_TopN1","F_TopN2","F_TopN3")
# F_Thrsh_Params<- vector("list",length(F_Thrsh_Params_Names))
# F_Thrsh_Params[[1]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_Score1",1])
# F_Thrsh_Params[[2]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_Score2",1])
# F_Thrsh_Params[[3]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_Score3",1])
# F_Thrsh_Params[[4]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_Delta1_Minus",1])
# F_Thrsh_Params[[5]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_Delta2_Minus",1])
# F_Thrsh_Params[[6]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_Delta3_Minus",1])
# F_Thrsh_Params[[7]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_Delta1_Plus",1])
# F_Thrsh_Params[[8]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_Delta2_Plus",1])
# F_Thrsh_Params[[9]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_Delta3_Plus",1])
# F_Thrsh_Params[[10]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_VegaE1",1])
# F_Thrsh_Params[[11]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_VegaE2",1])
# F_Thrsh_Params[[12]]<-as.numeric(ConfigParameters["ResultProcess_F_Thrsh_VegaE3",1])
# F_Thrsh_Params[[13]]<-as.numeric(ConfigParameters["ResultProcess_F_TopN1",1])
# F_Thrsh_Params[[14]]<-as.numeric(ConfigParameters["ResultProcess_F_TopN2",1])
# F_Thrsh_Params[[15]]<-as.numeric(ConfigParameters["ResultProcess_F_TopN3",1])
# names(F_Thrsh_Params)<-F_Thrsh_Params_Names ; rm(F_Thrsh_Params_Names) 
# (Thresh_AdvEffect)
# (F_Thrsh_Params)

tmp_fil %>% 
  dplyr::arrange(tmp_fil[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp_fil_w

tmp_fil2 %>% 
    dplyr::arrange(tmp_fil2[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp_fil_w2

tmp_fil3 %>% 
    dplyr::arrange(tmp_fil3[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp_fil_w3

tmp_fil4 %>% 
  dplyr::arrange(tmp_fil4[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp_fil_w4

## Save to a file
write.table(tmp_fil_w,paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition_",
                            format(Sys.time(),"%Y%b%d_%H%M%S"),".csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
write.table(tmp_fil_w2,paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition2_",
                             format(Sys.time(),"%Y%b%d_%H%M%S"),".csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
write.table(tmp_fil_w3,paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition3_",
                             format(Sys.time(),"%Y%b%d_%H%M%S"),".csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
write.table(tmp_fil_w4,paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition4_"
                             ,format(Sys.time(),"%Y%b%d_%H%M%S"),".csv",sep=""),row.names = F,col.names=F,sep=",",append=F)


