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
COMBINATION_LOSSLIMIT_MULTIPLE=2

#cache for the position
HASH_HIT_NUM=0

#touch the file whose name shows the configuration of this search
file.create(paste(ResultFiles_Path_G,LocalcreateSampleConditionStr(EvalFuncSetting)))

##
# Creating First Generation

#If speceif configuration is to be applied to first generation. 

#The objective to change the configuration when creating a population of first generation is
#to try to make the expected return (profit) as big as possible.
#Remember if X and Y is independent, E(X+Y)=E(X)+E(Y). So trying to get a population
#whose member's E(X) is as big as possible would be reasonable.
#Risk side evaluation is not so simple. RisK(X+Y) != Risk(X)+Risk(Y), expectiing risk of
#X and Y could be canceled by combination.
#This is the reason we could exploit the combinational optimization approach.

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
  loadToPositionHash(fname=paste(ResultFiles_Path_G,"1Cb.csv",sep=""))
  
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
  ##
  #  LossLimitPrice adjust
  originalLossLimitPrice=EvalFuncSetting$LossLimitPrice
  EvalFuncSetting$LossLimitPrice=EvalFuncSetting$LossLimitPrice*COMBINATION_LOSSLIMIT_MULTIPLE
  
  ###
  ##   2(exact x exact) Combinations (2Cb)
  
  ##read 1Cb.csv, then making pools for combinational search
  tmp<-read.table(paste(ResultFiles_Path_G,"1Cb.csv",sep=""),header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
  tmp=tmp[,1:(length(opchain$Position)+1)]
  colnames(tmp)=c(rep(1:length(opchain$Position)),"eval")
  tmp %>% dplyr::arrange(tmp[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp
  write.table(tmp,paste(ResultFiles_Path_G,"1Cb.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
  tmp %>% dplyr::arrange(.[,length(opchain$Position)+1]) %>% head(TopN_1) -> tmp
  
  ## revalue the position for the value to be compatilbe with following process
  if(SPECIFIC_FIRSTG_SETTING==T){
    file.copy(from=paste(ResultFiles_Path_G,"1Cb.csv",sep=""),to=paste(ResultFiles_Path_G,"1Cb_org.csv",sep=""),overwrite=T)
    tmp %>% dplyr::rowwise() %>%
      # x = unlist(.)[1:length(opchain$Position)]
      dplyr::do(revVal=obj_Income_sgmd(unlist(.)[1:length(opchain$Position)],
                                       EvalFuncSetting,isDebug=F,isDetail=F,
                                       udlStepNum=EvalFuncSetting$UdlStepNum,udlStepPct=EvalFuncSetting$UdlStepPct,
                                       PosMultip=PosMultip,
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
  
  # making pools 
  #   when all results are mixed together regardress of the number of Putn and Calln, pools[[1]] should be set as
  #   c(1,0,0) <- c(1Cb{=exact}, Putn not spicified, Calln not spicified)
  pools<-list(list(c(1,0,0),tmp)) #No.[[1]]
  rm(tmp)
  #combinational search
  create_combined_population(popnum=PopN_1,EvalFuncSetting,thresh=Thresh_1,plelem=c(1,1),ml=Optimize_ml,
                             fname=paste(".\\ResultData\\combine-Result-1Cb+1Cb-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                             isFileout=TRUE,isDebug=FALSE,maxposn=length(EvalFuncSetting$Vega_Direct_Prf),PosMultip=PosMultip)
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

  ###
  ##   3(2Cb x 1Cb) Combinations (3Cb)
  
  #adding pools' element
  tmp %>% dplyr::arrange(.[,length(iniPos)+1]) %>% head(TopN_2) -> tmp
  pools[2]<-list(list(c(1,0,0),tmp)) #No.[[2]]
  pools<<-pools
  #combinational search
  create_combined_population(popnum=PopN_2,EvalFuncSetting,thresh=Thresh_2,plelem=c(1,2),ml=Optimize_ml,
                             fname=paste(".\\ResultData\\combine-Result-1Cb+1Cb+1Cb-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                             isFileout=TRUE,isDebug=FALSE,maxposn=10,PosMultip=PosMultip)
  
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

  ###
  ##  4(3Cb x 1Cb currently shown as 2Cb+2Cb) Combinations (4Cb)
  
  #adding pools' element
  tmp %>% dplyr::arrange(.[,length(iniPos)+1]) %>% head(TopN_2) -> tmp
  pools[3]<-list(list(c(1,0,0),tmp)) #No.[[3]]
  pools<<-pools
  #combinational search
  create_combined_population(popnum=PopN_2,EvalFuncSetting,thresh=Thresh_2,plelem=c(1,3),ml=Optimize_ml,
                             fname=paste(".\\ResultData\\combine-Result-2Cb+2Cb-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                             isFileout=TRUE,isDebug=FALSE,maxposn=length(EvalFuncSetting$Vega_Direct_Prf),PosMultip=PosMultip)
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
  
  ##
  # LossLimitPrice's original value is returned
  EvalFuncSetting$LossLimitPrice=originalLossLimitPrice
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
  res1 %>% dplyr::filter(.[,length(opchain$Position)+1]<Thresh_1) -> res1
  #factorと認識されたときの変換 #res1$V1<-as.numeric(as.character(res1$V1))
  
  #full join
  dplyr::full_join(total_res,res1) %>% dplyr::arrange(.[,length(iniPos)+1]) %>% dplyr::distinct() -> total_res
  rm(res1)
  
  ##
  # 3Cb
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
  res1 %>% dplyr::filter(.[,length(opchain$Position)+1]<Thresh_2) -> res1
  #factorと認識されたときの変換 #res1$V1<-as.numeric(as.character(res1$V1))
  
  #full join
  dplyr::full_join(total_res,res1) %>% dplyr::arrange(.[,length(iniPos)+1]) %>% dplyr::distinct() -> total_res
  rm(res1)
  
  ##
  #  4Cb
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
  res1 %>% dplyr::filter(.[,length(opchain$Position)+1]<Thresh_2) -> res1
  
  #full join
  dplyr::full_join(total_res,res1) %>% dplyr::arrange(.[,length(iniPos)+1]) %>% dplyr::distinct() -> total_res
  rm(res1)
}

# Writing to files based on option legs total number
total_res %>% dplyr::mutate(posn=(putn+calln)) -> total_res
total_res %>%  dplyr::filter(posn==11 | posn==12) -> tmp_fil 
total_res %>%  dplyr::filter(posn==9 | posn==10) -> tmp_fil2
total_res %>%  dplyr::filter(posn==7 | posn==8) -> tmp_fil3
total_res %>%  dplyr::filter(posn<=6) -> tmp_fil4

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
#data for writing to files.
tmp_fil %>% 
  dplyr::arrange(tmp_fil[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp_fil_w

tmp_fil2 %>% 
    dplyr::arrange(tmp_fil2[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp_fil_w2

tmp_fil3 %>% 
    dplyr::arrange(tmp_fil3[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp_fil_w3

tmp_fil4 %>% 
  dplyr::arrange(tmp_fil4[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp_fil_w4


#MERGE
if(COMBINATION_HOT_START==T){
  LocalMergeWriteFiles(rf=paste(ResultFiles_Path_G,"2Cb.csv",sep=""))
  LocalMergeWriteFiles(rf=paste(ResultFiles_Path_G,"3Cb.csv",sep=""))
  LocalMergeWriteFiles(rf=paste(ResultFiles_Path_G,"4Cb.csv",sep=""))
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

write.table(tmp_fil_w,resultSaveFileRf[[1]],row.names = F,col.names=F,sep=",",append=F)
write.table(tmp_fil_w2,resultSaveFileRf[[2]],row.names = F,col.names=F,sep=",",append=F)
write.table(tmp_fil_w3,resultSaveFileRf[[3]],row.names = F,col.names=F,sep=",",append=F)
write.table(tmp_fil_w4,resultSaveFileRf[[4]],row.names = F,col.names=F,sep=",",append=F)

for(tmp_file_idx in 1:2){
  LocalflipScoreWriteToFile(resultSaveFileRf[[tmp_file_idx]],50)
}
