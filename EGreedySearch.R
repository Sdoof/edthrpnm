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

#Debug, Detail Mode
isDebug=F
isDetail=F

#iteration and eliete pop
ELITE_POP_NUM=100
GENERATION_ITR=10

#cache for the position
HASH_HIT_NUM=0

#read file and pool setting
readFname=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition.csv",sep='')
tmp<-read.table(readFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
tmp=tmp[,1:(length(opchain$Position)+1)]
colnames(tmp)=c(rep(1:length(opchain$Position)),"eval")
tmp %>% arrange(.[,length(opchain$Position)+1]) %>%
  #filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) ->pos_populations
  dplyr::distinct(eval,.keep_all=TRUE) -> pos_populations

## clear hash to improve performance
hash::clear(POSITION_OPTIM_HASH)
##create initail POSITION_OPTIM_HASH
pos_populations %>% dplyr::rowwise() %>%
  # x = unlist(.)[1:length(opchain$Position)]
  # revVal = unlist(.)[length(opchain$Position)+1]
  dplyr::do(md5sum=digest(paste(unlist(.)[1:length(opchain$Position)],collapse = "")),revVal=unlist(.)[length(opchain$Position)+1]
  ) -> tmp2
POSITION_OPTIM_HASH[ unlist(tmp2$md5sum) ]<-unlist(tmp2$revVal)
cat("hash hit:",HASH_HIT_NUM,"hash num:",length(POSITION_OPTIM_HASH),"\n")
#ELITE_POP_NUM must be equal or below nrow(pos_populations)
ELITE_POP_NUM=min(ELITE_POP_NUM,nrow(pos_populations))

start_t<-proc.time()
for(itr in 1:GENERATION_ITR){
  print(pos_populations)
  start_t<-proc.time()
  for(eval_pos_idx in 1:ELITE_POP_NUM){
    evaPos<-unlist(pos_populations[eval_pos_idx,])[1:length(opchain$Position)]
    theScore=unlist(pos_populations[eval_pos_idx,][length(opchain$Position)+1])
    
    #Put Position
    idxyPut<-as.numeric(opchain$TYPE==OpType_Put_G)*rep(1:length(opchain$Position),length=length(opchain$Position))
    putOnPos=(idxyPut!=0)*evaPos
    if(isDebug){cat("put pos\n",idxyPut,"\n");print(putOnPos) }
    
    #Call Position
    idxyCall<-as.numeric(opchain$TYPE==OpType_Call_G)*rep(1:length(opchain$Position),length=length(opchain$Position))
    callOnPos=(idxyCall!=0)*evaPos
    if(isDebug){cat("call pos\n",idxyCall,"\n");print(callOnPos) }
    
    #get Neiigbour
    #Put
    (elemType=which(putOnPos!=0))
    typeOnPos=putOnPos
    idxyType=idxyPut
    if(length(elemType)>0){
      for(elems in 1:length(elemType)){
        for(d_pos in c(-1,1)){
          if(elemType[elems]+d_pos<1 || elemType[elems]+d_pos>length(evaPos))
            next
          neighborElem=(idxyType[elemType[elems]+d_pos]!=0)*idxyType[elemType[elems]+d_pos]
          #print(neighborElem)
          if(neighborElem!=0){
            typeOnPosNeighbor=typeOnPos
            typeOnPosNeighbor[elemType[elems]]=typeOnPos[elemType[elems]]-typeOnPos[elemType[elems]]
            typeOnPosNeighbor[neighborElem]=typeOnPos[neighborElem]+typeOnPos[elemType[elems]]
            evalx=evaPos-typeOnPos+typeOnPosNeighbor
            if(isDebug){print(typeOnPos);print(typeOnPosNeighbor);print(evalx) }
            #Evaluations
            LocalapplyEvalufunction(evalx=evalx,
                               thresh=UNACCEPTABLEVAL, EvalFuncSetting=EvalFuncSetting,
                               isDebug=isDebug, isDetail=isDetail,isFileout=T)
            
          }
        }
      }
    }
    #Call
    (elemType=which(callOnPos!=0))
    typeOnPos=callOnPos
    idxyType=idxyCall
    if(length(elemType)>0){
      for(elems in 1:length(elemType)){
        for(d_pos in c(-1,1)){
          if(elemType[elems]+d_pos<1 || elemType[elems]+d_pos>length(evaPos))
            next
          neighborElem=(idxyType[elemType[elems]+d_pos]!=0)*idxyType[elemType[elems]+d_pos]
          #print(neighborElem)
          if(neighborElem!=0){
            typeOnPosNeighbor=typeOnPos
            typeOnPosNeighbor[elemType[elems]]=typeOnPos[elemType[elems]]-typeOnPos[elemType[elems]]
            typeOnPosNeighbor[neighborElem]=typeOnPos[neighborElem]+typeOnPos[elemType[elems]]
            evalx=evaPos-typeOnPos+typeOnPosNeighbor
            if(isDebug){print(typeOnPos);print(typeOnPosNeighbor);print(evalx) }
            #Evaluations
            LocalapplyEvalufunction(evalx=evalx,
                               thresh=UNACCEPTABLEVAL, EvalFuncSetting=EvalFuncSetting,
                               isDebug=isDebug, isDetail=isDetail,isFileout=T)
          }
        }
      }
    }
  }
  #Hash
  cat("hash hit:",HASH_HIT_NUM,"hash num:",length(POSITION_OPTIM_HASH),"itr:",itr,"time:",(proc.time()-start_t)[3],"\n")
  
  #Greedy.csv
  st <- "powershell.exe .\\shell\\cmd11.ps1"
  system(st)
  st <- "powershell.exe .\\shell\\cmd12.ps1"
  system(st)
  st <- "powershell.exe -Command \" del .\\ResultData\\greedy-.csv \" "
  system(st) ;rm(st)
  
  tmp<-read.table(paste(ResultFiles_Path_G,"Greedy.csv",sep=""),header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
  tmp=tmp[,1:(length(opchain$Position)+1)]
  colnames(tmp)=c(rep(1:length(opchain$Position)),"eval")
  tmp %>% dplyr::arrange(tmp[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> pos_populations
  #write to the inigreedy file
  write.table(pos_populations,paste(".\\ResultData\\inigreedy.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
  #remove Greedy.csv
  st <- "powershell.exe -Command \" del .\\ResultData\\Greedy.csv \" "
  system(st) ;rm(st)
}

#Writing to a result file
tmp_greedy<-read.table(paste(".\\ResultData\\inigreedy.csv",sep=""),header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
tmp_greedy=tmp_greedy[,1:(length(opchain$Position)+1)]
colnames(tmp_greedy)=c(rep(1:length(opchain$Position)),"eval")
tmp_greedy=tmp_greedy[,1:(length(opchain$Position)+1)]

readFname=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition.csv",sep='')
tmp_orig<-read.table(readFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
colnames(tmp_orig)<-c(rep(1:length(opchain$Position)),"eval")
tmp_orig = tmp_orig[,1:(length(opchain$Position)+1)]

tmp_greedy %>% dplyr::bind_rows(tmp_orig) -> tmp
tmp %>% dplyr::arrange(tmp[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp

tmp[,1:length(opchain$Position)] %>% dplyr::rowwise() %>% dplyr::do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp2
tmp2  %>% dplyr::rowwise() %>% dplyr::do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp3
tmp$putn<-unlist(tmp3$putn);tmp$calln<-unlist(tmp3$calln);rm(tmp2);rm(tmp3)
tmp %>% dplyr::mutate(posn=(putn+calln)) -> tmp

#write table and file copy
fname=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition_Greedy",format(Sys.time(),"%Y%b%d_%H%M%S"),".csv",sep="")
write.table(tmp,fname,row.names = F,col.names=F,sep=",",append=F)
#file.copy(from=fname, 
#          to=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition.csv",sep=""),
#          overwrite=T)
#LocalflipScoreWriteToFile(fname,50)
