#Load library and Configfile Setting in EOptimize.R beforehand

tmp<-read.table(paste(ResultFiles_Path_G,Underying_Symbol_G,"_EvalPosition_1St.csv",sep=""),
                header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
tmp %>% dplyr::arrange(tmp[,(length(opchain$Position)+1)]) %>% dplyr::distinct() -> tmp
tmp %>% arrange(.[,length(opchain$Position)+1]) %>% head(400) -> tmp
## or when all results are mixed together regardress of the number of Putn and Calln, pools[[1]] should be set as
# c(1,0,0) <- c(1Cb{=exact}, Putn not spicified, Calln not spicified)
pools<-list(list(c(1,0,0),tmp)) #No.[[1]]

tmp<-read.table(paste(ResultFiles_Path_G,Underying_Symbol_G,"_EvalPosition_2nd.csv",sep=""),
                header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
tmp %>% dplyr::arrange(tmp[,(length(opchain$Position)+1)]) %>% dplyr::distinct() -> tmp
tmp %>% arrange(.[,length(opchain$Position)+1]) %>% head(400) -> tmp
## or when all results are mixed together regardress of the number of Putn and Calln, pools[[1]] should be set as
# c(1,0,0) <- c(1Cb{=exact}, Putn not spicified, Calln not spicified)
pools[2]<-list(list(c(1,0,0),tmp)) #No.[[2]]

create_combined_population(popnum=30000,EvalFuncSetting,thresh=1.5,plelem=c(1,2),ml=Optimize_ml,fname=paste(".\\ResultData\\MixedResult-",format(Sys.time(),"%Y-%b-%d"),".csv",sep=""),
                           isFileout=TRUE,isDebug=F,maxposn=length(EvalFuncSetting$Delta_Direct_Prf),PosMultip=PosMultip)
