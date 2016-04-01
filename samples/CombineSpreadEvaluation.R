#Load library and Configfile Setting in EOptimize.R beforehand

combineSpreads<-function(spreadRatio,headN_1st,headN_2nd){
  tmp<-read.table(paste(ResultFiles_Path_G,Underying_Symbol_G,"_EvalPosition_1St.csv",sep=""),
                  header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(tmp[,(length(opchain$Position)+1)]) %>% dplyr::distinct() -> tmp
  tmp %>% arrange(.[,length(opchain$Position)+1]) %>% head(headN_1st) -> tmp
  tmp <- tmp*spreadRatio[1]
  ## or when all results are mixed together regardress of the number of Putn and Calln, pools[[1]] should be set as
  # c(1,0,0) <- c(1Cb{=exact}, Putn not spicified, Calln not spicified)
  # Notice pools is global variable
  pools<<-list(list(c(1,0,0),tmp)) #No.[[1]]
  
  tmp<-read.table(paste(ResultFiles_Path_G,Underying_Symbol_G,"_EvalPosition_2nd.csv",sep=""),
                  header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
  tmp %>% dplyr::arrange(tmp[,(length(opchain$Position)+1)]) %>% dplyr::distinct() -> tmp
  tmp %>% arrange(.[,length(opchain$Position)+1]) %>% head(headN_2nd) -> tmp
  tmp <- tmp*spreadRatio[2]
  ## or when all results are mixed together regardress of the number of Putn and Calln, pools[[1]] should be set as
  # c(1,0,0) <- c(1Cb{=exact}, Putn not spicified, Calln not spicified)
  pools[2]<<-list(list(c(1,0,0),tmp)) #No.[[2]]
  
  create_combined_population(popnum=30000,EvalFuncSetting,thresh=1.5,plelem=c(1,2),ml=Optimize_ml,fname=paste(".\\ResultData\\MixedResult-",format(Sys.time(),"%Y-%b-%d-%H%M%S"),".csv",sep=""),
                             isFileout=TRUE,isDebug=F,maxposn=length(EvalFuncSetting$Delta_Direct_Prf),PosMultip=PosMultip)
}
#Specify spreadRatio
spreadRatio=c(1,1)
combineSpreads(spreadRatio=spreadRatio,headN_1st=400,headN_2nd=1000)

spreadRatio=c(2,1)
combineSpreads(spreadRatio=spreadRatio,headN_1st=400,headN_2nd=1000)

