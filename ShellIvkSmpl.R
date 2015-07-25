##
# Combine  EvalCnd and make uniq UDLY_EvalPosition

library(RQuantLib)
library(ggplot2)
library(dplyr)

#Config File
ConfigFileName_G="ConfigParameters.csv"
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"

ConfigParameters<-read.table(paste(DataFiles_Path_G,ConfigFileName_G,sep=""),
                             row.names=1, comment.char="#",header=T,stringsAsFactors=F,sep=",")

#File
Underying_Symbol_G=ConfigParameters["Underying_Symbol_G",1]
ResultFiles_Path_G=ConfigParameters["ResultFiles_Path_G",1]

#combine
#edthrpnm
if(length(grep("2", ConfigFileName_G))<1 && length(grep("3", ConfigFileName_G))<1 ){
  st <- "powershell.exe -Command \" Get-ChildItem ..\\edthrpnm2\\ResultData\\EvalCnd.csv | cat >> .\\ResultData\\EvalCnd.csv \" "
  system(st) ;rm(st)
  st <- "powershell.exe -Command \" Get-ChildItem ..\\edthrpnm3\\ResultData\\EvalCnd.csv | cat >> .\\ResultData\\EvalCnd.csv \" "
  system(st) ;rm(st)
  st <- paste("powershell.exe -Command \" Get-Content  .\\ResultData\\EvalCnd.csv | Out-File -Filepath ",
              paste(ResultFiles_Path_G,Underying_Symbol_G,"_EvalPosition.csv",sep="")," -Encoding default  \"")
  system(st) ;rm(st)
}
#edthrpnm2
if(length(grep("2", ConfigFileName_G))>=1) {
  st <- "powershell.exe -Command \" Get-ChildItem ..\\edthrpnm\\ResultData\\EvalCnd2.csv | cat >> .\\ResultData\\EvalCnd2.csv \" "
  system(st) ;rm(st)
  st <- "powershell.exe -Command \" Get-ChildItem ..\\edthrpnm3\\ResultData\\EvalCnd2.csv | cat >> .\\ResultData\\EvalCnd2.csv \" "
  system(st) ;rm(st)
  st <- paste("powershell.exe -Command \" Get-Content  .\\ResultData\\EvalCnd2.csv | Out-File -Filepath ",
              paste(ResultFiles_Path_G,Underying_Symbol_G,"_EvalPosition.csv",sep="")," -Encoding default  \"")
  system(st) ;rm(st)
}

#edthrpnm3
if(length(grep("3", ConfigFileName_G))>=1){
  cat("aaa")
  st <- "powershell.exe -Command \" Get-ChildItem ..\\edthrpnm\\ResultData\\EvalCnd3.csv | cat >> .\\ResultData\\EvalCnd3.csv \" "
  system(st) ;rm(st)
  st <- "powershell.exe -Command \" Get-ChildItem ..\\edthrpnm2\\ResultData\\EvalCnd3.csv | cat >> .\\ResultData\\EvalCnd3.csv \" "
  system(st) ;rm(st)
  st <- paste("powershell.exe -Command \" Get-Content  .\\ResultData\\EvalCnd3.csv | Out-File -Filepath ",
              paste(ResultFiles_Path_G,Underying_Symbol_G,"_EvalPosition.csv",sep="")," -Encoding default  \"")
  system(st) ;rm(st)
}

#Option Chain 
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre.csv",sep="")
opchain<-read.table(rf,header=T,sep=",",stringsAsFactors=FALSE)

#EvalPosition
rf<-paste(ResultFiles_Path_G,Underying_Symbol_G,"_EvalPosition.csv",sep="")
evalPositions<-read.table(rf,header=F,sep=",") ;rm(rf)
length(opchain$Position)
evalPositions %>% distinct() -> evalPositions
#NAを含む行を削除する
na.omit(evalPositions) -> evalPositions
write.table(evalPositions,paste(ResultFiles_Path_G,Underying_Symbol_G,"_EvalPosition.csv",sep=""),row.names = FALSE,col.names=FALSE,sep=",",append=F)

rm(opchain,evalPositions)
rm(ConfigFileName_G,DataFiles_Path_G,ConfigParameters,Underying_Symbol_G,ResultFiles_Path_G)