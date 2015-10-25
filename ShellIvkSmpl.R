##
# Combine  EvalCnd and make uniq UDLY_EvalPosition
library(RQuantLib)
library(ggplot2)
library(dplyr)
rm(list=ls())
source('./ESourceRCode.R',encoding = 'UTF-8')

#Config File
ConfigFileName_G="ConfigParameters.csv"
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"

ConfigParameters<-read.table(paste(DataFiles_Path_G,ConfigFileName_G,sep=""),
                             row.names=1, comment.char="#",header=T,stringsAsFactors=F,sep=",")

#Definition
OpType_Put_G=as.numeric(ConfigParameters["OpType_Put_G",1])
OpType_Call_G=as.numeric(ConfigParameters["OpType_Call_G",1])

#File
Underying_Symbol_G=ConfigParameters["Underying_Symbol_G",1]
ResultFiles_Path_G=ConfigParameters["ResultFiles_Path_G",1]

#combine
#edthrpnm
if(length(grep("2", ConfigFileName_G))<1 && length(grep("3", ConfigFileName_G))<1 ){
  st <- "powershell.exe -Command \" Get-ChildItem ..\\edthrpnm2\\ResultData\\EvalCnd.csv | cat >  .\\ResultData\\EvalCnd-.csv  -Encoding default \" "
  system(st) ;rm(st)
  st <- paste("powershell.exe -Command \" Get-Content  .\\ResultData\\EvalCnd-.csv | Out-File -Filepath .\\ResultData\\EvalCnd-1.csv -Encoding default  \"")
  
  system(st) ;rm(st)
  st <- "powershell.exe -Command \" Get-ChildItem ..\\edthrpnm3\\ResultData\\EvalCnd.csv | cat > .\\ResultData\\EvalCnd-.csv  -Encoding default \" "
  system(st) ;rm(st)
  st <- paste("powershell.exe -Command \" Get-Content  .\\ResultData\\EvalCnd-.csv | Out-File -Filepath .\\ResultData\\EvalCnd-2.csv -Encoding default  \"")
  #st <- "powershell.exe -Command \" Get-ChildItem ..\\edthrpnm3\\ResultData\\EvalCnd.csv |  Out-File -Filepath  .\\ResultData\\EvalCnd--.csv \" "
  system(st) ;rm(st)
 
  theCand<-"EvalCnd.csv"
  
}
#edthrpnm2
if(length(grep("2", ConfigFileName_G))>=1) {
  
  st <- "powershell.exe -Command \" Get-ChildItem ..\\edthrpnm\\ResultData\\EvalCnd2.csv | cat >  .\\ResultData\\EvalCnd-.csv  -Encoding default \" "
  system(st) ;rm(st)
  st <- paste("powershell.exe -Command \" Get-Content  .\\ResultData\\EvalCnd-.csv | Out-File -Filepath .\\ResultData\\EvalCnd-1.csv -Encoding default  \"")
  
  system(st) ;rm(st)
  st <- "powershell.exe -Command \" Get-ChildItem ..\\edthrpnm3\\ResultData\\EvalCnd2.csv | cat > .\\ResultData\\EvalCnd-.csv  -Encoding default \" "
  system(st) ;rm(st)
  
  st <- paste("powershell.exe -Command \" Get-Content  .\\ResultData\\EvalCnd-.csv | Out-File -Filepath .\\ResultData\\EvalCnd-2.csv -Encoding default  \"")
  #st <- "powershell.exe -Command \" Get-ChildItem ..\\edthrpnm3\\ResultData\\EvalCnd.csv |  Out-File -Filepath  .\\ResultData\\EvalCnd--.csv \" "
  system(st) ;rm(st)
  
  theCand<-"EvalCnd2.csv"

}

#edthrpnm3
if(length(grep("3", ConfigFileName_G))>=1){
  
  st <- "powershell.exe -Command \" Get-ChildItem ..\\edthrpnm\\ResultData\\EvalCnd3.csv | cat >  .\\ResultData\\EvalCnd-.csv  -Encoding default \" "
  system(st) ;rm(st)
  st <- paste("powershell.exe -Command \" Get-Content  .\\ResultData\\EvalCnd-.csv | Out-File -Filepath .\\ResultData\\EvalCnd-1.csv -Encoding default  \"")
  
  system(st) ;rm(st)
  st <- "powershell.exe -Command \" Get-ChildItem ..\\edthrpnm2\\ResultData\\EvalCnd3.csv | cat > .\\ResultData\\EvalCnd-.csv  -Encoding default \" "
  system(st) ;rm(st)
  st <- paste("powershell.exe -Command \" Get-Content  .\\ResultData\\EvalCnd-.csv | Out-File -Filepath .\\ResultData\\EvalCnd-2.csv -Encoding default  \"")
  #st <- "powershell.exe -Command \" Get-ChildItem ..\\edthrpnm3\\ResultData\\EvalCnd.csv |  Out-File -Filepath  .\\ResultData\\EvalCnd--.csv \" "
  system(st) ;rm(st)
  
  theCand<-"EvalCnd3.csv"
}

#Option Chain 
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre.csv",sep="")
opchain<-read.table(rf,header=T,sep=",",stringsAsFactors=F)

#join
rf<-paste(ResultFiles_Path_G,theCand,sep="")
evalPositions<-read.table(rf,header=T,sep=",",colClasses="numeric") ;rm(rf)
length(opchain$Position)
evalPositions %>% arrange(evalPositions[,(length(opchain$Position)+1)]) %>% distinct() -> evalPositions
#NAを含む行を削除する
na.omit(evalPositions) -> evalPositions

rf<-paste(ResultFiles_Path_G,"EvalCnd-1.csv",sep="")
tmppos<-read.table(rf,header=T,sep=",",colClasses="numeric") ;rm(rf)
length(opchain$Position)
tmppos %>% arrange(tmppos[,(length(opchain$Position)+1)]) %>% distinct() -> tmppos
#NAを含む行を削除する
na.omit(tmppos) -> tmppos

evalPositions %>% full_join(tmppos) -> evalPositions
evalPositions %>% arrange(evalPositions[,(length(opchain$Position)+1)]) %>% distinct() -> evalPositions

rf<-paste(ResultFiles_Path_G,"EvalCnd-2.csv",sep="")
tmppos<-read.table(rf,header=T,sep=",",colClasses="numeric") ;rm(rf)
length(opchain$Position)
tmppos %>% arrange(tmppos[,(length(opchain$Position)+1)]) %>% distinct() -> tmppos
#NAを含む行を削除する
na.omit(tmppos) -> tmppos

evalPositions %>% full_join(tmppos) -> evalPositions
evalPositions %>% arrange(evalPositions[,(length(opchain$Position)+1)]) %>% distinct() -> evalPositions

st <- "powershell.exe -Command \" Remove-Item .\\ResultData\\EvalCnd-.csv  \""
system(st) ;rm(st)
st <- "powershell.exe -Command \" Remove-Item .\\ResultData\\EvalCnd-1.csv  \""
system(st) ;rm(st)
st <- "powershell.exe -Command \" Remove-Item .\\ResultData\\EvalCnd-2.csv  \""
system(st) ;rm(st)

#Separate into two groups. Theta > 0 and Theta<=0(Gamma positive)
evalPositions %>% filter(ThetaEffect>0) %>% distinct() -> evalPositions_Theta
rownames(evalPositions_Theta) <- c(1:nrow(evalPositions_Theta))
evalPositions %>% filter(ThetaEffect<=0) %>% distinct() -> evalPositions_Gamma
rownames(evalPositions_Gamma) <- c(1:nrow(evalPositions_Gamma))

#Write EvalPosition files
write.table(evalPositions,paste(ResultFiles_Path_G,Underying_Symbol_G,"_EvalPosition.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
write.table(evalPositions_Theta,paste(ResultFiles_Path_G,Underying_Symbol_G,"_EvalPosition_Theta.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)
write.table(evalPositions_Gamma,paste(ResultFiles_Path_G,Underying_Symbol_G,"_EvalPosition_Gamma.csv",sep=""),row.names = F,col.names=F,sep=",",append=F)

#Write APITicket files
targetPositions=evalPositions
fname=paste(ResultFiles_Path_G,Underying_Symbol_G,"_APITicket.txt",sep="")
for(Counter in 1:nrow(targetPositions)){
  opchain$Position = unlist(targetPositions[Counter,])[1:length(opchain$Position)]
  opchain %>% dplyr::filter(Position!=0) -> thePosition
  cat("#Ticket",Counter,file=fname,append=T,"\n")
  cat(opchain$Position,file=fname,sep=",","\n")
  writeIbAPITicket(out_text_file=fname,thePosition=thePosition,sep=",")
  thePosition %>% mutate(TYPE=ifelse(TYPE==OpType_Put_G,"Put","Call")) -> thePosition
  sink(fname, append = T)
  print(thePosition)
  sink()
}

targetPositions=evalPositions_Theta
fname=paste(ResultFiles_Path_G,Underying_Symbol_G,"_APITicket_Theta.txt",sep="")
for(Counter in 1:nrow(targetPositions)){
  opchain$Position = unlist(targetPositions[Counter,])[1:length(opchain$Position)]
  opchain %>% dplyr::filter(Position!=0) -> thePosition
  cat("#Ticket",Counter,file=fname,append=T,"\n")
  cat(opchain$Position,file=fname,sep=",","\n")
  writeIbAPITicket(out_text_file=fname,thePosition=thePosition,sep=",")
  thePosition %>% mutate(TYPE=ifelse(TYPE==OpType_Put_G,"Put","Call")) -> thePosition
  sink(fname, append = T)
  print(thePosition)
  sink()
}

targetPositions=evalPositions_Gamma
fname=paste(ResultFiles_Path_G,Underying_Symbol_G,"_APITicket_Gamma.txt",sep="")
for(Counter in 1:nrow(targetPositions)){
  opchain$Position = unlist(targetPositions[Counter,])[1:length(opchain$Position)]
  opchain %>% dplyr::filter(Position!=0) -> thePosition
  cat("#Ticket",Counter,file=fname,append=T,"\n")
  cat(opchain$Position,file=fname,sep=",","\n")
  writeIbAPITicket(out_text_file=fname,thePosition=thePosition,sep=",")
  thePosition %>% mutate(TYPE=ifelse(TYPE==OpType_Put_G,"Put","Call")) -> thePosition
  sink(fname, append = T)
  print(thePosition)
  sink()
}

rm(list=ls())
