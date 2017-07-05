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

st <- "powershell.exe .\\shell\\cmd13.ps1"
system(st)
st <- "powershell.exe .\\shell\\cmd14.ps1"
system(st)
st <- "powershell.exe -Command \" del .\\ResultData\\Grdy-.csv \" "
system(st) ;rm(st)

tmp<-read.table(paste(ResultFiles_Path_G,"Grdy.csv",sep=""),header=F,skipNul=TRUE,stringsAsFactors=F,sep=",")
tmp=tmp[,1:(length(opchain$Position)+1)]
colnames(tmp)=c(rep(1:length(opchain$Position)),"eval")
tmp %>% dplyr::arrange(tmp[,(length(opchain$Position)+1)]) %>% dplyr::distinct(eval,.keep_all=TRUE) -> tmp

tmp[,1:length(opchain$Position)] %>% dplyr::rowwise() %>% dplyr::do(putcalln=getPutCallnOfthePosition(unlist(.))) -> tmp2
tmp2  %>% dplyr::rowwise() %>% dplyr::do(putn=(unlist(.)[1]),calln=(unlist(.)[2]))->tmp3
tmp$putn<-unlist(tmp3$putn);tmp$calln<-unlist(tmp3$calln);rm(tmp2);rm(tmp3)
tmp %>% dplyr::mutate(posn=(putn+calln)) -> tmp

#write table and file copy
fname=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition_Grdy",format(Sys.time(),"%Y%b%d_%H%M%S"),".csv",sep="")
write.table(tmp,fname,row.names = F,col.names=F,sep=",",append=F)
file.copy(from=fname, 
          to=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition.csv",sep=""),
          overwrite=T)
LocalflipScoreWriteToFile(fname,50)

#remove Greedy.csv
st <- "powershell.exe -Command \" del .\\ResultData\\Grdy.csv \" "
system(st) ;rm(st)
