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

#read file and pool setting
readFname=paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition.csv",sep='')
#readFname=paste(ResultFiles_Path_G,"1Cb.csv",sep='')
tmp<-read.table(readFname,header=F,skipNul=T,stringsAsFactors=F,sep=",")
tmp=tmp[,1:(length(opchain$Position)+4)]
colnames(tmp)=c(rep(1:length(opchain$Position)),"eval","putn","calln","posn")
tmp %>% arrange(.[,length(opchain$Position)+1]) %>%
  #filter(.[,length(opchain$Position)+1]<UNACCEPTABLEVAL) ->pos_populations
  dplyr::distinct(eval,.keep_all=TRUE) -> pos_populations

#Spreads that only have Put Legs
pos_populations %>% dplyr::filter(putn==posn) %>% 
  arrange(.[,length(opchain$Position)+1]) %>%
  dplyr::distinct(eval,.keep_all=TRUE) -> tmp2
write.table(tmp2,
            paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition_Put",format(Sys.time(),"%Y%b%d_%H%M%S"),".csv",sep=""),
            row.names = F,col.names=F,sep=",",append=F)
#Spreads that contain Call Legs
pos_populations %>% dplyr::filter(calln>=2) %>% 
  arrange(.[,length(opchain$Position)+1]) %>%
  dplyr::distinct(eval,.keep_all=TRUE) -> tmp2
write.table(tmp2,
            paste(ResultFiles_Path_G,Underying_Symbol_G,"-EvalPosition_Call",format(Sys.time(),"%Y%b%d_%H%M%S"),".csv",sep=""),
            row.names = F,col.names=F,sep=",",append=F)


