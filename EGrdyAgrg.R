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

##
## create UDLY_OPChain_PreForSkew, UDLY_OPChain_PreForPos

dates=as.Date(Sys.time())-c(0:4)
fnames=paste(DataFiles_Path_G,Underying_Symbol_G,"OPT",dates,".csv",sep="")
fnames=fnames[file.exists(fnames)]
fnames

total_table=NULL
for(i in 1:length(fnames)){
  tmp<-read.table(fnames[i],header=T,skipNul=TRUE,stringsAsFactors=F,sep=",")
  print(tmp)
  total_table %>% dplyr::bind_rows(tmp) -> total_table
}

#Pre for Skew
total_table %>% 
  dplyr::arrange(desc(as.Date(Date,format="%Y/%m/%d")),as.Date(ExpDate,format="%Y/%m/%d"),TYPE,Strike) %>%
  dplyr::distinct() %>%
  dplyr::filter(Strike != "Strike") -> preForSkew
head(preForSkew,n=50)
tail(preForSkew,n=50)
#write to a file
write.table(preForSkew,paste(DataFiles_Path_G,Underying_Symbol_G,"_OPChain_PreForSkew.csv",sep=""),row.names = F,col.names=T,sep=",",append=F)

#Pre for Pos
last_day_distance=1
preForSkew %>% 
  dplyr::filter(as.Date(Date,format="%Y/%m/%d") == (as.Date(Sys.time())-last_day_distance))  %>% 
  dplyr::distinct() -> preForPos
head(preForPos,n=50)
tail(preForPos,n=50)
#write to a file
write.table(preForPos,paste(DataFiles_Path_G,Underying_Symbol_G,"_OPChain_PreForPos.csv",sep=""),row.names = F,col.names=T,sep=",",append=F)


##
## aggregate UDLY_EvalPosition_Grdyxxyymm.csv to make the best result.


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
tmp$putn<-unlist(tmp3$putn);tmp$calln<-unlist(tmp3$calln);rm(tmれ2);rm(tmp3)
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
