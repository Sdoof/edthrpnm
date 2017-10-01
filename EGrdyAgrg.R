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

#####
## create UDLY_OPChain_PreForSkew, UDLY_OPChain_PreForPos

dates=as.Date(Sys.time())-c(0:100)
fnames=paste(DataFiles_Path_G,Underying_Symbol_G,"OPT",dates,".csv",sep="")
fnames=fnames[file.exists(fnames)]
fnames

total_table=NULL
for(i in 1:length(fnames)){
  tmp<-read.table(fnames[i],header=T,skipNul=T,stringsAsFactors=F,sep=",")
  print(tmp)
  total_table %>% dplyr::bind_rows(tmp) -> total_table
}
total_table %>% 
  dplyr::arrange(desc(as.Date(Date,format="%Y/%m/%d")),as.Date(ExpDate,format="%Y/%m/%d"),TYPE,Strike) %>%
  dplyr::distinct() %>%
  dplyr::filter(Strike != "Strike") -> preForSkew
(preForSkew)
#preForSkew$Strike=as.integer(preForSkew$Strike)
preForSkew$Date=format(as.Date(preForSkew$Date,format="%Y/%m/%d"),"%Y/%m/%d")
preForSkew$ExpDate=format(as.Date(preForSkew$ExpDate,format="%Y/%m/%d"),"%Y/%m/%d")
na.omit(preForSkew)->preForSkew
preForSkew$Bid=as.numeric(preForSkew$Bid)
preForSkew$Ask=as.numeric(preForSkew$Ask)
preForSkew$Last=as.numeric(preForSkew$Last)

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

# copy _OPChain_PreForSkew.csv _OPChain_Pre.csv
file.copy(from=paste(DataFiles_Path_G,Underying_Symbol_G,"_OPChain_PreForSkew.csv",sep=""), 
          to=paste(DataFiles_Path_G,Underying_Symbol_G,"_OPChain_Pre.csv",sep=""),
          overwrite=T)
