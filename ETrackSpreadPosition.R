library(RQuantLib)
library(ggplot2)
library(plyr)
library(dplyr)
library(pracma)

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

#追跡対象のOption Spread。
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Hold.csv",sep="")
opchain<-read.table(rf,header=T,sep=",")
#get position where opchain$Position!=0
opchain %>% dplyr::filter(Position!=0) -> position
position %>% select(ExpDate,TYPE,Strike,Position) -> position

#本日のOption Chain読み込み
rf<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre.csv",sep="")
opchain<-read.table(rf,header=T,sep=",")
opchain %>% select(-(Position)) -> opchain ; head(opchain)

merge(opchain,position,sort=F) -> position_today
position<-position_today ; rm(position_today)

#列並べ変え。なぜか変な形式

#savefile or create object

rm(opchain,position)