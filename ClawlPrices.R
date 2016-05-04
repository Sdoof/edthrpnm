library("XML")
library("quantmod")

#Strike ContactName Last Bid Adk Change %Change Volume OI IV
today<-Sys.Date()
wfilename_<-paste("opprice",today,".csv",sep="")

#^RUT
quart<-getQuote("^RUT")
write.table(quart,wfilename_,quote=T,row.names=F,append=T,sep=",")
write("Strike,ContactName,Last,Bid,Ask,Change,%Change,Volume,OI,IV,ExpDate",wfilename_,append=T)
#2016/6/16
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^RUT&date=1466121600")
expdate<-"2016/6/16"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2016/9/15
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^RUT&date=1473984000")
expdate<-"2016/9/15"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2016/12/15
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^RUT&date=1481846400")
expdate<-"2016/12/15"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")

#^SPX(^GSPC)
# quart<-getQuote("^GSPC")
# write.table(quart,wfilename_,quote=T,row.names=F,append=T,sep=",")
# write("Strike,ContactName,Last,Bid,Ask,Change,%Change,Volume,OI,IV,ExpDate",wfilename_,append=T)

# #2016/9/15
# tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^SPX&date=1473984000")
# expdate<-"2016/9/15"
# tbl_[[2]]["ExpDate"]<-expdate
# write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
# tbl_[[3]]["ExpDate"]<-expdate



