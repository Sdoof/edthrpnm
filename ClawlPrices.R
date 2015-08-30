library("XML")
library("quantmod")

#Strike ContactName Last Bid Adk Change %Change Volume OI IV
today<-Sys.Date()
wfilename_<-paste("opprice",today,".csv",sep="")

#^RUT
quart<-getQuote("^RUT")
write.table(quart,wfilename_,quote=T,row.names=F,append=T,sep=",")
write("Strike,ContactName,Last,Bid,Ask,Change,%Change,Volume,OI,IV,ExpDate",wfilename_,append=T)
#2015/09/17
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^RUT&date=1442534400")
expdate<-"2015/9/17"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2015/10/15
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^RUT&date=1444953600")
expdate<-"2015/10/15"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2015/11/19
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^RUT&date=1447977600")
expdate<-"2015/11/19"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2015/12/18
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^RUT&date=1450483200")
expdate<-"2015/12/18"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2016/1/14
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^RUT&date=1452816000")
expdate<-"2016/1/14"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2016/03/17
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^RUT&date=1458259200")
expdate<-"2016/3/17"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")

#^SPX(^GSPC)
quart<-getQuote("^GSPC")
write.table(quart,wfilename_,quote=T,row.names=F,append=T,sep=",")
write("Strike,ContactName,Last,Bid,Ask,Change,%Change,Volume,OI,IV,ExpDate",wfilename_,append=T)
#2015/09/17
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^SPX&date=1442534400")
expdate<-"2015/9/17"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2015/10/15
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^SPX&date=1444953600")
expdate<-"2015/10/15"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2015/11/5
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^SPX&date=1446768000")
expdate<-"2015/11/5"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2015/11/19
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^SPX&date=1447977600")
expdate<-"2015/11/19"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2015/12/3
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^SPX&date=1449187200")
expdate<-"2015/12/3"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2015/12/18
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^SPX&date=1450483200")
expdate<-"2015/12/18"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2015/12/30
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^SPX&date=1451520000")
expdate<-"2015/12/30"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2016/1/14
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^SPX&date=1452816000")
expdate<-"2016/1/14"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2016/1/28
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^SPX&date=1454025600")
expdate<-"2016/1/28"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2016/03/17
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^SPX&date=1458259200")
expdate<-"2016/3/17"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")

#^SPXPM
quart<-getQuote("^SPXPM")
write.table(quart,wfilename_,quote=T,row.names=F,col.names=F,append=T,sep=",")
write("Strike,ContactName,Last,Bid,Ask,Change,%Change,Volume,OI,IV,ExpDate",wfilename_,append=T)
#2015/09/18
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^SPXPM&date=1442534400")
expdate<-"2015/9/18"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2015/10/16
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^SPXPM&date=1444953600")
expdate<-"2015/10/16"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2015/11/20
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^SPXPM&date=1447977600")
expdate<-"2015/11/20"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2015/12/19
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^SPXPM&date=1450483200")
expdate<-"2015/12/19"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2016/01/15
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^SPXPM&date=1452816000")
expdate<-"2016/1/15"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2016/03/18
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^SPXPM&date=1458259200")
expdate<-"2016/3/18"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,col.names=F,append=T,sep=",")

#AUD/USD Future Option
#2015 Sep
# write("Future,2015-09,Expr,2015-09,",wfilename_,append=T)
# tbl_<-readHTMLTable("http://www.cmegroup.com/trading/fx/g10/australian-dollar_quotes_globex_options.html?optionExpiration=U5#optionProductId=38&strikeRange=ALL")
# write.table(tbl_[[1]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
# write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2015 Dec Dec
# write("Future,2015-12,Expr,2015-12,",wfilename_,append=T)
# tbl_<-readHTMLTable("http://www.cmegroup.com/trading/fx/g10/australian-dollar_quotes_globex_options.html?optionExpiration=Z5#strikeRange=ALL")
# write.table(tbl_[[1]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
# write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")

#JPY/USD Future Option
#2015 Sep
# write("Future,2015-09,Expr,2015-09,",wfilename_,append=T)
# tbl_<-readHTMLTable("http://www.cmegroup.com/trading/fx/g10/japanese-yen_quotes_globex_options.html?optionExpiration=U5#optionProductId=71&strikeRange=ALL")
# write.table(tbl_[[1]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
# write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
#2015 Dec
# write("Future,2015-12,Expr,2015-12,",wfilename_,append=T)
# tbl_<-readHTMLTable("http://www.cmegroup.com/trading/fx/g10/japanese-yen_quotes_globex_options.html?optionExpiration=Z5#optionProductId=71&strikeRange=ALL")
# write.table(tbl_[[1]],wfilename_,row.names=F,col.names=F,append=T,sep=",")
# write.table(tbl_[[2]],wfilename_,row.names=F,col.names=F,append=T,sep=",")

