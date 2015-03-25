library("XML")
library("quantmod")

#Strike ContactName Last Bid Adk Change %Change Volume OI IV
today<-Sys.Date()
wfilename_<-paste("opprice",today,".csv",sep="")

#^RUT
quart<-getQuote("^RUT")
write.table(quart,wfilename_,quote=T,row.names=F,append=T,sep=",")
write("Strike,ContactName,Last,Bid,Ask,Change,%Change,Volume,OI,IV,ExpDate",wfilename_,append=T)
#2015/04/17
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^RUT&date=1429228800")
expdate<-"2015/04/17"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,append=T,sep=",")
#2015/06/19
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^RUT&date=1434672000")
expdate<-"2015/06/19"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,append=T,sep=",")
#2015/06/30
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^RUT&date=1435622400")
expdate<-"2015/06/30"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,append=T,sep=",")
#2015/09/18
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^RUT&date=1442534400")
expdate<-"2015/09/18"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,append=T,sep=",")
#2015/12/19
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^RUT&date=1450483200")
expdate<-"2015/12/19"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,append=T,sep=",")

#^SPXPM
quart<-getQuote("^SPXPM")
write.table(quart,wfilename_,quote=T,row.names=F,append=T,sep=",")
#2015/04/20
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^SPXPM&date=1429228800")
expdate<-"2015/04/20"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,append=T,sep=",")
#2015/06/19
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^SPXPM&date=1434672000")
expdate<-"2015/06/19"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,append=T,sep=",")
#2015/09/18
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^SPXPM&date=1442534400")
expdate<-"2015/09/18"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,append=T,sep=",")
#2015/12/19
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=^SPXPM&date=1450483200")
expdate<-"2015/12/19"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,append=T,sep=",")

#SPY
quart<-getQuote("SPY")
write.table(quart,wfilename_,quote=T,row.names=F,append=T,sep=",")

#2015/03/20
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=SPY&date=1426809600")
#2015/04/17
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=SPY&date=1429228800")
expdate<-"2015/04/17"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,append=T,sep=",")
#2015/06/19
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=SPY&date=1434672000")
expdate<-"2015/06/19"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,append=T,sep=",")

#EEM
quart<-getQuote("EEM")
write.table(quart,wfilename_,quote=T,row.names=F,append=T,sep=",")
write("Strike,ContactName,Last,Bid,Ask,Change,%Change,Volume,OI,IV")
#2015/06/19
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=EEM&date=1434672000")
expdate<-"2015/06/19"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,append=T,sep=",")

#SLV
quart<-getQuote("SLV")
write.table(quart,wfilename_,quote=T,row.names=F,append=T,sep=",")
write("Strike,ContactName,Last,Bid,Ask,Change,%Change,Volume,OI,IV")
#2015/04/17
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=SLV&date=1429228800")
expdate<-"2015/04/17"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,append=T,sep=",")
#2015/07/17
tbl_<-readHTMLTable("http://finance.yahoo.com/q/op?s=SLV&date=1437091200")
expdate<-"2015/07/17"
tbl_[[2]]["ExpDate"]<-expdate
write.table(tbl_[[2]],wfilename_,row.names=F,append=T,sep=",")
tbl_[[3]]["ExpDate"]<-expdate
write.table(tbl_[[3]],wfilename_,row.names=F,append=T,sep=",")

#AUD/USD Future Option
  #2015 March European
  #tbl_<-readHTMLTable("http://www.cmegroup.com/trading/fx/g10/australian-dollar_quotes_globex_options.html?optionExpiration=H5#optionProductId=2535&strikeRange=ALL")
#2015 March American
tbl_<-readHTMLTable("http://www.cmegroup.com/trading/fx/g10/australian-dollar_quotes_globex_options.html?optionExpiration=H5#optionProductId=38&strikeRange=ALL")
write.table(tbl_[[1]],wfilename_,row.names=F,append=T,sep=",")
write.table(tbl_[[2]],wfilename_,row.names=F,append=T,sep=",")
   #2015 Jun European
   #http://www.cmegroup.com/trading/fx/g10/australian-dollar_quotes_globex_options.html?optionExpiration=M5#optionProductId=2535&strikeRange=ALL
#2015 Jun Americal
tbl_<-readHTMLTable("http://www.cmegroup.com/trading/fx/g10/australian-dollar_quotes_globex_options.html?optionExpiration=M5#strikeRange=ALL&optionProductId=38")
write.table(tbl_[[1]],wfilename_,row.names=F,append=T,sep=",")
write.table(tbl_[[2]],wfilename_,row.names=F,append=T,sep=",")
#2015 Sep American
tbl_<-readHTMLTable("http://www.cmegroup.com/trading/fx/g10/australian-dollar_quotes_globex_options.html?optionExpiration=U5#optionProductId=38&strikeRange=ALL")
write.table(tbl_[[1]],wfilename_,row.names=F,append=T,sep=",")
write.table(tbl_[[2]],wfilename_,row.names=F,append=T,sep=",")

#JPY/USD Future Option
#2015 Mar JPYUSD
tbl_<-readHTMLTable("http://www.cmegroup.com/trading/fx/g10/japanese-yen_quotes_globex_options.html?optionExpiration=H5#strikeRange=ALL&optionProductId=71")
write.table(tbl_[[1]],wfilename_,row.names=F,append=T,sep=",")
write.table(tbl_[[2]],wfilename_,row.names=F,append=T,sep=",")
#2015 Jun JPYUSD
tbl_<-readHTMLTable("http://www.cmegroup.com/trading/fx/g10/japanese-yen_quotes_globex_options.html?optionExpiration=M5#optionProductId=71&strikeRange=ALL")
write.table(tbl_[[1]],wfilename_,row.names=F,append=T,sep=",")
write.table(tbl_[[2]],wfilename_,row.names=F,append=T,sep=",")
#2015 Sep JPYUSD
tbl_<-readHTMLTable("http://www.cmegroup.com/trading/fx/g10/japanese-yen_quotes_globex_options.html?optionExpiration=U5#optionProductId=71&strikeRange=ALL")
write.table(tbl_[[1]],wfilename_,row.names=F,append=T,sep=",")
write.table(tbl_[[2]],wfilename_,row.names=F,append=T,sep=",")