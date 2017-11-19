library(ggplot2)
library(dplyr)
library(digest)
library(hash)
library(readr)
library(tidyr)
library(stringr)
rm(list=ls())
source('./ESourceRCode.R',encoding = 'UTF-8')

##
# whole sheet
readFname=paste(DataFiles_Path_G,"UXXXXXX.csv",sep='')
sheet_colnames=c("Trades","Header","DataDiscriminator","AssetCategory","Currency","Symbol","DateTime","Exchange","Quantity","T.Price","Proceeds","Comm/Fee","Basis","RealizedP/L","Code")
sheet_whole=read_csv(readFname,col_names = sheet_colnames,
                     col_types = cols(
                       Trades = col_character(),
                       Header = col_character(),
                       DataDiscriminator = col_character(),
                       AssetCategory = col_character(),
                       Currency = col_character(),
                       Symbol = col_character(),
                       DateTime = col_character(),
                       Exchange = col_character(),
                       Quantity = col_integer(),
                       T.Price = col_double(),
                       Proceeds = col_double(),
                       `Comm/Fee` = col_double(),
                       Basis = col_double(),
                       `RealizedP/L` = col_double(),
                       Code = col_character()
                     ))
print(sheet_whole,n=nrow(sheet_whole),width = Inf)


##
# UDLY Trades
sheet_whole %>% dplyr::filter(Trades=="Trades"&AssetCategory=="Stocks") %>%
  dplyr::mutate(SymbolMod=ifelse(DataDiscriminator=="ClosedLot",DataDiscriminator,Symbol)) %>%
  dplyr::mutate(Symbol=SymbolMod) %>%
  dplyr::mutate(SymbolMod=ifelse(stringr::str_detect(Header,"Total"),stringr::str_c(Header,Currency,sep=" "),Symbol)) %>%
  dplyr::mutate(Symbol=SymbolMod) %>%
  dplyr::select(sheet_colnames) -> tmp
print(tmp,n=nrow(tmp),width = Inf)

tmp %>% tidyr::replace_na(list(DataDiscriminator = "MISC")) %>%
  dplyr::filter(DataDiscriminator!="Order") %>%
  dplyr::select("Symbol","DateTime","Exchange","Quantity","T.Price","Proceeds","Comm/Fee","Basis","RealizedP/L","Code") %>%
  tidyr::replace_na(list(DateTime="",Exchange="",`Comm/Fee`=0,Code="")) -> trades_UDLY
print(trades_UDLY,n=nrow(trades_UDLY),width = Inf)

##
# Option Trades
sheet_whole %>% 
  dplyr::filter(Trades=="Trades"&stringr::str_detect(AssetCategory,"Equity")&stringr::str_detect(AssetCategory,"Index")) %>% 
  dplyr::select(-Exchange) %>%
  dplyr::mutate(SymbolMod=ifelse(DataDiscriminator=="ClosedLot",DataDiscriminator,Symbol)) %>%
  dplyr::mutate(Symbol=SymbolMod) %>%
  dplyr::filter(Header!="SubTotal") %>%
  dplyr::mutate(SymbolMod=ifelse(stringr::str_detect(Header,"Total"),stringr::str_c(Header,Currency,sep=" "),Symbol)) %>%
  dplyr::mutate(Symbol=SymbolMod) %>%
  dplyr::select(sheet_colnames[-which(sheet_colnames=="Exchange")]) -> tmp
print(tmp,n=nrow(tmp),width = Inf)

tmp %>% tidyr::replace_na(list(DataDiscriminator = "MISC")) %>%
  dplyr::filter(DataDiscriminator!="Order") %>%
  dplyr::select("Symbol","DateTime","Quantity","T.Price","Proceeds","Comm/Fee","Basis","RealizedP/L","Code") %>%
  tidyr::fill(`Comm/Fee`)  %>%
  tidyr::replace_na(list(DateTime="",Code="")) -> trades_EqIdxOption
print(trades_EqIdxOption,n=nrow(trades_EqIdxOption),width = Inf)
#exclude Total JPY
trades_EqIdxOption %>% dplyr::filter((stringr::str_detect(Symbol,"Total")==T&stringr::str_detect(Symbol,"JPY")==T)!=T) -> trades_EqIdxOption
print(trades_EqIdxOption,n=nrow(trades_EqIdxOption),width = Inf)

##
# Option Holding
sheet_whole %>% 
  dplyr::filter(stringr::str_detect(Trades,"Open")&stringr::str_detect(Trades,"Positions")&stringr::str_detect(AssetCategory,"Equity")&stringr::str_detect(AssetCategory,"Index")) %>%
  dplyr::filter(DataDiscriminator!="Summary") %>%
  dplyr::mutate(Trades=NULL,Header=NULL,DataDiscriminator=NULL,AssetCategory=NULL,Currency=NULL,Exchange=NULL,Quantity=as.integer(Exchange),Code=NULL) %>%
  dplyr::rename(CostPrice='T.Price',CostBasis=Proceeds,ClosePrice=`Comm/Fee`,Value=Basis,`UnrealizedP/L`=`RealizedP/L`) -> sheet_OpPos
sum(abs(sheet_OpPos$Quantity))
print(sheet_OpPos,n=nrow(sheet_OpPos),width = Inf)


##
# USDJPY
readFname=paste(DataFiles_Path_G,"USDJPY17.csv",sep='')
sheet_colnames=c("Date","USDJPY")
sheet_USDJPY=read_csv(readFname,col_names = sheet_colnames,
                      col_types = cols(
                        Date = col_character(),
                        USDJPY = col_character()
                      ))
print(sheet_USDJPY,n=nrow(sheet_USDJPY),width = Inf)

##
# make new columns
trades_EqIdxOption %>%
  dplyr::left_join(sheet_OpPos %>% select(Symbol,DateTime,Value)) %>%
  dplyr::mutate(NextMonth=ifelse(is.na(Value),NA,
                                 ifelse(Code!="O",NA,abs(Quantity))),
                Value=NULL) -> trades_EqIdxOption
sum(trades_EqIdxOption$NextMonth,na.rm=T)
print(trades_EqIdxOption,n=nrow(trades_EqIdxOption),width = Inf)

trades_EqIdxOption %>%
  dplyr::mutate(Date=str_extract(DateTime,"\\d*-\\d*-\\d*")) %>%
  dplyr::mutate(Date=str_replace_all(Date,"-0","/")) %>%
  dplyr::mutate(Date=str_replace_all(Date,"-","/")) -> trades_EqIdxOption
print(trades_EqIdxOption,n=nrow(trades_EqIdxOption),width = Inf)

trades_EqIdxOption %>%
  dplyr::mutate(NewShortUSD=ifelse(Code=="O"&Proceeds>=0,Proceeds,NA)) %>%
  dplyr::mutate(NewLongUSD=ifelse(Code=="O"&Proceeds<0,Proceeds,NA)) -> trades_EqIdxOption

trades_EqIdxOption %>%
  dplyr::mutate(ProfitUSD=ifelse(Code=="C"&`RealizedP/L`>=0,`RealizedP/L`-`Comm/Fee`,NA)) %>%
  dplyr::mutate(LossUSD=ifelse(Code=="C"&`RealizedP/L`<0,`RealizedP/L`-`Comm/Fee`,NA)) -> trades_EqIdxOption

trades_EqIdxOption %>%
  dplyr::mutate(ShtBasisUSD=
                  ifelse(Basis<0,
                         ifelse(Code=="ST"|Code=="LT",
                                -(`Comm/Fee`+Basis),
                                NA),
                         NA)) %>%
  dplyr::mutate(LngBasisUSD=
                  ifelse(Basis>0,
                         ifelse(Code=="ST"|Code=="LT",
                                -(`Comm/Fee`+Basis),
                                NA),
                         NA)) -> trades_EqIdxOption

trades_EqIdxOption %>%
  dplyr::mutate(EOTShtPosUSD=ifelse(!is.na(NextMonth)&Proceeds>=0,Proceeds,NA)) %>%
  dplyr::mutate(EOTLngPosUSD=ifelse(!is.na(NextMonth)&Proceeds<0,Proceeds,NA)) -> trades_EqIdxOption

trades_EqIdxOption %>%
  dplyr::mutate(CommUSD=ifelse(Code=="C"|Code=="O",-`Comm/Fee`,NA)) -> trades_EqIdxOption

trades_EqIdxOption %>%
  dplyr::left_join(sheet_USDJPY) -> trades_EqIdxOption

##
#  position checking

#months list
months_num<-c("01","02","03","04","05","06","07","08","09","10","11","12")
months<-vector("list",length(months_num))
months[[1]]<-"JAN"
months[[2]]<-"FEB"
months[[3]]<-"MAR"
months[[4]]<-"APR"
months[[5]]<-"MAY"
months[[6]]<-"JUN"
months[[7]]<-"JUL"
months[[8]]<-"AUG"
months[[9]]<-"SEP"
months[[10]]<-"OCT"
months[[11]]<-"NOV"
months[[12]]<-"DEC"
names(months)<-months_num

sheet_OpPos %>% 
  dplyr::group_by(Symbol)  %>% 
  dplyr::summarise(Quantity=sum(Quantity),DateTime=dplyr::first(DateTime)) %>% 
  dplyr::arrange(Symbol) %>% 
  dplyr::rename(contactName=Symbol) %>%
  dplyr::select(contactName,DateTime,Quantity) -> sheet_OpPosSmry
print(sheet_OpPosSmry,n=nrow(sheet_OpPosSmry),width = Inf)
sum(sheet_OpPosSmry$Quantity)
sum(abs(sheet_OpPosSmry$Quantity))

#iterate this

#S3-C12
positions=NULL
SymbolTicket = c( 'SPX','SPX' ); ExpiryTicket = c( '20180119','20180119' ); StrikeTicket = c( 2500,2560 ) ; RightTicket = c( 'P','P' ); BuySell = c( 'SELL','BUY' )
positions %>% 
  dplyr::bind_rows(tibble(SymbolTicket=SymbolTicket, ExpiryTicket=ExpiryTicket, StrikeTicket=StrikeTicket,RightTicket=RightTicket,BuySell=ifelse(BuySell=="BUY",1,-1))) %>%
  dplyr::distinct() -> positions

SymbolTicket = c( 'SPX','SPX' ); ExpiryTicket = c( '20180216','20180216' ); StrikeTicket = c( 2425,2525 ) ; RightTicket = c( 'P','P' ); BuySell = c( 'BUY','SELL' )
positions %>% 
  dplyr::bind_rows(tibble(SymbolTicket=SymbolTicket, ExpiryTicket=ExpiryTicket, StrikeTicket=StrikeTicket,RightTicket=RightTicket,BuySell=ifelse(BuySell=="BUY",1,-1))) %>%
  dplyr::distinct() -> positions
print(positions,n=nrow(positions),width = Inf)
sum(abs(positions$BuySell))

#S5-C1
#SymbolTicket = c( 'SPX','SPX' ); ExpiryTicket = c( '20180131','20180228' ); StrikeTicket = c( 2470,2425 ) ; RightTicket = c( 'P','P' ); BuySell = c( 'SELL','BUY' )
#positions %>% 
#  dplyr::bind_rows(tibble(SymbolTicket=SymbolTicket, ExpiryTicket=ExpiryTicket, StrikeTicket=StrikeTicket,RightTicket=RightTicket,BuySell=ifelse(BuySell=="BUY",1,-1))) %>%
#  dplyr::distinct() -> positions

#SymbolTicket = c( 'SPX','SPX' ); ExpiryTicket = c( '20180131','20180228' ); StrikeTicket = c( 2600,2600 ) ; RightTicket = c( 'C','C' ); BuySell = c( 'BUY','SELL' )
#positions %>% 
#  dplyr::bind_rows(tibble(SymbolTicket=SymbolTicket, ExpiryTicket=ExpiryTicket, StrikeTicket=StrikeTicket,RightTicket=RightTicket,BuySell=ifelse(BuySell=="BUY",1,-1))) %>%
#  dplyr::distinct() -> positions
#print(positions,n=nrow(positions),width = Inf)
#sum(abs(positions$BuySell))

#c-2
#SymbolTicket = c( 'SPX','SPX' ); ExpiryTicket = c( '20180216','20180316' ); StrikeTicket = c( 2490,2450 ) ; RightTicket = c( 'P','P' ); BuySell = c( 'SELL','BUY' )
#positions %>% 
#  dplyr::bind_rows(tibble(SymbolTicket=SymbolTicket, ExpiryTicket=ExpiryTicket, StrikeTicket=StrikeTicket,RightTicket=RightTicket,BuySell=ifelse(BuySell=="BUY",1,-1))) %>%
#  dplyr::distinct() -> positions

#SymbolTicket = c( 'SPX','SPX' ); ExpiryTicket = c( '20180216','20180316' ); StrikeTicket = c( 2625,2625 ) ; RightTicket = c( 'C','C' ); BuySell = c( 'BUY','SELL' )
#positions %>% 
#  dplyr::bind_rows(tibble(SymbolTicket=SymbolTicket, ExpiryTicket=ExpiryTicket, StrikeTicket=StrikeTicket,RightTicket=RightTicket,BuySell=ifelse(BuySell=="BUY",1,-1))) %>%
#  dplyr::distinct() -> positions
#print(positions,n=nrow(positions),width = Inf)
#sum(abs(positions$BuySell))

positions %>%
  tidyr::separate(ExpiryTicket,into = c("year", "monthdate"),sep = 4) %>%
  tidyr::separate(monthdate,into = c("month", "date"),sep = 2) -> positions
positions  %>% 
  dplyr::mutate(year=stringr::str_sub(year,-2)) %>%
  dplyr::mutate(month=unlist(months[month])) -> positions
positions %>%
  dplyr::mutate(StrikeTicket=paste(StrikeTicket,".0",sep='')) %>%
  tidyr::unite(StrikeTicket, RightTicket,col = "StrikeRight", sep = " ") %>%
  tidyr::unite(SymbolTicket,date,col = "symboldate", sep = " ") %>%
  tidyr::unite(symboldate,month,col = "symboldatemonth", sep = "") %>%
  tidyr::unite(symboldatemonth,year,col = "symboldatemonthyear", sep = "") %>%
  tidyr::unite(symboldatemonthyear,StrikeRight,col = "contactName",sep = " ") -> positions
print(positions,n=nrow(positions),width = Inf)

#join original holdings
sheet_OpPosSmry %>% dplyr::full_join(positions,by="contactName") %>%
  dplyr::arrange(contactName) %>%
  tidyr::replace_na(list(BuySell = 0)) %>%
  tidyr::replace_na(list(Quantity = 0)) %>%
  tidyr::replace_na(list(DateTime = "")) %>%
  dplyr::mutate(cdn=ifelse(Quantity*BuySell<0,"X","O")) -> sheet_OpPosSmry_pos
print(sheet_OpPosSmry_pos,n=nrow(sheet_OpPosSmry_pos),width = Inf)
#prepare for next positions
sheet_OpPosSmry_pos %>%
  dplyr::rename(pos1=BuySell) %>%
  dplyr::mutate(TotalPos=Quantity+pos1) %>%
  dplyr::select("contactName","DateTime","Quantity","pos1","TotalPos","cdn") -> sheet_OpPosSmry_pos
print(sheet_OpPosSmry_pos,n=nrow(sheet_OpPosSmry_pos),width = Inf)
#new sheet
sheet_OpPosSmry_pos %>%
  dplyr::mutate(Quantity=TotalPos) %>%
  dplyr::select("contactName","DateTime","Quantity") -> sheet_OpPosSmry_pos
print(sheet_OpPosSmry_pos,n=nrow(sheet_OpPosSmry_pos),width = Inf)
sum(sheet_OpPosSmry_pos$Quantity)
sum(abs(sheet_OpPosSmry_pos$Quantity))
#if iterate next legs
sheet_OpPosSmry<-sheet_OpPosSmry_pos




