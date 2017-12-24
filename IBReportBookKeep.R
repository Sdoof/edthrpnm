library(dplyr)
library(readr)
library(tidyr)
library(stringr)
rm(list=ls())

#Data File Path
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"

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
readFname=paste(DataFiles_Path_G,"USDJPY.csv",sep='')
sheet_USDJPY_colnames=c("Date","USDJPY")
sheet_USDJPY=read_csv(readFname,col_names = sheet_USDJPY_colnames,
                      col_types = cols(
                        Date = col_character(),
                        USDJPY = col_double()
                      ))
print(sheet_USDJPY,n=nrow(sheet_USDJPY),width = Inf)


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

# Option Trades Analysis

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
print(trades_EqIdxOption,n=nrow(trades_EqIdxOption),width = Inf)

trades_EqIdxOption %>%
  dplyr::mutate(NewShortJPY=ifelse(!is.na(NewShortUSD),NewShortUSD*USDJPY,NA)) %>%
  dplyr::mutate(NewLongJPY=ifelse(!is.na(NewLongUSD),NewLongUSD*USDJPY,NA)) %>%
  dplyr::mutate(ProfitJPY=ifelse(!is.na(ProfitUSD),ProfitUSD*USDJPY,NA)) %>%
  dplyr::mutate(LossJPY=ifelse(!is.na(LossUSD),LossUSD*USDJPY,NA)) %>%
  dplyr::mutate(ShtBasisJPY=ifelse(!is.na(ShtBasisUSD),ShtBasisUSD*USDJPY,NA)) %>%
  dplyr::mutate(LngBasisJPY=ifelse(!is.na(LngBasisUSD),LngBasisUSD*USDJPY,NA)) %>%
  dplyr::mutate(EOTShtPosJPY=ifelse(!is.na(EOTShtPosUSD),EOTShtPosUSD*USDJPY,NA)) %>%
  dplyr::mutate(EOTLngPosJPY=ifelse(!is.na(EOTLngPosUSD),EOTLngPosUSD*USDJPY,NA)) %>%
  dplyr::mutate(CommJPY=ifelse(!is.na(CommUSD),CommUSD*USDJPY,NA)) -> trades_EqIdxOption
print(trades_EqIdxOption,n=nrow(trades_EqIdxOption),width = Inf)

#Total 
trades_EqIdxOption %>%
  dplyr::summarise(NewShortUSD=sum(NewShortUSD,na.rm = T),
                   ShtBasisUSD=sum(ShtBasisUSD,na.rm = T),
                   EOTShtPosUSD=sum(EOTShtPosUSD,na.rm = T),
                   CommUSD=sum(CommUSD,na.rm = T),
                   NewShortJPY=sum(NewShortJPY,na.rm = T),
                   ShtBasisJPY=sum(ShtBasisJPY,na.rm = T),
                   EOTShtPosJPY=sum(EOTShtPosJPY,na.rm = T),
                   CommJPY=sum(CommJPY,na.rm = T)
  ) -> tmp
trades_EqIdxOption %>%
  dplyr::bind_rows(tmp) -> trades_EqIdxOption
trades_EqIdxOption[nrow(trades_EqIdxOption),"Symbol"]="ShortTotal"

trades_EqIdxOption %>%
  dplyr::summarise(NewLongUSD=sum(-NewLongUSD,na.rm = T),
                   LngBasisUSD=sum(-LngBasisUSD,na.rm = T),
                   EOTLngPosUSD=sum(-EOTLngPosUSD,na.rm = T),
                   NewLongJPY=sum(-NewLongJPY,na.rm = T),
                   LngBasisJPY=sum(-LngBasisJPY,na.rm = T),
                   EOTLngPosJPY=sum(-EOTLngPosJPY,na.rm = T)
  ) -> tmp
trades_EqIdxOption %>%
  dplyr::bind_rows(tmp) -> trades_EqIdxOption
trades_EqIdxOption[nrow(trades_EqIdxOption),"Symbol"]="LongTotal"

trades_EqIdxOption %>%
  dplyr::summarise(ProfitUSD=sum(ProfitUSD,na.rm = T),
                   LossUSD=sum(LossUSD,na.rm = T),
                   ProfitJPY=sum(ProfitJPY,na.rm = T),
                   LossJPY=sum(LossJPY,na.rm = T)) -> tmp
trades_EqIdxOption %>%
  dplyr::bind_rows(tmp) -> trades_EqIdxOption
trades_EqIdxOption[nrow(trades_EqIdxOption),"Symbol"]="Profit/Loss"



##
# Heder 
sheet_whole %>% 
  dplyr::filter((Trades=="Statement"&
                   (DataDiscriminator=="BrokerName"|DataDiscriminator=="Title")|DataDiscriminator=="Period")) %>%
  dplyr::bind_rows(tibble(AssetCategory="Interactive Brokers LLC, Two Pickwick Plaza, Greenwich, CT 06830")) %>%
  dplyr::bind_rows(tibble(AssetCategory="Account Information")) -> tmp


sheet_whole %>% 
  dplyr::filter((stringr::str_detect(Trades,"Account")&stringr::str_detect(Trades,"Information"))&
                  (DataDiscriminator=="Name")) %>%
  dplyr::mutate(Currency=AssetCategory) %>%
  dplyr::mutate(AssetCategory=DataDiscriminator) -> tmp2

tmp %>% 
  dplyr::bind_rows(tmp2) %>%
  dplyr::mutate(Symbol=AssetCategory,DateTime=Currency) %>%
  dplyr::select(Symbol,DateTime) %>%
  dplyr::bind_rows(tibble(Symbol="Trades",DateTime=NA)) -> sheet_header

##### Double Entry BookKeep
##USD
BOTShtPosUSD=36758.00
BOTLngPosUSD=33053.00

NewShortPosUSD=max(trades_EqIdxOption$NewShortUSD,na.rm=T)
ShtPosBasisUSD=max(trades_EqIdxOption$ShtBasisUSD,na.rm=T)
EOTShtPosUSD=max(trades_EqIdxOption$EOTShtPosUSD,na.rm=T)
NewShortPosUSD
ShtPosBasisUSD

NewLongPosUSD=max(trades_EqIdxOption$NewLongUSD,na.rm=T)
LngPosBasisUSD=max(trades_EqIdxOption$LngBasisUSD,na.rm=T)
EOTLngPosUSD=max(trades_EqIdxOption$EOTLngPosUSD,na.rm=T)
NewLongPosUSD
LngPosBasisUSD

CommUSD=max(trades_EqIdxOption$CommUSD,na.rm=T)
CommUSD

ProfitUSD=max(trades_EqIdxOption$ProfitUSD,na.rm=T)
LossUSD=min(trades_EqIdxOption$LossUSD,na.rm=T)
ProfitUSD
LossUSD

BOTShtPosUSD+NewShortPosUSD-ShtPosBasisUSD
EOTShtPosUSD

BOTLngPosUSD+NewLongPosUSD-LngPosBasisUSD
EOTLngPosUSD

##JPY 
BOTShtPosJPY=4068845
BOTLngPosJPY=3659470

NewShortPosJPY=max(trades_EqIdxOption$NewShortJPY,na.rm=T)
ShtPosBasisJPY=max(trades_EqIdxOption$ShtBasisJPY,na.rm=T)
EOTShtPosJPY=max(trades_EqIdxOption$EOTShtPosJPY,na.rm=T)
NewShortPosJPY
ShtPosBasisJPY

NewLongPosJPY=max(trades_EqIdxOption$NewLongJPY,na.rm=T)
LngPosBasisJPY=max(trades_EqIdxOption$LngBasisJPY,na.rm=T)
EOTLngPosJPY=max(trades_EqIdxOption$EOTLngPosJPY,na.rm=T)
NewLongPosJPY
LngPosBasisJPY

CommJPY=max(trades_EqIdxOption$CommJPY,na.rm=T)
CommJPY

ProfitJPY=max(trades_EqIdxOption$ProfitJPY,na.rm=T)
LossJPY=min(trades_EqIdxOption$LossJPY,na.rm=T)
ProfitJPY
LossJPY

BOTShtPosJPY+NewShortPosJPY-ShtPosBasisJPY
EOTShtPosJPY

BOTLngPosJPY+NewLongPosJPY-LngPosBasisJPY
EOTLngPosJPY

sheet_DeBk_USD=tribble( ~Desc, ~DescCredit,~ValCredit, ~DescDebt,~ValDebt,
                        "ドル建", "",NA,"",NA,
                        "期初資産","",NA,"期初売建オプション",BOTShtPosUSD,
                        "", "期初買建オプション",BOTLngPosUSD,"",NA,
                        "仕訳", "現金",NewShortPosUSD,"売建オプション",NewShortPosUSD,
                        "", "売建オプション",ShtPosBasisUSD,"現金",ShtPosBasisUSD,
                        "","",NA,"",NA,
                        "", "買建オプション",NewLongPosUSD,"現金",NewLongPosUSD,
                        "", "現金",LngPosBasisUSD,"買建オプション",LngPosBasisUSD,
                        "","",NA,"",NA,
                        "", "証券売買手数料",CommUSD,"現金",CommUSD,
                        "","",NA,"",NA,
                        "", "現金",ProfitUSD,"オプション差益",ProfitUSD,
                        "","オプション差損",-LossUSD,"現金",-LossUSD,
                        "期末資産","期末売建オプション",NA,"",EOTShtPosUSD,
                        "", "期末買建オプション",EOTLngPosUSD,"",NA
)

sheet_DeBk_JPY=tribble( ~Desc, ~DescCredit,~ValCredit, ~DescDebt,~ValDebt,
                        "円建", "",NA,"",NA,
                        "期初資産","",NA,"期初売建オプション",BOTShtPosJPY,
                        "", "期初買建オプション",BOTLngPosJPY,"",NA,
                        "仕訳", "現金",NewShortPosJPY,"売建オプション",NewShortPosJPY,
                        "", "売建オプション",ShtPosBasisJPY,"現金",ShtPosBasisJPY,
                        "","",NA,"",NA,
                        "", "買建オプション",NewLongPosJPY,"現金",NewLongPosJPY,
                        "", "現金",LngPosBasisJPY,"買建オプション",LngPosBasisJPY,
                        "","",NA,"",NA,
                        "", "証券売買手数料",CommJPY,"現金",CommJPY,
                        "","",NA,"",NA,
                        "", "現金",ProfitJPY,"オプション差益",ProfitJPY,
                        "","オプション差損",-LossJPY,"現金",-LossJPY,
                        "期末資産","期末売建オプション",NA,"",EOTShtPosJPY,
                        "", "期末買建オプション",EOTLngPosJPY,"",NA
)

##
# Write to a file

writeFname=paste(DataFiles_Path_G,"OptionTradesBookeep.csv",sep='')
#Header
write_excel_csv(sheet_header, path=writeFname, na = "", append = F, col_names = F)
#Trade
write_excel_csv(trades_EqIdxOption, path=writeFname, na = "", append = T, col_names = T)
#BookKeep
write_excel_csv(sheet_DeBk_USD, path=writeFname, na = "", append = T, col_names = F)
write_excel_csv(sheet_DeBk_JPY, path=writeFname, na = "", append = T, col_names = F)

#FYI remove last row
#trades_EqIdxOption=trades_EqIdxOption[-nrow(trades_EqIdxOption),]

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
positions=NULL

#C1
SymbolTicket = c( 'SPX','SPX' ); ExpiryTicket = c( '20180216','20180216' ); StrikeTicket = c( 2610,2675 ) ; RightTicket = c( 'P','P' ); BuySell = c( 'SELL','BUY' )
positions %>% 
  dplyr::bind_rows(tibble(SymbolTicket=SymbolTicket, ExpiryTicket=ExpiryTicket, StrikeTicket=StrikeTicket,RightTicket=RightTicket,BuySell=ifelse(BuySell=="BUY",1,-1))) %>%
  dplyr::distinct() -> positions

SymbolTicket = c( 'SPX','SPX' ); ExpiryTicket = c( '20180329','20180329' ); StrikeTicket = c( 2550,2660 ) ; RightTicket = c( 'P','P' ); BuySell = c( 'BUY','SELL' )
positions %>% 
  dplyr::bind_rows(tibble(SymbolTicket=SymbolTicket, ExpiryTicket=ExpiryTicket, StrikeTicket=StrikeTicket,RightTicket=RightTicket,BuySell=ifelse(BuySell=="BUY",1,-1))) %>%
  dplyr::distinct() -> positions
print(positions,n=nrow(positions),width = Inf)
sum(abs(positions$BuySell))

#C2
SymbolTicket = c( 'SPX','SPX' ); ExpiryTicket = c( '20180228','20180228' ); StrikeTicket = c( 2610,2670 ) ; RightTicket = c( 'P','P' ); BuySell = c( 'SELL','BUY' )
positions %>%
  dplyr::bind_rows(tibble(SymbolTicket=SymbolTicket, ExpiryTicket=ExpiryTicket, StrikeTicket=StrikeTicket,RightTicket=RightTicket,BuySell=ifelse(BuySell=="BUY",1,-1))) %>%
  dplyr::distinct() -> positions

SymbolTicket = c( 'SPX','SPX' ); ExpiryTicket = c( '20180420','20180420' ); StrikeTicket = c( 2550,2650 ) ; RightTicket = c( 'P','P' ); BuySell = c( 'BUY','SELL' )
positions %>%
  dplyr::bind_rows(tibble(SymbolTicket=SymbolTicket, ExpiryTicket=ExpiryTicket, StrikeTicket=StrikeTicket,RightTicket=RightTicket,BuySell=ifelse(BuySell=="BUY",1,-1))) %>%
  dplyr::distinct() -> positions
print(positions,n=nrow(positions),width = Inf)
sum(abs(positions$BuySell))

#c3
SymbolTicket = c( 'SPX','SPX' ); ExpiryTicket = c( '20180316','20180430' ); StrikeTicket = c( 2625,2570 ) ; RightTicket = c( 'P','P' ); BuySell = c( 'SELL','BUY' )
positions %>%
  dplyr::bind_rows(tibble(SymbolTicket=SymbolTicket, ExpiryTicket=ExpiryTicket, StrikeTicket=StrikeTicket,RightTicket=RightTicket,BuySell=ifelse(BuySell=="BUY",1,-1))) %>%
  dplyr::distinct() -> positions

SymbolTicket = c( 'SPX','SPX' ); ExpiryTicket = c( '20180316','20180430' ); StrikeTicket = c( 2710,2700 ) ; RightTicket = c( 'C','C' ); BuySell = c( 'BUY','SELL' )
positions %>%
  dplyr::bind_rows(tibble(SymbolTicket=SymbolTicket, ExpiryTicket=ExpiryTicket, StrikeTicket=StrikeTicket,RightTicket=RightTicket,BuySell=ifelse(BuySell=="BUY",1,-1))) %>%
  dplyr::distinct() -> positions
print(positions,n=nrow(positions),width = Inf)
sum(abs(positions$BuySell))

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




