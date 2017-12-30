library(dplyr)
library(readr)
library(tidyr)
library(stringr)
rm(list=ls())

#Data File Path
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"

###
##  whole sheet
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


###
##  UDLY Trades
sheet_whole %>% dplyr::filter(Trades=="Trades"&AssetCategory=="Stocks")  -> StockTrade
print(StockTrade,n=nrow(StockTrade),width = Inf)

StockTrade %>% 
  dplyr::mutate(DataDiscriminator=ifelse(Header=="Total"&Currency!="JPY",
                                         str_c(Currency,AssetCategory),DataDiscriminator)) %>%
  tidyr::fill(DataDiscriminator) -> StockTrade

StockTrade %>%
  dplyr::mutate(RlzdPL2=ifelse((DataDiscriminator=="Trade")&(Code=="C"|Code=="C;P"),
                               `RealizedP/L`,
                               NA)) -> StockTrade

theCurrency="USD"
StockTrade %>%
  dplyr::filter(Currency==theCurrency|str_detect(DataDiscriminator,theCurrency)) %>%
  tidyr::drop_na() %>%
  dplyr::summarise(Quantity=sum(Quantity,na.rm = T)*(-1),
                   SalesProceeds=sum(Proceeds,na.rm = T),
                   `Comm/Fee`=sum(`Comm/Fee`,na.rm = T)*(-1),
                   Basis=sum(Basis,na.rm = T)*(-1),
                   `RealizedP/L`=sum(`RealizedP/L`,na.rm = T),
                   RlzdPL2=sum(RlzdPL2,na.rm = T)) %>%
  dplyr::mutate(Check=SalesProceeds-`Comm/Fee`-Basis) -> USDCapGain

StockTrade %>%
  dplyr::filter(Header=="Total",str_detect(DataDiscriminator,theCurrency)&Currency=="JPY") %>%
  dplyr::select(`RealizedP/L`) -> tmp

exchgRate=tmp$`RealizedP/L`/USDCapGain$`RealizedP/L`

USDCapGain %>%
  dplyr::mutate_at(vars(-Quantity),funs(exchgRate*(.))) -> BaseCurrenyCapGain


##
# Write to a file

writeFname=paste(DataFiles_Path_G,"StockCapitalGain-",format(Sys.time(),"%Y%b%d"),".csv",sep='')
#StockTrade
write_csv(StockTrade, path=writeFname, na = "", append = F, col_names = T)
#USDCapGain
write_csv(USDCapGain, path=writeFname, na = "", append = T, col_names = T)
#BaseCurrenyCapGain
write_csv(BaseCurrenyCapGain, path=writeFname, na = "", append = T, col_names = T)

