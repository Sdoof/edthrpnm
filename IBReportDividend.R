library(dplyr)
library(readr)
library(tidyr)
library(stringr)
rm(list=ls())

#Data File Path
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"

##
# whole sheet
readFname=paste(DataFiles_Path_G,"U1026713_Div-20170102_20171226.csv",sep='')
sheet_colnames=c("Trades","Header","Currency","Date","Description","Amount","Code")
sheet_whole=read_csv(readFname,col_names = sheet_colnames,
                     col_types = cols(
                       Trades = col_character(),
                       Header = col_character(),
                       Currency = col_character(),
                       Date = col_character(),
                       Description = col_character(),
                       Amount = col_double(),
                       Code = col_character()
                     ))
print(sheet_whole,n=nrow(sheet_whole),width = Inf)


sheet_whole %>%
  dplyr::filter(Trades=="Dividends"&Currency=="USD") -> DividendsUSD

sheet_whole %>%
  dplyr::filter(Trades=="Dividends"&Currency=="HKD") -> DividendsHKD

sheet_whole %>%
  dplyr::filter(Trades=="Dividends"&Currency=="SGD") -> DividendsSGD

sheet_whole %>%
  dplyr::filter(Trades=="Dividends"&Currency=="AUD") -> DividendsAUD

sheet_whole %>%
  dplyr::filter(Trades=="Dividends"&Currency=="GBP") -> DividendsGBP

sheet_whole %>%
  dplyr::filter(stringr::str_detect(Trades,"Financial")&stringr::str_detect(Trades,"Instrument")) -> InstrumentsData

InstrumentsData %>%
  dplyr::rename(AssetCategory=Currency,Symbol=Date,Code=Amount,SecurityID=Code) %>%
  dplyr::filter(!is.na(Code)) -> InstrumentsData
InstrumentsData %>%
  dplyr::filter(AssetCategory=="Stocks") -> InstrumentsData

#Exchange Rate
# USDJPY
readFname=paste(DataFiles_Path_G,"USDJPY.csv",sep='')
USDJPY_colnames=c("Date","Close")
USDJPY=read_csv(readFname,col_names = USDJPY_colnames,
                col_types = cols(
                  Date = col_character(),
                  Close= col_double()
                ))
print(USDJPY,n=nrow(USDJPY),width = Inf)
#HKDJPY
readFname=paste(DataFiles_Path_G,"HKDJPY.csv",sep='')
HKDJPY_colnames=c("Date","Start","High","Low","Close")
HKDJPY=read_csv(readFname,col_names = HKDJPY_colnames,
                col_types = cols(
                  Date = col_character(),
                  Start = col_double(),
                  High = col_double(),
                  Low = col_double(),
                  Close = col_double()
                ))
print(HKDJPY,n=nrow(HKDJPY),width = Inf)

#SGDJPY
readFname=paste(DataFiles_Path_G,"SGDJPY.csv",sep='')
SGDJPY_colnames=c("Date","Start","High","Low","Close")
SGDJPY=read_csv(readFname,col_names = SGDJPY_colnames,
                col_types = cols(
                  Date = col_character(),
                  Start = col_double(),
                  High = col_double(),
                  Low = col_double(),
                  Close = col_double()
                ))
print(SGDJPY,n=nrow(SGDJPY),width = Inf)

#AUDJPY
readFname=paste(DataFiles_Path_G,"AUDJPY.csv",sep='')
AUDJPY_colnames=c("Date","Start","High","Low","Close")
AUDJPY=read_csv(readFname,col_names = AUDJPY_colnames,
                col_types = cols(
                  Date = col_character(),
                  Start = col_double(),
                  High = col_double(),
                  Low = col_double(),
                  Close = col_double()
                ))
print(AUDJPY,n=nrow(AUDJPY),width = Inf)

#GBPJPY
readFname=paste(DataFiles_Path_G,"GBPJPY.csv",sep='')
GBPJPY_colnames=c("Date","Start","High","Low","Close")
GBPJPY=read_csv(readFname,col_names = GBPJPY_colnames,
                col_types = cols(
                  Date = col_character(),
                  Start = col_double(),
                  High = col_double(),
                  Low = col_double(),
                  Close = col_double()
                ))
print(GBPJPY,n=nrow(GBPJPY),width = Inf)

##
# join Currency Rate


# DividendsUSD
DividendsUSD %>%
  dplyr::mutate(DateForMatch=format(as.Date(Date,format="%Y-%m-%d"),"%Y/%m/%d")) -> DividendsUSD
USDJPY %>%
  dplyr::mutate(DateForMatch=format(as.Date(Date,format="%Y/%m/%d"),"%Y/%m/%d")) -> USDJPY
DividendsUSD %>%
  left_join(USDJPY %>% mutate(Date=NULL)) %>%
  dplyr::rename(USDJPY=Close) -> DividendsUSD

# DividendsHKD
DividendsHKD %>%
  dplyr::mutate(DateForMatch=format(as.Date(Date,format="%Y-%m-%d"),"%Y/%m/%d")) -> DividendsHKD
HKDJPY %>%
  dplyr::mutate(DateForMatch=format(as.Date(Date,format="%Y/%m/%d"),"%Y/%m/%d")) -> HKDJPY
DividendsHKD %>%
  left_join(HKDJPY %>% mutate(Date=NULL,Start=NULL,High=NULL,Low=NULL)) %>%
  dplyr::rename(HKDJPY=Close) -> DividendsHKD

# DividendsSGD
DividendsSGD %>%
  dplyr::mutate(DateForMatch=format(as.Date(Date,format="%Y-%m-%d"),"%Y/%m/%d")) -> DividendsSGD
SGDJPY %>%
  dplyr::mutate(DateForMatch=format(as.Date(Date,format="%Y/%m/%d"),"%Y/%m/%d")) -> SGDJPY
DividendsSGD %>%
  left_join(SGDJPY %>% mutate(Date=NULL,Start=NULL,High=NULL,Low=NULL)) %>%
  dplyr::rename(SGDJPY=Close) -> DividendsSGD

# DividendsAUD
DividendsAUD %>%
  dplyr::mutate(DateForMatch=format(as.Date(Date,format="%Y-%m-%d"),"%Y/%m/%d")) -> DividendsAUD
AUDJPY %>%
  dplyr::mutate(DateForMatch=format(as.Date(Date,format="%Y/%m/%d"),"%Y/%m/%d")) -> AUDJPY
DividendsAUD %>%
  left_join(AUDJPY %>% mutate(Date=NULL,Start=NULL,High=NULL,Low=NULL)) %>%
  dplyr::rename(AUDJPY=Close) -> DividendsAUD

# DividendsGBP
DividendsGBP %>%
  dplyr::mutate(DateForMatch=format(as.Date(Date,format="%Y-%m-%d"),"%Y/%m/%d")) -> DividendsGBP
GBPJPY %>%
  dplyr::mutate(DateForMatch=format(as.Date(Date,format="%Y/%m/%d"),"%Y/%m/%d")) -> GBPJPY
DividendsGBP %>%
  left_join(GBPJPY %>% mutate(Date=NULL,Start=NULL,High=NULL,Low=NULL)) %>%
  dplyr::rename(GBPJPY=Close) -> DividendsGBP

##
# Adjust Currency Rate to TTB

DividendsUSD %>%
  dplyr::mutate(USDJPY=USDJPY-0.5) -> DividendsUSD

DividendsHKD %>%
  dplyr::mutate(HKDJPY=HKDJPY*0.96) -> DividendsHKD

DividendsSGD %>%
  dplyr::mutate(SGDJPY=SGDJPY*0.96) -> DividendsSGD

DividendsAUD %>%
  dplyr::mutate(AUDJPY=AUDJPY*0.96) -> DividendsAUD

DividendsGBP %>%
  dplyr::mutate(GBPJPY=GBPJPY*0.96) -> DividendsGBP

##
# join Instrument

DividendsUSD %>%
  dplyr::mutate(Symbol=str_match(Description, "^[:alnum:]*")) -> DividendsUSD
DividendsUSD %>% dplyr::mutate(Description=NULL) %>% 
  dplyr::left_join(InstrumentsData %>% dplyr::select(Symbol,Description)) -> DividendsUSD

DividendsHKD %>%
  dplyr::mutate(Symbol=str_match(Description, "^[:alnum:]*")) -> DividendsHKD
DividendsHKD %>% dplyr::mutate(Description=NULL) %>% 
  dplyr::left_join(InstrumentsData %>% dplyr::select(Symbol,Description)) -> DividendsHKD

DividendsSGD %>%
  dplyr::mutate(Symbol=str_match(Description, "^[:alnum:]*")) -> DividendsSGD
DividendsSGD %>% dplyr::mutate(Description=NULL) %>% 
  dplyr::left_join(InstrumentsData %>% dplyr::select(Symbol,Description)) -> DividendsSGD

DividendsAUD %>%
  dplyr::mutate(Symbol=str_match(Description, "^[:alnum:]*")) -> DividendsAUD
DividendsAUD %>% dplyr::mutate(Description=NULL) %>% 
  dplyr::left_join(InstrumentsData %>% dplyr::select(Symbol,Description)) -> DividendsAUD

#just irregular case, I don't know why.
InstrumentsData %>% 
  dplyr::bind_rows(
    InstrumentsData %>% dplyr::filter(Symbol=="AZNl") %>% dplyr::mutate(Symbol="AZN")
  ) -> InstrumentsData

DividendsGBP %>%
  dplyr::mutate(Symbol=str_match(Description, "^[:alnum:]*")) -> DividendsGBP
DividendsGBP %>% dplyr::mutate(Description=NULL) %>% 
  dplyr::left_join(InstrumentsData %>% dplyr::select(Symbol,Description)) -> DividendsGBP

##
# Write to a file
writeFname=paste(DataFiles_Path_G,"DividendsBookeep.csv",sep='')
write_excel_csv(DividendsUSD %>% select(DateForMatch,Description,Amount,USDJPY,Symbol),
                path=writeFname, na = "", append = F, col_names = T)
write_excel_csv(DividendsHKD %>% select(DateForMatch,Description,Amount,HKDJPY,Symbol),
                path=writeFname, na = "", append = T, col_names = T)
write_excel_csv(DividendsSGD %>% select(DateForMatch,Description,Amount,SGDJPY,Symbol),
                path=writeFname, na = "", append = T, col_names = T)
write_excel_csv(DividendsAUD %>% select(DateForMatch,Description,Amount,AUDJPY,Symbol),
                path=writeFname, na = "", append = T, col_names = T)
#write_excel_csv(DividendsGBP %>% select(DateForMatch,Description,Amount,GBPJPY,Symbol),
#                path=writeFname, na = "", append = T, col_names = T)
write_excel_csv(DividendsGBP %>% select(DateForMatch,Description,Amount,Symbol),
                path=writeFname, na = "", append = T, col_names = T)
