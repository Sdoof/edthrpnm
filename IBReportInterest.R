library(dplyr)
library(readr)
library(tidyr)
library(stringr)
rm(list=ls())

#Data File Path
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"

##
# whole sheet
readFname=paste(DataFiles_Path_G,"UXXXXXXX_Interest.csv",sep='')
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
  dplyr::filter(Trades=="Broker Interest Received") %>%
  dplyr::filter(Header=="Data") %>%
  dplyr::select(Currency,Date,Amount) -> InterestWhole

##
# Write to a file
writeFname=paste(DataFiles_Path_G,"InterestReceived-",format(Sys.time(),"%Y%b%d"),".csv",sep='')
write_csv(InterestWhole,
          path=writeFname, na = "", append = F, col_names = T)

