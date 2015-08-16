library(IBrokers)
tws<-twsConnect()
serverVersion(tws)
reqCurrentTime(tws)
reqContractDetails(tws,twsOption("RUT",conId = "200862535"))
reqMktData(tws,twsOption("RUT",conId = "200862535"))

twsDisconnect(tws)
rm(tws)
