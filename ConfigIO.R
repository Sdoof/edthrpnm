##Configuration File Read/Write
ConfigFileName_G="ConfigParameters.csv"
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"

#row.names=1 measn 1st column is row names.
ConfigParameters<-read.table(paste(DataFiles_Path_G,ConfigFileName_G,sep=""),
                             row.names=1, comment.char="#",header=T,stringsAsFactors=F,sep=",")

(ConfigParameters["CALENDAR_G",1])
(ConfigParameters["Underying_Symbol_G",1])
(as.numeric(ConfigParameters["riskFreeRate_G",1]))
(as.numeric(ConfigParameters["divYld_G",1]))
(as.numeric(ConfigParameters["OpType_Put_G",1]))
(as.numeric(ConfigParameters["OpType_Call_G",1]))
(as.numeric(ConfigParameters["TimeToExp_Limit_Closeness_G",1]))
(ConfigParameters["ResultFiles_Path_G",1])
(as.numeric(ConfigParameters["holdDays",1]))
(as.numeric(ConfigParameters["dviv_caldays",1]))
(as.numeric(ConfigParameters["PosMultip",1]))
(as.numeric(ConfigParameters["SimEvalPosStart",1]))
(as.numeric(ConfigParameters["SimEvalPosEnd",1]))
(as.numeric(ConfigParameters["SimultaionNum",1]))
(as.numeric(ConfigParameters["MaxSimDay",1]))
#'-'を','に置換し#文字列をRコマンドとして実行
(eval(parse(text=gsub("-",",",ConfigParameters["SimScenarioWeight",1]))))
(as.numeric(ConfigParameters["SimReportPosStart",1]))
(as.numeric(ConfigParameters["SimReportPosEnd",1]))
(ConfigParameters["SimReportScenarioMode",1])
(as.numeric(ConfigParameters["SimInspct.Spread",1]))
(as.numeric(ConfigParameters["SimInspct.Scenario",1]))
(as.numeric(ConfigParameters["SimInspct.SimID",1]))
(ConfigParameters["SimInspctScenarioMode",1])
(as.numeric(ConfigParameters["PlaybackPosStart",1]))
(as.numeric(ConfigParameters["PlaybackPosEnd",1]))

#test
#read.table(paste(ConfigParameters["ResultFiles_Path_G",1],"1Cb.csv",sep=""),header=F,stringsAsFactors=F,sep=",")


rm(ConfigParameters)
rm(ConfigFileName_G,DataFiles_Path_G)
rm(CALENDAR_G,Underying_Symbol_G,riskFreeRate_G,divYld_G,OpType_Put_G,OpType_Call_G)
rm(TimeToExp_Limit_Closeness_G,ResultFiles_Path_G)
