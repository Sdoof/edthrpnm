library(ggplot2)
library(RQuantLib)
library(plyr)
library(dplyr)

#Config File
ConfigFileName_G="ConfigParameters.csv"
DataFiles_Path_G="C:\\Users\\kuby\\edthrpnm\\MarketData\\data\\"

ConfigParameters<-read.table(paste(DataFiles_Path_G,ConfigFileName_G,sep=""),
                             row.names=1, comment.char="#",header=T,stringsAsFactors=F,sep=",")

#File
Underying_Symbol_G=ConfigParameters["Underying_Symbol_G",1]
ResultFiles_Path_G=ConfigParameters["ResultFiles_Path_G",1]

#target spread IDs
SpreadIDs=c(1)

#plot data every this step days
PlotStepDay=3

#Scenario Mode
ScenarioMode="_modelScenario_"
#ScenarioMode="_adjustedScenario_"

#Max Plotting num of points for one image
PlotMAxPointNum=30000

#plotFile
FileChunk="_scenarioPlot_"

getPlotDataframe<-function(PlotDay){
  #process each scenario
  ScenarioNum<-length(modelStimRawlist$stimrslt)
  for(scenario_idx in 1:ScenarioNum){
    start_t<-proc.time()
    SimuNum<-length(modelStimRawlist$stimrslt[[scenario_idx]])
    #process each simulation
    for(sim_idx in 1:SimuNum){
      #In adjustedScenario_, could be liqudated early
      if(modelScenario$resdf[[scenario_idx]][sim_idx,]$liqDay>=PlotDay)
        modelStimRawlist$stimrslt[[scenario_idx]][[sim_idx]]$AdjustDay<-PlotDay
    }
    cat(" scenario ",scenario_idx, " time: ",(proc.time()-start_t)[3])
  }
  
  SimultaionNum<-length(modelStimRawlist$stimrslt[[1]])
  modelStimRawlist %>% rowwise() %>% do(resdf=getStimResultDataFrame(.$stimrslt,SimultaionNum)) -> tmp
  
  #full join day PlotDay's end value
  target_df<-tmp$resdf[[1]]
  for(i in 2:ScenarioNum){
    target_df %>% full_join(tmp$resdf[[i]]) -> target_df
  }
  return(target_df)
}

#create data frames and write to the files
for(SpreadID in SpreadIDs){
  cat("Spred ID:",SpreadID)
  #load modelStimRawlist and modelScenario.
  load(file=paste(ResultFiles_Path_G,Underying_Symbol_G,"_modelStimRawlist_",SpreadID,sep=""))
  load(file=paste(ResultFiles_Path_G,Underying_Symbol_G,ScenarioMode,SpreadID,sep=""))
  SimDays<-length(modelStimRawlist$stimrslt[[1]][[2]]$EvalScore)
  
  maxDay<-modelScenario$resdf
  
  totalstep=SimDays%/%PlotStepDay
  
  PlotDays<-rep(PlotStepDay,times=totalstep)
  PlotDays<- cumsum(PlotDays)
  PlotDays[length(PlotDays)+1]<-SimDays
  
  total_df<-getPlotDataframe(PlotDays[1])
  for(day in 2:length(PlotDays)){
    total_df %>% full_join(getPlotDataframe(PlotDays[day])) -> total_df 
  }
  write.table(total_df,paste(ResultFiles_Path_G,Underying_Symbol_G,FileChunk,SpreadID,".csv",sep=""),quote=T,row.names=F,append=F,sep=",")
} 
rm(day,totalstep,total_df)

for(SpreadID in SpreadIDs){
  cat("Spred ID:",SpreadID)
  plot_df<-read.table(paste(ResultFiles_Path_G,Underying_Symbol_G,FileChunk,SpreadID,".csv",sep=""),comment.char="#",header=T,sep=",")
  print(plot_df)
  
  #read raw data
  load(file=paste(ResultFiles_Path_G,Underying_Symbol_G,"_modelStimRawlist_",SpreadID,sep=""))
  InitUDLY<-modelStimRawlist$stimrslt[[1]][[1]]$IniEvalScore$UDLY
  
  #プロットする点が多すぎる場合はrandam sampleする
  if(nrow(plot_df)>PlotMAxPointNum){
    plot_df[sort(sample(nrow(plot_df),size=PlotMAxPointNum,replace=F)),] -> plot_df_view
    #sorting
    plot_df_view %>%  arrange(liqDay) -> plot_df_view
    #行番号の振り直し
    rownames(plot_df_view) <- c(1:nrow(plot_df_view))
  }else{
    plot_df_view <-plot_df 
  }
  
  #グラフ表示
  gg<-ggplot(plot_df_view,aes(x=udly,y=profit,colour=liqDay))+
    geom_point(alpha=0.3)+
    geom_point(x=InitUDLY,y=0,size=6.0,colour="red",pch=3)+
    ylim(min(plot_df_view$profit),max(plot_df_view$profit))
  print(gg)
  
  gg<-ggplot(plot_df_view,aes(x=udly,y=Delta,colour=liqDay))+
    geom_point(alpha=0.3)+
    geom_point(x=InitUDLY,y=0,size=6.0,colour="red",pch=4)+
    ylim(min(plot_df_view$Delta),max(plot_df_view$Delta))
  print(gg)
  
  gg<-ggplot(plot_df_view,aes(x=udly,y=Vega,colour=liqDay))+
    geom_point(alpha=0.3)+
    geom_point(x=InitUDLY,y=0,size=6.0,colour="red",pch=4)+
    ylim(min(plot_df_view$Vega),max(plot_df_view$Vega))
  print(gg)
  
  # 5倍plotする事に留意する
  if(nrow(plot_df)>ceiling(PlotMAxPointNum/5)){
    plot_df[sort(sample(nrow(plot_df),size=ceiling(PlotMAxPointNum/5),replace=F)),] -> plot_df_view
    #sorting
    plot_df_view %>%  arrange(liqDay) -> plot_df_view
    #行番号の振り直し
    rownames(plot_df_view) <- c(1:nrow(plot_df_view))
  } else{
    plot_df_view <-plot_df 
  }
  
  gg<-ggplot(plot_df_view,aes(x=udly,y=profit))+
    geom_point(alpha=0.2,size=ceiling(plot_df_view$liqDay/min(plot_df_view$liqDay)))+
    geom_point(x=plot_df_view$udly,y=plot_df_view$ThetaEffect,colour="orange",alpha=0.2,size=ceiling(plot_df_view$liqDay/min(plot_df_view$liqDay)))+
    geom_point(x=plot_df_view$udly,y=plot_df_view$GammaEffect,colour="red",alpha=0.2,size=ceiling(plot_df_view$liqDay/min(plot_df_view$liqDay)))+
    geom_point(x=plot_df_view$udly,y=plot_df_view$VegaEffect*ifelse(plot_df_view$Vega>0,-1,1),colour="green",alpha=0.2,size=ceiling(plot_df_view$liqDay/min(plot_df_view$liqDay)))+
    geom_point(x=plot_df_view$udly,y=plot_df_view$DeltaEffect*ifelse(plot_df_view$Delta>0,-1,1),colour="blue",alpha=0.2,size=ceiling(plot_df_view$liqDay/min(plot_df_view$liqDay)))+
    geom_point(x=InitUDLY,y=0,size=4.0,colour="black")+
    geom_point(x=InitUDLY,y=modelStimRawlist$stimrslt[[1]][[1]]$IniEvalScore$ThetaEffect,size=4.0,colour="orange")+
    geom_point(x=InitUDLY,y=modelStimRawlist$stimrslt[[1]][[1]]$IniEvalScore$GammaEffect,size=4.0,colour="red")+
    geom_point(x=InitUDLY,y=modelStimRawlist$stimrslt[[1]][[1]]$IniEvalScore$DeltaEffect*ifelse(modelStimRawlist$stimrslt[[1]][[1]]$IniEvalScore$Delta>0,-1,1),size=4.0,colour="blue")+
    geom_point(x=InitUDLY,y=modelStimRawlist$stimrslt[[1]][[1]]$IniEvalScore$VegaEffect*ifelse(modelStimRawlist$stimrslt[[1]][[1]]$IniEvalScore$Vega>0,-1,1),size=4.0,colour="green")+
    ylim(
      min(c(min(plot_df_view$ThetaEffect),min(plot_df_view$DeltaEffect),
            min(plot_df_view$GammaEffect),min(plot_df_view$VegaEffect),
            min(plot_df_view$profit))),
      max(c(max(plot_df_view$ThetaEffect),max(plot_df_view$DeltaEffect),
            max(plot_df_view$GammaEffect),max(plot_df_view$VegaEffect),
            max(plot_df_view$profit)))
    )  
  print(gg)
  
}

#read files and plot
rm(day,maxDay,totalstep)
rm(gg,SpreadID,InitUDLY,plot_df,plot_df_view,opchain)
rm(modelScenario,modelStimRawlist)
rm(SpreadIDs,PlotStepDay,SimDays,PlotDays)
rm(ConfigFileName_G,ConfigParameters,ScenarioMode,PlotMAxPointNum)
rm(FileChunk,SpreadIDS)
rm(DataFiles_Path_G,ResultFiles_Path_G,Underying_Symbol_G,evalPosStart,evalPosEnd)
rm(getPlotDataframe)

