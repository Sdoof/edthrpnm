## functions used by DataProvision

makeOpchainContainer<-function(){  
  #read data file
  rf_<-paste(DataFiles_Path_G,Underying_Symbol_G,ProcessFileName,sep="")
  opch_pr_<-read.table(rf_,header=T,skipNul=T,stringsAsFactors=F,sep=",")
  
  #standarize (unify) data expression
  opch_pr_$Date=format(as.Date(opch_pr_$Date,format="%Y/%m/%d"),"%Y/%b/%d")
  opch_pr_$ExpDate=format(as.Date(opch_pr_$ExpDate,format="%Y/%m/%d"),"%Y/%b/%d")
  
  opch_pr_ %>% dplyr::select(Strike,ContactName,TYPE,Date,ExpDate,Last,Bid,Ask) %>% 
    dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) -> opch_pr_
  
  na.omit(opch_pr_)->opch_pr_
  
  opch_pr_$Position<-0
  opch_pr_$Bid=as.numeric(opch_pr_$Bid)
  opch_pr_$Ask=as.numeric(opch_pr_$Ask)
  opch_pr_$Price<-(opch_pr_$Bid+opch_pr_$Ask)/2
  
  opch_pr_$Theta<-opch_pr_$Vega<-opch_pr_$Gamma<-opch_pr_$Delta<-0
  opch_pr_$IV<-opch_pr_$OrigIV<-opch_pr_$Rho<-0
  
  #read historical price
  rf_<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Hist.csv",sep="")
  histPrc_<-read.table(rf_,header=T,sep=",",nrows=1000)
  histPrc_<-data.frame(Date=histPrc_$Date,UDLY=histPrc_$Close)
  #standarize (unify) data expression
  histPrc_$Date=format(as.Date(histPrc_$Date,format="%Y/%m/%d"),"%Y/%b/%d")
  
  #remove not used row
  #opch_pr_<-subset(opch_pr_,Volume!=0)
  opch_pr2<-merge(opch_pr_,histPrc_,all.x=T)
  opch_pr_<-opch_pr2;
  
  na.omit(opch_pr_)->opch_pr_
  
  return(opch_pr_)
}

makeFOPChainContainer<-function(){  
  #read data file
  rf_<-paste(DataFiles_Path_G,Underying_Symbol_G,ProcessFileName,sep="")
  opch_pr_<-read.table(rf_,header=T,sep=",")
  
  opch_pr_ %>% dplyr::select(Strike,ContactName,TYPE,Date,ExpDate,Last,Bid,Ask) %>% 
    dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) -> opch_pr_
  
  opch_pr_$Price<-(opch_pr_$Bid+opch_pr_$Ask)/2
  #standarize (unify) data expression
  opch_pr_$Date=format(as.Date(opch_pr_$Date,format="%Y/%m/%d"),"%Y/%b/%d")
  opch_pr_$ExpDate=format(as.Date(opch_pr_$ExpDate,format="%Y/%m/%d"),"%Y/%b/%d")
  
  #read historical price
  rf_<-paste(DataFiles_Path_G,Underying_Symbol_G,"FUT.csv",sep="")
  futPrc_<-read.table(rf_,header=T,sep=",",nrows=1000)
  futPrc_$UDLY=(futPrc_$Bid+futPrc_$Ask)/2
  futPrc_ %>% dplyr::select(ContactName,Date,ExpDate,UDLY) %>% 
    dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d")) -> futPrc_
  #standarize (unify) data expression
  futPrc_$Date=format(as.Date(futPrc_$Date,format="%Y/%m/%d"),"%Y/%b/%d")
  futPrc_$ExpDate=format(as.Date(futPrc_$ExpDate,format="%Y/%m/%d"),"%Y/%b/%d")
  #Change Column names into Future Specefic
  futPrc_$FutContactName=futPrc_$ContactName
  futPrc_$ContactName=NULL
  futPrc_$FutExpDate=futPrc_$ExpDate
  futPrc_$ExpDate=NULL
  
  #Selecting Appropriate FutExPDate and UDLY
  opch_pr_ %>% dplyr::left_join(futPrc_,by="Date") -> tmp
  tmp$Tdiff = as.numeric(difftime(as.Date(tmp$FutExpDate,format="%Y/%m/%d"),as.Date(tmp$ExpDate,format="%Y/%m/%d")), units="days")
  tmp %>% dplyr::filter(Tdiff>=0) -> tmp
  
  #groub_by(Some1,Some2,Some3) acts as Unique Key (One or combination of Attribute(s) ) 
  #%>% Then other Columns(Attributes) are summarised.
  tmp %>% dplyr::group_by(Strike,Date,ExpDate,TYPE) %>% dplyr::summarise(ContactName=ContactName[which.min(Tdiff)],
                                                                         Last=Last[which.min(Tdiff)],Bid=Bid[which.min(Tdiff)],
                                                                         Ask=Ask[which.min(Tdiff)],Price=Price[which.min(Tdiff)],
                                                                         UDLY=UDLY[which.min(Tdiff)],
                                                                         FutExpDate=FutExpDate[which.min(Tdiff)],
                                                                         TdiffMin=min(Tdiff)) %>% as.data.frame() -> tmp
  tmp %>% arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) -> tmp
  
  #reassigne tmp as opch_pr_
  opch_pr_ <- tmp
  #remove unnecessary columns
  opch_pr_$TdiffMin<-NULL
  
  ##
  # Approximate Forward Rate
  time_DateToFutExpDate=as.numeric(difftime(as.Date(opch_pr_$FutExpDate,format="%Y/%m/%d"),as.Date(opch_pr_$Date,format="%Y/%m/%d")), units="days")/365
  opch_pr_$DateToFutExpDate=time_DateToFutExpDate
  opch_pr_ %>% dplyr::group_by(Date,FutExpDate) %>% dplyr::summarise(UDLY=UDLY[which.min(DateToFutExpDate)],
                                                                     DateToFutExpDate=DateToFutExpDate[which.min(DateToFutExpDate)])  %>% as.data.frame() -> tmp
  tmp %>% dplyr::arrange(desc(as.Date(Date,format="%Y/%m/%d")),as.Date(FutExpDate,format="%Y/%m/%d")) -> tmp
  
  # approximate forward rate by using 2 latest FUT Price, solving the equation that the 1st FUT's Spot price equals 2nd one.
  # for the details, consult paper specification
  fwrd_rate=divYld_G
  if(nrow(tmp)>=2){
    tmp$UDLY[1]*(1+fwrd_rate*tmp$DateToFutExpDate[1])/(1+riskFreeRate_G*tmp$DateToFutExpDate[1])
    tmp$UDLY[2]*(1+fwrd_rate*tmp$DateToFutExpDate[2])/(1+riskFreeRate_G*tmp$DateToFutExpDate[2])
    A_tmp=tmp$UDLY[2]/tmp$UDLY[1]*(1+riskFreeRate_G*tmp$DateToFutExpDate[1])/(1+riskFreeRate_G*tmp$DateToFutExpDate[2])
    fwrd_rate=(A_tmp-1)/(tmp$DateToFutExpDate[1]-A_tmp*tmp$DateToFutExpDate[2])
  }
  opch_pr_$UDLY=opch_pr_$UDLY*(1+fwrd_rate*opch_pr_$DateToFutExpDate)/(1+riskFreeRate_G*opch_pr_$DateToFutExpDate)
  ##
  # set the estimaed forward rate as global variable divYld_G
  divYld_G <<- fwrd_rate
  
  #add Greek and IV columns
  opch_pr_$Position<-0
  opch_pr_$Theta<-opch_pr_$Vega<-opch_pr_$Gamma<-opch_pr_$Delta<-0
  opch_pr_$IV<-opch_pr_$OrigIV<-opch_pr_$Rho<-0
  
  return(opch_pr_)
}

##
#  ATMIV IVIDX Moneyness etc. calculation for the opchain to be completed.
makePosition <- function(opch,isSkewCalc){
  #standarize (unify) data expression
  opch$Date=format(as.Date(opch$Date,format="%Y/%m/%d"),"%Y/%b/%d")
  opch$ExpDate=format(as.Date(opch$ExpDate,format="%Y/%m/%d"),"%Y/%b/%d")
  
  rf_<-paste(DataFiles_Path_G,Underying_Symbol_G,"_IV.csv",sep="")
  histIV<-read.table(rf_,header=T,sep=",",nrows=1000)
  rm(rf_)
  #standarize (unify) data expression
  histIV$Date=format(as.Date(histIV$Date,format="%Y/%m/%d"),"%Y/%b/%d")
  
  # volatility and moneyness calculation complete
  opch$Last<-opch$Bid<-opch$Ask<-opch$Ask<-opch$Volume<-opch$OI<-NULL
  #Histrical Volatility(Index). VOlatility % to DN. Column name Close to IVIDX
  histIV<-data.frame(Date=histIV$Date,IVIDX=(histIV$Close/100))
  
  ##Historical Implied Volatility(index) merge
  opch<-merge(opch,histIV,all.x=T)
  rm(histIV)
  
  #if >1.0 Strike is right(bigger) than UDLY, else(<1.0) left(smaller)
  opch$Moneyness.Frac<-opch$Strike/opch$UDLY
  # As fraction of UDLY. positive indicates OOM, negative ITM.
  opch$HowfarOOM<-(1-opch$Moneyness.Frac)*opch$TYPE
  #As fraction of Implied SD. >0 OOM, <0 ITM
  opch$Moneyness.SDFrac<-(opch$UDLY-opch$Strike)*opch$TYPE/(opch$UDLY*opch$IVIDX)
  
  #construct Time axis for regression. In this case expressed as Month.
  get.busdays.between(start=opch$Date,end=opch$ExpDate)
  bdays_per_month<-252/12
  opch$TimeToExpDate<-get.busdays.between(start=opch$Date,end=opch$ExpDate)/bdays_per_month
  rm(bdays_per_month)
  
  #sort
  opch %>% dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) -> opch
  
  ##END Got complete opch
  
  #Get ATM Implied Volatilities for each option types.
  ##START Create atmiv
  opch %>% dplyr::group_by(Date,ExpDate,TYPE) %>% dplyr::filter(HowfarOOM>0) %>% 
    #Get the OrigIV where HowfarOOM is minimum, ie. IV at ATM.
    dplyr::summarise(num=n(),OOM=min(abs(HowfarOOM)),min.i=which.min(abs(HowfarOOM)),ATMIV=OrigIV[which.min(abs(HowfarOOM))],
                     TimeToExpDate=TimeToExpDate[which.min(abs(HowfarOOM))],IVIDX=IVIDX[which.min(abs(HowfarOOM))],
                     UDLY=UDLY[which.min(abs(HowfarOOM))],Strike=Strike[which.min(abs(HowfarOOM))],
                     Moneyness.Frac=Moneyness.Frac[which.min(abs(HowfarOOM))]) %>% 
    as.data.frame() -> atmiv
  atmiv %>% dplyr::select(Date,ExpDate,TYPE,ATMIV,Strike,UDLY,IVIDX,TimeToExpDate,Moneyness.Frac) %>% as.data.frame() -> atmiv
  
  #sorting
  atmiv %>% dplyr::arrange(desc(TYPE),as.Date(ExpDate,format="%Y/%m/%d"),as.Date(Date,format="%Y/%m/%d")) -> atmiv
  opch %>% dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) -> opch
  
  #adjust AMTIV
  if(isSkewCalc==FALSE){
    load.Skew()
    SkewModel
    #load.Skew("_Put")
    #SkewModel_Put
    #load.Skew(pattern="_Call")
    #SkewModel_Call
    ATMIV_adj<-adjustATMIV(atmiv)
    atmiv$ATMIV<-ATMIV_adj
  }
  
  opch <- merge(opch,
                atmiv %>% dplyr::select(Date,ExpDate,TYPE,ATMIV) %>% as.data.frame(),
                by.x=c("Date","ExpDate","TYPE"),by.y=c("Date","ExpDate","TYPE"),all.x=T)
  
  #sorting
  atmiv %>% dplyr::arrange(desc(TYPE),as.Date(ExpDate,format="%Y/%m/%d"),as.Date(Date,format="%Y/%m/%d")) -> atmiv
  opch %>% dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) -> opch
  
  #Calculate Moneyness.Nm using just merged ATMIV
  opch$Moneyness.Nm<-log(opch$Moneyness.Frac)/opch$ATMIV/sqrt(opch$TimeToExpDate)
  opchain<-opch
  return(opchain)
}

##
#  atmiv not adjusted usig SkewModel
makeNotAdjustedBySkewATMiv <- function(opch){
  #standarize (unify) data expression
  opch$Date=format(as.Date(opch$Date,format="%Y/%m/%d"),"%Y/%b/%d")
  opch$ExpDate=format(as.Date(opch$ExpDate,format="%Y/%m/%d"),"%Y/%b/%d")
  
  # volatility and moneyness calculation complete
  opch$Last<-opch$Bid<-opch$Ask<-opch$Ask<-opch$Volume<-opch$OI<-NULL
  
  #if >1.0 Strike is right(bigger) than UDLY, else(<1.0) left(smaller)
  opch$Moneyness.Frac<-opch$Strike/opch$UDLY
  # As fraction of UDLY. positive indicates OOM, negative ITM.
  opch$HowfarOOM<-(1-opch$Moneyness.Frac)*opch$TYPE
  #As fraction of Implied SD. >0 OOM, <0 ITM
  #opch$Moneyness.SDFrac<-(opch$UDLY-opch$Strike)*opch$TYPE/(opch$UDLY*opch$IVIDX)
  
  #construct Time axis for regression. In this case expressed as Month.
  get.busdays.between(start=opch$Date,end=opch$ExpDate)
  bdays_per_month<-252/12
  opch$TimeToExpDate<-get.busdays.between(start=opch$Date,end=opch$ExpDate)/bdays_per_month
  rm(bdays_per_month)
  
  #sort
  opch %>% dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) -> opch
  
  ##END Got complete opch
  
  #Get ATM Implied Volatilities for each option types.
  ##START Create atmiv
  opch %>% dplyr::group_by(Date,ExpDate,TYPE) %>% dplyr::filter(HowfarOOM>0) %>% 
    #Get the OrigIV where HowfarOOM is minimum, ie. IV at ATM.
    dplyr::summarise(num=n(),OOM=min(abs(HowfarOOM)),min.i=which.min(abs(HowfarOOM)),ATMIV=OrigIV[which.min(abs(HowfarOOM))],
                     TimeToExpDate=TimeToExpDate[which.min(abs(HowfarOOM))],#IVIDX=IVIDX[which.min(abs(HowfarOOM))],
                     UDLY=UDLY[which.min(abs(HowfarOOM))],Strike=Strike[which.min(abs(HowfarOOM))],
                     Moneyness.Frac=Moneyness.Frac[which.min(abs(HowfarOOM))]) %>% 
    as.data.frame() -> atmiv
  atmiv %>% dplyr::select(Date,ExpDate,Strike,UDLY,TYPE,ATMIV,TimeToExpDate) %>% as.data.frame() -> atmiv
  
  #sorting
  atmiv %>% dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE)) -> atmiv
  
  return(atmiv)
}
##
#  Using SkewModel, adjust ATMIV to reflect the differnce between Strike and UDLY
adjustATMIV <- function(atmiv){
  displacement<-log(atmiv$Moneyness.Frac)/atmiv$ATMIV/sqrt(atmiv$TimeToExpDate)
  smileCurve<-get.predicted.spline.skew(SkewModel,displacement)
  ATMIV_adjst<-atmiv$ATMIV/smileCurve
  return(ATMIV_adjst)
}

#called by filterPosition
filterVerticalSpread <- function(opchain,
                                 Delta_Limit_Put_MIN,Delta_Limit_Put_MAX,
                                 Delta_Limit_Call_MIN,Delta_Limit_Call_MAX,
                                 VerticalSpread_FilterPtn,PriceInterval,Remainder){
  if(VerticalSpread_FilterPtn==1 || VerticalSpread_FilterPtn==3){
    opchain %>%  dplyr::filter(ExpDate==TARGET_EXPDATE,TYPE==OpType_Put_G) %>% dplyr::filter(abs(Delta)>Delta_Limit_Put_MIN)  %>% 
      dplyr::filter(abs(Delta)<Delta_Limit_Put_MAX) %>% dplyr::filter((Strike%%PriceInterval$Put)==Remainder$Put) -> opchain_cond_2_Put
    
    opchain %>%  dplyr::filter(ExpDate==TARGET_EXPDATE,TYPE==OpType_Call_G) %>% dplyr::filter(abs(Delta)>Delta_Limit_Call_MIN)  %>% 
      dplyr::filter(abs(Delta)<Delta_Limit_Call_MAX) %>% dplyr::filter((Strike%%PriceInterval$Call)==Remainder$Call) -> opchain_cond_2_Call
    
    opchain_cond_2_Put %>%  dplyr::full_join(opchain_cond_2_Call) %>% 
      dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) %>%
      distinct() -> opchain_cond_2
    
    if(VerticalSpread_FilterPtn==1)
      opchain_cond<-opchain_cond_2
  }
  
  if(VerticalSpread_FilterPtn==2 || VerticalSpread_FilterPtn==3){
    
    opchain %>%  dplyr::filter(ExpDate==TARGET_EXPDATE,TYPE==OpType_Put_G) %>% dplyr::filter(abs(Delta)>Delta_Limit_Put_MIN)  %>% 
      dplyr::filter(abs(Delta)<Delta_Limit_Put_MAX) %>% dplyr::filter((Strike%%25)==0) -> tmp_put_cond
    
    opchain %>%  dplyr::filter(ExpDate==TARGET_EXPDATE,TYPE==OpType_Put_G) %>% dplyr::filter(abs(Delta)>Delta_Limit_Call_MIN)  %>% 
      dplyr::filter(abs(Delta)<Delta_Limit_Call_MAX) %>% dplyr::filter((Strike%%25)==0) -> tmp_call_cond
    
    tmp_put_cond  %>% dplyr::full_join(tmp_call_cond) %>%
      dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) %>%
      dplyr::distinct() -> opchain_cond
  }
  
  if(VerticalSpread_FilterPtn==3)
    opchain_cond %>% dplyr::full_join(opchain_cond_2) %>% 
    dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) %>%
    dplyr::distinct() -> opchain_cond
  
  return(opchain_cond)
  
}

#called by filterPosition
filterDiagonalSpread <- function(opchain, TARGET_D,
                                 Delta_Limit_Put_MIN,Delta_Limit_Put_MAX,
                                 Delta_Limit_Call_MIN,Delta_Limit_Call_MAX){
  
  opchain %>%  dplyr::filter(ExpDate==TARGET_D,TYPE==OpType_Put_G) %>% dplyr::filter(abs(Delta)>Delta_Limit_Put_MIN)  %>% 
    dplyr::filter(abs(Delta)<Delta_Limit_Put_MAX) -> opchain_diag_Put
  
  opchain %>%  dplyr::filter(ExpDate==TARGET_D,TYPE==OpType_Call_G) %>% dplyr::filter(abs(Delta)>Delta_Limit_Call_MIN)  %>% 
    dplyr::filter(abs(Delta)<Delta_Limit_Call_MAX) -> opchain_diag_Call
  
  opchain_diag_Put %>%  dplyr::full_join(opchain_diag_Call) %>% 
    dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) %>%
    dplyr::distinct() -> opchain_diag
  
  return(opchain_diag)
}

##
# filter position by Date and Delta
# Delta_Limit_(Put/Call)_(MAX/MIN) c(IronCondor{VerticalSpread},DIAGONAL_FRONT,DIAGONAL_BACK)
filterPosition <- function(opchain,
                           #Put{OOM,OOM}/ Call{OOM,OOM}
                           #Delta_Limit_Put_MAX=c(0,0.32,0.30),Delta_Limit_Put_MIN=c(0,0.08,0.08),
                           #Delta_Limit_Call_MAX=c(0,0.32,0.30),Delta_Limit_Call_MIN=c(0,0.08,0.08),
                           #Put{ATM,ATM}/Call{ATM,ATM}
                           #Delta_Limit_Put_MAX=c(0,0.55,0.55),Delta_Limit_Put_MIN=c(0,0.25,0.25),
                           #Delta_Limit_Call_MAX=c(0,0.55,0.55),Delta_Limit_Call_MIN=c(0,0.25,0.25),
                           #Put{OOM,OOM}/Call{ATM,ATM}
                           #Delta_Limit_Put_MAX=c(0,0.30,0.30),Delta_Limit_Put_MIN=c(0,0.11,0.11),
                           #Delta_Limit_Call_MAX=c(0,0.55,0.55),Delta_Limit_Call_MIN=c(0,0.25,0.25),
                           #Put{ATM,ATM}/Call{OOM,OOM}
                           #Delta_Limit_Put_MAX=c(0,0.55,0.55),Delta_Limit_Put_MIN=c(0,0.25,0.25),
                           #Delta_Limit_Call_MAX=c(0,0.30,0.30),Delta_Limit_Call_MIN=c(0,0.11,0.11),
                           #Delta_Limit_Call_MAX=c(0,0.32,0.30),Delta_Limit_Call_MIN=c(0,0.08,0.08),
                           #CALL ONLY{ALL,ALL}
                           #Delta_Limit_Put_MAX=c(0,0,0),Delta_Limit_Put_MIN=c(0,0,0),
                           #Delta_Limit_Call_MAX=c(0,0.55,0.55),Delta_Limit_Call_MIN=c(0,0.08,0.08),
                           #Put{ATM,ATM}/Call{ATM,OOM} TUNED
                           Delta_Limit_Put_MAX=c(0,0.5,0.5),Delta_Limit_Put_MIN=c(0,0.10,0.10),
                           Delta_Limit_Call_MAX=c(0,0.5,0.5),Delta_Limit_Call_MIN=c(0,0.08,0.08),
                           TARGET_EXPDATE,TARGET_EXPDATE_FRONT,TARGET_EXPDATE_BACK){
  ##
  #  Filter Target Ranges 
  # VerticalSpread (1:PriceInterval 2:25 3:PriceInterval and 25)
  VerticalSpread_FilterPtn=3
  
  # selected price pattern
  PriceInterval<-vector("list",2)
  PriceInterval[[1]]=10
  PriceInterval[[2]]=10
  names(PriceInterval)=c("Put","Call")
  Remainder<-vector("list",2)
  Remainder[[1]]=0
  Remainder[[2]]=0
  names(Remainder)=c("Put","Call")
  
  ## Vertical Spread 
  opchain_cond=filterVerticalSpread(opchain,
                                    Delta_Limit_Put_MIN[1],Delta_Limit_Put_MAX[1],
                                    Delta_Limit_Call_MIN[1],Delta_Limit_Call_MAX[1],
                                    VerticalSpread_FilterPtn,PriceInterval,Remainder)
  
  ## DIAGONAL Spread Front Date
  opchain_diag_F=filterDiagonalSpread(opchain, TARGET_D=TARGET_EXPDATE_FRONT,
                                      Delta_Limit_Put_MIN[2],Delta_Limit_Put_MAX[2],
                                      Delta_Limit_Call_MIN[2],Delta_Limit_Call_MAX[2])
  
  ## DIAGONAL Spread Back Date
  opchain_diag_B=filterDiagonalSpread(opchain, TARGET_D=TARGET_EXPDATE_BACK,
                                      Delta_Limit_Put_MIN[3],Delta_Limit_Put_MAX[3],
                                      Delta_Limit_Call_MIN[3],Delta_Limit_Call_MAX[3])
  
  #Join them all
  opchain_cond %>%  dplyr::full_join(opchain_diag_B) %>% dplyr::full_join(opchain_diag_F) %>%
    dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) %>%
    dplyr::distinct() -> opchain_ret
  
  return(opchain_ret)
}


## Interporating Strike Price if necessary
if(isInterpolatedPrice){
  createInterpolatedPrice <- function(Date=DateTmp,
                                      ExpDate=ExpDateTmp,
                                      TYPE=TYPETmp,
                                      Strike=StrikeTmp,
                                      atmiv){
    
    interpolated=data.frame(Date=Date,ExpDate=ExpDate,TYPE=TYPE,Strike=Strike)
    
    interpolated %>% 
      dplyr::left_join(atmiv,by=c("Date","ExpDate","TYPE")) -> interpolated
    
    interpolated$Moneyness.Frac=interpolated$Strike/interpolated$UDLY
    interpolated$TimeToExpDate=get.busdays.between(start=interpolated$Date,end=interpolated$ExpDate)/(252/12)
    interpolated$Moneyness.Nm=log(interpolated$Moneyness.Frac)/interpolated$ATMIV/sqrt(interpolated$TimeToExpDate)
    interpolated$OrigIV=interpolated$ATMIV*get.predicted.spline.skew(SkewModel,interpolated$Moneyness.Nm)
    
    tmp<-set.EuropeanOptionValueGreeks(interpolated)
    interpolated$Price<-tmp$Price
    interpolated$Delta<-tmp$Delta
    interpolated$Gamma<-tmp$Gamma
    interpolated$Vega<-tmp$Vega
    interpolated$Theta<-tmp$Theta
    interpolated$Rho<-tmp$Rho
    
    interpolated %>% dplyr::select(Date,ExpDate,TYPE,StriNke,UDLY,Price,
                                   Delta,Gamma,Vega,Theta,Rho,OrigIV,ATMIV,IVIDX,
                                   TimeToExpDate,Moneyness.Nm) %>% 
      dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE),Strike) -> interpolated
    
    retun(interpolated)
    
  }
  #create atmiv
  opchain %>% dplyr::select(Date,ExpDate,TYPE,UDLY,ATMIV,IVIDX) %>%
    dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE)) %>%
    dplyr::distinct() -> atmiv
  
  TYPETmp=c(-1,-1,-1,-1)
  StrikeTmp=c(2325,2375,2425,2475)
  DateTmp=rep("2017/1/06",times=length(StrikeTmp))
  ExpDateTmp=rep("2017/4/28",times=length(StrikeTmp))
  
  interpolated=
    createInterpolatedPrice(Date=DateTmp,ExpDate=ExpDateTmp,TYPE=TYPETmp,Strike=StrikeTmp,atmiv=atmiv)
  
  #Write to a file
  write.table(interpolated,
              paste(DataFiles_Path_G,Underying_Symbol_G,"_Positions_Pre-Interpolated.csv",sep=""),quote=T,row.names=F,sep=",")
  
  interpolated$ContactName=rep("Interpolated",times=nrow(interpolated))
  interpolated$Last=interpolated$Bid=interpolated$Ask=interpolated$Price
  
  #for UDL_OPChain_PreForPos.csv
  interpolated %>% dplyr::select(Strike,ContactName,Last,Bid,Ask,Date,ExpDate,TYPE) %>%
    dplyr::arrange(as.Date(Date,format="%Y/%m/%d"),as.Date(ExpDate,format="%Y/%m/%d"),desc(TYPE)) -> interpolated
  
  write.table(interpolated,
              paste(DataFiles_Path_G,Underying_Symbol_G,"_OPChain_PreForPos-Interpolated.csv",sep=""),quote=T,row.names=F,sep=",")
}
