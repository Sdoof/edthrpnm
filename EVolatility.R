#get variable name utility.
getvarname <- function(v) {
  deparse(substitute(v))
}

#Volatility Level correlaiton and regression functions
#PCIVndCtC
PCIVndCtC <- function(hist,iv,n){
  p_ <- replace(hist, rep(1:(length(hist)-n)), hist[(1+n):length(hist)])
  cp_n_<- replace(p_,rep((length(p_)-(n-1)):length(p_)),NA)
  q_ <- replace(iv, rep(1:(length(iv)-n)),iv[(1+n):length(iv)])
  civ_n_ <- replace(q_,rep((length(q_)-(n-1)):length(q_)),NA)
  ret_<-(hist-cp_n_)/(civ_n_/100*cp_n_)
  ret_
}
#PCndCtC
PCndCtC <- function(hist,n){
  q_ <- replace(hist, rep(1:(length(hist)-n)),hist[(1+n):length(hist)])
  cp_n_ <- replace(q_,rep((length(q_)-(n-1)):length(q_)),NA)
  ret_ <- (hist-cp_n_)/cp_n_
  ret_
}
#IVCFndCtC
IVCFndCtC <- function(iv,n){
  q_ <- replace(iv, rep(1:(length(iv)-n)),iv[(1+n):length(iv)])
  civ_n_ <- replace(q_,rep((length(q_)-(n-1)):length(q_)),NA)
  ret_ <- (iv-civ_n_)/civ_n_
  ret_
}

#Save volatility Level correlaiton and regression function
save.PC2IV <- function (model, PC, IVC,cor,pcstat,ivstat) {
  reg_saved<-list(model)
  reg_saved<-c(reg_saved,list(cor))
  data.frame(Avr=pcstat[1],SD=pcstat[2]) %>% 
    full_join(data.frame(Avr=ivstat[1],SD=ivstat[2])) -> stat
  rownames(stat)<-c("PC","IVC") 
  reg_saved<-c(reg_saved,list(stat))
  names(reg_saved)<-c("model","cor","stat")
  
  #saved file name.
  reg_saved_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_",PC,"_",IVC,sep="")
  save(reg_saved,file=reg_saved_fn)  
}

#Load volatility Level correlaiton and regression function
load.PC2IV <- function (PC,IVC) {
  #load file name.
  reg_load_fn <- paste(DataFiles_Path_G,Underying_Symbol_G,"_",PC,"_",IVC,sep="")
  load(reg_load_fn)
  assign(paste(PC,"_",IVC,sep=""), reg_saved,env=.GlobalEnv)
}

#Just like get.predected.Skew. The differnce is here we only use linier regression
get.predicted.IVIDXChange<-function(model,xmin=-0.03,xmax=0.03,x_by=0.01){  
  intercept=model$coefficient[1]
  slope=model$coefficient[2]
  names(intercept)<-c("1"); names(slope)<-c("1")
  if(x_by==0){
    x<-c(xmin)
    y<-intercept+slope*xmin
  }else{
    x<-seq(xmin,xmax,by=x_by)
    y<-intercept+slope*x   
  }
  ivchg<-data.frame(PC=x,IVIDXC=y)
  ivchg
}

#Return the new data frame that contains the predicted value (fit column)
#from xmin to xmax by x_by data interval. If x_by is given as 0, then
#single predicted value of Moneyness=xmin is returned as a one data vector.
get.predicted.skew<-function(models,regtype=1,xmin=-2.0,xmax=1.5,x_by=0.01){  
  if(x_by==0){
    if(regtype==1){
      if(xmin<=0){
        vplot<-predict(models$model.m,newdata=data.frame(Moneyness=xmin))
      }else{
        vplot<-predict(models$model.p,newdata=data.frame(Moneyness=xmin))
      }
    }else if(regtype==5){
      if(xmin<=0){
        vplot<-predict(models$model.m,x=xmin)
      }else{
        vplot<-predict(models$model.p,x=xmin)
      }
      vplot<-vplot$y
    }
    return(vplot)
  }
  if(regtype==1){
    vplot_m<-data.frame(Moneyness=seq(xmin,0,by=x_by))  
    predict.m <- predict(models$model.m,newdata=vplot_m)
    vplot_m<-data.frame(vplot_m,fit=predict.m)
    
    vplot_p<-data.frame(Moneyness=seq(x_by,xmax,by=x_by))  
    predict.p <- predict(models$model.p,newdata=vplot_p)
    vplot_p<-data.frame(vplot_p,fit=predict.p)
    vplot_m %>% dplyr::full_join(vplot_p) -> vplot
  }else if(regtype==5){
    vplot_m<-predict(models$model.m,x=seq(xmin,0,by=x_by))
    vplot_p<-predict(models$model.p,x=seq(x_by,xmax,by=x_by))
    vplot<-list(append(vplot_m$x,vplot_p$x))
    vplot<-c(vplot,list(append(vplot_m$y,vplot_p$y)))
    names(vplot)<-c("x","y")
  }
  vplot
}

# This is the get.predicted.skew() dedicated to SmoothSpline regression.
# monesness and retuned values are Vectorized.
get.predicted.spline.skew<-function(models,moneyness){
  skew_nm<-as.numeric(moneyness<=0)*predict(models$model.m,x=moneyness)$y
  skew_nm<-skew_nm + as.numeric(moneyness>0)*predict(models$model.p,x=moneyness)$y
  skew_nm
}


#return list that includes model.m(Moneyness<0) and model.p(Moneyness>0)
#if the same model is employed regardless of Moneyness's value, then
# model.p and model.p is identical.
get.skew.regression.Models<-function(vplot,regtype=1,moneyness_adjust=0.1,atm_adjust=0.0,df=5){
  data.frame(Moneyness=vplot$Moneyness.Nm,
             Month=vplot$TimeToExpDate,
             IV2ATMIV=vplot$OrigIV/vplot$ATMIV)->vplot
  vplot %>% dplyr::filter(Moneyness<=moneyness_adjust) %>% 
    dplyr::filter(abs(IV2ATMIV-1.0)>=atm_adjust) -> vplot_mns
  vplot %>% dplyr::filter(Moneyness>(-1*moneyness_adjust)) %>% 
    dplyr::filter(abs(IV2ATMIV-1.0)>=atm_adjust) -> vplot_pls
  
  if(regtype==1){
    model.m<-lm(IV2ATMIV~1+Moneyness+I(Moneyness^2),data=vplot_mns)
    #predict.m <- predict(model.m)
    model.p<-lm(IV2ATMIV~1+Moneyness+I(Moneyness^2),data=vplot_pls)
    #predict.p <- predict(model.p)
  }else if(regtype==5){
    model.m<-smooth.spline(vplot_mns$Moneyness,vplot_mns$IV2ATMIV,df=df)
    model.p<-smooth.spline(vplot_pls$Moneyness,vplot_pls$IV2ATMIV,df=df)
  }
  
  models<-list(model.m)
  models<-c(models,list(model.p))
  names(models)<-c("model.m","model.p")
  
  models
}

save.Skew<- function(models,pattern=""){
  reg_saved_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Skew",pattern,sep="")
  save(models,file=reg_saved_fn)
}

load.Skew<- function(pattern="") {
  #load file name.
  reg_load_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_Skew",pattern,sep="")
  load(reg_load_fn)
  #assign("SkewModel",models,env=.GlobalEnv)
  assign(paste("SkewModel", pattern, sep=""),models,env=.GlobalEnv)
}

# ATM IV Volatility Change to IVIDX as to Time 

save.IVChg<- function(model,optype,up_dn){
  if(optype==OpType_Put_G){
    if(up_dn>=0){
      reg_saved_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_PutIVChgUp",sep="")
    }else if(up_dn<0){
      reg_saved_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_PutIVChgDown",sep="")
    }
  }else if(optype==OpType_Call_G){
    if(up_dn>=0){
      reg_saved_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_CallIVChgUp",sep="")
    }else if(up_dn<0){
      reg_saved_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_CallIVChgDown",sep="")
    }
  }
  save(model,file=reg_saved_fn)
}

load.IVChg<- function(optype,up_dn) {
  if(optype==OpType_Put_G){
    if(up_dn>=0){
      reg_load_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_PutIVChgUp",sep="")
      load(reg_load_fn)
      assign("PutIVChgUp",model,env=.GlobalEnv)
    }else if(up_dn<0){
      reg_load_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_PutIVChgDown",sep="")
      load(reg_load_fn)
      assign("PutIVChgDown",model,env=.GlobalEnv)
    }
  }else if(optype==OpType_Call_G){
    if(up_dn>=0){
      reg_load_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_CallIVChgUp",sep="")
      load(reg_load_fn)
      assign("CallIVChgUp",model,env=.GlobalEnv)
    }else if(up_dn<0){
      reg_load_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_CallIVChgDown",sep="")
      load(reg_load_fn)
      assign("CallIVChgDown",model,env=.GlobalEnv)
    }
  }
}

# Vcone Analysis Functions

#Making Volatility Cone data frame
make.vcone.df<-function(atmiv,type=0){
  if(type!=0){
    atmiv %>% filter(TYPE==type) -> atmiv
  }
  vcone<-data.frame(Month=atmiv$TimeToExpDate, IV=atmiv$ATMIV,IVIDX=atmiv$IVIDX,TYPE=atmiv$TYPE)  
  #volatility normalize
  iv_mean_p<-mean(vcone$IV)
  ividx_mean_p<-mean(vcone$IVIDX)
  vcone %>% dplyr::mutate(IV.nm=IV/iv_mean_p) -> vcone
  #vcone %>% dplyr::mutate(IV2IDX.nm=IV/ividx_mean_p) -> vcone
  vcone %>% dplyr::mutate(IV2IDX.nm=IV/IVIDX) -> vcone
  #Time filtering, because when IV is not stable when Time is very close to ExpDate .
  vcone %>% dplyr::filter(Month>=0.30) -> vcone
  vcone
}

#Making Volatility Change data frame
make.vchg.df<-function(vcone,type=0){
  if(type!=0){
    vcone %>% filter(TYPE==type) -> vcone
  }
  vcone %>% dplyr::mutate(VC.f=ATMIV.f/IVIDX.f) -> vcone
  #Time filtering, because when IV is not stable when Time is very close to ExpDate .
  vcone %>% dplyr::filter(TimeToExpDate>=0.25) -> vcone
  vcone
}

# regression functions

vcone_regression<-function(vcone,regtype=1,ret=1){
  if(regtype==1){
    nls.m<-lm(IV2IDX.nm~Month,data=vcone)
  }else if(regtype==3){
    nls.m<-lm(IV2IDX.nm~1+Month+I(Month^2),data=vcone)
  }
  if(ret==1){
    nls.m
  }else{
    predict.m <- predict(nls.m)
    predict.m
  }
}

vchg_regression<-function(vchg,regtype=2,start=NULL,ret=1){
  if(regtype==2){
    nls.m<-nls(VC.f~a*TimeToExpDate^b+c,data=vchg,start=start)
  }else if(regtype==1){
    nls.m<-lm(VC.f~TimeToExpDate,data=vchg)
  }
  if(ret==1){
    nls.m
  }else{
    predict.m <- predict(nls.m)
    predict.m
  }
}

save.VCone<- function(model,optype){
  if(optype==OpType_Put_G){
    reg_saved_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_PutVCone",sep="")
  }else if(optype==OpType_Call_G){
    reg_saved_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_CallVCone",sep="")
  }
  save(model,file=reg_saved_fn)
}

load.VCone<- function(optype) {
  if(optype==OpType_Put_G){
    reg_load_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_PutVCone",sep="")
    load(reg_load_fn)
    assign("PutVCone",model,env=.GlobalEnv)
  }else if(optype==OpType_Call_G){
    reg_load_fn<-paste(DataFiles_Path_G,Underying_Symbol_G,"_CallVCone",sep="")
    load(reg_load_fn)
    assign("CallVCone",model,env=.GlobalEnv)
  }
}


# Linear 3D Fitting **Just a test 
#Functions
#モデルオブジェエクとを引数として、xvarとyvarからzvarを予測する
#デフォルトでは指定されたxとy変数の範囲で、16x16グリッドを計算
predictgrid<-function(model,xvar,yvar,zvar,res=16,type=NULL){
  #モデルオブジェクトから予測面のxとy変数の範囲を決める
  #lmとglmなどで使用可能だが、他のモデルではカスタマイズが必要
  xrange<-range(model$model[[xvar]])
  yrange<-range(model$model[[yvar]])
  
  newdata<-expand.grid(x=seq(xrange[1],xrange[2],length.out=res),
                       y=seq(yrange[1],yrange[2],length.out=res))
  names(newdata)<-c(xvar,yvar)
  newdata[[zvar]]<-predict(model,newdata,type=type)
  newdata
}

#x,y,zの値を格納したlong形式のデータフレームを、xとyのベクトル行列zを
#含むリストに変換する
df2mat<-function(p,xvar=NULL,yvar=NULL,zvar=NULL){
  if(is.null(xvar)) xvar <- names(p)[1]
  if(is.null(yvar)) yvar <- names(p)[2]
  if(is.null(zvar)) zvar <- names(p)[3]
  
  x<-unique(p[[xvar]])
  y<-unique(p[[yvar]])
  z<-matrix(p[[zvar]],nrow=length(y),ncol=length(x))
  
  m<-list(x,y,z)
  names(m)<-c(xvar,yvar,zvar)
  m
}
