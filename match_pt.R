### To find the point that cohort 1 meets cohort 2
match_pt_v1<-function(data1 ,true,plot=T){
  
  N=nrow(data1[[1]])
  leng_t =ncol(data1[[1]])
  tp=seq(from=0, to=60, length.out= leng_t)
  
  long_form_1=data.frame(ID= rep(1:N, leng_t) , time=rep(tp, each=N) , y=as.vector(data1[[1]]))
  model1=  lmer( y ~ time + (1 |ID), data = long_form_1, REML=FALSE)
  summary(model1)
  long_form_2=data.frame(ID= rep(1:N, leng_t) , time=rep(tp, each=N) , y=as.vector(data1[[2]]))
  model2=  lmer( y ~ time + (1 |ID), data = long_form_2, REML=FALSE)
  summary(model2)
  
  
  newdat1<-newdat2<-data.frame(x=tp)
  mm<-model.matrix(~x,newdat1)
  
  predFun<-function(.) mm%*%fixef(.) 
  newdat1$fitted=predFun(model1) 
  newdat2$fitted=predFun(model2)
  
  bb<-bootMer(model1,FUN=predFun,nsim=200) 
  bb_se<-apply(bb$t,2,function(x) x[order(x)])
  newdat1$blo<-bb_se[1,]
  newdat1$bhi<-bb_se[nrow(bb_se),]
  
  
  ### cohort 2
  
  mm<-model.matrix(~x,newdat1)
  predFun<-function(.) mm%*%fixef(.) 
  newdat2$fitted=predFun(model2)
  bb<-bootMer(model2,FUN=predFun,nsim=200) #do this 200 times
  bb_se<-apply(bb$t,2,function(x) x[order(x)])
  newdat2$blo<-bb_se[1,]
  newdat2$bhi<-bb_se[nrow(bb_se),]
  
  meet_pt =tp[which(newdat1$blo<= newdat2$fitted)[1]]
  # cohort1_h=(newdat1$blo)
  # thres=(newdat2$fitted)[1]
  # meet_pt=which(cohort1_h >thres)[1]
  #new_meet_pt=(( thres ) - fixef(model1)[1])/  fixef(model1)[2]
  #meet_temp=floor(new_meet_pt)
  
  if(plot==TRUE){
    
    par(mfrow=c(2,2))
    plot(tp, c(1:leng_t), type='n' , xlab='Time (Month)', ylab='Y', ylim=range(data1), main='Observation (Cohort 1)')
    for(id in 1:N){
      points(tp , data1[[1]][id,],col=gray(0.7))
    }
    lines(tp,newdat1$fitted, col=2 )
    legend('topleft',c('obs', 'fitted line'), col=c(gray(0.7), 2), lty=c(NA,1), pch=c(19, NA))
    
    plot(tp, c(1:leng_t), type='n' , xlab='Time (Month)', ylab='Y', ylim=range(data1), main='Observation (Cohort 2)')
    for(id in 1:N){
      points(tp , data1[[2]][id,],col=gray(0.7))
    }
    lines(tp,(newdat2$fitted), col=2 )
    legend('topleft',c('obs', 'fitted line'), col=c(gray(0.7), 2), lty=c(NA,1), pch=c(19, NA))
    
    
    plotCI(tp,(newdat1$fitted),li=(newdat1$blo),ui=(newdat1$bhi),ylab="Y",ylim=range(data1), xlab='Time (Month)', main='CI plot')
    par(new=TRUE)
    plotCI(tp,(newdat2$fitted),li=(newdat2$blo),ui=(newdat2$bhi),ylab="Y",col="skyblue",ylim=range(data1), xlab='') 
    legend('topleft',c('Cohort 1', 'Cohort 2'), col=c(1, 'skyblue'), pch=1)
    abline(v= meet_pt, col=2)
    text(30,1, paste('estimation:', meet_pt, '(true=', true, ')') )
  }
  
  
  
  # comine two cohort
  com_data_full<-rbind(data1[[1]], data1[[2]])
  long_form_c=data.frame(ID= rep(1:(2*N), leng_t) , time=rep(tp, each=2*N) , y=as.vector(com_data_full))
  model_final=  lmer( y ~ time + (1 |ID), data = long_form_c, REML=FALSE)
  summary(model_final)
  newdat3<-data.frame(x= tp)
  mm3<-model.matrix(~x,newdat3)
  predFun2<-function(.) mm3%*%fixef(.) 
  final_fitted=predFun2(model_final)
  coeff_list=fixef(model_final)
  
  
  
  if(plot==TRUE){
    plot(tp,tp, type='n' , xlab='Time (Month)', ylab='Y',ylim=range(com_data_full), main='Combined Cohort')
    for(id in 1:(2*N)){
      points(tp , com_data_full[id,],col=gray(0.7))
    }		
    lines(tp, final_fitted, col=2)
    
  }
  
  return(list(meet_pt=meet_pt, coeff_list=coeff_list))
  
  
  
}