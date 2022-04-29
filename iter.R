
########################### clean function ##############################

data_gen <- function(b10, b11, b20, b21,sigma10,sigma20,sigma_e1,sigma_e2){
  ID <- rep(seq.int(100),each=61)
  length(ID)
  
  ### cohort1
  b10<-rnorm(100, mean=b10, sd=sigma10) ###절편  ##sd 바꿔가면서 분석
  b10<-rep(round(b10, digits = 3), each=61)
  length(b10)
  mean(b10)
  
  
  time = rep(seq(0,60,1), times=100)
  length(time)
  
  X = model.matrix(~time)
  dimnames(X)[[2]]
  
  beta1 <- c(0,b11)
  error1 <- rnorm(dim(X)[1],mean = 0, sd = sigma_e1)
  mean(error1)
  
  ln_y1 <- b10+ X%*%beta1 + error1
  #round the data to be more realistic
  ln_y1 <- round(ln_y1, digits = 3)
  
  ######################
  ##cohort2
  b20<-rnorm(100, mean=b20, sd=sigma20) ###절편
  b20<-rep(round(b20, digits = 3), each=61)
  length(b20)
  mean(b20)
  
  X = model.matrix(~time)
  dimnames(X)[[2]]
  beta2 <- c(0,b21)
  error2 <- rnorm(dim(X)[1],mean = 0, sd = sigma_e2)
  
  ln_y2 <- b20 + X%*%beta2  + error2
  #round the data to be more realistic
  ln_y2 <- round(ln_y2, digits = 3)
  
  mydata1 <- data.frame(ID, time, ln_y1, ln_y2)
  return( mydata1 )
  
}

################################ iteration ##################################


library("lme4")

time<-seq(0,60,1)

data<-data.frame()
newdat1<-data.frame(x=time)
mm<-model.matrix(~x,newdat1)
newdat2<-data.frame(x=time)
mm<-model.matrix(~x,newdat2)

predFun<-function(.) mm%*%fixef(.)

simul<-c()

for (i in 1:100) {
  
  set.seed(i)
  
  data1<-data_gen(b10=1, b11=0.025, b20=1.9, b21=0.035,sigma10=0.2,sigma20=0.3,sigma_e1=0.2,sigma_e2=0.2)
  model1<-lmer( ln_y1 ~ time + (1 |ID), data = data1, REML=FALSE)
  model2<-lmer( ln_y2 ~ time + (1 |ID), data = data1, REML=FALSE)
  
  
  newdat1$fitted=predFun(model1)
  bb<-bootMer(model1,FUN=predFun,nsim=200)
  bb_se<-apply(bb$t,2,function(x) x[order(x)])
  newdat1$bhi<-bb_se[nrow(bb_se),]
  cohort1_h=exp(newdat1$bhi)
  
  newdat2$fitted=predFun(model2)
  bb<-bootMer(model2,FUN=predFun,nsim=200)
  bb_se<-apply(bb$t,2,function(x) x[order(x)])
  newdat2$blo<-bb_se[1,]
  thres=exp(newdat2$blo)[1]
  
  meet_pt=which(cohort1_h >thres)[1]
  simul[i]<-time[meet_pt]
}

mean(simul) ; round(sd(simul),3)


############################ graph ########################################

set.seed(100)

data1=data_gen(b10=1, b11=0.025, b20=1.9, b21=0.035,sigma10=0.2,sigma20=0.3,sigma_e1=0.2,sigma_e2=0.2)

library(lme4)
model1=  lmer( ln_y1 ~ time + (1 |ID), data = data1, REML=FALSE)
summary(model1)
model2=  lmer( ln_y2 ~ time + (1 |ID), data = data1, REML=FALSE)
summary(model2)

par(mfrow=c(2,2))

time<-seq(0,60,1)
plot(seq(0,60,1), c(1:length(seq(0,60,1))), type='n' , xlab='Time (Month)', ylab='Y',ylim=c(0,120), main='Cohort 1')
for(i in 1:100){
  points(data1[which(data1$ID==i),]$time, exp(data1[which(data1$ID==i),]$ln_y1),col=gray(0.7))
}

newdat1<-data.frame(x=time)
mm<-model.matrix(~x,newdat1)
predFun<-function(.) mm%*%fixef(.) 
newdat1$fitted=predFun(model1)
bb<-bootMer(model1,FUN=predFun,nsim=200)

bb_se<-apply(bb$t,2,function(x) x[order(x)])
newdat1$blo<-bb_se[1,]
newdat1$bhi<-bb_se[nrow(bb_se),]

lines(time,exp(newdat1$fitted),col="red",lwd=2)
lines(time,exp(newdat1$blo),col="red",lwd=2,lty=2)
lines(time,exp(newdat1$bhi),col="red",lwd=2, lty=2)

cohort1_h=exp(newdat1$bhi)

### cohort 2

plot(seq(0,60,1), c(1:length(seq(0,60,1))), type='n' , xlab='Time (Month)', ylab='Y',ylim=c(0,120), main='Cohort 2')
for(i in 1:100){
  points(data1[which(data1$ID==i),]$time, exp(data1[which(data1$ID==i),]$ln_y2),col=gray(0.7))
}

newdat2<-data.frame(x=time)
mm<-model.matrix(~x,newdat2)
predFun<-function(.) mm%*%fixef(.) 
newdat2$fitted=predFun(model2)
bb<-bootMer(model2,FUN=predFun,nsim=200) #do this 200 times
#as we did this 200 times the 95% CI will be bordered by the 5th and 195th value
bb_se<-apply(bb$t,2,function(x) x[order(x)])
newdat2$blo<-bb_se[1,]
newdat2$bhi<-bb_se[nrow(bb_se),]

lines(time,exp(newdat2$fitted),col="red",lwd=2)
lines(time,exp(newdat2$blo),col="red",lwd=2,lty=2)
lines(time,exp(newdat2$bhi),col="red",lwd=2, lty=2)

thres=exp(newdat2$blo)[1]



################################# plot CI ################################

library(plotrix)

plotCI(time,exp(newdat1$fitted),li=exp(newdat1$blo),ui=exp(newdat1$bhi),ylab="ln(Y)",ylim=c(0,70), main="CI plot")
par(new=TRUE)
plotCI(time,exp(newdat2$fitted),li=exp(newdat2$blo),ui=exp(newdat2$bhi),ylab="ln(Y)",col="skyblue",ylim=c(0,70)) 
legend()

meet_pt=which(cohort1_h >thres)[1]
time[meet_pt]
abline(v=time[meet_pt], col=2)
abline(h= cohort1_h[meet_pt], col=2)
legend("topleft",legend=c("cohort1","cohort2"),col=c("black","skyblue"),pch=1)


############################### new graph ##################################

plot(seq(0,60+time[meet_pt],1), c(1:length(seq(0,60+time[meet_pt],1))), type='n' , xlab='Time (Month)', ylab='Y',ylim=c(0,120), main='New Graph')
for(i in 1:100){
  points(data1[which(data1$ID==i),]$time, exp(data1[which(data1$ID==i),]$ln_y1),col="black")
}
for(i in 1:100){
  points(time[meet_pt]+data1[which(data1$ID==i),]$time, exp(data1[which(data1$ID==i),]$ln_y2),col="sky blue")
}


newdata1<-subset(data1,time<=time[meet_pt])
newmodel = lmer( ln_y1 ~ time + (1 |ID), data = newdata1, REML=FALSE)

newtime<-seq(0,time[meet_pt],1)

newdat<-data.frame(x=newtime)
mm<-model.matrix(~x,newdat)
predFun<-function(.) mm%*%fixef(.) 
newdat$fitted=predFun(newmodel)
bb<-bootMer(newmodel,FUN=predFun,nsim=200)

bb_se<-apply(bb$t,2,function(x) x[order(x)])
newdat$blo<-bb_se[1,]
newdat$bhi<-bb_se[nrow(bb_se),]



lines(newtime,exp(newdat$fitted),col="red",lwd=2)
lines(newtime,exp(newdat$blo),col="red",lwd=2,lty=2)
lines(newtime,exp(newdat$bhi),col="red",lwd=2, lty=2)


newdat2<-data.frame(x=time)
mm<-model.matrix(~x,newdat2)
predFun<-function(.) mm%*%fixef(.) 
newdat2$fitted=predFun(model2)
bb<-bootMer(model2,FUN=predFun,nsim=200)

bb_se<-apply(bb$t,2,function(x) x[order(x)])
newdat2$blo<-bb_se[1,]
newdat2$bhi<-bb_se[nrow(bb_se),]

lines(time[meet_pt]+time,exp(newdat2$fitted),col="red",lwd=2)
lines(time[meet_pt]+time,exp(newdat2$blo),col="red",lwd=2,lty=2)
lines(time[meet_pt]+time,exp(newdat2$bhi),col="red",lwd=2, lty=2)

legend("topleft",legend=c("cohort1","cohort2","fitted line","CI"),
       col=c("black","skyblue","red","red"),pch=c(1,1,NA,NA),lty=c(NA,NA,1,2))

