set.seed(777)

library(lme4)
library(mvtnorm)
library(plotrix)
library(MASS)


############ cohort 1 ##############
#### b0_1 :  intercept mean
#### sigma_b0_1: intercept sd
#### b1_1 : slope
#### sigma_b1_1 : slope sd
#### sigma_e1 : error sd
#### cov_b0b1_1 : cov between intercept and slope
############ cohort 2 ##############
#### b0_2 :  intercept mean
#### sigma_b0_2: intercept sd
#### b1_2 : slope
#### sigma_b1_2 : slope sd
#### sigma_e2 : error sd
#### cov_b0b1_2 : cov between intercept and slope
####################################
#### mr : missing rate
#### b1_1type : type of effect (fixed or random)
#### b1_2type : type of effect (fixed or random)
#### cortype : correlation matrix type (indep or cs or ar1)



data_gen <- function(N=100,b0_1 =1, b1_1 =0.025,sigma_b0_1 =0.1, sigma_b1_1 = 0.0025,sigma_e1=0.1,b0_2=1.525, b1_2 = 0.035,
                     sigma_b0_2 =0.1, sigma_b1_2 = 0.0025, sigma_e2=0.2,rho=0.4 ,cov_b0b1_1=0 ,cov_b0b1_2=0,mr=0.05, 
                     true=21,b1_1type="random",b1_2type="random",cortype="cs",mrtype="constant" , plot=FALSE){
  
  
  leng_t=21
  tp=seq(from=0, to=60, length.out=leng_t)
  
############### cohort1
  
 if(b1_1type=="random"){
     tmp<-rmvnorm( N, mean=c(b0_1,b1_1), sigma=rbind(c(1.0, cov_b0b1_1),c(cov_b0b1_1, 1.0)) )
     ### slope type(fixed or random)
     data = data.frame(ID   = rep(1:N,   each=leng_t), time = rep(1:leng_t,   times=N), RE.i = rep(RE[,1], each=leng_t), RE.s = rep(RE[,2], each=leng_t))
  } else {
    b0_1<-rnorm(tp,mean=b0_1,sd=sigma_b0_1)
  }
    
###    
 if(cortype=="cs"){
  
  var_mat1= matrix(sigma_b0_1^2 + sigma_b1_1^2 + 2 * cov_b0b1_1+ rho*sigma_e1^2, leng_t, leng_t)   ### correlation matrix type(indep or cs or ar1)
  diag(var_mat1)= sigma_b0_1^2 + sigma_b1_1^2 + 2 * cov_b0b1_1+ 1*sigma_e1^2 
  
 }  else if(cortype=="ar1"){
  
    ex <- abs(matrix(1:leng_t - 1, nrow = leng_t, ncol = leng_t, byrow = TRUE) - (1:leng_t - 1))
    var_mat1= matrix(sigma_b0_1^2 + sigma_b1_1^2 + 2 * cov_b0b1_1,leng_t, leng_t) + (rho^ex)*sigma_e1^2

    } else{
      var_mat1=matrix()
}
  
  y1= rmvnorm(N, mean=  b0_1+ b1_1*tp  , sigma=var_mat1 )  ### suppose mvnormal dist'n for y value
 
###
 if(mrtype=="constant") {
    
    m = matrix(rbinom(N*leng_t, size=1, prob=mr),nrow=N,ncol=leng_t)   ### missing rate
    
  } else if(mrtype=="increasing") {
    
    for(i in 1:leng_t){
     
       m[,i]=rbinom(N,size=1,prob=seq(0,0.5,length=leng_t))
       m<-matrix(m,nrow=N,ncol=leng_t) 
    }
    
  }
 
y1[m==1]<-NA   

  
  
############## cohort2  
  
if(cortype=="cs"){
  
    var_mat2= matrix(sigma_b0_2^2 + sigma_b1_2^2 + 2 * cov_b0b1_2+ rho*sigma_e2^2, leng_t, leng_t)
    diag(var_mat2)= sigma_b0_2^2 + sigma_b1_2^2 + 2 * cov_b0b1_2+ 1*sigma_e2^2
    
  }   else if(cortype=="ar1"){
    
    ex <- abs(matrix(1:leng_t - 1, nrow = leng_t, ncol = leng_t, byrow = TRUE) - (1:leng_t - 1))
    var_mat2= matrix(sigma_b0_2^2 + sigma_b1_2^2 + 2 * cov_b0b1_2, leng_t, leng_t) + (rho^ex)*sigma_e2^2
    
  } else{
    
    var_mat2=matrix()
  }
  
  
  
y2 = rmvnorm(N, mean=  b0_2+ b1_2*tp  , sigma=var_mat2 )
  
y2[m==1]<-NA   
  
  
  
  
############ plot
  
    
  if(plot==TRUE){
    
    par(mfrow=c(2,2))
    plot(tp, c(1:leng_t), type='n' , xlab='Time (Month)', ylab='Y', ylim=range(y1,y2), main='Observation (Cohort 1)')
    for(id in 1:N){
      points(tp , y1[id,],col=gray(0.7))
    }
    plot(tp, c(1:leng_t), type='n' , xlab='Time (Month)', ylab='Y', ylim=range(y1,y2), main='Observation (Cohort 2)')
    for(id in 1:N){
      points(tp , y2[id,],col=gray(0.7))
    }
    
  }
  
  
  mydata=list()
  mydata[[1]]=y1
  mydata[[2]]=y2
  return(mydata)
}


data1<-data_gen()










  

