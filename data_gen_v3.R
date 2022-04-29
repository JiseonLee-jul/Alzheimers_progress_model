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
#### cortype : correlation matrix type (indep or cs or ar1)


data_gen_new <- function(N=100,b0_1 =1, b1_1 =0.025,sigma_b0_1 =0.1, sigma_b1_1 = 0.0025,sigma_e1=0.1,
                         b0_2=1.525, b1_2 = 0.035,sigma_b0_2 =0.1, sigma_b1_2 = 0.0025, sigma_e2=0.2,
                         rho=0.4 ,cor_b0b1_1=0 ,cor_b0b1_2=0, mr=0.05,cortype="cs",mrtype="constant", true=21, plot=FALSE){
  
  ########################### cohort 1
  
  leng_t=21
  tp=seq(from=0, to=60, length.out= leng_t)
  cov_b0b1_1=cor_b0b1_1*sigma_b1_1*sigma_b0_1
  
  if(cortype=="cs"){
    
    var_mat1= matrix(sigma_b0_1^2 + sigma_b1_1^2 + 2 * cov_b0b1_1+ rho*sigma_e1^2, leng_t, leng_t)   ### correlation matrix type(indep or cs or ar1)
    diag(var_mat1)= sigma_b0_1^2 + sigma_b1_1^2 + 2 * cov_b0b1_1+ 1*sigma_e1^2 
    
    
  }   else if(cortype=="ar1"){
    
    ex <- abs(matrix(1:leng_t - 1, nrow = leng_t, ncol = leng_t, byrow = TRUE) - (1:leng_t - 1))
    
    var_mat1= matrix(sigma_b0_1^2 + sigma_b1_1^2 + 2 * cov_b0b1_1,leng_t, leng_t) + (rho^ex)*sigma_e1^2
    
  }
  
  y1= rmvnorm(N, mean=  b0_1+ b1_1*tp  , sigma=var_mat )
  
  if(mrtype=="constant") {
    
    m = matrix(rbinom(N*leng_t, size=1, prob=mr),nrow=N,ncol=leng_t)   ### missing rate
    
  } else if(mrtype=="increasing") {
    
    for(i in 1:leng_t){
      
      m[,i]=rbinom(N,size=1,prob=seq(0,0.5,length=leng_t))
      m<-matrix(m,nrow=N,ncol=leng_t) 
    }
    
  }
  
  y1[m==1]<-NA   
  
  ########################### cohort 2
  
  cov_b0b1_1=cor_b0b1_2*sigma_b1_2*sigma_b0_2
  
  if(cortype=="cs"){
    
    var_mat= matrix(sigma_b0_2^2 + sigma_b1_2^2 + 2 * cov_b0b1_2+ rho*sigma_e2^2, leng_t, leng_t)
    diag(var_mat)= sigma_b0_2^2 + sigma_b1_2^2 + 2 * cov_b0b1_2+ 1*sigma_e2^2
    
  }   else if(cortype=="ar1"){
    
    ex <- abs(matrix(1:leng_t - 1, nrow = leng_t, ncol = leng_t, byrow = TRUE) - (1:leng_t - 1))
    
    var_mat2= matrix(sigma_b0_2^2 + sigma_b1_2^2 + 2 * cov_b0b1_2, leng_t, leng_t) + (rho^ex)*sigma_e2^2
    
  }
  
  y2= rmvnorm(N, mean=  b0_2+ b1_2*tp  , sigma=var_mat )
 
  y2[m==1]<-NA 
  
   
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

