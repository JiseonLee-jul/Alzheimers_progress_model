library(lme4)
library(mvtnorm)
library(plotrix)

b0_1 =1; b1_1 =0.025;sigma_b0_1 =0.1; sigma_b1_1 = 0.0025;sigma_e1=0.1
b0_2=0.79; b1_2 = 0.035 ;sigma_b0_2 =0.1; sigma_b1_2 = 0.0025; sigma_e2=0.1; true=21
b0_2=0.58; b1_2 = 0.035 ;sigma_b0_2 =0.1; sigma_b1_2 = 0.0025; sigma_e2=0.1; true=42

rho=0.4 #0 or 0.4
cor_b0b1_1=0 # 0 or 0.4 or 0.75
cor_b0b1_2=0.4
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


data_gen <- function(N=100,b0_1 =1, b1_1 =0.025,sigma_b0_1 =0.1, sigma_b1_1 = 0.0025,sigma_e1=0.1,b0_2=1.525, b1_2 = 0.035,sigma_b0_2 =0.1, sigma_b1_2 = 0.0025, sigma_e2=0.2,rho=0.4 ,cor_b0b1_1=0 ,cor_b0b1_2=0, true=21, plot=FALSE){
  
  leng_t=21
  tp=seq(from=0, to=60, length.out= leng_t)
  cov_b0b1_1=cor_b0b1_1*sigma_b1_1*sigma_b0_1
  var_mat= matrix(sigma_b0_1^2 + sigma_b1_1^2 + 2 * cov_b0b1_1+ rho*sigma_e1^2, leng_t, leng_t)
  diag(var_mat)= sigma_b0_1^2 + sigma_b1_1^2 + 2 * cov_b0b1_1+ 1*sigma_e1^2 
  y1= rmvnorm(N, mean=  b0_1+ b1_1*tp  , sigma=var_mat )
  
  
  cov_b0b1_1=cor_b0b1_2*sigma_b1_2*sigma_b0_2
  var_mat= matrix(sigma_b0_2^2 + sigma_b1_2^2 + 2 * cov_b0b1_2+ rho*sigma_e2^2, leng_t, leng_t)
  diag(var_mat)= sigma_b0_2^2 + sigma_b1_2^2 + 2 * cov_b0b1_2+ 1*sigma_e2^2
  y2= rmvnorm(N, mean=  b0_2+ b1_2*tp  , sigma=var_mat )
  
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