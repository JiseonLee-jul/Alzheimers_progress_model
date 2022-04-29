library(doParallel)
n.cores<-detectCores() - 1 ; s.cluster<-makeCluster(n.cores)      #### 코어 사용 개수 지정, 메모리 리밋 풀기
registerDoParallel(s.cluster)
memory.limit(50000000000) ; gc()


################## data frames for simulation setting  
############ cohort 1 ##############
#### b0_1 :  intercept mean
#### sigma_b0_1: intercept sd
#### b1_1 : slope
#### sigma_b1_1 : slope sd
#### sigma_e1 : error sd
#### cor_b0b1_1 : cor between intercept and slope
############ cohort 2 ##############
#### b0_2 :  intercept mean
#### sigma_b0_2: intercept sd
#### b1_2 : slope
#### sigma_b1_2 : slope sd
#### sigma_e2 : error sd
#### cor_b0b1_2 : cor between intercept and slope
####################################
#### mr : missing rate
#### b1_1type : type of effect (fixed or random)
#### b1_2type : type of effect (fixed or random)
#### cortype : correlation matrix type (indep or cs or ar1)
####################################
##N=100,b0_1, b1_1,sigma_b0_1, sigma_b1_1,sigma_e1,b0_2, b1_2,sigma_b0_2, sigma_b1_2, sigma_e2
##rho,cor_b0b1_1,cor_b0b1_2, mr,true=21, plot=FALSE


#####################################true=21

N<-rep(100,16) ; b0_1<-rep(1,16) ; b1_1<-rep(0.025,16) 
sigma_b0_1<-c(rep(0.1,8),rep(0.2,8)) ; sigma_b1_1<-rep(0,16) ; sigma_e1<-rep(c(rep(0.01,4),rep(0.02,4)),length=16)

b0_2<-rep(1.525,16) ; b1_2<-rep(0.035,16)
sigma_b0_2<-c(rep(0.1,8),rep(0.2,8)) ; sigma_b1_2<-rep(0,16) ; sigma_e2<-rep(c(rep(0.01,4),rep(0.02,4)),length=16)
rho<-rep(c(rep(0,2),rep(0.4,2)),length=16) ; cor_b0b1_1<-rep(0,16) ; cor_b0b1_2<-rep(0,16) ; mr<-rep(c(0,0.05),length=16)

case1<-cbind(N,b0_1,b1_1,sigma_b0_1,sigma_b1_1,sigma_e1,b0_2,b1_2,sigma_b0_2,
            sigma_b1_2,sigma_e2,rho,cor_b0b1_1,cor_b0b1_2,mr)




N<-rep(100,32) ; b0_1<-rep(1,32) ; b1_1<-rep(0.025,32) 
sigma_b0_1<-c(rep(0.1,16),rep(0.2,16)) ; sigma_b1_1<-rep(c(rep(0.0025,8),rep(0.005,8)),length=32) ; sigma_e1<-rep(c(rep(0.01,4),rep(0.02,4)),length=32)

b0_2<-rep(1.525,32) ; b1_2<-rep(0.035,32)
sigma_b0_2<-c(rep(0.1,16),rep(0.2,16)) ; sigma_b1_2<-rep(c(rep(0.0025,8),rep(0.005,8)),length=32) ; sigma_e2<-rep(c(rep(0.01,4),rep(0.02,4)),length=32)
rho<-rep(c(rep(0,2),rep(0.4,2)),length=32) ; cor_b0b1_1<-rep(0,32) ; cor_b0b1_2<-rep(0,32) ; mr<-rep(c(0,0.05),length=32)

case2<-cbind(N,b0_1,b1_1,sigma_b0_1,sigma_b1_1,sigma_e1,b0_2,b1_2,sigma_b0_2,
             sigma_b1_2,sigma_e2,rho,cor_b0b1_1,cor_b0b1_2,mr)




N<-rep(100,32) ; b0_1<-rep(1,32) ; b1_1<-rep(0.025,32) 
sigma_b0_1<-c(rep(0.1,16),rep(0.2,16)) ; sigma_b1_1<-rep(c(rep(0.0025,8),rep(0.005,8)),length=32) ; sigma_e1<-rep(c(rep(0.01,4),rep(0.02,4)),length=32)

b0_2<-rep(1.525,32) ; b1_2<-rep(0.035,32)
sigma_b0_2<-c(rep(0.1,16),rep(0.2,16)) ; sigma_b1_2<-rep(c(rep(0.0025,8),rep(0.005,8)),length=32) ; sigma_e2<-rep(c(rep(0.01,4),rep(0.02,4)),length=32)
rho<-rep(c(rep(0,2),rep(0.4,2)),length=32) ; cor_b0b1_1<-rep(0.4,32) ; cor_b0b1_2<-rep(0.4,32) ; mr<-rep(c(0,0.05),length=32)

case3<-cbind(N,b0_1,b1_1,sigma_b0_1,sigma_b1_1,sigma_e1,b0_2,b1_2,sigma_b0_2,
             sigma_b1_2,sigma_e2,rho,cor_b0b1_1,cor_b0b1_2,mr)


case<-rbind(case1,case2,case3)



#####################################true=42

N<-rep(100,16) ; b0_1<-rep(1,16) ; b1_1<-rep(0.025,16) 
sigma_b0_1<-c(rep(0.1,8),rep(0.2,8)) ; sigma_b1_1<-rep(0,16) ; sigma_e1<-rep(c(rep(0.01,4),rep(0.02,4)),length=16)

b0_2<-rep(2.05,16) ; b1_2<-rep(0.035,16)
sigma_b0_2<-c(rep(0.1,8),rep(0.2,8)) ; sigma_b1_2<-rep(0,16) ; sigma_e2<-rep(c(rep(0.01,4),rep(0.02,4)),length=16)
rho<-rep(c(rep(0,2),rep(0.4,2)),length=16) ; cor_b0b1_1<-rep(0,16) ; cor_b0b1_2<-rep(0,16) ; mr<-rep(c(0,0.05),length=16)

case1<-cbind(N,b0_1,b1_1,sigma_b0_1,sigma_b1_1,sigma_e1,b0_2,b1_2,sigma_b0_2,
             sigma_b1_2,sigma_e2,rho,cor_b0b1_1,cor_b0b1_2,mr)




N<-rep(100,32) ; b0_1<-rep(1,32) ; b1_1<-rep(0.025,32) 
sigma_b0_1<-c(rep(0.1,16),rep(0.2,16)) ; sigma_b1_1<-rep(c(rep(0.0025,8),rep(0.005,8)),length=32) ; sigma_e1<-rep(c(rep(0.01,4),rep(0.02,4)),length=32)

b0_2<-rep(2.05,32) ; b1_2<-rep(0.035,32)
sigma_b0_2<-c(rep(0.1,16),rep(0.2,16)) ; sigma_b1_2<-rep(c(rep(0.0025,8),rep(0.005,8)),length=32) ; sigma_e2<-rep(c(rep(0.01,4),rep(0.02,4)),length=32)
rho<-rep(c(rep(0,2),rep(0.4,2)),length=32) ; cor_b0b1_1<-rep(0,32) ; cor_b0b1_2<-rep(0,32) ; mr<-rep(c(0,0.05),length=32)

case2<-cbind(N,b0_1,b1_1,sigma_b0_1,sigma_b1_1,sigma_e1,b0_2,b1_2,sigma_b0_2,
             sigma_b1_2,sigma_e2,rho,cor_b0b1_1,cor_b0b1_2,mr)




N<-rep(100,32) ; b0_1<-rep(1,32) ; b1_1<-rep(0.025,32) 
sigma_b0_1<-c(rep(0.1,16),rep(0.2,16)) ; sigma_b1_1<-rep(c(rep(0.0025,8),rep(0.005,8)),length=32) ; sigma_e1<-rep(c(rep(0.01,4),rep(0.02,4)),length=32)

b0_2<-rep(2.05,32) ; b1_2<-rep(0.035,32)
sigma_b0_2<-c(rep(0.1,16),rep(0.2,16)) ; sigma_b1_2<-rep(c(rep(0.0025,8),rep(0.005,8)),length=32) ; sigma_e2<-rep(c(rep(0.01,4),rep(0.02,4)),length=32)
rho<-rep(c(rep(0,2),rep(0.4,2)),length=32) ; cor_b0b1_1<-rep(0.4,32) ; cor_b0b1_2<-rep(0.4,32) ; mr<-rep(c(0,0.05),length=32)

case3<-cbind(N,b0_1,b1_1,sigma_b0_1,sigma_b1_1,sigma_e1,b0_2,b1_2,sigma_b0_2,
             sigma_b1_2,sigma_e2,rho,cor_b0b1_1,cor_b0b1_2,mr)


case<-rbind(case1,case2,case3)


##########################################
########################################## 

new_result<-old_result<-beta_0<-beta_1<-list()

true=21 #true=42
b0_2=1.525#0.79

for(sim in 1:nrow(case)){
  
  print(sim)
  new_result[[sim]]<-beta_0[[sim]]<-beta_1[[sim]]<-vector()
  
  for(k in 1:100){
    

    data1=data_gen(N=case[sim,1],b0_1 =case[sim,2], b1_1 =case[sim,3],sigma_b0_1 =case[sim,4], sigma_b1_1 = case[sim,5],sigma_e1=case[sim,6],
                   b0_2=case[sim,7], b1_2 = case[sim,8],sigma_b0_2 =case[sim,9], sigma_b1_2 = case[sim,10], sigma_e2=case[sim,11],
                   rho=case[sim,12] ,cor_b0b1_1=case[sim,13] ,cor_b0b1_2=case[sim,14],mr=case[sim,15], true=true, plot=FALSE)  #,as.numeric(case[sim,])

    result= match_pt_v1(data1, true= true, plot=F)
    
    new_result[[sim]][k]=result$meet_pt
    beta_0[[sim]][k]=result$coeff_list[1]
    beta_1[[sim]][k]=result$coeff_list[2]
    
  }
  print(sim)
}

for(sim in 1:nrow(case)){
  print(sim)
  print(paste('Est overlap time=', round(mean(new_result[[sim]], na.rm=T),3) , '(',  round(sd(new_result[[sim]], na.rm=T),3), ')  ', 
              'Full model beta0=', round(mean(beta_0[[sim]], na.rm=T),3) , '(',  round(sd(beta_0[[sim]], na.rm=T),3), ')  ',
              'Full model beta1=', round(mean(beta_1[[sim]], na.rm=T),8) , '(',  round(sd(beta_1[[sim]], na.rm=T),8), ')'))
}

overlap<-beta0<-beta1<-c()

for(sim in 1:80){
  
  overlap[sim]<-paste(round(mean(new_result[[sim]], na.rm=T),3) , '(',  round(sd(new_result[[sim]], na.rm=T),3), ')')
  beta0[sim]<-paste(round(mean(beta_0[[sim]], na.rm=T),3) , '(',  round(sd(beta_0[[sim]], na.rm=T),3), ')')
  beta1[sim]<-paste(round(mean(beta_1[[sim]], na.rm=T),8) , '(',  round(sd(beta_1[[sim]], na.rm=T),8), ')')
  
}

frame<-data.frame(overlap,beta0,beta1)
write.csv(frame,file="C:\\Users\\이지선\\Desktop\\바이오 논문\\ex.csv")


















