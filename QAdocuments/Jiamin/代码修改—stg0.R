################################
#####一、不筛查
###############################

###1.建立概率转换矩阵
setwd("C:/Users/佳敏/Desktop/研究生/北医学报文章")
library(openxlsx)
rate_data<-read.csv("data/ghdx_data.csv")
source("function/transform_func.R")
p1_0<-RateToProb(rate_data$rate_incidence_CVD,1) ##一般人群发病概率
p2_0<-RateToProb(rate_data$rate_death_CVD,1) ##一般人群死于cvd的概率
p3<-RateToProb(rate_data$rate_death_nonCVD,1) ##一般人群死于非cvd的概率
p5_0<-ProbFactor(p2_0,3.12)  ##有cvd病史的人死亡的概率
p7<-ProbFactor(p1_0,1.37)  ##有cvd病史复发cvd的概率

n_state=4
names_state=c("s1","s2","s3","s4")
n_population=14
names_population=c("m_40","m_45","m_50","m_55","m_60","m_65","m_70","f_40","f_45","f_50","f_55","f_60","f_65","f_70")

a_P<-array(0, dim=c(n_state,n_state,10,n_population),
           dimnames=list(names_state,names_state,1:10,names_population))
p1<-array(0,dim=c(10,n_population),dimnames=list(1:10,names_population))  ##s1到s2状态的概率
p2<-array(0,dim=c(10,n_population),dimnames=list(1:10,names_population))  ##s1到s3的概率
p4<-array(0,dim=c(10,n_population),dimnames=list(1:10,names_population))  ##s1维持在s1的概率
set.seed(1)
p8<-array(runif(140,min=0.02,max=0.1),dim=c(10,n_population),dimnames=list(1:10,names_population))  ##cvd发作急性死亡概率
###这里设定每个层的人群，在每一年的循环中，急性死亡的概率都是不同的，因此产生140个随机数，取值服从2%~10%的均匀分布
p5<-array(0,dim=c(10,n_population),dimnames=list(1:10,names_population))  ##s2到s4的概率
p6<-array(0,dim=c(10,n_population),dimnames=list(1:10,names_population))  ##s2维持在s2的概率

for (i in 1:14){ 
  if (i==7 | i==14){
    ##10年都是i，后五年无法增加，因为少了一个年龄段的率
    a_P["s1","s3",1:10,i]<-p3[i]
    a_P["s2","s3",1:10,i]<-p3[i]
    for ( j in 1:10){
      p2[j,i]<-p2_0[i]+p1_0[i]*p8[j,i]
      p1[j,i]<-p1_0[i]*(1-p8[j,i])
      p4[j,i]<-1-p1[j,i]-p2[j,i]-p3[i]
      p5[j,i]<-p5_0[i]+p7[i]*p8[j,i]
      p6[j,i]<-1-p5[j,i]-p3[i]
      a_P["s1","s2",j,i]<-p1[j,i]
      a_P["s1","s4",j,i]<-p2[j,i]
      a_P["s1","s1",j,i]<-p4[j,i]
      a_P["s2","s2",j,i]<-p6[j,i]
      a_P["s2","s4",j,i]<-p5[j,i]
    }
    a_P["s3","s3",1:10,i]<-1
    a_P["s4","s4",1:10,i]<-1
  }
  else{
    ###前5年
    a_P["s1","s3",1:5,i]<-p3[i]
    a_P["s2","s3",1:5,i]<-p3[i]
    for ( j in 1:5){
      p2[j,i]<-p2_0[i]+p1_0[i]*p8[j,i]
      p1[j,i]<-p1_0[i]*(1-p8[j,i])
      p4[j,i]<-1-p1[j,i]-p2[j,i]-p3[i]
      p5[j,i]<-p5_0[i]+p7[i]*p8[j,i]
      p6[j,i]<-1-p5[j,i]-p3[i]
      a_P["s1","s2",j,i]<-p1[j,i]
      a_P["s1","s4",j,i]<-p2[j,i]
      a_P["s1","s1",j,i]<-p4[j,i]
      a_P["s2","s2",j,i]<-p6[j,i]
      a_P["s2","s4",j,i]<-p5[j,i]
    }
    a_P["s3","s3",1:5,i]<-1
    a_P["s4","s4",1:5,i]<-1
    
    ##后5年
    a_P["s1","s3",6:10,i]<-p3[i+1]
    a_P["s2","s3",6:10,i]<-p3[i+1]
    for ( j in 6:10){
      p2[j,i]<-p2_0[i+1]+p1_0[i+1]*p8[j,i]
      p1[j,i]<-p1_0[i+1]*(1-p8[j,i])
      p4[j,i]<-1-p1[j,i]-p2[j,i]-p3[i+1]
      p5[j,i]<-p5_0[i+1]+p7[i+1]*p8[j,i]
      p6[j,i]<-1-p5[j,i]-p3[i+1]
      a_P["s1","s2",j,i]<-p1[j,i]
      a_P["s1","s4",j,i]<-p2[j,i]
      a_P["s1","s1",j,i]<-p4[j,i]
      a_P["s2","s2",j,i]<-p6[j,i]
      a_P["s2","s4",j,i]<-p5[j,i]
    }
    a_P["s3","s3",6:10,i]<-1
    a_P["s4","s4",6:10,i]<-1
  }
}

## a_P

###2.计算CVD事件数、CVD死亡数和全因死亡数、QALY、获得生命年
###1)人群分布
population<-matrix(c(40,45,50,55,60,65,70,40,45,50,55,60,65,70,229,444,422,410,322,160,116,498,856,822,842,519,394,187,
                    1,1,1,1,1,1,1,2,2,2,2,2,2,2),ncol=3,nrow=14,dimnames=list(1:14,c("age","num","sex")),byrow=F)
## population  ##sex:1表示男，2表示女

## 这里跑一下策略3的人群分布
## population<-matrix(c(40,45,50,55,60,65,70,40,45,50,55,60,65,70,229,444,0,0,0,0,0,498,856,0,0,0,0,0,
##                     1,1,1,1,1,1,1,2,2,2,2,2,2,2),ncol=3,nrow=14,dimnames=list(1:14,c("age","num","sex")),byrow=F)

## population
###2)初始状态人数
s_start<-matrix(NA,nrow=14,ncol=4,dimnames=list(1:14,names_state))
s_start[,1]<-population[1:14,2]
s_start[,2]<-0
s_start[,3]<-0
s_start[,4]<-0
## s_start
m_M<-array(NA,dim=c(10,4,14),dimnames=list(1:10,names_state,names_population))
for (i in 1:14) {
  m_M[1,,i]<-s_start[i,]
}
## m_M
###3)记录10年中每个状态的人数，并计算CVD事件数、CVD死亡数和全因死亡数、QALY、获得生命年
uti<-c(1,0.9,0,0)  ##s1~s4的效用赋值
uti2<--0.038  ##话说这个值怎么算出来的??
year<-c(1,1,0,0)
n_cnew<-array(0,dim=c(10,1,14),dimnames=list(1:10,"num",names_population))  ##记录新发cvd人数
n_clive<-array(0,dim=c(10,1,14),dimnames=list(1:10,"num",names_population)) ##记录首发或复发并存活后维持在cvd状态的人数
n_cvd<-0 ##cvd发病数
n_cd<-0  ##因cvd死亡数
n_nd<-0  ##非cvd死亡数
n_d<-0   ##全死因死亡数
qaly<-0  ##质量调整生命年
qaly_m<-array(0,dim=c(10,4,14),dimnames=list(1:10,names_state,names_population))  ##qaly数组
for (i in 1:14){
  qaly_m[1,,i]<-m_M[1,,i] * uti  ##计算第一年的qaly的情况
}                     
lifeyear<-0  ##获得生命年
lifeyear_m<-array(0,dim=c(10,4,14),dimnames=list(1:10,names_state,names_population))  ##获得生命年数组
for (i in 1:14){
  lifeyear_m[1,,i]<-m_M[1,,i]*year
}
## qaly_m
## lifeyear_m

####增加一个概率转换，即各人群中有cvd病史且复发存活的概率；
p9<-array(NA,dim=c(10,n_population),dimnames=list(1:10,names_population))
for (i in 1:14){
  if (i==7 | i==14){
    for (j in 1:10)
      p9[j,i]<-p7[i]*(1-p8[j,i])
  }
  else {
    for (j in 1:5){
      p9[j,i]<-p7[i]*(1-p8[j,i])
    }
    for (j in 6:10){
      p9[j,i]<-p7[i+1]*(1-p8[j,i])
    }
  }
}
## p9
###########################################################

########增加一个概率转换，即各人群中无cvd的人cvd发病概率
p10<-array(NA,dim=c(10,n_population),dimnames=list(1:10,names_population))
for (i in 1:14){
  if (i==7 | i==14){
    for (j in 1:10)
      p10[j,i]<-p1_0[i]
  }
  else {
    for (j in 1:5){
      p10[j,i]<-p1_0[i]
    }
    for (j in 6:10){
      p10[j,i]<-p1_0[i+1]
    }
  }
}
## p10
######################################################


for (i in 1:14){
  for (j in 1:9){
    m_M[j+1,,i]<-m_M[j,,i] %*% a_P[,,j,i]
    n_cnew[j+1,1,i]<-m_M[j,1,i] * p10[j,i]   ##新发cvd=s1人数*p10(无cvd发病概率)
    n_clive[j+1,1,i]<-m_M[j,1,i]*a_P["s1","s2",j,i] +m_M[j,2,i]*p9[j,i] ##首发或复发cvd并存活的人数=s1人数*p1+s2人数*p9
    qaly_m[j+1,,i]<-m_M[j+1,,i] * uti+n_clive[j+1,1,i]*uti2 ##qaly=等于四个状态的qaly之和+损失的qaly
    lifeyear_m[j+1,,i]<-m_M[j+1,,i]*year
  }
  n_cvd[i]<-sum(n_cnew[1:10,,i])
  n_cd[i]<-m_M[10,4,i]
  n_nd[i]<- m_M[10,3,i]
  n_d[i]<-n_cd[i]+n_nd[i]
  qaly[i]<-sum(qaly_m[1:10,,i])
  lifeyear[i]<-sum(lifeyear_m[1:10,,i])
}

## m_M  ##各人群各状态每个周期循环的人数
## n_cnew  ##各人群每个周期新增的cvd人数 
## n_clive  ##各人群每个周期发生cvd且存活的人数


## n_cvd ##各人群cvd发病数
## n_cd##各人群因cvd死亡数
## n_nd##各人群非cvd死亡数
## n_d##各人群全死因死亡数
## qaly ##各人群qaly
## lifeyear ##各人群生命年

N_cvd<-sum(n_cvd)
N_cd<-sum(n_cd)
N_nd<-sum(n_nd)
N_d<-sum(n_d)
QALY<-sum(qaly)
LIFEYEAR<-sum(lifeyear)

N_cvd ##总人群cvd发病数
N_cd ##总人群因cvd死亡数
N_nd ##总人群非cvd死亡数
N_d ##总人群全死因死亡数
QALY ##总人群qaly
LIFEYEAR  ##总人数获得生命年