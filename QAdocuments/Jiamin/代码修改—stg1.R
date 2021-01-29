#########################################
##二、进行筛查:stg1
#########################################

####(一)输入参数
##1.输入和循环设定相关的参数：状态数，循环次数，人群数分层情况;基本的率的设定
#循环参数设定
n_risk<-3
names_risk<-c("low-risk","middle-risk","high-risk")
n_state<-4
names_state<-c("s1","s2","s3","s4")
n_cycle<-10
names_cycle<-1:10
n_population<-14
names_population=c("m_40","m_45","m_50","m_55","m_60","m_65","m_70","f_40","f_45","f_50","f_55","f_60","f_65","f_70")

#基本率的输入
library(openxlsx)
rate_data<-read.csv("data/ghdx_data.csv")
source("function/transform_func.R")
p1_0<-RateToProb(rate_data$rate_incidence_CVD,1) ##一般人群发病概率
p2_0<-RateToProb(rate_data$rate_death_CVD,1) ##一般人群死于cvd的概率
p3_0<-RateToProb(rate_data$rate_death_nonCVD,1) ##一般人群死于非cvd的概率
p5_0<-ProbFactor(p2_0,3.12)  ##有cvd病史的人死亡的概率
p7_0<-ProbFactor(p1_0,1.37)  ##有cvd病史复发cvd的概率

#效用值的赋值
uti_stg<-c(1,0.9,0,0)  ##s1~s4的效用赋值
uti2_stg<--0.038  ##话说这个值怎么算出来的??
year_stg<-c(1,1,0,0)

###2.输入策略相关的参数
##2.1 HR
HR_l_stg1<-0.63  ##策略1低危人群的发病HR
HR_m_stg1<-1.56  ##策略1中危人群的发病HR
HR_h_stg1<-1.6   ##策略1高危人群的发病HR

HR_high_live_cvdth<-1.17  ##策略1、2、3的高危人群死于cvd的HR
p8_min<-0.02  
p8_max<-0.1  ##急性期死亡的均匀分布下限和上限

##2.2输入干预策略的HR
HR_smk_cvd<-0.85   ##戒烟对cvd发病的影响
HR_smk_cvdth<-0.72  ##戒烟对cvd死亡的影响
HR_salt_cvd<-0.81
HR_salt_cvdth<-0.66
HR_wtc_cvd<-0.93
HR_wtc_cvdth<-0.93
HR_hpt_lip_cvd<-0.7
HR_hpt_lip_cvdth<-0.82


##2.3 输入不同策略的高低中危人群分布
#策略1：
population_stg1<-array(NA,dim = c(n_population,n_risk),dimnames = list(names_population,names_risk))
population_stg1[,1]<-c(159,173,188,200,129,71,48,444,738,677,440,216,128,65) ##低危人群，前7位男性，后7位为女性
population_stg1[,2]<-c(8,64,63,89,89,42,28,6,18,20,217,164,136,55) ##中危人群，前7位男性，后7位为女性
population_stg1[,3]<-c(62,207,171,121,104,47,40,48,100,125,185,139,130,67) ##高危人群，前7位男性，后7位为女性

###3.输入选择哪一种策略进行模拟
##如：当选择第一种策略
#不同策略的低中高危人群发病HR不同
HR_l_stg<-HR_l_stg1
HR_m_stg<-HR_m_stg1
HR_h_stg<-HR_h_stg1

#不同策略的低中高危人群分布不同

population_stg<-population_stg1


###(二)运行程序，当选择不同策略及修改输入参数，不需以下改动代码，只需改动以上参数

p1_0_stg<-matrix(0,nrow=n_risk,ncol=n_population,dimnames=list(names_risk,names_population),byrow=F)  ##低中高危人群发生cvd的概率
p1_0_stg[1,]<-ProbFactor(p1_0,HR_l_stg)  ##不同策略筛出的低危人群有不同的发病概率
p1_0_stg[2,]<-ProbFactor(p1_0,HR_m_stg)  ##不同策略筛出的中危人群有不同的发病概率
p1_0_stg[3,]<-ProbFactor(p1_0,HR_h_stg)  ##不同策略筛出的高危人群有不同的发病概率
p1_0_stg[1,]<-ProbFactor(p1_0_stg[1,],1)  ##低危不进行干预
p1_0_stg[2,]<-ProbFactor(p1_0_stg[2,],HR_smk_cvd*HR_salt_cvd*HR_wtc_cvd)  ##中危进行生活方式干预
p1_0_stg[3,]<-ProbFactor(p1_0_stg[3,],HR_smk_cvd*HR_salt_cvd*HR_wtc_cvd*HR_hpt_lip_cvd)  ##高危进行生活方式干预和药物干预

p2_0_stg<-matrix(0,nrow=n_risk,ncol=n_population,dimnames=list(names_risk,names_population),byrow=F) ##低中高危人群死于cvd的概率
p2_0_stg[1,]<-ProbFactor(p2_0,1)  ##低危人群的死于cvd的概率和一般人群相同
p2_0_stg[2,]<-ProbFactor(p2_0,1)  ##中危人群的死于cvd的概率和一般人群相同
p2_0_stg[3,]<-ProbFactor(p2_0,HR_high_live_cvdth)  ##高危人群的死于cvd的概率需乘以HR
p2_0_stg[1,]<-ProbFactor(p2_0_stg[1,],1)  ##低危不进行干预
p2_0_stg[2,]<-ProbFactor(p2_0_stg[2,],HR_smk_cvdth*HR_salt_cvdth*HR_wtc_cvdth)  ##中危进行生活方式干预
p2_0_stg[3,]<-ProbFactor(p2_0_stg[3,],HR_smk_cvdth*HR_salt_cvdth*HR_wtc_cvdth*HR_hpt_lip_cvdth)  ##高危进行生活方式干预和药物干预

p3_stg<-matrix(0,nrow=n_risk,ncol=n_population,dimnames=list(names_risk,names_population),byrow=F)  ##低中高危人群死于非cvd的概率
p3_stg[1,]<-p3_0   
p3_stg[2,]<-p3_0   
p3_stg[3,]<-p3_0   ##低中高危人群死于非cvd的概率都和一般人群相同

p5_0_stg<-matrix(0,nrow=n_risk,ncol=n_population,dimnames=list(names_risk,names_population),byrow=F)  #低中高危人群有cvd病史的人死亡的概率
p5_0_stg[1,]<-p5_0
p5_0_stg[2,]<-p5_0
p5_0_stg[3,]<-p5_0 ##低中高危人群有cvd病史死于cvd的概率和有cvd病史死于cvd的概率相同

p7_0_stg<-matrix(0,nrow=n_risk,ncol=n_population,dimnames=list(names_risk,names_population),byrow=F)  ##低中高危人群有cvd病史的人复发cvd的概率
p7_0_stg[1,]<-p7_0
p7_0_stg[2,]<-p7_0
p7_0_stg[3,]<-p7_0 ##低中高危人群有cvd病史的人复发cvd的概率

p1_stg<-array(0,dim=c(n_cycle,n_risk,n_population),dimnames=list(names_cycle,names_risk,names_population))  ##s1到s2状态的概率
p2_stg<-array(0,dim=c(n_cycle,n_risk,n_population),dimnames=list(names_cycle,names_risk,names_population))  ##s1到s3的概率
p4_stg<-array(0,dim=c(n_cycle,n_risk,n_population),dimnames=list(names_cycle,names_risk,names_population))  ##s1维持在s1的概率
set.seed(1)
p8_stg<-array(runif(n_cycle*n_risk*n_population,min=p8_min,max=p8_max),
              dim=c(n_cycle,n_risk,n_population),dimnames=list(names_cycle,names_risk,names_population))  ##cvd发作急性死亡概率
###这里设定每个层的人群，在每一年的循环中，急性死亡的概率都是不同的，取值服从2%~10%的均匀分布
p5_stg<-array(0,dim=c(n_cycle,n_risk,n_population),dimnames=list(names_cycle,names_risk,names_population))  ##s2到s4的概率
p6_stg<-array(0,dim=c(n_cycle,n_risk,n_population),dimnames=list(names_cycle,names_risk,names_population))  ##s2维持在s2的概率

a_P_stg<-array(0,dim=c(n_state,n_state,n_cycle,n_risk,n_population),
               dimnames=list(names_state,names_state,names_cycle,names_risk,names_population))   ##总的概率转换矩阵

###1)设定概率矩阵
for (i in 1:n_population){ 
  for (m in 1:n_risk){
    if (i==7 | i==14){
      ##10年都是i，后五年无法增加，因为少了一个年龄段的率
      a_P_stg["s1","s3",1:n_cycle,m,i]<-p3_stg[m,i]
      a_P_stg["s2","s3",1:n_cycle,m,i]<-p3_stg[m,i]
      for ( j in 1:n_cycle){
        p2_stg[j,m,i]<-p2_0_stg[m,i]+p1_0_stg[m,i]*p8_stg[j,m,i]
        p1_stg[j,m,i]<-p1_0_stg[m,i]*(1-p8_stg[j,m,i])
        p4_stg[j,m,i]<-1-p1_stg[j,m,i]-p2_stg[j,m,i]-p3_stg[m,i]
        p5_stg[j,m,i]<-p5_0_stg[m,i]+p7_0_stg[m,i]*p8_stg[j,m,i]
        p6_stg[j,m,i]<-1-p5_stg[j,m,i]-p3_stg[m,i]
        a_P_stg["s1","s2",j,m,i]<-p1_stg[j,m,i]
        a_P_stg["s1","s4",j,m,i]<-p2_stg[j,m,i]
        a_P_stg["s1","s1",j,m,i]<-p4_stg[j,m,i]
        a_P_stg["s2","s2",j,m,i]<-p6_stg[j,m,i]
        a_P_stg["s2","s4",j,m,i]<-p5_stg[j,m,i]
      }
      a_P_stg["s3","s3",1:n_cycle,m,i]<-1
      a_P_stg["s4","s4",1:n_cycle,m,i]<-1
    }
    else{
      ###前5年
      for ( j in 1:5){
        a_P_stg["s1","s3",j,m,i]<-p3_stg[m,i]
        a_P_stg["s2","s3",j,m,i]<-p3_stg[m,i]
        p2_stg[j,m,i]<-p2_0_stg[m,i]+p1_0_stg[m,i]*p8_stg[j,m,i]
        p1_stg[j,m,i]<-p1_0_stg[m,i]*(1-p8_stg[j,m,i])
        p4_stg[j,m,i]<-1-p1_stg[j,m,i]-p2_stg[j,m,i]-p3_stg[m,i]
        p5_stg[j,m,i]<-p5_0_stg[m,i]+p7_0_stg[m,i]*p8_stg[j,m,i]
        p6_stg[j,m,i]<-1-p5_stg[j,m,i]-p3_stg[m,i]
        a_P_stg["s1","s2",j,m,i]<-p1_stg[j,m,i]
        a_P_stg["s1","s4",j,m,i]<-p2_stg[j,m,i]
        a_P_stg["s1","s1",j,m,i]<-p4_stg[j,m,i]
        a_P_stg["s2","s2",j,m,i]<-p6_stg[j,m,i]
        a_P_stg["s2","s4",j,m,i]<-p5_stg[j,m,i]
        a_P_stg["s3","s3",j,m,i]<-1
        a_P_stg["s4","s4",j,m,i]<-1
      }
      
      ##后5年
      a_P_stg["s1","s3",6:10,m,i]<-p3_stg[m,i+1]
      a_P_stg["s2","s3",6:10,m,i]<-p3_stg[m,i+1]
      for ( j in 6:10){
        p2_stg[j,m,i]<-p2_0_stg[m,i+1]+p1_0_stg[m,i+1]*p8_stg[j,m,i]
        p1_stg[j,m,i]<-p1_0_stg[m,i+1]*(1-p8_stg[j,m,i])
        p4_stg[j,m,i]<-1-p1_stg[j,m,i]-p2_stg[j,m,i]-p3_stg[m,i+1]
        p5_stg[j,m,i]<-p5_0_stg[m,i+1]+p7_0_stg[m,i+1]*p8_stg[j,m,i]
        p6_stg[j,m,i]<-1-p5_stg[j,m,i]-p3_stg[m,i+1]
        a_P_stg["s1","s2",j,m,i]<-p1_stg[j,m,i]
        a_P_stg["s1","s4",j,m,i]<-p2_stg[j,m,i]
        a_P_stg["s1","s1",j,m,i]<-p4_stg[j,m,i]
        a_P_stg["s2","s2",j,m,i]<-p6_stg[j,m,i]
        a_P_stg["s2","s4",j,m,i]<-p5_stg[j,m,i]
      }
      a_P_stg["s3","s3",6:10,m,i]<-1
      a_P_stg["s4","s4",6:10,m,i]<-1
    }
  }
}

## a_P_stg



###2)初始状态人数
m_M_stg<-array(0,dim=c(n_cycle,n_state,n_risk,n_population),dimnames=list(names_cycle,names_state,names_risk,names_population))
for (i in 1:n_population) {
  for (m in 1:n_risk) {
    m_M_stg[1,1,m,i]<-population_stg[i,m]
  }
}
## m_M_stg

###3)记录10年中每个状态的人数，并计算CVD事件数、CVD死亡数和全因死亡数、QALY、获得生命年

n_cnew_stg<-array(0,dim=c(n_cycle,1,n_risk,n_population),dimnames=list(names_cycle,"num",names_risk,names_population))  ##记录新发cvd人数
n_clive_stg<-array(0,dim=c(n_cycle,1,n_risk,n_population),dimnames=list(names_cycle,"num",names_risk,names_population)) ##记录首发或复发并存活后维持在cvd状态的人数

n_cvd_stg<-matrix(0,ncol = n_population, nrow=n_risk, dimnames = list(names_risk,names_population)) ##cvd发病数
n_cd_stg<-matrix(0,ncol = n_population, nrow=n_risk, dimnames = list(names_risk,names_population))  ##因cvd死亡数
n_nd_stg<-matrix(0,ncol = n_population, nrow=n_risk, dimnames = list(names_risk,names_population))  ##非cvd死亡数
n_d_stg<-matrix(0,ncol = n_population, nrow=n_risk, dimnames = list(names_risk,names_population))   ##全死因死亡数

qaly_stg<-matrix(0,ncol = n_population, nrow=n_risk, dimnames = list(names_risk,names_population))  ##质量调整生命年
qaly_m_stg<-array(0,dim=c(n_cycle,1,n_risk,n_population),dimnames=list(names_cycle,"num",names_risk,names_population))  ##qaly数组
for (i in 1:n_population){
  for (m in 1:n_risk){
    qaly_m_stg[1,1,m,i]<-sum(m_M_stg[1,,m,i] * uti_stg)  ##计算第一年的qaly的情况
  }  
}

lifeyear_stg<-matrix(0,ncol = n_population, nrow=n_risk, dimnames = list(names_risk,names_population)) ##获得生命年
lifeyear_m_stg<-array(0,dim=c(n_cycle,1,n_risk,n_population),dimnames=list(names_cycle,"num",names_risk,names_population))  ##获得生命年数组
for (i in 1:n_population){
  for (m in 1:n_risk){
    lifeyear_m_stg[1,1,m,i]<-sum(m_M_stg[1,,m,i]*year_stg) ##计算第一年的获得生命年的情况
  }
}

#qaly_m_stg
#lifeyear_m_stg


#------------------------------------------------------------------------------
####增加一个概率转换，即各人群中有cvd病史且复发存活的概率；
p9_stg<-array(NA,dim=c(n_cycle,n_risk,n_population),dimnames=list(names_cycle,names_risk,names_population))
for (i in 1:n_population){
  for(m in 1:n_risk){
    if (i==7 | i==14){
      for (j in 1:n_cycle)
        p9_stg[j,m,i]<-p7_0_stg[m,i]*(1-p8_stg[j,m,i])
    }
    else {
      for (j in 1:5){
        p9_stg[j,m,i]<-p7_0_stg[m,i]*(1-p8_stg[j,m,i])
      }
      for (j in 6:10){
        p9_stg[j,m,i]<-p7_0_stg[m,i+1]*(1-p8_stg[j,m,i])
      }
    }
  }
}
## p9_stg
###########################################################

########增加一个概率转换，即各人群中无cvd的人cvd发病概率
p10_stg<-array(NA,dim=c(n_cycle,n_risk,n_population),dimnames=list(names_cycle,names_risk,names_population))
for (i in 1:n_population){
  for (m in 1:n_risk){
    if (i==7 | i==14){
      for (j in 1:n_cycle)
        p10_stg[j,m,i]<-p1_0_stg[m,i]
    }
    else {
      for (j in 1:5){
        p10_stg[j,m,i]<-p1_0_stg[m,i]
      }
      for (j in 6:10){
        p10_stg[j,m,i]<-p1_0_stg[m,i+1]
      }
    }
  }
}
## p10_stg
######################################################


for (i in 1:n_population){
  for (m in 1:n_risk){
    for (j in 1:(n_cycle-1)){
      m_M_stg[j+1,,m,i]<-m_M_stg[j,,m,i] %*% a_P_stg[,,j,m,i]
      n_cnew_stg[j+1,1,m,i]<-m_M_stg[j,1,m,i] * p10_stg[j,m,i]   ##新发cvd=s1人数*p10(无cvd发病概率)
      n_clive_stg[j+1,1,m,i]<-m_M_stg[j,1,m,i]*a_P_stg["s1","s2",j,m,i] +m_M_stg[j,2,m,i]*p9_stg[j,m,i] ##首发或复发cvd并存活的人数=s1人数*p1+s2人数*p9
      qaly_m_stg[j+1,1,m,i]<-sum(m_M_stg[j+1,,m,i] * uti_stg)+sum(n_clive_stg[j+1,1,m,i]*uti2_stg) ##qaly=等于四个状态的qaly之和+损失的qaly
      lifeyear_m_stg[j+1,,m,i]<-sum(m_M_stg[j+1,,m,i]*year_stg)
    }
    n_cvd_stg[m,i]<-sum(n_cnew_stg[1:n_cycle,1,m,i])
    n_cd_stg[m,i]<-m_M_stg[n_cycle,4,m,i]
    n_nd_stg[m,i]<- m_M_stg[n_cycle,3,m,i]
    n_d_stg[m,i]<-n_cd_stg[m,i]+n_nd_stg[m,i]
    qaly_stg[m,i]<-sum(qaly_m_stg[1:n_cycle,1,m,i])
    lifeyear_stg[m,i]<-sum(lifeyear_m_stg[1:n_cycle,1,m,i])
  }
}

# m_M_stg  ##各人群各状态每个周期循环的人数
# n_cnew_stg  ##各人群每个周期新增的cvd人数 
# n_clive_stg  ##各人群每个周期发生cvd且存活的人数


# n_cvd_stg ##各人群cvd发病数
# n_cd_stg##各人群因cvd死亡数
# n_nd_stg##各人群非cvd死亡数
# n_d_stg##各人群全死因死亡数
# qaly_stg ##各人群qaly
# lifeyear_stg ##各人群生命年

N_cvd_stg1<-sum(n_cvd_stg)
N_cd_stg1<-sum(n_cd_stg)
N_nd_stg1<-sum(n_nd_stg)
N_d_stg1<-sum(n_d_stg)
QALY_stg1<-sum(qaly_stg)
LIFEYEAR_stg1<-sum(lifeyear_stg)

N_cvd_stg1 ##总人群cvd发病数
N_cd_stg1 ##总人群因cvd死亡数
N_nd_stg1 ##总人群非cvd死亡数
N_d_stg1 ##总人群全死因死亡数yy
QALY_stg1 ##总人群qaly
LIFEYEAR_stg1  ##总人数获得生命年