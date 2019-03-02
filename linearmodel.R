data<-read.csv("C:/Users/traceur/Desktop/data.csv",header=T)
head(data)

#散点图分析
pairs(data)
cor(data)
cor.test(data$edu_expenditurepercent,data$nonagr_per)

#回归分析
fit0<-lm(edu_expenditurepercent~.,data=data)
summary(fit0)
anova(fit0)
drop1(fit0)
step(fit0)

#outlier
h<-hatvalues(fit0)
hstan<-1
count<-0
for(i in h){
  if(i>hstan)
    count<-count+1
}
count
#数据完整度高，没有outlier

#人均gdp对模型影响不显著，去掉进行分析
fit1<-lm(edu_expenditurepercent~nonagr_per+ sex + edu + age,data=data)
summary(fit1)


#全方差分析模型
data$edu<-factor(data$edu)
data$age<-factor(data$age)
data$sex<-factor(data$sex)
fita1<-aov(edu_expenditurepercent~.,data=data)
summary(fita1)
fita2<-aov(edu_expenditurepercent~sex+edu + nonagr_per+pergdp,data=data)
summary(fita2)

library(lsmeans)
sex.means<-lsmeans(ref.grid(fita2),"sex")
edu.means<-lsmeans(ref.grid(fita2),"edu")
age.means<-lsmeans(ref.grid(fita2),"age")
age.means

#偏相关系数分析
library(ggm)
library(ggm)
library(igraph)

s<-cov(data)
r<-pcor(c(1,4,2),s)
r
pcor.test(r,1,dim(data))
r<-pcor(c(1,5,2),s)
r
pcor.test(r,1,dim(data))
r<-pcor(c(4,2,1,3),s)
r
pcor.test(r,2,dim(data))

