## data preparation
library(data.table)
gender<-c(rep(0,20),rep(1,20))#0--female
place<-c(rep(0,10),rep(1,10),rep(0,10),rep(1,10))#0--city
safe<-rep(c(rep(0,5),rep(1,5)),4)#0--none
injure<-rep(c("case1","case2","case3","case4","case5"),8)
fre<-c(c(7287,175,720,91,10),
          c(11587,126,577,48,8),
          c(3246,73,710,159,31),
          c(6134,94,564,82,17),
          c(10381,136,566,96,14),
          c(10969,83,259,37,1),
          c(6123,141,710,188,45),
          c(6693,74,353,74,12))
inc<-data.table(gender,place,safe,injure,fre)
mydata<-dcast(inc,gender+place+safe~injure,value.var = "fre")
mydata0<-as.matrix(mydata)


## fit the model
library(VGAM)
model_1<-vglm(formula=cbind(case1,case2,case3,case4,case5) ~ gender+place+safe,family = multinomial,data=mydata)
summary(model_1)
model_2<-vglm(formula=cbind(case1,case2,case3,case4,case5) ~ place+safe,family = multinomial,data=mydata)
summary(model_2)

library(MASS)
inc$injure<-ordered(as.factor(inc$injure))
inc$gender<-as.factor(inc$gender)
inc$place<-as.factor(inc$place)
inc$safe<-as.factor(inc$safe)
#mydata2<-melt.data.table(mydata,id.vars = c("gender","place","safe"),variable.name =c("CASE"),value.name="fre")
model_3<-polr(as.factor(injure)~gender+place+safe, data=inc, weights = fre,method = "logistic")
summary(model_3)

pr <- profile(model_3)
confint(pr)
plot(pr)
pairs(pr)

model_31<-polr(as.factor(injure)~place+safe, data=inc, weights = fre,method = "logistic")
summary(model_31)

model_4<-polr(as.factor(injure)~gender+place+safe, data=inc, weights = fre,method ="probit" )
summary(model_4)








library(MASS)
house.plr<-polr(Sat~Infl+Type+Cont,weights=Freq,data=housing)
summary(house.plr)
summary(house.plr)