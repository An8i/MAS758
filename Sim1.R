p<-rep(NA, 10000)

for (i in 1:10000){
ar1<-arima.sim(model=list(ar=0.9),n=200,sd=1)+1
ar2<-arima.sim(model=list(ar=0.9),n=200,sd=1)+1
ar3<-arima.sim(model=list(ar=0.9),n=200,sd=1)+1 

subgrp<-as.factor(rep(1:3, c(200,200,200)))

data1<- c(ar1,ar2,ar3)

data<-data.frame(cbind(data1,subgrp))

fit <- aov(data1 ~ subgrp, data=data)

summary(fit)

p[i]<-summary(fit)[[1]][["Pr(>F)"]][[1]]

}

result<-length(which(p<=0.05))/10000
result