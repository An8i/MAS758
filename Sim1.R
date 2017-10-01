nsim<-3000
nts<-20
autoc<-0.9

count<-0
for (i in 1:nsim){

  ar1<-arima.sim(model=list(ar=autoc),n=nts)+1
  ar2<-arima.sim(model=list(ar=autoc),n=nts)+1
  ar3<-arima.sim(model=list(ar=autoc),n=nts)+1

  subgrp<-as.factor(rep(c("a","b","c"), c(nts,nts,nts)))

  tser<- c(ar1,ar2,ar3)

  data2<-data.frame(tser,subgrp)

  fit <- aov(tser~ subgrp, data=data2)
  if (summary(fit)[[1]][["Pr(>F)"]][[1]] <= 0.05) {count<-count+1}
}

result<-count/nsim
result
