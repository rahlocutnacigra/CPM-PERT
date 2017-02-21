source("U:/OR/CPM-PERT/CPM-PERT model/Program.R", encoding="UTF-8")
#install.packages("msm")
library(msm)

#Eksponentna porazdelitev posameznih opravil
C <- sapply(1:1000, function(i) trajanje(Opr,Pred,rexp(18, rate =1)))
hist(C)
#pri eksponentni porazdelitvi trajanja opravil spet ugotovimo, da je porazdelitev slučajne 
#spremenljivke trajanja projekta podobna normalni

#najprej izvedemo simulacijo za lambda < 1
O1<-c()
P1<-c()
R1<-c()
for(j in seq(0.1,1,0.01)){
  C1 <- sapply(1:100, function(i) trajanje(Opr,Pred,rexp(18, rate =j)))
  P1<-c(P1,mean(C1))
  R1<-c(R1, var(C1))
  O1<-c(O1,j)
}
plot(1/O1,P1)#odvisnost pričakovane vrednosti trajanja projekta od pričakovanega trajanja opravil
plot(1/O1^2,R1)#odvisnost varianc pri eksponentni porazdelitvi
plot(1/O1^2,P1)#odvisnost pričakovanega trajanja projekta od variance trajanja posameznih opravil

#za lambda > 1
O3<-c()
P3<-c()
R3<-c()
for(j in seq(1,10,0.1)){
  C2 <- sapply(1:100, function(i) trajanje(Opr,Pred,rexp(18, rate =j)))
  P3<-c(P3,mean(C2))
  R3<-c(R3, var(C2))
  O3<-c(O3,j)
}
plot(1/O3,P3)#odvisnost pričakovane vrednosti trajanja projekta od pričakovane vrednosti trajanja opravil
plot(1/O3^2,P3)#odvisnost pričakovane vrednosti trajanja projekta od variance trajanja posameunih opravil
plot(1/O3^2,R3)#odvisnost varianc
