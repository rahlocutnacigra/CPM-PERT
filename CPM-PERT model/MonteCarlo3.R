source("U:/OR/CPM-PERT/CPM-PERT model/Program.R", encoding="UTF-8")
#install.packages("msm")
library(msm)

#Porazdelitev hi-kvadrat 
D<-sapply(1:1000, function(i) trajanje(Opr,Pred,rchisq(18, 1)))
hist(D)

O4<-c()
P4<-c()
R4<-c()
for(j in 1:100){
  D1 <- sapply(1:100, function(i) trajanje(Opr,Pred,rchisq(18, j)))
  P4<-c(P4,mean(D1))
  R4<-c(R4, var(D1))
  O4<-c(O4,j)
}
plot(O4,P4)#odvisnost obeh pričakovanih vrednosti
plot(2*O4,P4)#odvisnost pričakovane vrednosti trajanja projekta od variance trajanja opravil
plot(2*O4,R4)#odvisnost varianc
