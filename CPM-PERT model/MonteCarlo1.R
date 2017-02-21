source("U:/OR/CPM-PERT/CPM-PERT model/Program.R", encoding="UTF-8")
#install.packages("msm")
library(msm)

#V naslednjem delu je čas opravljanja posamezne naloge porazdeljen okrnjeno normalno

B <- sapply(1:1000, function(i) trajanje(Opr,Pred,rtnorm(18,50, 10, lower = 0, upper = 200)))
hist(B)

#Vidimo, da porazdelitev trajanja tudi v tem primeru spominja na normalno porazdelitev. Verjetno
#bi z vedno večjim številom poskusov dobili vedno "bolj normalno" porazdelitev

O2<-c()
P2<-c()
R2<-c()
for(j in 1:100){
  B1 <- sapply(1:1000, function(i) trajanje(Opr,Pred,rtnorm(18,50, j, lower = 0, upper = 200)))
  P2<-c(P2,mean(B1))
  R2<-c(R2, var(B1))
  O2<-c(O2,j)
}

plot(O2,P2)#odvisnost med varianco trajanja posameznih opravil in varanco trajanja projekta
plot(O2,R2)#odvisnost med obema variancama
