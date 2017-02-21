source("U:/OR/CPM-PERT/CPM-PERT model/Program.R", encoding="UTF-8")
#install.packages("msm")
library(msm)

#Gamma porazdelitev
G<-sapply(1:1000, function(i) trajanje(Opr,Pred,rgamma(18, 1,2)))
hist(G)

#spreminjamo parameter shape
OG<-c()
PG<-c()
RG<-c()
for(j in 1:100){
  G1 <- sapply(1:100, function(i) trajanje(Opr,Pred,rgamma(18, 3, j)))
  PG<-c(PG,mean(G1))
  RG<-c(RG, var(G1))
  OG<-c(OG,j)
}
plot(3/OG,PG)
plot(3/OG^2,PG)

#spreminajmo parameter "scale"
OG1<-c()
PG1<-c()
RG1<-c()
for(j in 1:100){
  G2 <- sapply(1:100, function(i) trajanje(Opr,Pred,rgamma(18, j, 2)))
  PG1<-c(PG1,mean(G2))
  RG1<-c(RG1, var(G2))
  OG1<-c(OG1,j)
}
plot(OG1/2,PG1)
plot(OG1/4,PG1)