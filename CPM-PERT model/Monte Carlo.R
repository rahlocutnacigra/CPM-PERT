#source("CPM-PERT model/Program.R",encoding="UTF-8")
source("U:/OR/CPM-PERT/CPM-PERT model/Program.R", encoding="UTF-8")

#Če je čas slučajna spremenljivka, porazdeljena enakomerno zvezno na intervalu [1,20], je 
#pričakovana vrednost časa posameznega opravila enaka 10, varianca pa je enaka 100/3

A<-c()
for (i in 1:20000){
  C<-runif(18,0,20)
  A<-c(A,trajanje(Opr,Pred,C))
}
#S poskusom za 20000 ponovitev (precej dolgotrajen), sem za pričakovano vrednost dobila 96.854096,
#za varianco pa 227.17752
E<-mean(A)
V<-var(A)
hist(A)
#Histogram, ki ga dobimo v tem primeru ima obliko normalne porazdelitve, 


#V naslednjem delu opazujemo kakšna je odvisnost variance in pričakovane vrednosti trajanja projekta
#od pričakovane vrednosti in variance trajanja posameznega opravila
O<-c()
P<-c()
R<-c()
for (j in 1:100){
  A1<-c()
  for (i in 5:100){
    C1<-runif(18,0,j)
    A1<-c(A1,trajanje(Opr,Pred,C1))
  }
  P<-c(P,mean(A1))
  R<-c(R, var(A1))
  O<-c(O,j)
}
Pl1<-plot(O/2,P)
Pl2<-plot((O^2)/12,R)
plot(R)

#V naslednjem delu je čas opravljanja posamezne naloge porazdeljen normalno
B<-c()
for (i in 1:1000){
  D<-rnorm(18,20,15)
  i<-min(D)
  D<-D+i
  B<-c(B,trajanje(Opr,Pred,D))
}

E1<-mean(B)
V1<-var(B)

hist(B)

#Eksponentna porazdelitev
M<-c()
for (i in 1:10000){
  N<-rexp(18,rate=i)
  M<-c(M,trajanje(Opr,Pred,N))
}
hist(M)

O1<-c()
P1<-c()
R1<-c()
for(j in 1:100){
  M1<-c()
  for (i in 1:100){
    N1<-rexp(18,rate=i)
    M1<-c(M1,trajanje(Opr,Pred,N1))
  }
  P1<-c(P1,mean(M1))
  R1<-c(R1, var(M1))
  O1<-c(O1,j)
}
plot(1/O1,P1)
plot(1/O1^2,R1)
