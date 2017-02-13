source("CPM-PERT model/Program.R",encoding="UTF-8")

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
#Histogram, ki ga dobimo v tem primeru ima obliko normalne porazdelitve 

B<-c()
for (i in 1:500){
  D<-runif(18,0,20)
  B<-c(B,trajanje(Opr,Pred,D))
}

E1<-mean(B)
V1<-var(B)

hist(B)
