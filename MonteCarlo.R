source("CPM-PERT model/Program.R",encoding="UTF-8")
A<-c()

for (i in 1:10000){
  C<-runif(18,0,20)
  A<-c(A,trajanje(Opr,Pred,C))
}
plot(A)
mean(A)
var(A)
B<-c()
for (i in 1:2000){
  D<-runif(18,0,20)
  B<-c(B,trajanje(Opr,Pred,D))
}
plot(B)

