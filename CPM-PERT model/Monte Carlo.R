#source("CPM-PERT model/Program.R",encoding="UTF-8")
source("U:/OR/CPM-PERT/CPM-PERT model/Program.R", encoding="UTF-8")
#install.packages("msm")
library(msm)
#Če je čas slučajna spremenljivka, porazdeljena enakomerno zvezno na intervalu [1,20], je 
#pričakovana vrednost časa posameznega opravila enaka 10, varianca pa je enaka 100/3
A <- sapply(1:1000, function(i) trajanje(Opr,Pred,runif(18,0,20)))
#S poskusom za 20000 ponovitev (precej dolgotrajen), sem za pričakovano vrednost dobila 96.854096,
#za varianco pa 227.17752
hist(A)
#Histogram, ki ga dobimo v tem primeru ima obliko normalne porazdelitve, 


#V naslednjem delu opazujemo kakšna je odvisnost variance in pričakovane vrednosti trajanja projekta
#od pričakovane vrednosti in variance trajanja posameznega opravila
O<-c()
P<-c()
R<-c()
for (j in 1:100){
  A1 <- sapply(1:100, function(i) trajanje(Opr,Pred,runif(18,0,j)))
  P<-c(P,mean(A1)) #vektor P je vektor vseh pričakovanih vrednosti trajanja projekta
  R<-c(R, var(A1)) #vektor R je vektor vseh varianc trajanja projekta
  O<-c(O,j) #vektor števil od 1 do 100
}
plot(O/2,P) #odvisnost od pričakovane vrednosti trajanja naloge in pričakovane vrednosti trajanja projekta
plot((O^2)/12,R)#odvisnost variance trajanja posameznega opravila in variance trajanja projekta
plot((O^2)/12,P) #odvisnost variance trajanja posamezne naloge in pričakovanega trajanja projekta

