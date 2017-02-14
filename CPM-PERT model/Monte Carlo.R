source("CPM-PERT model/Program.R",encoding="UTF-8")
#source("U:/OR/CPM-PERT/CPM-PERT model/Program.R", encoding="UTF-8")
install.packages("msm")
library(msm)
#Če je čas slučajna spremenljivka, porazdeljena enakomerno zvezno na intervalu [1,20], je 
#pričakovana vrednost časa posameznega opravila enaka 10, varianca pa je enaka 100/3

A<-c()
for (i in 1:1000){
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
  for (i in 1:100){
    C1<-runif(18,0,j)
    A1<-c(A1,trajanje(Opr,Pred,C1))
  }
  P<-c(P,mean(A1)) #vektor P je vektor vseh pričakovanih vrednosti trajanja projekta
  R<-c(R, var(A1)) #vektor R je vektor vseh varianc trajanja projekta
  O<-c(O,j) #vektor števil od 1 do 100
}
plot(O/2,P) #odvisnost od pričakovane vrednosti trajanja naloge in pričakovane vrednosti trajanja projekta
plot((O^2)/12,R) #odvisnost variance trajanja posamezne naloge in variance trajanja projekta
#Ti dve odvisnosti sta linearni, kar se vidi pri valikem številu ponovitev. Ker je to precej 
#časovno zahtevno, je bil poskus s 1000 simulacijami na vsakoem koraku ponovljen le enkrat, je pa
#veliko bolj očitno prikazal linearnost

#V naslednjem delu je čas opravljanja posamezne naloge porazdeljen okrnjeno normalno
B<-c()
for (i in 1:1000){
  D<-rtnorm(18, 50, 10, lower = 0, upper = 200)
  B<-c(B,trajanje(Opr,Pred,D))
}

E1<-mean(B)
V1<-var(B)
hist(B)
hist(D)
#Vidimo, da porazdelitev trajanja tudi v tem primeru spominja na normalno porazdelitev. Verjetno
#bi z vedno večjim številom poskusov dobili vedno "bolj normalno" porazdelitev

O2<-c()
P2<-c()
R2<-c()
for(j in 1:100){
  B1<-c()
  for (i in 1:100){
    D1<-rtnorm(18, 50, j, lower = 0, upper = 100)
    B1<-c(B1,trajanje(Opr,Pred,D1))
  }
  P2<-c(P2,mean(B1))
  R2<-c(R2, var(B1))
  O2<-c(O2,j)
}
hist(P2) #histogram nam pokaže, da je v največ primerih pričakovana vednost trajanja projekta med
#345 in 355, porazdelitve pa ne prepoznam kot točno določene
plot(O2,R2)#odvisnost med varianco trajanja posameznih opravil in varanco trajanja projekta

#Eksponentna porazdelitev posameznih opravil
M<-c()
for (i in 1:1000){
  N<-rexp(18,rate=1)
  M<-c(M,trajanje(Opr,Pred,N))
}
hist(M)
#pri eksponentni porazdelitvi trajanja opravil spet ugotovimo, da je porazdelitev slučajne 
#spremenljivke trajanja projekta podobna normalni

O1<-c()
P1<-c()
R1<-c()
for(j in seq(0.1,10,0.1)){
  M1<-c()
  for (i in 1:100){
    N1<-rexp(18,rate=j)
    M1<-c(M1,trajanje(Opr,Pred,N1))
  }
  P1<-c(P1,mean(M1))
  R1<-c(R1, var(M1))
  O1<-c(O1,j)
}
plot(1/O1,P1)#odvisnost pričakovane vrednosti trajanja projekta od pričakovanega trajanja opravil
plot(1/O1^2,R1)#odvisnost varianc pri eksponentni porazdelitvi
#Pri eksponentni porazdelitvi vidimo, da sta upanji in varianci med seboj linearno odvisni.
