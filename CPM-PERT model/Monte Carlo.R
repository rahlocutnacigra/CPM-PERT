source("CPM-PERT model/Program.R",encoding="UTF-8")
#source("U:/OR/CPM-PERT/CPM-PERT model/Program.R", encoding="UTF-8")
install.packages("msm")
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

               
###############################################################################################################



# Opr<-(1:18)
# Pred<-list(c(0),c(1),c(2),c(3),c(4,17),c(5),c(6),c(6),c(2,10),c(1),c(10),c(11),c(12,17),c(13),c(11),c(15,3),c(9,15,16),c(5,13))
# Cas<-c(5,3,4,9,12,3,1,7,5,1,3,6,5,3,4,9,12,3)

require("truncnorm")

####################################################################
#Premaknjena normalna
simul1 <- function(cas, sd, p) {
        sluc <- vector(length = length(cas))
        i <- 1
        for(c in cas){
                sluc[i] <- rtruncnorm(1, c * p, mean = c, sd = sd)
                i <- i+1
        }
        return(sluc)
}

#simulacija
sim1 <- replicate(10000, trajanje(Opr, Pred, simul1(Cas, 1, 0.7)))
u1 <- mean(sim1) 
s1 <- sd(sim1)
mi1 <- min(sim1)
ma1 <- max(sim1)

#histogram
hist(sim1, breaks =20, xlab = "Trajanje v dnevih", 
     ylab = "Frekvenca", main = "Trajanje projekta")
abline(v = u1, col = "blue", lwd = 2)
arrows(u1, 0, x1 = u1-s1, y1 = 0, length = 0.25, angle = 30,
       code = 2, col = "red", lwd = 2)
arrows(u1, 0, x1 = u1+s1, y1 = 0, length = 0.25, angle = 30,
       code = 2, col = "red", lwd = 2)

# spreminjanje sd opravil
#matrika z več ponovitvami(po stolpcih) pri različnih standardnih odklonih(po vrsticah)

std <- seq(0, 3, by = 0.05)
pon <- 100
A <- matrix(ncol = pon, nrow = length(std))

for(c in 1:ncol(A)) {
        i <- 1
        for(s in std){
                A[i, c] <- trajanje(Opr, Pred, simul1(Cas, s, 0.8))
                i<- i+1
        }
}

# Povezava med standardnim odklonom opravil in povprečnim trajanjem projekta

#linearno ?
plot(std, rowMeans(A), col = "black", bg = "red", pch =21,
     main ="Povezava med standardnim odklonom opravil in trajanjem projekta", 
     xlab = "Standardni odklon", ylab = "Trajanje projekta")

####################################################################
# Enakomerno zvezna

simul2 <- function(cas, m) {
        if(m>2 | m < 0) {return(FALSE)}
        sluc <- vector(length = length(cas))
        n <- max(m, 2-m)
        m <- min(m, 2-m)
        i <- 1
        for(c in cas){
                sluc[i] <- runif(1, min = m*c, max = n * c)
                i <- i+1
        }
        return(sluc)
}

#simulacija
sim2 <- replicate(10000, trajanje(Opr, Pred, simul2(Cas, 0.5)))
u2 <- mean(sim2) 
s2 <- sd(sim2)
mi2 <- min(sim2)
ma2 <- max(sim2)

#histogram
hist(sim2, breaks =20, xlab = "Trajanje v dnevih", 
     ylab = "Frekvenca", main = "Trajanje projekta")
abline(v = u2, col = "blue", lwd = 2)
arrows(u2, 0, x1 = u2-s2, y1 = 0, length = 0.25, angle = 30,
       code = 2, col = "red", lwd = 2)
arrows(u2, 0, x1 = u2+s2, y1 = 0, length = 0.25, angle = 30,
       code = 2, col = "red", lwd = 2)

# spreminjanje intervala pri EZ

#matrika z več ponovitvami(po stolpcih) pri različnih intervalih(po vrsticah)

a2 <- seq(1, 0, by = -0.02)
pon <- 100
B <- matrix(ncol = pon, nrow = length(a2))

for(c in 1:ncol(B)) {
        i <- 1
        for(s in a2){
                B[i, c] <- trajanje(Opr, Pred, simul2(Cas, s))
                i<- i+1
        }
}


#matrika z več ponovitvami(po stolpcih) pri različnih standardnih odklonih(po vrsticah)

plot(seq(0, 1, by = 0.02), rowMeans(B), col = "black", bg = "red", pch =21,
     main ="Povezava med standardnim odklonom opravil in trajanjem projekta", 
     xlab = "Smer naraščanja standardnega odklona", ylab = "Trajanje projekta")



####################################################################
# Enakomerno zvezna, naslednje opravilo lahko začnemo šele naslednji dan 

# Najdemo opravila ki so predhodniki 
ip <- c()
for(a in 1:(length(Opr)-1)) {
        for(b in 1:length(Pred)){
                if(a %in% Pred[[b]] && !(a %in% ip)) {ip <- c(ip, a)}
        }
}

simul21 <- function(cas, m) {
        if(m>2 | m < 0) {return(FALSE)}
        sluc <- vector(length = length(cas))
        n <- max(m, 2-m)
        m <- min(m, 2-m)
        i <- 1
        for(c in cas){
                sluc[i] <- if(i %in% ip) {ceiling(runif(1, min = m*c, max = n * c))}else{runif(1, min = m*c, max = n * c)}
                i <- i+1
        }
        return(sluc)
}

#simulacija
sim21 <- replicate(10000, trajanje(Opr, Pred, simul21(Cas, 0.5)))
u21 <- mean(sim21) 
s21 <- sd(sim21)
mi21 <- min(sim21)
ma21 <- max(sim21)

#histogram
hist(sim21, breaks =20, xlab = "Trajanje v dnevih", 
     ylab = "Frekvenca", main = "Trajanje projekta")
abline(v = u21, col = "blue", lwd = 2)
arrows(u21, 0, x1 = u21-s21, y1 = 0, length = 0.25, angle = 30,
       code = 2, col = "red", lwd = 2)
arrows(u21, 0, x1 = u21+s21, y1 = 0, length = 0.25, angle = 30,
       code = 2, col = "red", lwd = 2)


#matrika z več ponovitvami(po stolpcih) pri različnih intervalih(po vrsticah)
w

####################################################################
# BINOMSKA, diskretni časi trajanja opravil

simul3 <- function(cas, n) {
        sluc <- vector(length = length(cas))
        i <- 1
        for(c in cas){
                p <- c / n
                sluc[i] <- max(rbinom(1, n, p), round(0.7*c)) # opravilo najhitreje opravimo v pribl. 80% predvidenega časa
                i <- i+1
        }
        return(sluc)
}

#simulacija
sim3 <- replicate(10000, trajanje(Opr, Pred, simul3(Cas, 25)))
u3 <- mean(sim3) 
s3 <- sd(sim3)
mi3 <- min(sim3)
ma3 <- max(sim3)


#histogram
hist(sim3, breaks =20, xlab = "Trajanje v dnevih", 
     ylab = "Frekvenca", main = "Trajanje projekta")
abline(v = u3, col = "blue", lwd = 2)
arrows(u3, 0, x1 = u3-s3, y1 = 0, length = 0.25, angle = 30,
       code = 2, col = "red", lwd = 2)
arrows(u3, 0, x1 = u3+s3, y1 = 0, length = 0.25, angle = 30,
       code = 2, col = "red", lwd = 2)


# #matrika z več ponovitvami(po stolpcih) pri različnih intervalih(po vrsticah)
# 
# a3 <- seq((max(Cas)+1), 50, by = 1)
# pon <- 100
# D <- matrix(ncol = pon, nrow = length(a3))
# 
# for(c in 1:ncol(D)) {
#         i <- 1
#         for(s in a3){
#                 D[i, c] <- trajanje(Opr, Pred, simul3(Cas, s))
#                 i<- i+1
#         }
# }
# 
# 
# plot(seq((max(Cas)+1), 50, by = 1), rowMeans(D), type = "l", col = "black", bg = "red", pch =21,
#      main ="Povezava med standardnim odklonom opravil in trajanjem projekta", 
#      xlab = "n (Smer naraščanja standardnega odklona)", ylab = "Trajanje projekta")
# 










