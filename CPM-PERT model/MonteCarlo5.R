source("U:/OR/CPM-PERT/CPM-PERT model/Program.R", encoding="UTF-8")
#install.packages("msm")
library(msm)

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