#Opr<-(1:12)
#Pred<-list(c(0),c(1),c(2,7),c(0),c(0),c(4,5),c(6),c(3),c(3),c(8),c(9),c(9))
#Cas<-c(5,3,4,9,12,3,1,7,5,1,3,6)
Opr<-(1:18)
Pred<-list(c(0),c(1),c(2),c(3),c(4,17),c(5),c(6),c(6),c(2,10),c(1),c(10),c(11),c(12,17),c(13),c(11),c(15,3),c(9,15,16),c(5,13))
Cas<-c(5,3,4,9,12,3,1,7,5,1,3,6,5,3,4,9,12,3)
#Funkcija Adj sestavi matriko sosednosti, če imamo podan vektor opravil in seznam sosedov
Adj<-function(Opr, Pred){
  z=length(Opr)+1
  NO<-c(0, Opr,z) #dodamo opravili "začetek" in "konec"
  A<-matrix(nrow=z+1,ncol=z+1)
  O<-Opr
  #za vse sosede vozlišča na ustrezna mesta v matriki napišemo enice:
  for (i in Opr){
    p<-Pred[[i]]
    for (j in p){
     A[j+1,i+1]<-1
     O[!O %in% p]
    }
  } 
  #na mesta kjer ni enice napišemo 0:
  for(i in 1:(z+1)){
    for(j in 1:(z+1)){
      if(is.na(A[i,j])){
        A[i,j]<-0
      }
    }
  }
 #opravila, ki niso pogoj za nobeno drugo opravilo "povežemo" z opravilom "konec" 
  for(i in 1:z){
    if(sum(A[i,])==0){
      A[i,z+1]<-1
    }
  }
    
  return(A)
}
#Topološko urejanje
top.sort<-function(Opr,Pred){
  Q<-c()
  st<-c()
  top.ured<-c()
  adj<-Adj(Opr,Pred)
  z=length(Opr)+1
  #zaradi lažjega programiranja, ima vozlišče "začetek" od sedaj dalje številko 1, podana opravila 
  #imajo številke 2 do n+1, vozlišče "konec" pa ima številko n+2 (n je število opravil)
  NO<-c(1, Opr+1,z+1)
  #določimo stopnjo vozlišč
  for(v in NO){
    st[v]<-sum(adj[,v])
  }
  #vozlišča s stopnjo 0 dodamo v vektor Q
  for(v in NO){
    if (st[v]==0){
      Q<-c(Q,v)
    }
  }
  j<-0
  top.ured<-c()
  while(j<(length(Pred)+2)){ 
    for(u in Q){
      remove<-c(u) #
      Q<-Q[!Q %in% remove] #iz vektorja Q izbrišemo obravnavamo vozlišče
      top.ured[u]<-j # obravnavanemu opravilu dodamo zaporedno številko opravljanja
      j<-j+1
      s<-adj[u,] #vektor, ki nam pove za katera opravila je pogoj obravnavano opravilo u
      #znižamo stopnjo vseh vozlišč, katerih pogoj je u, opravila katerih stopnja je 0 dodamo v Q
      for(v in 1:length(s)){
        if(s[v]==1){
          st[v]<-st[v]-1 
          if(st[v]==0){
            Q<-c(Q,v) 
          }
        }
      }
    }
  }
  #dobimo vektor zaporednih številk opravljanja opravil
  return(top.ured)
}
#funkcija vrstni.red nam vrne vrstni red opravljanja opravil
vrstni.red<-function(sez){
  j<-0
  zaporedje<-c()
  while(j<length(sez)){
    for(i in 1:length(sez)){
      if(sez[i]==j){
        zaporedje[j]<-i-1
        j<-j+1
      }
    }
  }
  return(zaporedje)
}
#funkcija trajanje nam izračuna čas trajanja projekta
trajanje<-function(Opr,Pred,Cas){
  vr<-vrstni.red(top.sort(Opr,Pred))
  cas<-c()
  cas[vr[1]]<-0 #prvo opravilo lahko začnemo v času 0
  for (i in 2:(length(vr)-1)){ #gledamo za opravila, brez končnega - končno opravilo obdelamo na koncu
    #za vsako predhodno opravilo opravila na i-tem mestu poiščemo čas, ko to opravilo lahko začnemo in
    #mu prištejemo čas opravljanja
    c.pred <- c(0, sapply(Pred[[vr[i]]], function(j) cas[j]+Cas[j]))
    #maksimum teh števil je minimalni čas, ko lahko začnemo opravljati opravilo z zaporedno številko i
    cas[vr[i]]<-max(c.pred)
  }
  #obravnavamo še končno opravilo. Delamo po istem postopku kot prej. Dobimo čas zaključka končnega opravila
  a<-Adj(Opr,Pred)[,length(vr)+1]
  predh<-c()
  for(i in 1:length(a)){
    if(a[i]==1){
      predh<-c(predh,(i-1))
    }
  }
  c1<-c()
  for(j in predh){
    c1<-c(c1,cas[j]+Cas[j])
  }
  konec<-max(c1)
  return(konec)
}
