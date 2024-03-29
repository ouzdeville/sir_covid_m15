current_hosp=4  # le nombre de cas connus
doubling_time=6 #le temps de dédoublement 6 jours
distanciation=0.1 # dépendant des mesures de distanciation sociale
hosp_rate=6/100 # taux d'hospitalisation simple
icu_rate=1/100 # taux de soins intensifs
vent_rate=3/100 # taux de personnes en ventilation
asymp_rate=1-hosp_rate-icu_rate-vent_rate # le taux de personne asyptomatique
hosp_los=7 # durée moyenne de séjour length of stay
icu_los=9
vent_los=10

market_share= 15/100 # Part de marché hospitalier

initial_infection=as.integer(current_hosp/market_share/hosp_rate)
N=16000000 # Population Tatale
S=N-initial_infection # les persones succeptibles
I=initial_infection # les infectées
R=N-S-I # les rétablies
growth_rate=2**(1/doubling_time)-1 # Pente de croissance

recovery_days=14 # incubation+etat c'est lamda
gamma=1/recovery_days # gamma 1 sur lambda, le taux de personnes rétablies
beta=(growth_rate+gamma)/N # taux de propagation (taux de personnes infectés après contact)
beta=beta*(1-distanciation) # taux de personnes infectées après contact en respectant les mesures barières
R_0=beta/gamma*N # Taux de reproduction de base
R_1=R_0/(1-distanciation) # Nouveau Taux de reproduction de base après mesures de distantiation sociale
print(R_1)
# Ceci est une fonction qui permet de calculer les prochaines valeurs S, I, R 
SIR_next_step <- function (S,I,R,beta, gamma, N){
  # S, I, R sont les valeurs précédentes.
  # St, It, Rt les valeurs prochaines
  St=S-(beta*S*I)
  It=I+(beta*S*I)-(gamma*I)
  Rt=R+(gamma*I)
  if(St < 0){
    St=0
  }
  if(It < 0){
    It=0
  }
  if(Rt < 0){
    Rt=0
  }
  scale=N/(St+Rt+It) # agrandissement
  St=St*scale # Ajustement
  It=It*scale
  Rt=Rt*scale
  print(St+It+Rt)
  return(c(St,It,Rt))
}
# cette fonction permet de faire une projection sur n_days jours des valeurs de S, I, et R
SIR<- function (S,I,R,beta, gamma, n_days){
  # l'idée est de mettre en première ligne les valeurs initiales
  # on fait une boucle (répétition) pour trouver les prochaines valeurs une à une
    
  #valeur initiale dans le dateframe df
  df<- data.frame(
    Days=c(1),
    Sains=c(S),
    Infectes=c(I),
    Retablis=c(R)
  ) 
  #répéter du premier au dernier jour
  for(day in 1:n_days){
    #pour chaque day on calculer les valeurs qui suivent
    step_value=SIR_next_step(S,I,R,beta,gamma,N)
    # S à la premiere position
    S=as.integer(step_value[1])
    I=as.integer(step_value[2])
    R=as.integer(step_value[3])
    # Ajoute lanouvelle ligne à df
    df[nrow(df)+1,]=c(day+1,S,I,R)
  }
  
  return(df)
}
n_days=160
# execution de la fnction SIR 
Res=SIR(S,I,R,beta, gamma,n_days)

# Parmi les infectés on y tire les personnes qui ont besoin d'une hospi simple
hosp=as.integer(Res$Infectes*hosp_rate* market_share)
# ceux qui ont besoin d'une ventilation
vent=as.integer(Res$Infectes*vent_rate* market_share)
# ceux qui ont besoin de soins intensifs
icu=as.integer(Res$Infectes*icu_rate* market_share)

# On regroupe les valeurs dans un dataframe colonne par colonne
projection = data.frame(days=Res$Days,
                        hosp=hosp,
                        vent=vent,
                        icu=icu)

#u ligne nulle (0,0,0,0)
zero=data.frame(
  days=c(0),
  hosp=c(0),
  vent=c(0),
  icu=c(0)
) 

#On ajoute zero à la premiere ligne en éliminant la derniere
decalage=rbind(zero, projection[1:n_days,])
# les nouveau cas admits chaque jour
incidents=projection-decalage
#les valeurs négatives remplacées par des zeros
incidents[incidents<0 ]=0
incidents["days"]=1:nrow(incidents)

#Création de la fonction d'affichage
 plot_cas_incidents<- function(incidents){
   plot(incidents$days, incidents$hosp, type="l", col="green", xlab="jour", ylab="Nbre Cas")
   lines(incidents$days, incidents$vent, type="l", col="blue")
   lines(incidents$days, incidents$icu, type="l", col="red")
   #legend(1, 200, legend = c("Hosp", "Vent", "Icu"), lwd = c(5,2), col = c("green", "blue", "red") )
 }

 plot_cas_incidents(incidents)
 
 