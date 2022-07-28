current_hosp=4  # le nombre de cas connus
doubling_time=6 #le temps de dédoublement 6 jours
distanciation=0 # dépendant des mesures de distanciation sociale
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
growth_rate=2**(1/doubling_time)-1 # le taux de propagation

recovery_days=14 # incubation+etat c'est lamda
gamma=1/recovery_days # gamma 1 sur lambda, le taux de personnes rétablies
beta=(growth_rate+gamma)/N*(1-distanciation) # taux de propagation artificiel (taux de personnes infectés après contact)
R_0=beta/gamma*N # Taux de reproduction de base
R_1=R_0/(1-distanciation) # Nouveau Taux de reproduction de base après mesures de distantiation sociale
