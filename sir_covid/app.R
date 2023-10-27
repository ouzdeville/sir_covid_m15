#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

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

#Création de la fonction d'affichage
plot_cas_incidents<- function(incidents){
  plot(incidents$days, incidents$hosp, type="l", col="green", xlab="jour", ylab="Nbre Cas")
  lines(incidents$days, incidents$vent, type="l", col="blue")
  lines(incidents$days, incidents$icu, type="l", col="red")
  legend(1, 500, legend = c("Hosp", "Vent", "Icu"), lwd = c(5,2), col = c("green", "blue", "red") )
}

#Création de la fonction d'affichage
gplot_cas_incidents<- function(incidents){
  ggplot(incidents)+geom_line(aes(x=days,y=hosp,col="hosp",))+
    geom_line(aes(x=days,y=vent,col="vent"))+
    geom_line(aes(x=days,y=icu,color="icu"))+
    scale_color_manual(name = "Cas", values = c("hosp" = "green","vent"="blue", "icu" = "red"))
}
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

# Definir un user interface 
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 SN"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "current_hosp",
                        label =  "Patients actuellement hospitalisés:",
                        min=1,
                        max=100,
                        value = current_hosp
                        ),
            
            numericInput(inputId = "doubling_time", 
                        label = "Temps de doublement avant la distanciation sociale (jours) :",
                        value = "6"
                        ),
            numericInput(inputId = "distanciation", 
                         label = "Distanciation sociale (% de réduction des contacts sociaux):",
                         
                         min=0, max=100, value=distanciation*100, step=5,
            ),
            numericInput(inputId = "hosp_rate", 
                         label = "Hospitalisation %(total infections):",
                         min=0.0, max=100.0, value=hosp_rate*100, step=1.0
            ),
            
            numericInput(inputId = "icu_rate", 
                         label = "ICU %(total infections):",
                         min=0.0, max=100.0, value=icu_rate*100, step=1.0
            ),
            numericInput(inputId = "vent_rate", 
                         label = "Ventilation %(total infections):",
                         min=0.0, max=100.0, value=vent_rate*100, step=1,
            ),
            
            numericInput(inputId = "hosp_los", 
                         label = "Durée de séjour en Hospitalisation:",
                         , value=hosp_los, step=1,
            ),
            numericInput(inputId = "icu_los", 
                         label = "Durée Séjour en soin Intensif ICU:",
                         value=icu_los, step=1,
            ),
            numericInput(inputId = "vent_los", 
                         label = "Durée Séjour Ventilation:",
                         value=vent_los, step=1,
            ),
            numericInput(inputId = "market_share", 
                         label =  "Part de Marché Hospitalier (%)", 
                         0.0, 100.0, value=market_share*100, step=1.0
            ),
            
            
            numericInput(inputId = "S", 
                         label = "Population Régionale", value=17000000, step=100000
            ),
            
          
        
          
      
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h1("Nouvelles Admissions"),
          h2("Projection du Nombre de cas par jour dans nos hopitaux"),
          sliderInput(inputId = "n_days",
                      label =  "Number of days to project", min=30, max=500, value=160, step=1
          ),
           plotOutput("distPlot"),
          h1("Occupation des ressources par jour"),
          h2(
            "Projection de la prise en charge en tenant compte des entrées et des sorties"
          ),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$distPlot <- renderPlot({
    
    current_hosp=input$current_hosp  # le nombre de cas connus
    doubling_time=input$doubling_time #le temps de dédoublement 6 jours
    distanciation=input$distanciation/100 # dépendant des mesures de distanciation sociale
    hosp_rate=input$hosp_rate/100 # taux d'hospitalisation simple
    icu_rate=input$icu_rate/100 # taux de soins intensifs
    vent_rate=input$vent_rate/100 # taux de personnes en ventilation
    asymp_rate=1-hosp_rate-icu_rate-vent_rate # le taux de personne asyptomatique
    hosp_los=input$hosp_los # durée moyenne de séjour length of stay
    icu_los=input$icu_los
    vent_los=input$vent_los
    
    market_share= input$market_share/100 # Part de marché hospitalier
    
    initial_infection=as.integer(current_hosp/market_share/hosp_rate)
    N=input$S+initial_infection # Population Tatale
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
    
    
    n_days=input$n_days
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
    
    
    
    gplot_cas_incidents(incidents)
       
  })
   
        
}

# Run the application 
shinyApp(ui = ui, server = server)
