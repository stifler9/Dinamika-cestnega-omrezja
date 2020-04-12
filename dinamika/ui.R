library(Rlab)
library(shiny)

fluidPage(
  
  # Application title
  titlePanel("Dinamika cestnega omrezja"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(7,uiOutput("resetbutton"))
      ),
      fluidRow(
        column(4,h5("Semafor (ad/bd->dc):")),
        uiOutput("semaforadbd")
      ),
      fluidRow(
        h5("Prehodna AB: [BC : 1/3 | BD : 2/3]")              
      ),
      # inputi potrebni za izracune
      column(6,
             sliderInput("intenzivnostab",
                         "Intenzivnost prihoda novih AB (/s):",
                         min = 0.0,
                         max = 1.0,
                         step = 0.02,
                         value = 0.3)
      ),
      column(6,
             sliderInput("intenzivnostad",
                         "Intenzivnost prihoda novih AD (/s):",
                         min = 0.0,
                         max = 1.0,
                         step = 0.02,
                         value = 0.3)
      ),
      column(6,
             sliderInput("hitrostab",
                         "Omejitev hitrosti AB (km/h):",
                         min = 30,
                         max = 130,
                         step = 5,
                         value = 70)
      ),
      column(6,
             sliderInput("hitrostad",
                         "Omejitev hitrosti AD (km/h):",
                         min = 30,
                         max = 130,
                         step = 5,
                         value = 70)
      ),
      column(6,
             sliderInput("hitrostbc",
                         "Omejitev hitrosti BC (km/h):",
                         min = 30,
                         max = 130,
                         step = 5,
                         value = 70)
      ),
      column(6,
             sliderInput("hitrostbd",
                         "Omejitev hitrosti BD (km/h):",
                         min = 30,
                         max = 130,
                         step = 5,
                         value = 70)
      ),
      column(6,
             sliderInput("hitrostdc",
                         "Omejitev hitrosti DC (km/h):",
                         min = 30,
                         max = 130,
                         step = 5,
                         value = 70)
      ),
      #
      
      fluidRow(
        column(4,actionButton("stop","Stop"), actionButton("play","Play")),
        column(7,sliderInput('animacija', "Hitrost animacije:",
                             min=0.25,
                             max=4,
                             step = 0.25,
                             value = 1))
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Avti", plotOutput("omrezje", height = '800px')),
        tabPanel("Obremenitve", plotOutput("obremenitev", height = '800px'))
      )
    )
  )
)