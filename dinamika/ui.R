library(Rlab)
library(shiny)

fluidPage(
  
  # Application title
  titlePanel("Dinamika cestnega omrezja"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(4,h5("Semafor (ad/bd->dc):")),
        uiOutput("semaforadbd")
      ),
      fluidRow(h4("Prehodne verjetnosti:")),
      fluidRow(
        column(12,
               sliderInput("prehodnaab",
                           "AB->BC / 1 - AB->BD",
                           min = 0,
                           max = 1,
                           step = 0.01,
                           value = 0.33)
        )
      ),
      fluidRow(h4("Intenzivnosti prihoda novih:")),
      # inputi potrebni za izracune
      fluidRow(
        column(6,
               sliderInput("intenzivnostab",
                           "AB (/s):",
                           min = 0.0,
                           max = 1.0,
                           step = 0.02,
                           value = 0.3)
        ),
        column(6,
               sliderInput("intenzivnostad",
                           "AD (/s):",
                           min = 0.0,
                           max = 1.0,
                           step = 0.02,
                           value = 0.3)
        )
      ),
      fluidRow(h4("Omejitve hitrosti:")),
      fluidRow(
        column(6,
               sliderInput("hitrostab",
                           "AB (km/h):",
                           min = 30,
                           max = 130,
                           step = 5,
                           value = 70)
        ),
        column(6,
               sliderInput("hitrostad",
                           "AD (km/h):",
                           min = 30,
                           max = 130,
                           step = 5,
                           value = 70)
        ),
        column(6,
               sliderInput("hitrostbc",
                           "BC (km/h):",
                           min = 30,
                           max = 130,
                           step = 5,
                           value = 70)
        ),
        column(6,
               sliderInput("hitrostbd",
                           "BD (km/h):",
                           min = 30,
                           max = 130,
                           step = 5,
                           value = 70)
        ),
        column(6,
               sliderInput("hitrostdc",
                           "DC (km/h):",
                           min = 30,
                           max = 130,
                           step = 5,
                           value = 70)
        )
      ),
      #
      
      fluidRow(
        column(4,actionButton("stop","Stop"), actionButton("play","Play"),uiOutput("resetbutton")),
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