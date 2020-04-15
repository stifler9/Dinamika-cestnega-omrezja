library(Rlab)
library(shiny)
library(shinyjs)
library(foreach)

source("../podatki_ceste.r")

fluidPage(
  
  useShinyjs(),
  # Application title
  titlePanel("Dinamika cestnega omrezja"),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        h4("Semafor (ad/bd->dc):"),
        uiOutput("semaforadbd")
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
      wellPanel(
        fluidRow(
          column(5,
                 selectInput("cestahitrost", "Cesta:", choices = rownames(ceste))
          )
        ),
        h4("Omejitev hitrosti:"),
        fluidRow(
          column(3,
                 actionButton("manjhit", "<")
          ),
          column(4,
                 uiOutput("hitrost")
          ),
          column(2,
                 actionButton("vechit", ">")
          )
        ),
        shinyjs::hidden(
          div(
            id = "menjavaprehodnih",
            h4("Prehodne verjetnosti:"),
            uiOutput("nakaterocesto"),
            fluidRow(
              column(3,actionButton("manjprehodno", label = "<")),
              column(4,uiOutput("prehodna")),
              column(2,actionButton("boljprehodno", label = ">"))
            )
          )
        )
      ),
      #
      
      wellPanel(
        actionButton("stop","Stop"), actionButton("play","Play"),uiOutput("resetbutton"),
        sliderInput('animacija', "Hitrost animacije:",
                    min=0.25,
                    max=4,
                    step = 0.25,
                    value = 1)
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