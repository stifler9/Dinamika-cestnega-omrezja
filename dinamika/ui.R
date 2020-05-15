library(Rlab)
library(shiny)
library(shinyjs)

#source("../podatki_ceste.r")
source("../podatki/ceste_app.R")

fluidPage(
  
  useShinyjs(),
  # Application title
  titlePanel("Dinamika cestnega omrezja"),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        fluidRow(
          column(6, selectInput("semafor", "Semafor:", choices = names(semaforji))),
          column(6, actionButton("spr_semafor", "Spremeni"))
        ),
        uiOutput("stanjesemaforjev")
      ),
      wellPanel(
        fluidRow(
          column(8, offset = 2,
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
          column(2, offset = 1,
                 actionButton("vechit", ">")
          )
        ),
        shinyjs::hidden(
          div(
            id = "menjavaintenzivnosti",
            h4("Intenzivnost prihoda (/s):"),
            fluidRow(
              column(3,actionButton("manjintenzivno", label = "<")),
              column(4,uiOutput("intenzivnost")),
              column(2,actionButton("boljintenzivno", label = ">"))
            )
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
        actionButton("stop","Stop"), actionButton("play","Play"), uiOutput("resetbutton"),
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