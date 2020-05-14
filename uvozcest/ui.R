library(shiny)

shinyUI(fluidPage(

    titlePanel("Uvoz cest"),

    sidebarLayout(
        sidebarPanel(
            wellPanel(
                textInput("iskanje", "Najdi vozlisce:", value = ""),
                uiOutput("izbira_vozlisca"),
                uiOutput("koordinate"),
                actionButton("dodajvozlisce", "Dodaj vozlisce")
            ),
            wellPanel(
                actionButton("shraniomrezje", "Shrani cestno omrezje")
            )
        ),

        mainPanel(
            tabsetPanel(
                ## Vse ceste
                tabPanel("Ceste", tableOutput("ceste")),
                tabPanel("Dodana vozlisca", 
                         plotOutput("omrezje", click = "omrezje_klik", height = '700px'), 
                         textOutput("vkljucenavozlisca")),
                tabPanel("Pripadajoce ceste",
                         fluidRow(
                             column(9,tableOutput("cestezdvema")),
                             column(3,uiOutput("zacetneui"))
                             )
                         ),
                tabPanel("Ceste z enim vozliscem",
                         tableOutput("cestezenim")),
                tabPanel("Semaforji", uiOutput("semaforji"),
                         uiOutput("semaforjirdece"),
                         fluidRow(
                            column(6, numericInput("opcijasemaforja", "Opcija semaforja:", value = 1, min = 1, max = 10, step = 1)),
                            column(6, actionButton("dodajopcijosemaforja", "Dodaj opcijo"))
                         ),
                         fluidRow(actionButton("dodajsemafor", "Dodaj semafor")),
                         tableOutput("tabelardecih"))
            )
        )
    )
))
