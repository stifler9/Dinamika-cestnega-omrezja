library(shiny)

shinyUI(fluidPage(

    titlePanel("Uvoz cest"),
    
    #hocemo tako velike presledke, kot visina tabele
    tags$style("
      .checkbox { /* checkbox is a div class*/
        line-height: 25px;
        margin-bottom: 16px; /*set the margin, so boxes don't overlap*/
      }
      input[type='checkbox']{ /* style for checkboxes */
        width: 16px; /*Desired width*/
        height: 16px; /*Desired height*/
        line-height: 16px; 
      }
      span { 
          margin-left: 10px;  /*set the margin, so boxes don't overlap labels*/
          line-height: 16px; 
      }
    "),

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
                tabPanel("Omrezje", 
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
                         wellPanel(actionButton("dodajsemafor", "Dodaj semafor")),
                         tableOutput("tabelardecih"))
            )
        )
    )
))
