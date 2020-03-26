#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(Rlab)
library(shiny)

ui <- fluidPage(
    
    # Application title
    titlePanel("Dinamika cestnega omrezja"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(7,uiOutput("resetbutton"))
            ),
            fluidRow(
                column(5,numericInput("skipnum", "Korakov:", 1)),
                column(7,actionButton("skip","Next"))
            ),
            selectInput("semafor","Semafor:",choices=c("Rdeca","Zelena")),
            sliderInput("intenzivnost",
                        "Intenzivnost prihoda novih (/s):",
                        min = 0.0,
                        max = 2.0,
                        step = 0.05,
                        value = 0.3),
            sliderInput("hitrost",
                        "Omejitev hitrosti (km/h):",
                        min = 30,
                        max = 120,
                        step = 5,
                        value = 70),
            sliderInput("lambda",
                        "Prilagajalni faktor (lambda):",
                        min = 0.5,
                        max = 15,
                        step = 0.5,
                        value = 3.5),
            fluidRow(
                column(7,actionButton("stop","Stop")),
                column(3,actionButton("play","Play"))
            )
        ),
        
        mainPanel(
            plotOutput("omrezje")
        )
    )
)

server <- function(input, output) {
    
    dolzina = 1000
    avto = 5 #m (dolzina avta)
    varnost = 2 #m (koliko prej se zeli ustaviti)
    bremza = -10
    #m/s^2
    maxposp = 10
    #maksimalni pospesek
    dt = 1/20
    
    #parametri ki se spreminjajo
    odziv <- reactiveValues()
    odziv$resetind <- 0
    odziv$avti <- c(0.1, 0.15, 0.2, 0.5, 0.7, 0.8, 0.85, 0.88, 0.9)*dolzina
    odziv$hitrosti <- c(13, 17, 17, 18, 16, 15, 15, 14, 14) #m/s
    
    output$resetbutton<-renderUI({
        if(odziv$resetind==0){
            lbl<-"All set"
        }else{
            lbl<-"Reset"
        }
        actionButton("reset",label=lbl)
    })
    
    # funkcija pospeska
    acc = function(v1, v2, gap){
        if((v1^2/(2*bremza)) > (v2^2/(2*bremza)) + gap - avto - varnost){
            return(bremza)
        }
        if(gap > 50){
            rez = input$lambda*((input$hitrost/3.6) - v1)
        }else{
            rez = input$lambda*(v2 - v1)
        }
        return(min(max(bremza, rez), maxposp))
    }
    
    # funkcija premika avtomobilov pri spremembi casa dt
    premakni = function(){
        
        # te spremenljivke potrebujemo a nove izracune
        req(input$semafor)
        req(input$intenzivnost)
        req(input$hitrost)
        req(input$lambda)
        
        # indikator se spremeni na Reset
        odziv$resetind <- 1
        
        n = length(odziv$avti)
        noviavti = vector(length = n)
        novehitrosti = vector(length = n)
        i = 1
        while(i < n){
            noviavti[i] = odziv$avti[i] + odziv$hitrosti[i]*dt
            novehitrosti[i] = max(odziv$hitrosti[i] + acc(odziv$hitrosti[i], odziv$hitrosti[i+1], odziv$avti[i+1] - odziv$avti[i])*dt, 0)
            i = i+1
        }
        noviavti[n] = odziv$avti[n] + odziv$hitrosti[n]*dt
        if(noviavti[n] > dolzina){
            # ce vodilni avto pride do konca ga vrzemo ven
            n = n-1
            noviavti = noviavti[1:n]
            novehitrosti = novehitrosti[1:n]
        }else{
            if(input$semafor == "Rdeca"){
                # ce je na koncu rdeca luc
                novehitrosti[n] = max(0, odziv$hitrosti[n] + acc(odziv$hitrosti[n], 0, dolzina - odziv$avti[n])*dt)
            }else{
                novehitrosti[n] = max(0, odziv$hitrosti[n] + acc(odziv$hitrosti[n], input$hitrost/3.6, dolzina + 100 - odziv$avti[n])*dt)
            }
        }
        if(rbern(1, dt*input$intenzivnost)){
            # ce se na zacetku pojavi nov avto
            odziv$avti = c(0.0, noviavti)
            odziv$hitrosti = c(runif(1, (input$hitrost - 10)/3.6, (input$hitrost + 10)/3.6), novehitrosti)
        }else{
            odziv$avti = noviavti
            odziv$hitrosti = novehitrosti
        }
    }
    
    # sprozilci
    observeEvent(input$skip, {
        req(input$skipnum)
        for(i in 1:input$skipnum){
            premakni()
        }
    })
    
    #timer
    session<-reactiveValues()
    session$timer<-reactiveTimer(Inf)
    
    observeEvent(input$play,{
        session$timer<-reactiveTimer(50)
        observeEvent(session$timer(),{
            premakni()
        })
    })
    
    observeEvent(input$stop,{
        session$timer<-reactiveTimer(Inf)
    })
    
    observeEvent(input$reset,{
        odziv$resetind <- 0
        odziv$avti <- c(0.1, 0.15, 0.2, 0.5, 0.7, 0.8, 0.85, 0.88, 0.9)*dolzina
        odziv$hitrosti <- c(13, 17, 17, 18, 16, 15, 15, 14, 14)
    })
    
    # izrisemo trenutno stanje avtov
    output$omrezje <- renderPlot({
        plot(odziv$avti, rep(0, length(odziv$avti)), type = 'p', xlim = c(0, dolzina), ylim = c(0,1), xlab = 'Avti', ylab = '')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
