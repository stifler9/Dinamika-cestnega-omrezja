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
library(doParallel)
registerDoParallel(cores = 4)

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
                        max = 130,
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
    
    zacetki = c("ab")
    dolzine = NULL
    dolzine$"ab" = 1000
    
    ceste = NULL
    ceste$"ab" = c("a", "b")
    povezave = NULL
    
    koor = NULL
    koor$"a"$x = 0
    koor$"a"$y = 0
    koor$"b"$x = 1000
    koor$"b"$y = 1000
    
    avto = 5 #m (dolzina avta)
    varnost = 2 #m (koliko prej se zeli ustaviti)
    bremza = -10
    #m/s^2
    maxposp = 10
    #maksimalni pospesek
    dt = 1/20
    #sprememba casa
    
    #parametri ki se spreminjajo
    odziv <- reactiveValues()
    odziv$resetind <- 0
    odziv$avti <- list(ab = c(0.1, 0.15, 0.2, 0.5, 0.7, 0.8, 0.85, 0.88, 0.9)*dolzine$"ab")
    odziv$hitrosti <- list(ab = c(13, 17, 17, 18, 16, 15, 15, 14, 14)) #m/s
    
    output$resetbutton<-renderUI({
        if(odziv$resetind==0){
            lbl<-"All set"
        }else{
            lbl<-"Reset"
        }
        actionButton("reset",label=lbl)
    })
    
    # funkcija premika avtomobilov pri spremembi casa dt
    premaknicesto = function(cesta, prihod){
        
        # te spremenljivke potrebujemo a nove izracune
        req(input$semafor)
        req(input$intenzivnost)
        req(input$hitrost)
        req(input$lambda)
        
        # indikator se spremeni na Reset
        odziv$resetind <- 1
        
        # funkcija pospeska
        acc = function(v1, v2, gap){
            if((v1^2/(2*bremza)) > (v2^2/(2*bremza)) + gap - avto - varnost){
                # Ce velja neenakost, potem se avto zadaj ne bo mogel ustaviti
                # na varnostni razdalji, ce bo avto pred njim zabremzal.
                return(bremza)
            }
            if(gap > 4*(input$hitrost/3.6)){
                rez = input$lambda*((input$hitrost/3.6) - v1)
            }else{
                rez = input$lambda*(v2 - v1)
            }
            return(min(max(bremza, rez), maxposp))
        }
        
        n = length(odziv$avti[[cesta]])
        noviavti = vector(length = n)
        novehitrosti = vector(length = n)
        i = 1
        
        pospeski <- foreach(j = 1:(n-1), .combine = 'rbind') %do% 
            acc(odziv$hitrosti[[cesta]][j], 
                odziv$hitrosti[[cesta]][j+1], 
                odziv$avti[[cesta]][j+1] - odziv$avti[[cesta]][j])
        while(i < n){
            noviavti[i] = odziv$avti[[cesta]][i] + odziv$hitrosti[[cesta]][i]*dt
            novehitrosti[i] = max(odziv$hitrosti[[cesta]][i] + pospeski[[i]]*dt, 0)
            i = i+1
        }
        noviavti[n] = odziv$avti[[cesta]][n] + odziv$hitrosti[[cesta]][n]*dt
        if(noviavti[n] > dolzine[[cesta]]){
            # ce vodilni avto pride do konca ga vrzemo ven
            n = n-1
            noviavti = noviavti[1:n]
            novehitrosti = novehitrosti[1:n]
        }else{
            if(input$semafor == "Rdeca"){
                # ce je na koncu rdeca luc
                novehitrosti[n] = max(0, odziv$hitrosti[[cesta]][n] + acc(odziv$hitrosti[[cesta]][n], 
                                                                          0, 
                                                                          dolzine[[cesta]] - odziv$avti[[cesta]][n])*dt)
            }else{
                if(length(povezave[[cesta]]) == 0){
                    novehitrosti[n] = max(0, odziv$hitrosti[[cesta]][n] + acc(odziv$hitrosti[[cesta]][n], 
                                                                              input$hitrost/3.6, 
                                                                              dolzine[[cesta]] + 100 - odziv$avti[[cesta]][n])*dt)
                }else{
                    # TODO ce je na koncu drugga cesta
                }
            }
        }
        if(prihod){
            if(rbern(1, dt*input$intenzivnost)){
                # ce se na zacetku pojavi nov avto
                odziv$avti[[cesta]] = c(0.0, noviavti)
                odziv$hitrosti[[cesta]] = c(runif(1, (input$hitrost - 10)/3.6, (input$hitrost + 10)/3.6), novehitrosti)
            }else{
                odziv$avti[[cesta]] = noviavti
                odziv$hitrosti[[cesta]] = novehitrosti
            }
        }else{
            odziv$avti[[cesta]] = noviavti
            odziv$hitrosti[[cesta]] = novehitrosti
        }
        for(c in povezave[[cesta]]){
            premaknicesto(c, FALSE)
        }
    }
    
    premakni <- function(){
        for(z in zacetki){
            premaknicesto(z, TRUE)
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
        plot(c(0,1000, 1000, 0, 0), c(0,0, 1000, 1000, 0), type = 'l', xlim = c(0, 1000), ylim = c(0,1000), xlab = 'x', ylab = 'y')
        for (c in names(ceste)) {
            x = (odziv$avti[[c]]/dolzine[[c]])*(koor[[ceste[[c]][2]]]$x - koor[[ceste[[c]][1]]]$x) + koor[[ceste[[c]][1]]]$x
            y = (odziv$avti[[c]]/dolzine[[c]])*(koor[[ceste[[c]][2]]]$y - koor[[ceste[[c]][1]]]$y) + koor[[ceste[[c]][1]]]$y
            lines(c(koor[[ceste[[c]][1]]]$x, koor[[ceste[[c]][2]]]$x), c(koor[[ceste[[c]][1]]]$y, koor[[ceste[[c]][2]]]$y))
            points(x, y)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
