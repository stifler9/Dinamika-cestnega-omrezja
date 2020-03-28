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
                column(5,numericInput("skipnum", "Korakov:", 1)),
                column(7,actionButton("skip","Next")),
                column(7,uiOutput("resetbutton"))
            ),
            fluidRow(
                h5("Semafor (ab->bc):"),
                uiOutput("semaforabbc")
            ),
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
    dolzine$"bc" = 700
    
    ceste = NULL
    ceste$"ab" = c("a", "b", 'blue')
    ceste$"bc" = c("b", "c", 'red')
    povezave = NULL
    povezave$"ab" = c("bc")
    prehodne = NULL
    prehodne$"ab" = c(1.0)
    
    koor = NULL
    koor$"a"$x = 0
    koor$"a"$y = 0
    koor$"b"$x = 1000
    koor$"b"$y = 500
    koor$"c"$x = 500
    koor$"c"$y = 1000
    
    avto = 5 #m (dolzina avta)
    varnost = 5 #m (koliko prej se zeli ustaviti)
    bremza = -10
    #m/s^2
    maxposp = 10
    #maksimalni pospesek
    dt = 1/20
    #sprememba casa
    
    zrebaj <- function(cesta){
        u = runif(1)
        i = 1
        while(i < length(prehodne[[cesta]])){
            u = u - prehodne[[cesta]][i]
            if(u <= 0.0){
                break
            }
            i = i+1
        }
        return(povezave[[cesta]][i])
    }
    
    #parametri ki se spreminjajo
    odziv <- reactiveValues()
    odziv$resetind <- 0
    odziv$avti <- list(ab = c(0.1, 0.15, 0.2, 0.5, 0.7, 0.8, 0.85, 0.88, 0.9)*dolzine$"ab",
                       bc = c())
    odziv$hitrosti <- list(ab = c(13, 17, 17, 18, 16, 15, 15, 14, 14),
                           bc = c()) #m/s
    #semaforji, iz kje kam, ce je list prazen je zelena
    odziv$semaforji <- list(abbc = c(TRUE))
    #kam gre naslednji avto
    odziv$kam = list(ab = zrebaj("ab"))
    #za shranjevanje avtov ki so prisli na novo cesto
    odziv$noviavti = list(ab = c(), bc = c())
    
    output$resetbutton<-renderUI({
        if(odziv$resetind==0){
            lbl<-"All set"
        }else{
            lbl<-"Reset"
        }
        actionButton("reset",label=lbl)
    })
    
    output$semaforabbc<-renderUI({
        if(length(odziv$semaforji$abbc)>0){
            lbl<-"Rdeca"
        }else{
            lbl<-"Zelena"
        }
        actionButton("semaforAbBc",label=lbl)
    })
    
    # funkcija premika avtomobilov pri spremembi casa dt
    premaknicesto = function(cesta, zacetna){

        # funkcija pospeska
        acc = function(v1, v2, gap){
            if((v1^2/(2*bremza)) > (v2^2/(2*bremza)) + gap - avto - varnost){
                # Ce velja neenakost, potem se avto zadaj ne bo mogel ustaviti
                # na varnostni razdalji, ce bo avto pred njim zabremzal.
                return(bremza)
            }
            if(gap > 4*v1){
                rez = input$lambda*((input$hitrost/3.6) - v1)
            }else{
                rez = input$lambda*(v2 - v1)
            }
            return(min(max(bremza, rez), maxposp))
        }
        
        n = length(odziv$avti[[cesta]])
        noviavti = vector(length = n)
        novehitrosti = vector(length = n)
        avtonaprej = c()
        if(n > 0){
            i = 1
            while(i < n){
                noviavti[i] = odziv$avti[[cesta]][i] + odziv$hitrosti[[cesta]][i]*dt
                novehitrosti[i] = max(odziv$hitrosti[[cesta]][i] + acc(odziv$hitrosti[[cesta]][i], 
                                                                       odziv$hitrosti[[cesta]][i+1], 
                                                                       odziv$avti[[cesta]][i+1] - odziv$avti[[cesta]][i])*dt, 0)
                i = i+1
            }
            noviavti[n] = odziv$avti[[cesta]][n] + odziv$hitrosti[[cesta]][n]*dt
            if(noviavti[n] > dolzine[[cesta]]){
                # ce vodilni avto pride do konca
                if(length(odziv$kam[[cesta]]) == 0){
                    #ce naprej ni ceste
                    n = n-1
                    noviavti = noviavti[1:n]
                    novehitrosti = novehitrosti[1:n]
                    if(n == 0){
                        noviavti = c()
                        novehitrosti = c()
                    }
                }else{
                    a_nap = noviavti[n]
                    n = n-1
                    noviavti = noviavti[1:n]
                    novehitrosti = novehitrosti[1:n]
                    if(n == 0){
                        noviavti = c()
                        novehitrosti = c()
                    }
                    if(length(odziv$avti[[odziv$kam[[cesta]]]]) == 0){
                        # ce je naslednja cesta prazna
                        odziv$noviavti[[odziv$kam[[cesta]]]] = c(a_nap - dolzine[[cesta]],
                                       max(0, odziv$hitrosti[[cesta]][n+1] + acc(odziv$hitrosti[[cesta]][n+1], 
                                                                               input$hitrost/3.6, 
                                                                               200)*dt))
                    }else{
                        # ce ne se prilagaja avtu na naslednji cesti
                        odziv$noviavti[[odziv$kam[[cesta]]]] = c(a_nap - dolzine[[cesta]],
                                       max(0, odziv$hitrosti[[cesta]][n+1] + acc(odziv$hitrosti[[cesta]][n+1], 
                                                                               odziv$hitrosti[[odziv$kam[[cesta]]]][1], 
                                                                               dolzine[[cesta]] - odziv$avti[[cesta]][n+1] + odziv$avti[[odziv$kam[[cesta]]]][1])*dt))
                    }
                    odziv$kam[[cesta]] = zrebaj(cesta)
                }
            }else{
                if(length(odziv$semaforji[[paste(cesta, odziv$kam[[cesta]], sep='')]]) > 0){
                    # ce je na koncu rdeca luc
                    novehitrosti[n] = max(0, odziv$hitrosti[[cesta]][n] + acc(odziv$hitrosti[[cesta]][n], 
                                                                              0, 
                                                                              dolzine[[cesta]] - odziv$avti[[cesta]][n])*dt)
                }else{
                    if(length(povezave[[cesta]]) == 0){
                        #ce ni naprej ceste
                        novehitrosti[n] = max(0, odziv$hitrosti[[cesta]][n] + acc(odziv$hitrosti[[cesta]][n], 
                                                                                  input$hitrost/3.6, 
                                                                                  200)*dt)
                    }else{
                        # na koncu druga cesta
                        if(length(odziv$avti[[odziv$kam[[cesta]]]]) == 0){
                            # ce je naslednja cesta prazna
                            novehitrosti[n] = max(0, odziv$hitrosti[[cesta]][n] + acc(odziv$hitrosti[[cesta]][n], 
                                                                                   input$hitrost/3.6, 
                                                                                   200)*dt)
                        }else{
                            # ce ne se prilagaja avtu na naslednji cesti
                            novehitrosti[n] = max(0, odziv$hitrosti[[cesta]][n] + acc(odziv$hitrosti[[cesta]][n], 
                                                                                   odziv$hitrosti[[odziv$kam[[cesta]]]][1], 
                                                                                   dolzine[[cesta]] - odziv$avti[[cesta]][n] + odziv$avti[[odziv$kam[[cesta]]]][1])*dt)
                        }
                    }
                }
            }
        }
        if(zacetna){
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
    }
    
    premakni <- function(){
        
        # te spremenljivke potrebujemo za nove izracune
        req(input$intenzivnost)
        req(input$hitrost)
        req(input$lambda)
        
        # indikator se spremeni na Reset
        odziv$resetind <- 1
        
        for(c in names(ceste)){
            if(c %in% zacetki){
                premaknicesto(c, TRUE)
            }else{
                premaknicesto(c, FALSE)
            }
        }
        for(c in names(ceste)){
            if(length(odziv$noviavti[[c]]) == 2){
                odziv$avti[[c]] = c(odziv$noviavti[[c]][1], odziv$avti[[c]])
                odziv$hitrosti[[c]] = c(odziv$noviavti[[c]][2], odziv$hitrosti[[c]])
                odziv$noviavti[[c]] = c()
            }
        }
    }
    
    # sprozilci
    observeEvent(input$skip, {
        req(input$skipnum)
        for(i in 1:input$skipnum){
            premakni()
        }
    })
    
    observeEvent(input$semaforAbBc,{
        if(length(odziv$semaforji$abbc)>0){
            odziv$semaforji$abbc = c()
        }else{
            odziv$semaforji$abbc = c(TRUE)
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
        odziv$avti <- list(ab = c(0.1, 0.15, 0.2, 0.5, 0.7, 0.8, 0.85, 0.88, 0.9)*dolzine$"ab",
                           bc = c())
        odziv$hitrosti <- list(ab = c(13, 17, 17, 18, 16, 15, 15, 14, 14),
                               bc = c()) #m/s
        odziv$semaforji <- list(abbc = c(TRUE))
        odziv$kam = list(ab = zrebaj("ab"))
    })
    
    # izrisemo trenutno stanje avtov
    output$omrezje <- renderPlot({
        plot(c(0,1000, 1000, 0, 0), c(0,0, 1000, 1000, 0), type = 'l', xlim = c(0, 1000), ylim = c(0,1000), xlab = 'x', ylab = 'y')
        for (c in names(ceste)) {
            lines(c(koor[[ceste[[c]][1]]]$x, koor[[ceste[[c]][2]]]$x), c(koor[[ceste[[c]][1]]]$y, koor[[ceste[[c]][2]]]$y), col = ceste[[c]][3])
            if(length(odziv$avti[[c]]) > 0){
                x = (odziv$avti[[c]]/dolzine[[c]])*(koor[[ceste[[c]][2]]]$x - koor[[ceste[[c]][1]]]$x) + koor[[ceste[[c]][1]]]$x
                y = (odziv$avti[[c]]/dolzine[[c]])*(koor[[ceste[[c]][2]]]$y - koor[[ceste[[c]][1]]]$y) + koor[[ceste[[c]][1]]]$y
                points(x, y, col = ceste[[c]][3])
            }
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
