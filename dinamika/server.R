library(Rlab)
library(shiny)

server <- function(input, output) {
  
  # konstantni podatki o cestah, povezanosti, prehodnosti
  #
  zacetki = c("ab")
  dolzine = NULL
  dolzine$"ab" = 1000
  dolzine$"bc" = 700
  dolzine$"bd" = 800
  
  ceste = NULL
  ceste$"ab" = c("a", "b", 'blue')
  ceste$"bc" = c("b", "c", 'red')
  ceste$"bd" = c("b", "d", 'green4')
  povezave = NULL
  povezave$"ab" = c("bc","bd")
  prehodne = NULL
  prehodne$"ab" = c(1/3,2/3)
  
  koor = NULL
  koor$"a"$x = 0
  koor$"a"$y = 500
  koor$"b"$x = 600
  koor$"b"$y = 200
  koor$"c"$x = 1000
  koor$"c"$y = 600
  koor$"d"$x = 700
  koor$"d"$y = 900
  
  lambda = 3 #prilagajalni faktor
  avto = 5 #m (dolzina avta)
  varnost = 5 #m (koliko prej se zeli ustaviti)
  bremza = -10
  #m/s^2
  maxposp = 10
  #maksimalni pospesek
  dt = 1/20
  #sprememba casa
  
  # funkcija ki zreba na katero cesto zeli avto, ki pride do konca,
  # glede na prehodno verjetnost
  zrebaj <- function(cesta){
    i = 1
    n = length(prehodne[[cesta]])
    if(n > 1){
      u = runif(1)
      while(i < n){
        u = u - prehodne[[cesta]][i]
        if(u <= 0.0){
          break
        }
        i = i+1
      }
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
  #semaforji, "iz kje" + "kam", ce je list prazen je zelena
  odziv$semaforji <- list(abbc = c(TRUE))
  #kam gre naslednji avto
  odziv$kam = list(ab = zrebaj("ab"))
  #za shranjevanje avtov ki so prisli na novo cesto
  odziv$noviavti = list(bc = c(), bd = c())
  #izpisujemo povprecne hitrosti na cestah
  odziv$povprecnehit = rep(0,length(names(ceste)))
  
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
  
  # funkcija premika avtomobilov na cesti pri spremembi casa dt
  premaknicesto = function(cesta, zacetna){
    
    # funkcija pospeska
    acc = function(v1, v2, gap){
      if((v1^2/(2*bremza)) > (v2^2/(2*bremza)) + gap - avto - varnost){
        # Ce velja neenakost, potem se avto zadaj ne bo mogel ustaviti
        # na varnostni razdalji, ce bo avto pred njim zabremzal.
        return(bremza)
      }
      if(gap > 4*v1){
        rez = lambda*((input[[paste("hitrost",cesta,sep='')]]/3.6) - v1)
      }else{
        rez = lambda*(v2 - v1)
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
        noviavti[i] = odziv$avti[[cesta]][i] + odziv$hitrosti[[cesta]][i]*dt*input$animacija
        novehitrosti[i] = max(odziv$hitrosti[[cesta]][i] + acc(odziv$hitrosti[[cesta]][i], 
                                                               odziv$hitrosti[[cesta]][i+1], 
                                                               odziv$avti[[cesta]][i+1] - odziv$avti[[cesta]][i])*dt*input$animacija, 0)
        i = i+1
      }
      noviavti[n] = odziv$avti[[cesta]][n] + odziv$hitrosti[[cesta]][n]*dt*input$animacija
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
          if(n == 0){
            noviavti = c()
            novehitrosti = c()
          }else{
            noviavti = noviavti[1:n]
            novehitrosti = novehitrosti[1:n]
          }
          if(length(odziv$avti[[odziv$kam[[cesta]]]]) == 0){
            # ce je naslednja cesta prazna
            odziv$noviavti[[odziv$kam[[cesta]]]] = c(a_nap - dolzine[[cesta]],
                                                     max(0, odziv$hitrosti[[cesta]][n+1] + acc(odziv$hitrosti[[cesta]][n+1],
                                                                                               input[[paste("hitrost",cesta,sep='')]]/3.6,
                                                                                               200)*dt*input$animacija))
          }else{
            # ce ne se prilagaja avtu na naslednji cesti
            odziv$noviavti[[odziv$kam[[cesta]]]] = c(a_nap - dolzine[[cesta]],
                                                     max(0, odziv$hitrosti[[cesta]][n+1] + acc(odziv$hitrosti[[cesta]][n+1],
                                                                                               odziv$hitrosti[[odziv$kam[[cesta]]]][1],
                                                                                               dolzine[[cesta]] - odziv$avti[[cesta]][n+1] + odziv$avti[[odziv$kam[[cesta]]]][1])*dt*input$animacija))
          }
          odziv$kam[[cesta]] = zrebaj(cesta)
        }
      }else{
        if(length(odziv$semaforji[[paste(cesta, odziv$kam[[cesta]], sep='')]]) > 0){
          # ce je na koncu rdeca luc
          novehitrosti[n] = max(0, odziv$hitrosti[[cesta]][n] + acc(odziv$hitrosti[[cesta]][n], 
                                                                    0, 
                                                                    dolzine[[cesta]] - odziv$avti[[cesta]][n])*dt*input$animacija)
        }else{
          if(length(povezave[[cesta]]) == 0){
            #ce ni naprej ceste
            novehitrosti[n] = max(0, odziv$hitrosti[[cesta]][n] + acc(odziv$hitrosti[[cesta]][n], 
                                                                      input[[paste("hitrost",cesta,sep='')]]/3.6, 
                                                                      200)*dt*input$animacija)
          }else{
            # na koncu druga cesta
            if(length(odziv$avti[[odziv$kam[[cesta]]]]) == 0){
              # ce je naslednja cesta prazna
              novehitrosti[n] = max(0, odziv$hitrosti[[cesta]][n] + acc(odziv$hitrosti[[cesta]][n], 
                                                                        input[[paste("hitrost",cesta,sep='')]]/3.6, 
                                                                        200)*dt*input$animacija)
            }else{
              # ce ne se prilagaja avtu na naslednji cesti
              novehitrosti[n] = max(0, odziv$hitrosti[[cesta]][n] + acc(odziv$hitrosti[[cesta]][n], 
                                                                        odziv$hitrosti[[odziv$kam[[cesta]]]][1], 
                                                                        dolzine[[cesta]] - odziv$avti[[cesta]][n] + odziv$avti[[odziv$kam[[cesta]]]][1])*dt*input$animacija)
            }
          }
        }
      }
    }
    if(zacetna){
      if(rbern(1, dt*input[[paste("intenzivnost", cesta, sep = '')]])){
        # ce se na zacetku pojavi nov avto
        odziv$avti[[cesta]] = c(0.0, noviavti)
        odziv$hitrosti[[cesta]] = c(runif(1, 
                                          (input[[paste("hitrost",cesta,sep='')]] - 10)/3.6,
                                          (input[[paste("hitrost",cesta,sep='')]] + 10)/3.6),
                                    novehitrosti)
      }else{
        odziv$avti[[cesta]] = noviavti
        odziv$hitrosti[[cesta]] = novehitrosti
      }
    }else{
      odziv$avti[[cesta]] = noviavti
      odziv$hitrosti[[cesta]] = novehitrosti
    }
  }
  
  # funkcija premika avtomobilov na vseh cestah
  premakni <- function(){
    
    # te spremenljivke potrebujemo za nove izracune
    req(input$animacija)
    req(input$intenzivnostab)
    req(input$hitrostab)
    req(input$hitrostbc)
    req(input$hitrostbd)
    
    # indikator se spremeni na Reset
    odziv$resetind <- 1
    
    for(c in names(ceste)){
      if(c %in% zacetki){
        premaknicesto(c, TRUE)
      }else{
        premaknicesto(c, FALSE)
      }
    }
    i = 1
    for(c in names(ceste)){
      if(length(odziv$noviavti[[c]]) == 2){
        odziv$avti[[c]] = c(odziv$noviavti[[c]][1], odziv$avti[[c]])
        odziv$hitrosti[[c]] = c(odziv$noviavti[[c]][2], odziv$hitrosti[[c]])
        odziv$noviavti[[c]] = c()
      }
      if(length(odziv$hitrosti[[c]])>0){
        odziv$povprecnehit[i] = mean(odziv$hitrosti[[c]])*3.6
      }else{
        odziv$povprecnehit[i] = input[[paste("hitrost", c, sep='')]]
      }
      i = i+1
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
    odziv$noviavti = list(bc = c(), bd = c())
  })
  
  # izrisemo trenutno stanje avtov
  output$omrezje <- renderPlot({
    plot(c(0,1000, 1000, 0, 0), c(0,0, 1000, 1000, 0), type = 'l', xlim = c(0, 1000), ylim = c(0,1000), xlab = 'x', ylab = 'y')
    barve = c()
    i = 1
    for (c in names(ceste)) {
      obr = odziv$povprecnehit[i]/input[[paste("hitrost", c, sep='')]]
      obrbarva = ""
      if(obr > 1/2){
        obrbarva = rgb(min(1,max(0,(1-obr)*2)), 1, 0, 1)
      }else{
        obrbarva = rgb(1, min(1,max(0,2*obr)), 0, 1)
      }
      lines(c(koor[[ceste[[c]][1]]]$x, koor[[ceste[[c]][2]]]$x), c(koor[[ceste[[c]][1]]]$y, koor[[ceste[[c]][2]]]$y), col = obrbarva, lwd=2)
      if(length(odziv$avti[[c]]) > 0){
        x = (odziv$avti[[c]]/dolzine[[c]])*(koor[[ceste[[c]][2]]]$x - koor[[ceste[[c]][1]]]$x) + koor[[ceste[[c]][1]]]$x
        y = (odziv$avti[[c]]/dolzine[[c]])*(koor[[ceste[[c]][2]]]$y - koor[[ceste[[c]][1]]]$y) + koor[[ceste[[c]][1]]]$y
        points(x, y, col = ceste[[c]][3])
      }
      barve = c(barve, ceste[[c]][3])
      i = i+1
    }
    legend(0, 1000, legend = names(ceste), col = barve, lty=1, cex=1)
  })
  
  output$povphit <- renderTable(matrix(odziv$povprecnehit, ncol = 1, dimnames = list(names(ceste),c("v (km/h)"))), rownames = TRUE)
}