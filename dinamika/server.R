library(Rlab)
library(shiny)

server <- function(input, output) {
  
  # konstantni podatki o cestah, povezanosti, prehodnosti
  #
  dolzine = NULL
  dolzine$"ab" = 1000
  dolzine$"ad" = 900
  dolzine$"bc" = 700
  dolzine$"bd" = 800
  dolzine$"dc" = 700
  
  ceste = NULL
  ceste$"ab" = c("a", "b", 'blue', TRUE)
  ceste$"ad" = c("a", "d", 'gold3', TRUE)
  ceste$"bc" = c("b", "c", 'red', FALSE)
  ceste$"bd" = c("b", "d", 'green4', FALSE)
  ceste$"dc" = c("d", "c", 'purple', FALSE)
  # ceste[[c]][4] == TRUE, ce je cesta zacetna
  povezave = NULL
  povezave$"ab" = c("bc","bd")
  povezave$"bd" = c("dc")
  povezave$"ad" = c("dc")
  prehodne = NULL
  prehodne$"ab" = c(1/3,2/3)
  prehodne$"bd" = c(1)
  prehodne$"ad" = c(1)
  #spremeni odziv$kam
  
  koor = NULL
  koor$"a"$x = 0
  koor$"a"$y = 500
  koor$"b"$x = 600
  koor$"b"$y = 1000
  koor$"c"$x = 1000
  koor$"c"$y = 600
  koor$"d"$x = 600
  koor$"d"$y = 50
  ##
  
  lambda = 3 #prilagajalni faktor
  avto = 7 #m (dolzina avta)
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
  odziv$semaforji <- list(addc = c(TRUE))
  #kam gre naslednji avto
  odziv$kam = list(ab = zrebaj("ab"),
                   ad = zrebaj("ad"),
                   bd = zrebaj("bd"))
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
  
  output$semaforadbd<-renderUI({
    if(length(odziv$semaforji$addc)>0){
      lbl<-"Zelena (bd->dc)"
    }else{
      lbl<-"Zelena (ad->dc)"
    }
    actionButton("semaforAdBd",label=lbl)
  })
  
  # funkcija premika avtomobilov na cesti pri spremembi casa dt
  premaknicesto = function(cesta, zacetna){
    
    # funkcija pospeska
    acc = function(v1, v2, gap){
      if((v1^2/(2*bremza)) > (v2^2/(2*bremza)) + gap - avto){
        # Ce velja neenakost, potem se avto zadaj ne bo mogel ustaviti
        # na varnostni razdalji, ce bo avto pred njim zabremzal.
        return(bremza)
      }
      if(gap - avto > 3*v1){
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
                                                                    0.0, 
                                                                    dolzine[[cesta]] - odziv$avti[[cesta]][n])*dt*input$animacija)
        }else{
          if(length(povezave[[cesta]]) == 0){
            #ce naprej ni ceste
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
      if(rbern(1, dt*input$animacija*input[[paste("intenzivnost", cesta, sep = '')]])){
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
    req(input$intenzivnostad)
    req(input$hitrostab)
    req(input$hitrostad)
    req(input$hitrostbc)
    req(input$hitrostbd)
    req(input$hitrostdc)
    
    # indikator se spremeni na Reset
    odziv$resetind <- 1
    
    for(c in names(ceste)){
      premaknicesto(c, ceste[[c]][4])
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
  observeEvent(input$semaforAdBd,{
    if(length(odziv$semaforji$addc)>0){
      odziv$semaforji$addc = c()
      odziv$semaforji$bddc = c(TRUE)
    }else{
      odziv$semaforji$addc = c(TRUE)
      odziv$semaforji$bddc = c()
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
    odziv$avti <- list(ab = c(0.1, 0.15, 0.2, 0.5, 0.7, 0.8, 0.85, 0.88, 0.9)*dolzine$"ab", bc = c())
    odziv$hitrosti <- list(ab = c(13, 17, 17, 18, 16, 15, 15, 14, 14), bc = c())
    odziv$semaforji <- list(abbc = c(TRUE))
    odziv$kam = list(ab = zrebaj("ab"),
                     ad = zrebaj("ad"),
                     bd = zrebaj("bd"))
    odziv$noviavti = list(bc = c(), bd = c())
  })
  
  # izrisemo trenutno stanje avtov
  #
  output$omrezje <- renderPlot({
    plot(c(0,10), c(1000, 1000), type = 'l', xlim = c(0, 1000), ylim = c(0,1000), xlab = 'x', ylab = 'y')
    barve = c()
    i = 1
    for (c in names(ceste)) {
      obr = odziv$povprecnehit[i]/input[[paste("hitrost", c, sep='')]]
      # barva glede na obremenitev, zelena-rumena-rdeca
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
        points(x, y, col = ceste[[c]][3], pch = 19)
      }
      barve = c(barve, ceste[[c]][3])
      i = i+1
    }
    legend(0, 1000, legend = names(ceste), col = barve, lty=1, pch = 19)
  })
  
  # izrisemo cestne obremenitve
  #
  output$obremenitev <- renderPlot({
    plot(c(0,10), c(1000, 1000), type = 'l', xlim = c(0, 1000), ylim = c(0,1000), xlab = 'x', ylab = 'y')
    barve = c()
    for (c in names(ceste)) {
      #pogledamo odseke po 100 m
      odsekov = (dolzine[[c]]/100)
      avtov = vector(length = odsekov)
      kje = 1
      for(a in odziv$avti[[c]]){
        while (kje*100 < a) {
          kje = kje + 1
        }
        avtov[kje] = avtov[kje] + 1
      }
      x = koor[[ceste[[c]][1]]]$x
      y = koor[[ceste[[c]][1]]]$y
      dx = (koor[[ceste[[c]][2]]]$x - koor[[ceste[[c]][1]]]$x)/odsekov
      dy = (koor[[ceste[[c]][2]]]$y - koor[[ceste[[c]][1]]]$y)/odsekov
      for(j in 1:odsekov){
        #izracunamo hitrost s katero bi se dalo peljati po odseku
        # s hitrostjo v_1 se da peljati na varnostni razdalji avto + r*v_1 + v_1^2/(2*bremza)
        # kjer je r reakcijski cas, va tem primeru 1/20s. Obrnemo enacbo da dabimo hitrost hit
        # razdalja je ocenjena glede na stevilo avtomobilov
        # 
        obrbarva = ""
        if(avtov[j] > 1){
          r = dt*input$animacija
          hit = -bremza*(-r + sqrt(max(0,r*r - 2*(((100/(avtov[j]))-avto)/bremza))))
          obr = hit/input[[paste("hitrost",c, sep = '')]]
          #barva glede na obremenitev
          if(obr > 1/2){
            obrbarva = rgb(min(1,max(0,(1-obr)*2)), 1, 0, 1)
          }else{
            obrbarva = rgb(1, min(1,max(0,2*obr)), 0, 1)
          }
        }
        else{
          # ce je nejvec 1 avto na odseku, ni obremenjen
          obrbarva = rgb(0,1,0,1)
        }
        x_nas = x + dx
        y_nas = y + dy
        lines(c(x, x_nas),c(y,y_nas), col = obrbarva, lwd=2)
        x = x_nas
        y = y_nas
      }
      barve = c(barve, ceste[[c]][3])
    }
    legend(0, 1000, legend = names(ceste), col = barve, lty=1, pch = 19)
  })
}