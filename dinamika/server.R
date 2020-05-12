library(Rlab)
library(shiny)
library(shinyjs)

server <- function(input, output) {
  
  source("../podatki_ceste.r")
  source("../vozniki.r")
  
  initavti <- function(){
    ret = list()
    for (c in rownames(ceste)) {
      ret[[c]] = c(0.1, 0.2, 0.5, 0.7, 0.85, 0.88, 0.9)*ceste[c,5]
    }
    return(ret)
  }
  inithitrosti <- function(){
    ret = list()
    for (c in rownames(ceste)) {
      ret[[c]] = c(13, 17, 18, 16, 15, 14, 14) #m/s
    }
    return(ret)
  }
  initkam <- function(){
    ret = list()
    for (p in names(povezave)) {
      ret[[p]] = povezave[[p]][1]
    }
    return(ret)
  }
  initomejitve <- function(){
    ret = list()
    for (c in rownames(ceste)) {
      ret[[c]] = 70
    }
    return(ret)
  }
  initprehodne <- function(){
    ret = list()
    for (c in names(povezave)) {
      n = length(povezave[[c]])
      ret[[c]] = round(rep(1/n, n),2)
    }
    return(ret)
  }
  initintenzivnosti <- function(){
    ret = list()
    for (c in rownames(ceste)) {
      if(ceste[c,4]){
        ret[[c]] = 0.3
      }
    }
    return(ret)
  }
  
  #parametri ki se spreminjajo
  odziv <- reactiveValues()
  odziv$resetind <- 0
  odziv$avti = initavti()
  odziv$hitrosti <- inithitrosti()
  # semaforji, "iz kje" + "kam", ce je list prazen je zelena
  odziv$semaforji <- list(addc = c(TRUE))
  # kam gre naslednji avto
  odziv$kam = initkam()
  # za shranjevanje avtov ki so prisli na novo cesto
  odziv$noviavti = list()
  odziv$omejitve = initomejitve()
  odziv$prehodne = initprehodne()
  odziv$intenzivnosti = initintenzivnosti()
  
  output$resetbutton<-renderUI({
    if(odziv$resetind==0){
      lbl<-"All set"
    }else{
      lbl<-"Reset"
    }
    actionButton("reset",label=lbl)
  })
  
  output$hitrost<-renderUI({
    actionButton("hit", label = paste(odziv$omejitve[[input$cestahitrost]],"km/h"))
  })
  
  output$nakaterocesto<-renderUI({
    selectInput("nacesto", "Na katero cesto:",
                choices = povezave[[input$cestahitrost]]
    )
  })
  
  output$intenzivnost<-renderUI({
    actionButton("int", label = odziv$intenzivnosti[[input$cestahitrost]])
  })
  
  output$prehodna<-renderUI({
    i = 1
    req(input$nacesto)
    for (c in povezave[[input$cestahitrost]]) {
      if(c == input$nacesto){
        break
      }
      i = i+1
    }
    lbl = odziv$prehodne[[input$cestahitrost]][i]
    actionButton("preh", label = lbl)
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
        rez = lambda*((odziv$omejitve[[cesta]]/3.6) - v1)
      }else{
        rez = lambda*(v2 - v1)
      }
      return(min(max(bremza, rez), maxposp))
    }
    
    n = length(odziv$avti[[cesta]])
    noviavti = vector(length = n)
    novehitrosti = vector(length = n)
    if(n > 0){
      if(n > 1){
        i = 1
        while(i < n){
          noviavti[i] = odziv$avti[[cesta]][i] + odziv$hitrosti[[cesta]][i]*dt*input$animacija
          novehitrosti[i] = max(odziv$hitrosti[[cesta]][i] + acc(odziv$hitrosti[[cesta]][i], 
                                                                 odziv$hitrosti[[cesta]][i+1], 
                                                                 odziv$avti[[cesta]][i+1] - odziv$avti[[cesta]][i])*dt*input$animacija, 0)
          i = i+1
        }
      }
      noviavti[n] = odziv$avti[[cesta]][n] + odziv$hitrosti[[cesta]][n]*dt*input$animacija
      if(noviavti[n] > ceste[cesta,5]){
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
            odziv$noviavti[[odziv$kam[[cesta]]]] = c(a_nap - ceste[cesta,5],
                                                     max(0, odziv$hitrosti[[cesta]][n+1] + acc(odziv$hitrosti[[cesta]][n+1],
                                                                                               odziv$omejitve[[cesta]]/3.6,
                                                                                               200)*dt*input$animacija))
          }else{
            # ce ne se prilagaja avtu na naslednji cesti
            odziv$noviavti[[odziv$kam[[cesta]]]] = c(a_nap - ceste[cesta,5],
                                                     max(0, odziv$hitrosti[[cesta]][n+1] + acc(odziv$hitrosti[[cesta]][n+1],
                                                                                               odziv$hitrosti[[odziv$kam[[cesta]]]][1],
                                                                                               ceste[cesta,5] - odziv$avti[[cesta]][n+1] + odziv$avti[[odziv$kam[[cesta]]]][1])*dt*input$animacija))
          }
        }
        # zrebamo kam gre naslednji
        p = length(povezave[[cesta]])
        if(p > 0){
          ci = 1
          u = runif(1)
          while(ci <= p){
            if(odziv$prehodne[[cesta]][ci] > u){
              break
            }
            u = u - odziv$prehodne[[cesta]][ci]
            ci = ci + 1
          }
          if(ci <= p){
            odziv$kam[[cesta]] = povezave[[cesta]][ci]
          }else{
            odziv$kam[[cesta]] = NULL
          }
        }
      }else{
        if(length(odziv$semaforji[[paste(cesta, odziv$kam[[cesta]], sep='')]]) > 0){
          # ce je na koncu rdeca luc
          novehitrosti[n] = max(0, odziv$hitrosti[[cesta]][n] + acc(odziv$hitrosti[[cesta]][n], 
                                                                    0.0, 
                                                                    ceste[cesta,5] - odziv$avti[[cesta]][n])*dt*input$animacija)
        }else{
          if(length(odziv$kam[[cesta]]) == 0){
            #ce gre ven iz cest
            novehitrosti[n] = max(0, odziv$hitrosti[[cesta]][n] + acc(odziv$hitrosti[[cesta]][n], 
                                                                      odziv$omejitve[[cesta]]/3.6, 
                                                                      200)*dt*input$animacija)
          }else{
            # na koncu druga cesta
            if(length(odziv$avti[[odziv$kam[[cesta]]]]) == 0){
              # ce je naslednja cesta prazna
              novehitrosti[n] = max(0, odziv$hitrosti[[cesta]][n] + acc(odziv$hitrosti[[cesta]][n], 
                                                                        odziv$omejitve[[cesta]]/3.6, 
                                                                        200)*dt*input$animacija)
            }else{
              # ce ne se prilagaja avtu na naslednji cesti
              novehitrosti[n] = max(0, odziv$hitrosti[[cesta]][n] + acc(odziv$hitrosti[[cesta]][n], 
                                                                        odziv$hitrosti[[odziv$kam[[cesta]]]][1], 
                                                                        ceste[cesta,5] - odziv$avti[[cesta]][n] + odziv$avti[[odziv$kam[[cesta]]]][1])*dt*input$animacija)
            }
          }
        }
      }
    }
    if(zacetna){
      if(rbern(1, dt*input$animacija*odziv$intenzivnosti[[cesta]])){
        # ce se na zacetku pojavi nov avto
        odziv$avti[[cesta]] = c(0.0, noviavti)
        odziv$hitrosti[[cesta]] = c(runif(1, 
                                          (odziv$omejitve[[cesta]] - 10)/3.6,
                                          (odziv$omejitve[[cesta]] + 10)/3.6),
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
    
    # indikator se spremeni na Reset
    odziv$resetind <- 1
    for(c in rownames(ceste)){
      premaknicesto(c, ceste[c,4])
    }
    i = 1
    for(c in rownames(ceste)){
      if(length(odziv$noviavti[[c]]) == 2){
        odziv$avti[[c]] = c(odziv$noviavti[[c]][1], odziv$avti[[c]])
        odziv$hitrosti[[c]] = c(odziv$noviavti[[c]][2], odziv$hitrosti[[c]])
        odziv$noviavti[[c]] = c()
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
    odziv$avti <- initavti()
    odziv$hitrosti <- inithitrosti()
    odziv$semaforji <- list(addc = c(TRUE))
    odziv$kam = initkam()
    odziv$noviavti = list(bc = c(), bd = c())
    odziv$omejitve = initomejitve()
    odziv$prehodne = initprehodne()
    odziv$intenzivnosti = initintenzivnosti()
  })
  
  observeEvent(input$cestahitrost, {
    if(length(povezave[[input$cestahitrost]]) > 0){
      shinyjs::show("menjavaprehodnih")
    }else{
      shinyjs::hide("menjavaprehodnih")
    }
    if(ceste[input$cestahitrost,4]){
      shinyjs::show("menjavaintenzivnosti")
    }else{
      shinyjs::hide("menjavaintenzivnosti")
    }
  })
  
  observeEvent(input$manjhit, {
    req(input$cestahitrost)
    if(odziv$omejitve[[input$cestahitrost]] > 5){
      odziv$omejitve[[input$cestahitrost]] = odziv$omejitve[[input$cestahitrost]] - 5
    }
  })
  
  observeEvent(input$vechit, {
    req(input$cestahitrost)
    if(odziv$omejitve[[input$cestahitrost]] < 150){
      odziv$omejitve[[input$cestahitrost]] = odziv$omejitve[[input$cestahitrost]] + 5
    }
  })
  
  observeEvent(input$manjintenzivno, {
    req(input$cestahitrost)
    if(odziv$intenzivnosti[[input$cestahitrost]] > 0){
      odziv$intenzivnosti[[input$cestahitrost]] = round(odziv$intenzivnosti[[input$cestahitrost]] - 0.02, 2)
    }
  })
  
  observeEvent(input$boljintenzivno, {
    req(input$cestahitrost)
    if(odziv$intenzivnosti[[input$cestahitrost]] < 1){
      odziv$intenzivnosti[[input$cestahitrost]] = round(odziv$intenzivnosti[[input$cestahitrost]] + 0.02, 2)
    }
  })
  
  #menjamo prehodno verjetnost za cestahitrost -> nacesto
  observeEvent(input$boljprehodno, {
    i = 1
    req(input$nacesto)
    req(input$cestahitrost)
    for (c in povezave[[input$cestahitrost]]) {
      if(c == input$nacesto){
        break
      }
      i = i+1
    }
    if(sum(odziv$prehodne[[input$cestahitrost]]) < 1){
      odziv$prehodne[[input$cestahitrost]][i] = round(odziv$prehodne[[input$cestahitrost]][i] + 0.01,2)
    }
  })
  
  observeEvent(input$manjprehodno, {
    i = 1
    req(input$nacesto)
    req(input$cestahitrost)
    for (c in povezave[[input$cestahitrost]]) {
      if(c == input$nacesto){
        break
      }
      i = i+1
    }
    if(odziv$prehodne[[input$cestahitrost]][i] > 0){
      odziv$prehodne[[input$cestahitrost]][i] = round(odziv$prehodne[[input$cestahitrost]][i] - 0.01,2)
    }
  })
  
  # izrisemo trenutno stanje avtov
  #
  output$omrezje <- renderPlot({
    req(session$timer)
    plot(c(0,10), c(1000, 1000), type = 'l', xlim = c(0, 1000), ylim = c(0,1000), xlab = 'x', ylab = 'y')
    i = 1
    for (c in rownames(ceste)) {
      # barva glede na obremenitev, zelena-rumena-rdeca
      obr = 1
      if(length(odziv$hitrosti[[c]]) > 0){
        obr = mean(odziv$hitrosti[[c]])*3.6/odziv$omejitve[[c]]
      }
      obrbarva = ""
      if(obr > 1/2){
        obrbarva = rgb(min(1,max(0,(1-obr)*2)), 1, 0, 1)
      }else{
        obrbarva = rgb(1, min(1,max(0,2*obr)), 0, 1)
      }
      dx = koor[ceste[c,2],1] - koor[ceste[c,1],1]
      dy = koor[ceste[c,2],2] - koor[ceste[c,1],2]
      norma = sqrt(dx*dx + dy*dy)
      x = koor[ceste[c,1],1] + 3*dy/norma
      y = koor[ceste[c,1],2] - 3*dx/norma
      lines(c(x, x+dx), c(y, y+dy), col = obrbarva, lwd=2)
      if(length(odziv$avti[[c]]) > 0){
        x = (odziv$avti[[c]]/ceste[c,5])*dx + x
        y = (odziv$avti[[c]]/ceste[c,5])*dy + y
        points(x, y, col = ceste[c,3], pch = 19)
      }
      i = i+1
    }
    legend(0, 1000, legend = rownames(ceste), col = ceste[,3], lty=1, pch = 19)
  })
  
  # izrisemo cestne obremenitve
  #
  output$obremenitev <- renderPlot({
    req(session$timer)
    plot(c(0,10), c(1000, 1000), type = 'l', xlim = c(0, 1000), ylim = c(0,1000), xlab = 'x', ylab = 'y')
    for (c in rownames(ceste)) {
      #pogledamo odseke po 100 m
      odsekov = as.integer((ceste[c,5]-1)/100) + 1
      l_odsek = ceste[c,5]/odsekov
      avtov = vector(length = odsekov)
      kje = 1
      for(a in odziv$avti[[c]]){
        while (kje*l_odsek < a) {
          kje = kje + 1
        }
        avtov[kje] = avtov[kje] + 1
      }
      dx = (koor[ceste[c,2],1] - koor[ceste[c,1],1])/odsekov
      dy = (koor[ceste[c,2],2] - koor[ceste[c,1],2])/odsekov
      norma = sqrt(dx*dx + dy*dy)
      x = koor[ceste[c,1],1] + 3*dy/norma
      y = koor[ceste[c,1],2] - 3*dx/norma
      for(j in 1:odsekov){
        # izracunamo hitrost s katero bi se dalo peljati po odseku
        # s hitrostjo v_1 se da peljati na varnostni razdalji avto + r*v_1 + v_1^2/(2*bremza)
        # kjer je r reakcijski cas, v tem primeru 1/20s. Obrnemo enacbo da dobimo hitrost hit
        # razdalja je ocenjena glede na stevilo avtomobilov
        # 
        obrbarva = ""
        if(avtov[j] > 1){
          r = dt*input$animacija
          hit = -bremza*(-r + sqrt(max(0,r*r - 2*(((l_odsek/(avtov[j]))-avto)/bremza))))
          obr = hit/odziv$omejitve[[c]]
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
    }
    legend(0, 1000, legend = rownames(ceste), col = ceste[,3], lty=1, pch = 19)
  })
}