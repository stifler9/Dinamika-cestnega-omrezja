library(shiny)

shinyServer(function(input, output) {
    
    #vsi podatki o slovenskih cestah
    podatki_ceste <- read.csv2("../podatki/urejeni_podatki.csv")
    
    odziv = reactiveValues()
    #ceste, ki delno ali pripadajo omrezju
    odziv$cestedva = data.frame()
    odziv$cesteena = data.frame()
    #vozlisca, ki jih lahko dodajamo
    odziv$vsavozlisca = unique(c(levels(podatki_ceste[,"Od"]), levels(podatki_ceste[,"Do"])))
    odziv$iskanavozlisca = unique(c(levels(podatki_ceste[,"Od"]), levels(podatki_ceste[,"Do"])))
    odziv$dodanavozlisca = c()
    odziv$dodanx = c()
    odziv$dodany = c()
    odziv$vozlisceblizu = ""
    #tabela za trenutni semafor, ki ga zelimo uvesti
    odziv$semafortabela = data.frame()
    # imena za semafor (od katere ceste do katere)
    odziv$semaforopcije = c()
    # shranimo semaforje, ki jih bomo potrebovali za omrezje
    odziv$semaforji = list()
    
    najdiVozlisca <- function(){
      if(input$iskanje == ""){
        odziv$iskanavozlisca = odziv$vsavozlisca
      }else{
        odziv$iskanavozlisca = odziv$vsavozlisca[grep(input$iskanje, odziv$vsavozlisca, ignore.case = TRUE)]
      }
    }
    
    #sproti prikazujemo, katere ceste ze pripadajo omrezj in katere delno
    updateCeste <- function(){
      ind1 = c()
      ind2 = c()
      for(i in 1:dim(podatki_ceste)[1]) {
        if(podatki_ceste[i,"Od"] %in% odziv$dodanavozlisca){
          if(podatki_ceste[i,"Do"] %in% odziv$dodanavozlisca){
            ind2 = c(ind2, i)
          }else{
            ind1 = c(ind1, i)
          }
        }else{
          if(podatki_ceste[i,"Do"] %in% odziv$dodanavozlisca){
            ind1 = c(ind1, i)
          }
        }
      }
      odziv$cesteena = podatki_ceste[ind1,]
      odziv$cestedva = podatki_ceste[ind2,]
    }
    
    observeEvent(input$iskanje, {
      najdiVozlisca()
    })
    
    observeEvent(input$dodajvozlisce, {
      if(is.null(input$omrezje_klik)){
        print("Ni izbranih koordinat!")
      }else{
        odziv$dodanavozlisca = c(odziv$dodanavozlisca, input$izbranovozlisce)
        odziv$dodanx = c(odziv$dodanx, round(input$omrezje_klik$x,2))
        odziv$dodany = c(odziv$dodany, round(input$omrezje_klik$y,2))
        odziv$vsavozlisca = odziv$vsavozlisca[odziv$vsavozlisca != input$izbranovozlisce]
        najdiVozlisca()
        updateCeste()
      }
    })
    
    # Za shranjevanje cestnega omrezja
    observeEvent(input$shraniomrezje, {
      n = dim(odziv$cestedva)[1]
      print("shranjujem")
      if(n > 0){
        #konstruiramo podatke o cestah
        ceste = data.frame()
        ceste[,1] = character()
        ceste[,2] = character()
        ceste[,3] = character()
        ceste[,4] = logical()
        ceste[,5] = numeric()
        ceste[,6] = numeric()
        ceste[,7] = numeric()
        sekund_d = 60*60*24
        vozila = list()
        for(i in 1:n){
          barva = ""
          omejitev = 50
          if(odziv$cestedva[i,"Kategorija"] == "AC"){
            barva = "green"
            omejitev = 130
          }else if(odziv$cestedva[i,"Kategorija"] == "HC"){
            barva = "blue"
            omejitev = 110
          }else if(odziv$cestedva[i,"Kategorija"] %in% c("G1", "R1")){
            barva = "red"
          }else{
            barva = "orange"
            omejitev = 90
          }
          zacetna = FALSE
          if(i %in% input$zacetneceste){zacetna = TRUE}
          ceste[paste(odziv$cestedva[i,"Od"], odziv$cestedva[i,"Do"], odziv$cestedva[i,"Kategorija"], sep="_"),c(1,2,3)] = c(as.character(odziv$cestedva[i,"Od"]),
                                                                                             as.character(odziv$cestedva[i,"Do"]),
                                                                                             barva)
          ceste[paste(odziv$cestedva[i,"Od"], odziv$cestedva[i,"Do"], odziv$cestedva[i,"Kategorija"], sep="_"),4] = zacetna
          ceste[paste(odziv$cestedva[i,"Od"], odziv$cestedva[i,"Do"], odziv$cestedva[i,"Kategorija"], sep="_"),c(5,6,7)] = c(odziv$cestedva[i,"Dolzina"],
                                                                                                                             omejitev,
                                                                                                                             round(min(1,2*(odziv$cestedva[i, "Vsa.vozila"]/sekund_d)), 4))
          vozila[[paste(odziv$cestedva[i,"Od"], odziv$cestedva[i,"Do"], odziv$cestedva[i,"Kategorija"], sep="_")]] = odziv$cestedva[i, "Vsa.vozila"]
        }
        ceste[,1] = as.character(ceste[,1])
        ceste[,2] = as.character(ceste[,2])
        ceste[,3] = as.character(ceste[,3])
        ceste[,4] = as.logical(ceste[,4])
        ceste[,5] = as.numeric(ceste[,5])
        ceste[,6] = as.numeric(ceste[,6])
        ceste[,7] = as.numeric(ceste[,7])
        
        #konstruiramo podatke o koordinatah, povezavah, verjetnostih
        koor = data.frame()
        koor[,1] = numeric()
        koor[,2] = numeric()
        povezave = list()
        verjetnosti = list()
        for(i in 1:length(odziv$dodanavozlisca)){
          koor[odziv$dodanavozlisca[i],] = c(odziv$dodanx[i], odziv$dodany[i])
          vhodne = c()
          izhodne = c()
          vhod_vozila = 0
          izhod_vozila = 0
          for(c in rownames(ceste)){
            if(ceste[c,1] == odziv$dodanavozlisca[i]){
              if(!ceste[c,4]){
                izhodne = c(izhodne, c)
                izhod_vozila = izhod_vozila + vozila[[c]]
              }
            }
            if(ceste[c,2] == odziv$dodanavozlisca[i]){
              vhodne = c(vhodne, c)
              vhod_vozila = vhod_vozila + vozila[[c]]
            }
          }
          # verjetnosti take, da se izide stevilo vseh vozil na cestah
          # ce je manj ali enako vozil, ki pridejo v krizisce, kot teh, ki odidejo, se prehodne verjetnosti sestejejo v 1,
          # sicer v nekaj manj
          vhod_vozila = max(vhod_vozila, izhod_vozila)
          if(length(izhodne) > 0){
            for(v in vhodne){
              povezave[[v]] <- izhodne
              verj = c()
              for(iz in izhodne){
                verj = c(verj, vozila[[iz]])
              }
              verjetnosti[[v]] = round(verj/vhod_vozila, 3)
            }
          }
        }
        semaforji = list()
        for(sem in names(odziv$semaforji)){
          semaforji[[sem]] = odziv$semaforji[[sem]]
        }
        
        #shranimo
        dump(c("ceste", "koor", "povezave", "verjetnosti", "semaforji"), file = "../podatki/ceste_app.R")
        print("shranjeno")
      }else{
        print("Ni cest!")
      }
    })
    
    #ko spremenimo v katerem vozliscu zelimo semafor, se posodobi katere rdece luci so lahko na voljo
    observeEvent(input$vozliscesemafor, {
      if(!is.null(input$vozliscesemafor)){
        vhodne = c()
        izhodne = c()
        n = dim(odziv$cestedva)[1]
        if(n > 0){
          for(i in 1:n){
            if(odziv$cestedva[i,"Od"] == input$vozliscesemafor){
              if(!(i %in% input$zacetneceste)){
                izhodne = c(izhodne, i)
              }
            }
            if(odziv$cestedva[i,"Do"] == input$vozliscesemafor){
              vhodne = c(vhodne, i)
            }
          }
          if(length(izhodne)*length(vhodne) > 0){
            odziv$semafortabela = data.frame(matrix(FALSE, nrow = 1, ncol = length(vhodne)*length(izhodne)))
            imena = c()
            for(v in vhodne){
              for(iz in izhodne){
                imena = c(imena, paste(paste(odziv$cestedva[v, "Od"], odziv$cestedva[v, "Do"], odziv$cestedva[v, "Kategorija"], sep = "_"),
                                       paste(odziv$cestedva[iz, "Od"], odziv$cestedva[iz, "Do"], odziv$cestedva[iz, "Kategorija"], sep = "_"),
                                       sep = "__"))
              }
            }
            colnames(odziv$semafortabela) = imena
            odziv$semaforopcije = imena
          }else{
            odziv$semafortabela = data.frame()
            odziv$semaforopcije = NULL
          }
        }
      }
    })
    
    # dodamo eljeno opcijo semaforja v tabelo
    observeEvent(input$dodajopcijosemaforja, {
      dodatek = c()
      for(opt in odziv$semaforopcije){
        if(opt %in% input$rdeceluci){
          dodatek = c(dodatek, TRUE)
        }else{
          dodatek = c(dodatek, FALSE)
        }
      }
      odziv$semafortabela[input$opcijasemaforja,] = dodatek
    })
    
    #shranimo tabelo za zeljen semafor
    observeEvent(input$dodajsemafor, {
      odziv$semaforji[[input$vozliscesemafor]] = odziv$semafortabela
      odziv$semafortabela = data.frame()
      odziv$semaforopcije = NULL
    })
    
    observeEvent(input$omrezje_klik, {
      if(is.null(input$omrezje_klik)){
        odziv$vozlisceblizu = ""
      }else{
        odziv$vozlisceblizu = ""
        dist = Inf
        n = length(odziv$dodanavozlisca)
        if(n > 0){
          for(i in 1:n){
            raz = (input$omrezje_klik$x - odziv$dodanx[i])^2 + (input$omrezje_klik$y - odziv$dodany[i])^2
            if(raz < dist){
              odziv$vozlisceblizu = odziv$dodanavozlisca[i]
              dist = raz
            }
          }
        }
      }
    })
    
    #izberemo vozlisce za dodajanje v omrezje
    output$izbira_vozlisca <- renderUI({
      selectInput("izbranovozlisce", "Izberi vozlisce:", choices = odziv$iskanavozlisca)
    })
    
    output$koordinate <- renderUI({
      if(is.null(input$omrezje_klik)){
        h5("Izberi koordinate!")
      }else{
        h5(paste0("x: ", round(input$omrezje_klik$x,2), "\ny: ", round(input$omrezje_klik$y,2)))
      }
    })
    
    output$bliznjevozlisce <- renderUI({
      h5(paste("Najblizje:",odziv$vozlisceblizu))
    })
    
    # podatki o vseh cestah
    output$ceste <- renderTable(podatki_ceste)
    
    izpisvozlisc <- function(){
      n = length(odziv$dodanavozlisca)
      if(n > 0){
        txt = 'Izbrana vozlisca:  '
        for(i in 1:n){
          txt = paste0(txt, '\n', odziv$dodanavozlisca[i],
                      " (", odziv$dodanx[i], ", ", odziv$dodany[i], '),  ', sep = "")
        }
        return(txt)
      }else{
        return("Ni izbranih vozlisc")
      }
    }
    
    output$vkljucenavozlisca <- renderText({
      izpisvozlisc()
    })
    
    # za oznacevanje katere ceste naj bodo zacetne
    output$zacetneui <- renderUI({
      checkboxGroupInput("zacetneceste", "Zacetne ceste:",
                         choices = c(1:dim(odziv$cestedva)[1]))
    })
    
    #semafor
    output$semaforji <- renderUI({
      selectInput("vozliscesemafor", "Vozlisce:", choices = odziv$dodanavozlisca)
    })
    output$semaforjirdece <- renderUI({
      checkboxGroupInput("rdeceluci", "Rdece luci:", choices = odziv$semaforopcije)
    })
    output$tabelardecih <- renderTable(odziv$semafortabela)
    
    #iris cestnega omrezja
    output$omrezje <- renderPlot({
      plot(c(), c(), type = 'l', xlim = c(0, 1000), ylim = c(0,1000), xlab = 'x', ylab = 'y')
      n = length(odziv$dodanavozlisca)
      if(n > 0){
        points(odziv$dodanx, odziv$dodany, cex = 1.5, pch = 16)
      }
      l = dim(odziv$cestedva)[1]
      if(l > 0){
        for(c in 1:l){
          barva = ""
          deb = 2
          if(odziv$cestedva[c,"Kategorija"] == "AC"){
            barva = "green"
            deb = 3
          }else if(odziv$cestedva[c,"Kategorija"] == "HC"){
            barva = "blue"
            deb = 3
          }else if(odziv$cestedva[c,"Kategorija"] %in% c("G1", "R1")){
            barva = "red"
          }else{
            barva = "orange"
          }
          dx = odziv$dodanx[which(odziv$dodanavozlisca == odziv$cestedva[c,"Do"])] -
                odziv$dodanx[which(odziv$dodanavozlisca == odziv$cestedva[c,"Od"])]
          dy = odziv$dodany[which(odziv$dodanavozlisca == odziv$cestedva[c,"Do"])] -
                odziv$dodany[which(odziv$dodanavozlisca == odziv$cestedva[c,"Od"])]
          norma = sqrt(dx*dx + dy*dy)
          x = odziv$dodanx[which(odziv$dodanavozlisca == odziv$cestedva[c,"Od"])] + 5*dy/norma
          y = odziv$dodany[which(odziv$dodanavozlisca == odziv$cestedva[c,"Od"])] - 5*dx/norma
          lines(c(x, x+dx), c(y, y+dy), col = barva, lwd=deb)
        }
      }
    })
    
    output$cestezenim <- renderTable(odziv$cesteena)
    output$cestezdvema <- renderTable(odziv$cestedva)

})
