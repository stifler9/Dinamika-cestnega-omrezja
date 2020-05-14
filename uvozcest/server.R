library(shiny)

shinyServer(function(input, output) {
    
    podatki_ceste <- read.csv2("../podatki/urejeni_podatki.csv")
    
    odziv = reactiveValues()
    odziv$cestedva = data.frame()
    odziv$cesteena = data.frame()
    odziv$vsavozlisca = unique(c(levels(podatki_ceste[,"Od"]), levels(podatki_ceste[,"Do"])))
    odziv$iskanavozlisca = unique(c(levels(podatki_ceste[,"Od"]), levels(podatki_ceste[,"Do"])))
    odziv$dodanavozlisca = c()
    odziv$dodanx = c()
    odziv$dodany = c()
    odziv$semafortabela = data.frame()
    odziv$semaforopcije = c()
    odziv$semaforji = list()
    
    najdiVozlisca <- function(){
      if(input$iskanje == ""){
        odziv$iskanavozlisca = odziv$vsavozlisca
      }else{
        odziv$iskanavozlisca = odziv$vsavozlisca[grep(input$iskanje, odziv$vsavozlisca, ignore.case = TRUE)]
      }
    }
    
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
        for(i in 1:n){
          barva = ""
          if(odziv$cestedva[i,"Kategorija"] == "AC"){
            barva = "green"
          }else if(odziv$cestedva[i,"Kategorija"] == "HC"){
            barva = "blue"
          }else if(odziv$cestedva[i,"Kategorija"] %in% c("G1", "R1")){
            barva = "red"
          }else{
            barva = "orange"
          }
          zacetna = FALSE
          if(i %in% input$zacetneceste){zacetna = TRUE}
          ceste[paste(odziv$cestedva[i,"Od"], odziv$cestedva[i,"Do"], odziv$cestedva[i,"Kategorija"], sep="_"),c(1,2,3)] = c(as.character(odziv$cestedva[i,"Od"]),
                                                                                             as.character(odziv$cestedva[i,"Do"]),
                                                                                             barva)
          ceste[paste(odziv$cestedva[i,"Od"], odziv$cestedva[i,"Do"], odziv$cestedva[i,"Kategorija"], sep="_"),4] = zacetna
          ceste[paste(odziv$cestedva[i,"Od"], odziv$cestedva[i,"Do"], odziv$cestedva[i,"Kategorija"], sep="_"),5] = odziv$cestedva[i,"Dolzina"]
        }
        ceste[,1] = as.character(ceste[,1])
        ceste[,2] = as.character(ceste[,2])
        ceste[,3] = as.character(ceste[,3])
        ceste[,4] = as.logical(ceste[,4])
        ceste[,5] = as.numeric(ceste[,5])
        
        #konstruiramo podatke o koordinatah
        koor = data.frame()
        koor[,1] = numeric()
        koor[,2] = numeric()
        povezave = NULL
        for(i in 1:length(odziv$dodanavozlisca)){
          koor[odziv$dodanavozlisca[i],] = c(odziv$dodanx[i], odziv$dodany[i])
          vhodne = c()
          izhodne = c()
          for(c in rownames(ceste)){
            if(ceste[c,1] == odziv$dodanavozlisca[i]){
              if(!ceste[c,4]){
                izhodne = c(izhodne, c)
              }
            }
            if(ceste[c,2] == odziv$dodanavozlisca[i]){
              vhodne = c(vhodne, c)
            }
          }
          if(length(izhodne) > 0){
            for(v in vhodne){
              povezave[[v]] = izhodne
            }
          }
        }
        semaforji = list()
        for(sem in names(odziv$semaforji)){
          semaforji[[sem]] = odziv$semaforji[[sem]]
        }
        
        #shranimo
        dump(c("ceste", "koor", "povezave", "semaforji"), file = "../podatki/ceste_app.R")
        print("shranjeno")
      }
    })
    
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
    
    observeEvent(input$dodajsemafor, {
      odziv$semaforji[[input$vozliscesemafor]] = odziv$semafortabela
      odziv$semafortabela = data.frame()
      odziv$semaforopcije = NULL
    })
    
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

    output$ceste <- renderTable(podatki_ceste)
    
    izpisvozlisc <- function(){
      n = length(odziv$dodanavozlisca)
      if(n > 0){
        txt = "Izbrana vozlisca: "
        for(i in 1:n){
          txt = paste(txt, odziv$dodanavozlisca[i],
                      " (", odziv$dodanx[i], ", ", odziv$dodany[i], "), ", sep = "")
        }
        return(txt)
      }else{
        return("Ni izbranih vozlisc")
      }
    }
    
    output$vkljucenavozlisca <- renderText({
      izpisvozlisc()
    })
    
    output$zacetneui <- renderUI({
      checkboxGroupInput("zacetneceste", "Zacetne ceste:",
                         choices = c(1:dim(odziv$cestedva)[1]))
    })
    
    output$semaforji <- renderUI({
      selectInput("vozliscesemafor", "Vozlisce:", choices = odziv$dodanavozlisca)
    })
    
    output$semaforjirdece <- renderUI({
      checkboxGroupInput("rdeceluci", "Rdece luci:", choices = odziv$semaforopcije)
    })
    
    output$tabelardecih <- renderTable(odziv$semafortabela)
    
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
