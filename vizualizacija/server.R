#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# start
source('~/RStudio/Matematika z racunalnikom/Dinamika-cestnega-omrezja/semafor.R')
source('~/RStudio/Matematika z racunalnikom/Dinamika-cestnega-omrezja/nov_na_sek.R')
source('~/RStudio/Matematika z racunalnikom/Dinamika-cestnega-omrezja/avti.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$omrezje <- renderPlot({
        istisemafor = TRUE
        for(i in 1:2){
            if(semafor[i] != input$semafor[i]){
                istisemafor = FALSE
                break
            }
        }
        if(!istisemafor | (nov_na_sek != input$intenzivnost)){
            semafor = input$semafor
            dump('semafor', file = 'semafor.R')
            nov_na_sek = input$intenzivnost
            dump('nov_na_sek', file = 'nov_na_sek.R')
            source('~/RStudio/Matematika z racunalnikom/Dinamika-cestnega-omrezja/avti.R')
        }

        tavti = vsiavti[casi == input$t]
        plot(tavti, rep(0, length(tavti)), type = 'p', xlim = c(0, dolzina), ylim = c(0, steps))
    })

})
