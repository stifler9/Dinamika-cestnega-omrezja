#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source('C:/Users/Zan/Documents/RStudio/Matematika z racunalnikom/Dinamika-cestnega-omrezja/avti.R')

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Dinamika cestnega omrezja"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("t",
                        "Time:",
                        min = 0,
                        max = steps,
                        value = 0,
                        animate = animationOptions(interval = 10)),
            
            sliderInput("semafor",
                        "Semafor:",
                        min = 0,
                        max = steps,
                        value = semafor),
            sliderInput("intenzivnost",
                        "Intenzivnost prihoda novih (/s):",
                        min = 0.0,
                        max = 1.0,
                        step = 0.05,
                        value = nov_na_sek)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("omrezje")
        )
    )
))
