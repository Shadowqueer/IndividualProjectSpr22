#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

tbChars <- c("Washerwoman", "Librarian", "Investigator", "Chef", 
             "Empath", "Fortune Teller", "Monk", "Undertaker", 
             "Ravenkeeper", "Slayer", "Virgin", "Soldier", "Mayor",
             "Butler", "Drunk", "Recluse", "Saint",
             "Poisoner", "Spy", "Scarlet Woman", "Baron",
             "Imp")

# Define UI for application that draws a stacked bar chart
ui <- fluidPage(

    # Application title
    titlePanel("Trouble Brewing"),

    # Sidebar with a select input for character name 
    sidebarLayout(
        sidebarPanel(
            selectInput("character", "Character Choice:", tbChars)
        ),

        # documentation
        mainPanel(
           plotOutput("charPercPlot")
        )
    )
)

# documentation
server <- function(input, output) {

    output$charPercPlot <- renderPlot({
        # documentation
        x    <- botc[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # documentation
        ggplot(data = filter(botc, !(is.na(Drunk)), Script == "TB"))+
          geom_bar(mapping = aes(y = Drunk))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
