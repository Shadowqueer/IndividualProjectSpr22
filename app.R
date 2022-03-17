library(shiny)
library(tidyverse)

# This function
strDetectAllList <- function(strColumn, filterList){
  output <- rep(TRUE, length(strColumn))
  for(i in seq_along(filterList)){
    output <- output & str_detect(strColumn, filterList[i])
  }
  return(output)
}

# This function removes Travellers and Fabled from a list of characters
# This is important, because Travellers and Fabled don't appear on scripts, 
# but can still be in games of any script.
travFablFilter <- function(filterList){
  output <- vector("character", 0)
  for(i in seq_along(filterList)){
    if(!(any(str_detect(filterList[i], c(na.omit(botcDef$Travellers), 
                                       na.omit(botcDef$Fabled)))))){
    output <- c(output, filterList[i])
  }}
  return(output)
}

# This function clears out strings of just 0 that it may find in the dataset;
# They are useful for viewing the data, and for some work, but not for the unite
# function I used to create a column of the in-play characters in a given game.
removeZeroStr <- function(data, strCol){
  for (i in seq_along(strCol)){
    if(str_detect(strCol[i], "0")){
      strCol[i] <- NA
    }
  }
  return(data)
}

# This creates a new column containing all of the in-play characters
botc$GameComp <- (
  botc %>%
    removeZeroStr(botc$TF) %>%
    removeZeroStr(botc$Out) %>%
    removeZeroStr(botc$Min) %>%
    removeZeroStr(botc$Dem) %>%
    unite(., GameComp, TF, Out, Min, Dem, Trav, Fabled, 
          sep = ", ", remove = FALSE, na.rm = TRUE))$GameComp

# This function determines which of the characters on charList are in each game
# in data. It then outputs a list containing either None if none of them are, or
# a string of the form Char1+Char2+Char3...+CharN containing all the characters
# that are in that game and charList. 
InPlayCharacters <- function(data, charList){
  output <- rep("None", length(data$GameComp))
  for(i in seq_along(data$GameComp)){
    for(j in seq_along(charList)){
      if(str_detect(data$GameComp[[i]], charList[j])){
        if(str_detect(output[i], "^None$")){
          output[i] <- charList[j]
        }else{
          output[i] <- str_c(output[i], charList[j], sep = "+")
        }}}}
  return(output)
}

# Define UI starts here
ui <- fluidPage(
  # Application Title
  titlePanel("Blood on the Clocktower Data Analyzer"),
  
  tabsetPanel(
    # This tab contains all the visualizations around Characters being in games
    # together vs seperate vs could have been in but aren't.
    tabPanel(
      "Character Percent In-Play",
      sidebarLayout(
        # Sidebar with selection inputs for how you want to select the catagory
        # to draw upon, the catagory itself, and the character(s) in that script
        # or type to evaluate 
        sidebarPanel(
          selectInput("scriptOrType", "Search by Character Type or Script?", 
                      choices = c("Character Type", "Script")),
          selectInput("catagory", "Catagory Choice:", choices = NULL),
          selectInput("character", "Character Choice:", 
                      choices = NULL, multiple = TRUE)
        ),
        
        # There are two plots dependent on the chosen features here. They show
        # how often those characters are in play with and without the others.
        # The first shows it as a percentage  
        mainPanel(
          plotOutput("PercentPlot"),
          plotOutput("CountedPlot"),
        )
      )
    ),
    # This tab contains visualizations of who won what games, and why, of a 
    # given script.
    tabPanel(
      "Wins by Script",
      sidebarLayout(
        sidebarPanel(
          selectInput("winScript", "Script Choice:", 
                      choices = na.omit(botc$Script))
        ),
        mainPanel(
          plotOutput("WinTeamPlot"),
          plotOutput("WinCondPlot"),
          plotOutput("GoodWinCondPlot"),
          plotOutput("EvilWinCondPlot")
        )
      )
    )
  )
)

server <- function(input, output) {
  # These functions are to do with the first Tab, Character Percent In-Play
  
  # These are the observers, which react when an input is changed to update the 
  # options on the other inputs. 
  
  # This observer updates catagory with either the list of scripts or types
  observeEvent(input$scriptOrType, {
    if(input$scriptOrType == "Script"){
      updateSelectInput(
        inputId = "catagory", label = "Script Choice", 
        choices = na.omit(botc$Script))
    }else{
      updateSelectInput(
        inputId = "catagory", label = "Character Type", 
        choices = na.omit(names(botcDef)))
    }
  })
  # This observer updates character, changing the list of offered characters to 
  # only include what is in the list currently selected in catagory
  observeEvent(input$catagory, {
    req(input$catagory)
    if(input$scriptOrType == "Script"){
      curScriptComp <- str_split(
        filter(botc, 
               botc$Script == input$catagory)$ScriptComp[[1]],
        ", ")
      updateSelectInput(inputId = "character", 
                        choices = curScriptComp[[1]])
    }else{
      updateSelectInput(inputId = "character", 
                        choices = na.omit(botcDef[[input$catagory]]))
    }
  })
  # Now plotting starts
  
  # This plot is by percentage how often chosen character(s) were in play, in 
  # all combinations, and how often they weren't, for either the chosen script 
  # or all scripts with all the chosen characters, depending on scriptOrType
  output$PercentPlot <- renderPlot({
    req(input$character)
    # The Titles and Dataset used vary depending on scriptOrType chosen
    # The data is only of games of the chosen Script, if scriptOrType is Script,
    # or all Scripts with all of the choicen characters if scriptOrType is
    # Character Type. The titles/subtitles change to convay that.
    if(input$scriptOrType == "Script"){
      dataset <- filter(botc, botc$Script == input$catagory)
      titleStr <- "Percentage of Games of Picked Script Chosen Characters are in Play"
      subTitleStr <- NULL
    }else{
      dataset <- filter(botc, strDetectAllList(botc$ScriptComp, 
                                                  travFablFilter(input$character)))
      titleStr <- "Percentage of Games Selected Characters are in Play"
      subTitleStr <- "Out of Games with All Selected Characters on the Script"
    }
    sortedData <- InPlayCharacters(dataset, input$character)

    ggplot(data = dataset)+
      geom_bar(mapping = aes(y = TRUE, fill = sortedData), position = "fill")+
      labs(title = titleStr,
           subtitle = subTitleStr,
           x = "Percent of Games",
           y = "",
           fill = "In Play Characters")+
      scale_x_continuous(labels=scales::percent)+
      scale_y_discrete(labels = NULL)
  })
  # This plot handles the same combinations as the prior one, but it shows them 
  # in terms of the actual numbers of games of various combinations.
  output$CountedPlot <- renderPlot({
    req(input$character)
    # The Titles and Dataset used vary depending on scriptOrType chosen
    # The data is only of games of the chosen Script, if scriptOrType is Script,
    # or all Scripts with all of the choicen characters if scriptOrType is
    # Character Type. The titles/subtitles change to convay that.
    if(input$scriptOrType == "Script"){
      dataset <- filter(botc, botc$Script == input$catagory)
      titleStr <- "Games of Chosen Script with Selected Characters in Play"
      subTitleStr <- NULL
    }else{
      dataset <- filter(botc, strDetectAllList(botc$ScriptComp, 
                                                  travFablFilter(input$character)))
      titleStr <- "Games with Selected Characters in Play"
      subTitleStr <- "Out of Games with All Selected Characters on the Script"
    }
    sortedData <- InPlayCharacters(dataset, input$character)
    
    ggplot(data = dataset)+
      geom_bar(mapping = aes(x = sortedData, fill = Win))+
      labs(title = titleStr,
           subtitle = subTitleStr,
           x = "In Play Characters",
           y = "Games", 
           fill = "Winning Team")
  })
  
  # These functions have to do with the Second Tab, Wins by Script
  # This shows percentages for who won games of this Script
  output$WinTeamPlot <- renderPlot({
    req(input$winScript)
    dataset <- filter(botc, botc$Script == input$winScript)
    
    ggplot(data = dataset)+
      geom_bar(mapping = aes(y = TRUE, fill = Win), position = "fill")+
      labs(title = "Win Percentage for Chosen Script",
           x = "Percent of Games",
           y = "",
           fill = "Winning Team")+
      scale_x_continuous(labels=scales::percent)+
      scale_y_discrete(labels = NULL)
  })
  # This shows percentages for why games of this script ended
  output$WinCondPlot <- renderPlot({
    req(input$winScript)
    dataset <- filter(botc, botc$Script == input$winScript)
    
    ggplot(data = dataset)+
      geom_bar(mapping = aes(y = TRUE, fill = GameEnd), position = "fill")+
      labs(title = "How Games of Chosen Script Ended by Percent",
           subtitle = "\"Good\" and \"Evil\" Represent Each Team's Standard Win Condition",
           x = "Percent of Games",
           y = "",
           fill = "Game Ending Condition")+
      scale_x_continuous(labels=scales::percent)+
      scale_y_discrete(labels = NULL)
  })
  # This shows percentages for why games of this script ended, when Good Won
  output$GoodWinCondPlot <- renderPlot({
    req(input$winScript)
    dataset <- filter(botc, botc$Script == input$winScript, botc$Win == "Good")
    req(dataset$GameEnd[1])
    
    ggplot(data = dataset)+
      geom_bar(mapping = aes(y = TRUE, fill = GameEnd), position = "fill")+
      labs(title = "How Games of Chosen Script Good Won Ended by Percent",
           subtitle = "\"Good\" Represent Good's Standard Win Condition, aka No Living Demons",
           x = "Percent of Games",
           y = "",
           fill = "Game Ending Condition")+
      scale_x_continuous(labels=scales::percent)+
      scale_y_discrete(labels = NULL)
  })
  # This shows percentages for why games of this script ended, when Evil Won
  output$EvilWinCondPlot <- renderPlot({
    req(input$winScript)
    dataset <- filter(botc, botc$Script == input$winScript, botc$Win == "Evil")
    req(dataset$GameEnd[1])
    
    ggplot(data = dataset)+
      geom_bar(mapping = aes(y = TRUE, fill = GameEnd), position = "fill")+
      labs(title = "How Games of Chosen Script Evil Won Ended by Percent",
           subtitle = "\"Evil\" Represent Evil's Standard Win Condition, aka 2 Living Players",
           x = "Percent of Games",
           y = "",
           fill = "Game Ending Condition")+
      scale_x_continuous(labels=scales::percent)+
      scale_y_discrete(labels = NULL)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
