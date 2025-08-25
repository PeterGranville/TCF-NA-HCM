
#### Setup ####

library(shiny)
library(scales)
library(plotly)
library(tidyverse)

#### End #### 

shinyServer(function(output, input)({
  
  ###############################################
  #### UI-responsive inputs                  ####
  ###############################################
  
  #### Establish selection1 choices ####
  
  choices1a <- c(
    "choices1a"
  )
  
  choices1b <- c(
    "choices1b"
  )
  
  choices1c <- c(
    "choices1c"
  )
  
  #### End #### 
  
  #### Establish selection2 choices ####
  
  choices2a <- c(
    "choices2a"
  )
  
  choices2b <- c(
    "choices2b"
  )
  
  choices2c <- c(
    "choices2c"
  )
  
  #### End #### 
  
  #### Establish selection3 choices ####
  
  choices3a <- c(
    "choices3a"
  )
  
  choices3b <- c(
    "choices3b"
  )
  
  choices3c <- c(
    "choices3c"
  )
  
  #### End #### 
  
  #### Establish selection4 choices ####
  
  choices4a <- c(
    "choices4a"
  )
  
  choices4b <- c(
    "choices4b"
  )
  
  choices4c <- c(
    "choices4c"
  )
  
  #### End #### 
  
  #### Set choice lists ####
  
  targetChoices <- reactive({
    
    switch(input$view, 
           "Fed-State Model" = choices1a,
           "Fed-College Model" = choices1b, 
           "Fed-Student Model" = choices1c
    )
    
  })
  
  rowChoices <- reactive({
    
    switch(input$view, 
           "Fed-State Model" = choices2a,
           "Fed-College Model" = choices2b, 
           "Fed-Student Model" = choices2c
    )
    
  })
  
  sectorChoices <- reactive({
    
    switch(input$view, 
           "Fed-State Model" = choices3a,
           "Fed-College Model" = choices3b, 
           "Fed-Student Model" = choices3c
    )
    
  })
  
  #### End #### 
  
  #### Define renderUI for dropdown menus #### 
  
  output$selection1 <- renderUI({
    
    selectInput("selectTarget", 
                "Select a target variable", 
                choices=targetChoices())
    
  })
  
  output$selection2 <- renderUI({
    
    selectInput("selectRow", 
                "Select a grouping variable", 
                choices=rowChoices())
    
  })
  
  output$selection3 <- renderUI({
    
    selectInput("selectSector", 
                "Select a sector", 
                choices=sectorChoices())
    
  })
  
  output$selection4 <- renderUI({
    
    checkboxInput("toggleInState", 
                  "Check to filter for in-state students", 
                  value=TRUE)
    
  })
  
  #### End #### 
  
}))