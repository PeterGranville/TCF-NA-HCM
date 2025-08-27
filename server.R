
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
  
  choices1 <- reactive({
    
    switch(input$view, 
           "Fed-State Model" = choices1a,
           "Fed-College Model" = choices1b, 
           "Fed-Student Model" = choices1c
    )
    
  })
  
  choices2 <- reactive({
    
    switch(input$view, 
           "Fed-State Model" = choices2a,
           "Fed-College Model" = choices2b, 
           "Fed-Student Model" = choices2c
    )
    
  })
  
  choices3 <- reactive({
    
    switch(input$view, 
           "Fed-State Model" = choices3a,
           "Fed-College Model" = choices3b, 
           "Fed-Student Model" = choices3c
    )
    
  })
  
  choices4 <- reactive({
    
    switch(input$view, 
           "Fed-State Model" = choices4a,
           "Fed-College Model" = choices4b, 
           "Fed-Student Model" = choices4c
    )
    
  })
  
  #### End #### 
  
  #### Define renderUI for dropdown menus #### 
  
  output$selection1 <- renderUI({
    
    selectInput("select1", 
                "Make selection 1:", 
                choices=choices1())
    
  })
  
  output$selection2 <- renderUI({
    
    selectInput("select2", 
                "Make selection 2:", 
                choices=choices2())
    
  })
  
  output$selection3 <- renderUI({
    
    selectInput("select3", 
                "Make selection 3:", 
                choices=choices3())
    
  })
  
  output$selection4 <- renderUI({
    
    selectInput("select4", 
                "Make selection 4:", 
                choices=choices4())
    
  })
  
  #### End #### 
  
  ###############################################
  #### Figure generation                     ####
  ###############################################
  
  output$baselineTable <- renderTable({
    
    #### Create "print" objects ####
    
    printSelect1 <- input$select1
    printSelect2 <- input$select2
    printSelect3 <- input$select3
    printSelect4 <- input$select4
    
    #### End #### 
    
    #### Load baseline data ####
    
    setwd("/Users/peter_granville/Fed State Modeling")
    
    baselineDF <- read.csv("Baseline results.csv", header=FALSE, check.names=FALSE)
    
    #### End #### 
    
    #### Format baseline data ####
    
    baselineDF <- as.data.frame(t(baselineDF)) %>% rename(
      `Label` = `V1`, 
      `Value` = `V2`
    ) 
    
    baselineDF <- baselineDF %>% mutate(
      `Label` = ifelse(`Label`=="table1.students", "Total students", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table2.netprice0", "Share with net price $0", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table2.netprice5000", "Share with net price $1 to $5,000", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table2.netprice10000", "Share with net price $5,001 to $10,000", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table2.netprice15000", "Share with net price $10,001 to $15,000", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table2.netprice20000", "Share with net price $15,001 to $20,000", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table2.netpriceceiling", "Share with net price above $20,000", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table3.totalloans0", "Share with total loans $0", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table3.totalloans5000", "Share with total loans $1 to $5,000", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table3.totalloans10000", "Share with total loans $5,001 to $10,000", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table3.totalloans15000", "Share with total loans $10,001 to $15,000", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table3.totalloans20000", "Share with total loans $15,001 to $20,000", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table3.totalloansceiling", "Share with total loans above $20,000", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table4.aian", "Share Native American / Alaska Native", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table4.asia", "Share Asian", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table4.bkaa", "Share Black / African American", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table4.hisp", "Share Hispanic / Latino", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table4.2mor", "Share two or more races", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table4.nhpi", "Share Native Hawaiian / Other Pacific Islander", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table4.unkn", "Share race/ethnicity unknown", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table4.nonr", "Share nonresident", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table4.whit", "Share white", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table4.male", "Share male", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table4.female", "Share female", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table4.firstgen", "Share first-gen", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table4.notfirstgen", "Share not first-gen", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table4.dependent", "Share dependent", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table4.independent", "Share independent", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table4.zeroEFC", "Share zero-EFC", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table4.nonzeroEFC", "Share nonzero-EFC", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table4.parent", "Share parent", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table4.nonparent", "Share non-parent", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table4.veteran", "Share veteran", `Label`)
    ) %>% mutate(
      `Label` = ifelse(`Label`=="table4.nonveteran", "Share nonveteran", `Label`)
    ) 
    
    baselineDF <- baselineDF %>% mutate(
      `Value` = as.numeric(`Value`)
    ) %>% mutate(
      `Value` = ifelse(`Label`=="Total students", comma(`Value`, accuracy=1), percent(`Value`, accuracy=0.1))
    )
    
    #### End #### 
    
    #### Print baseline data ####
    
    print(baselineDF)
    
    #### End #### 
    
  })
  
  
}))