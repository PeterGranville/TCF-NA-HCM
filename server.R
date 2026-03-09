
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
    "$0 at all eligible institutions", 
    "$1,000 at all eligible institutions",
    "$1,000 at eligible two-year institutions and $3,000 at eligible four-year institutions"
  )
  
  choices1b <- c(
    "100%", 
    "50%", 
    "25%"
  )
  
  choices1c <- choices1b
  
  choices1d <- c(
    "10%", 
    "20%", 
    "30%"
  )
  
  choices1e <- c(
    "65%", 
    "75%", 
    "85%"
  )
  
  choices1f <- c(
    "No tuition charged", 
    "No undergraduate loans"
  )
  
  choices1g <- choices1b
  
  #### End #### 
  
  #### Establish selection2 choices ####
  
  choices2a <- c(
    "$0.10", 
    "$0.25", 
    "$0.50", 
    "$1.00"
  )
  
  choices2b <- choices2a
  
  choices2c <- choices2a
  
  choices2d <- choices2a
  
  choices2e <- c(
    "$0.50", 
    "$1.00", 
    "$1.50"
  )
  
  choices2f <- c(
    "Skipped"
  )
  
  choices2g <- c(
    "Skipped"
  )
  
  #### End #### 
  
  #### Establish selection3 choices ####
  
  choices3a <- c(
    "No restriction based on enrollment intensity", 
    "Restricted to students enrolled full-time"
  )
  
  choices3b <- choices3a
  
  choices3c <- choices3a
  
  choices3d <- choices3a
  
  choices3e <- c(
    "Skipped"
  )
  
  choices3f <- choices3a
  
  choices3g <- c(
    "Skipped"
  )
  
  #### End #### 
  
  #### Establish selection4 choices ####
  
  choices4a <- c(
    "No means testing", 
    "Pell Grant recipients only"
  )
  
  choices4b <- choices4a
  
  choices4c <- choices4a
  
  choices4d <- choices4a
  
  choices4e <- c(
    "Skipped"
  )
  
  choices4f <- choices4a
  
  choices4g <- c(
    "Skipped"
  )
  
  #### End #### 
  
  #### Establish selection5 choices ####
  
  choices5a <- c(
    "Yes", 
    "No"
  )
  
  choices5b <- choices5a
  
  choices5c <- choices5a
  
  choices5d <- choices5a
  
  choices5e <- c(
    "Skipped"
  )
  
  choices5f <- choices5a
  
  choices5g <- c(
    "Skipped"
  )
  
  #### End #### 
  
  #### Establish selection6 choices ####
  
  choices6a <- c(
    "Skipped"
  )
  
  choices6b <- c(
    "Skipped"
  )
  
  choices6c <- c(
    "Skipped"
  )
  
  choices6d <- c(
    "Skipped"
  )
  
  choices6e <- c(
    "Skipped"
  )
  
  choices6f <- c(
    "Public only", 
    "Public and nonprofit only", 
    "All controls"
  )
  
  choices6g <- choices6f
  
  #### End #### 
  
  #### Establish selection7 choices ####
  
  choices7a <- c(
    "Only two-year institutions", 
    "Both two- and four-year institutions"
  )
  
  choices7b <- choices7a
  
  choices7c <- choices7a
  
  choices7d <- choices7a
  
  choices7e <- choices7a
  
  choices7f <- choices7a
  
  choices7g <- choices7a
  
  #### End #### 
  
  #### Establish selection8 choices ####
  
  choices8a <- c(
    "No", 
    "Yes"
  )
  
  choices8b <- choices8a
  
  choices8c <- choices8a
  
  choices8d <- choices8a
  
  choices8e <- choices8a
  
  choices8f <- c(
    "Skipped"
  )
  
  choices8g <- c(
    "Skipped"
  )
  
  #### End #### 
  
  #### Establish selection9 choices ####
  
  choices9a <- c(
    "15% and above", 
    "25% and above", 
    "35% and above"
  )
  
  choices9b <- choices9a
  
  choices9c <- choices9a
  
  choices9d <- choices9a
  
  choices9e <- choices9a
  
  choices9f <- c(
    "5% and above", 
    "10% and above", 
    "15% and above"
  )
  
  choices9g <- c(
    "Skipped"
  )
  
  #### End #### 
  
  #### Establish selection10 choices ####
  
  choices10a <- c(
    "Institutional participation map", 
    "State participation map", 
    "Net price percentiles", 
    "Educational attainment", 
    "Economic impact", 
    "State funding"
  )
  
  choices10b <- choices10a
  
  choices10c <- choices10a
  
  choices10d <- choices10a
  
  choices10e <- choices10a
  
  choices10f <- c(
    "Institutional participation map", 
    # "State participation map", 
    "Net price percentiles", 
    "Educational attainment", 
    "Economic impact", 
    "State funding"
  )
  
  choices10g <- choices10f
  
  #### End #### 
  
  #### Establish selection11 choices ####
  
  choices11a <- c(
    "Student participation", 
    "Student debt", 
    "Degrees and certificates", 
    "Government cost"
  )
  
  choices11b <- choices11a
  
  choices11c <- choices11a
  
  choices11d <- choices11a
  
  choices11e <- choices11a
  
  choices11f <- choices11a
  
  choices11g <- choices11a
  
  #### End #### 
  
  #### Set choice lists ####
  
  choices1 <- reactive({
    
    switch(input$goal, 
           "Plan A" = choices1a,
           "Plan B" = choices1b, 
           "Plan C" = choices1c, 
           "Plan D" = choices1d, 
           "Plan E" = choices1e, 
           "Plan F" = choices1f, 
           "Plan G" = choices1g
    )
    
  })
  
  choices2 <- reactive({
    
    switch(input$goal, 
           "Plan A" = choices2a,
           "Plan B" = choices2b, 
           "Plan C" = choices2c, 
           "Plan D" = choices2d, 
           "Plan E" = choices2e, 
           "Plan F" = choices2f, 
           "Plan G" = choices2g
    )
    
  })
  
  choices3 <- reactive({
    
    switch(input$goal, 
           "Plan A" = choices3a,
           "Plan B" = choices3b, 
           "Plan C" = choices3c, 
           "Plan D" = choices3d, 
           "Plan E" = choices3e, 
           "Plan F" = choices3f, 
           "Plan G" = choices3g
    )
    
  })
  
  choices4 <- reactive({
    
    switch(input$goal, 
           "Plan A" = choices4a,
           "Plan B" = choices4b, 
           "Plan C" = choices4c, 
           "Plan D" = choices4d, 
           "Plan E" = choices4e, 
           "Plan F" = choices4f, 
           "Plan G" = choices4g
    )
    
  })
  
  choices5 <- reactive({
    
    switch(input$goal, 
           "Plan A" = choices5a,
           "Plan B" = choices5b, 
           "Plan C" = choices5c, 
           "Plan D" = choices5d, 
           "Plan E" = choices5e, 
           "Plan F" = choices5f, 
           "Plan G" = choices5g
    )
    
  })
  
  choices6 <- reactive({
    
    switch(input$goal, 
           "Plan A" = choices6a,
           "Plan B" = choices6b, 
           "Plan C" = choices6c, 
           "Plan D" = choices6d, 
           "Plan E" = choices6e, 
           "Plan F" = choices6f, 
           "Plan G" = choices6g
    )
    
  })
  
  choices7 <- reactive({
    
    switch(input$goal, 
           "Plan A" = choices7a,
           "Plan B" = choices7b, 
           "Plan C" = choices7c, 
           "Plan D" = choices7d, 
           "Plan E" = choices7e, 
           "Plan F" = choices7f, 
           "Plan G" = choices7g
    )
    
  })
  
  choices8 <- reactive({
    
    switch(input$goal, 
           "Plan A" = choices8a,
           "Plan B" = choices8b, 
           "Plan C" = choices8c, 
           "Plan D" = choices8d, 
           "Plan E" = choices8e, 
           "Plan F" = choices8f, 
           "Plan G" = choices8g
    )
    
  })
  
  choices9 <- reactive({
    
    switch(input$goal, 
           "Plan A" = choices9a,
           "Plan B" = choices9b, 
           "Plan C" = choices9c, 
           "Plan D" = choices9d, 
           "Plan E" = choices9e, 
           "Plan F" = choices9f, 
           "Plan G" = choices9g
    )
    
  })
  
  choices10 <- reactive({
    
    switch(input$goal, 
           "Plan A" = choices10a,
           "Plan B" = choices10b, 
           "Plan C" = choices10c, 
           "Plan D" = choices10d, 
           "Plan E" = choices10e, 
           "Plan F" = choices10f, 
           "Plan G" = choices10g
    )
    
  })
  
  choices11 <- reactive({
    
    switch(input$goal, 
           "Plan A" = choices11a,
           "Plan B" = choices11b, 
           "Plan C" = choices11c, 
           "Plan D" = choices11d, 
           "Plan E" = choices11e, 
           "Plan F" = choices11f, 
           "Plan G" = choices11g
    )
    
  })
  
  #### End #### 
  
  #### Set question visibilities ####
  
  visibility1 <- c(
    "Plan A",
    "Plan B", 
    "Plan C", 
    "Plan D",
    "Plan E", 
    "Plan F", 
    "Plan G"
  )
  
  visibility2 <- c(
    "Plan A", 
    "Plan B",
    "Plan C", 
    "Plan D",
    "Plan E", 
    "Plan F", 
    "Plan G"
  )
  
  visibility3 <- c(
    "Plan A", 
    "Plan B", 
    "Plan C",
    "Plan D",
    "Plan E", 
    "Plan F", 
    "Plan G"
  )
  
  visibility4 <- c(
    "Plan A", 
    "Plan B", 
    "Plan C", 
    "Plan D",
    "Plan E", 
    "Plan F", 
    "Plan G"
  )
  
  visibility5 <- c(
    "Plan A", 
    "Plan B", 
    "Plan C", 
    "Plan D",
    "Plan E",
    "Plan F", 
    "Plan G"
  )
  
  visibility6 <- c(
    "Plan A", 
    "Plan B", 
    "Plan C", 
    "Plan D",
    "Plan E", 
    "Plan F",
    "Plan G"
  )
  
  visibility7 <- c(
    "Plan A", 
    "Plan B", 
    "Plan C", 
    "Plan D",
    "Plan E", 
    "Plan F", 
    "Plan G"
  )
  
  visibility8 <- c(
    "Plan A", 
    "Plan B", 
    "Plan C", 
    "Plan D",
    "Plan E", 
    "Plan F", 
    "Plan G"
  )
  
  visibility9 <- c(
    "Plan A", 
    "Plan B", 
    "Plan C", 
    "Plan D",
    "Plan E", 
    "Plan F", 
    "Plan G"
  )
  
  visibility10 <- c(
    "Plan A", 
    "Plan B", 
    "Plan C", 
    "Plan D",
    "Plan E", 
    "Plan F", 
    "Plan G"
  )
  
  visibility11 <- c(
    "Plan A", 
    "Plan B", 
    "Plan C", 
    "Plan D",
    "Plan E", 
    "Plan F", 
    "Plan G"
  )
  
  #### End #### 
  
  #### Set questions ####
  
  question1 <- reactive({
    
    switch(input$goal, 
           "Plan A" = "To what amount does tuition and fees among eligible students decrease?",
           "Plan B" = "By what percentage does tuition and fees among eligible students decrease?", 
           "Plan C" = "By what percentage does student debt among eligible students decrease?", 
           "Plan D" = "To what maximum percentage of family income is the net price among eligible students reduced?", 
           "Plan E" = "To what minimum percentage of total revenue are federal and state funds increased?", 
           "Plan F" = "What pricing policy is required among participating colleges?", 
           "Plan G" = "By what percentage do total federal grants among eligible students increase?"
    )
    
  })
  
  question2 <- reactive({
    
    switch(input$goal, 
           "Plan A" = "For each $1 in the federal block grant, how much does a participating state need to match?",
           "Plan B" = "For each $1 in the federal block grant, how much does a participating state need to match?", 
           "Plan C" = "For each $1 in the federal block grant, how much does a participating state need to match?", 
           "Plan D" = "For each $1 in the federal block grant, how much does a participating state need to match?", 
           "Plan E" = "For each $1 in state funding for public institutions, how much does the federal government match?", 
           "Plan F" = "Skipped", 
           "Plan G" = "Skipped"
    )
    
  })
  
  question3 <- reactive({
    
    switch(input$goal, 
           "Plan A" = "Is student eligibility limited on the basis of enrollment intensity?",
           "Plan B" = "Is student eligibility limited on the basis of enrollment intensity?", 
           "Plan C" = "Is student eligibility limited on the basis of enrollment intensity?", 
           "Plan D" = "Is student eligibility limited on the basis of enrollment intensity?", 
           "Plan E" = "Skipped", 
           "Plan F" = "Is student eligibility limited on the basis of enrollment intensity?", 
           "Plan G" = "Skipped"
    )
    
  })
  
  question4 <- reactive({
    
    switch(input$goal, 
           "Plan A" = "Is student eligibility limited on a financial basis?",
           "Plan B" = "Is student eligibility limited on a financial basis?", 
           "Plan C" = "Is student eligibility limited on a financial basis?", 
           "Plan D" = "Is student eligibility limited on a financial basis?", 
           "Plan E" = "Skipped", 
           "Plan F" = "Is student eligibility limited on a financial basis?", 
           "Plan G" = "Skipped"
    )
    
  })
  
  question5 <- reactive({
    
    switch(input$goal, 
           "Plan A" = "Is student eligibility limited to in-state students?",
           "Plan B" = "Is student eligibility limited to in-state students?", 
           "Plan C" = "Is student eligibility limited to in-state students?", 
           "Plan D" = "Is student eligibility limited to in-state students?", 
           "Plan E" = "Skipped", 
           "Plan F" = "Does the policy only apply to in-state students?", 
           "Plan G" = "Skipped"
    )
    
  })
  
  question6 <- reactive({
    
    switch(input$goal, 
           "Plan A" = "Skipped",
           "Plan B" = "Skipped", 
           "Plan C" = "Skipped", 
           "Plan D" = "Skipped", 
           "Plan E" = "Skipped", 
           "Plan F" = "Is institutional eligibility limited to a certain control?", 
           "Plan G" = "Is institutional eligibility limited to a certain control?"
    )
    
  })
  
  question7 <- reactive({
    
    switch(input$goal, 
           "Plan A" = "Is institutional eligibility limited to a certain level?",
           "Plan B" = "Is institutional eligibility limited to a certain level?", 
           "Plan C" = "Is institutional eligibility limited to a certain level?", 
           "Plan D" = "Is institutional eligibility limited to a certain level?", 
           "Plan E" = "Is institutional eligibility limited to a certain level?", 
           "Plan F" = "Is institutional eligibility limited to a certain level?", 
           "Plan G" = "Is institutional eligibility limited to a certain level?"
    )
    
  })
  
  question8 <- reactive({
    
    switch(input$goal, 
           "Plan A" = "Should it be assumed that states that declined to participate in ACA’s Medicaid expansion will decline to participate in this program?",
           "Plan B" = "Should it be assumed that states that declined to participate in ACA’s Medicaid expansion will decline to participate in this program?", 
           "Plan C" = "Should it be assumed that states that declined to participate in ACA’s Medicaid expansion will decline to participate in this program?", 
           "Plan D" = "Should it be assumed that states that declined to participate in ACA’s Medicaid expansion will decline to participate in this program?", 
           "Plan E" = "Should it be assumed that states that declined to participate in ACA’s Medicaid expansion will decline to participate in this program?", 
           "Plan F" = "Skipped", 
           "Plan G" = "Skipped"
    )
    
  })
  
  question9 <- reactive({
    
    switch(input$goal, 
           "Plan A" = "What level of increase in a state’s annual appropriations for higher education would be too large to participate in the program?",
           "Plan B" = "What level of increase in a state’s annual appropriations for higher education would be too large to participate in the program?", 
           "Plan C" = "What level of increase in a state’s annual appropriations for higher education would be too large to participate in the program?", 
           "Plan D" = "What level of increase in a state’s annual appropriations for higher education would be too large to participate in the program?", 
           "Plan E" = "What level of increase in a state’s annual appropriations for higher education would be too large to participate in the program?", 
           "Plan F" = "What level of decrease in a college’s total revenue would be too large to participate in the program?", 
           "Plan G" = "Skipped"
    )
    
  })
  
  question10 <- reactive({
    
    switch(input$goal, 
           "Plan A" = "What figure would you like displayed in the Figure View?",
           "Plan B" = "What figure would you like displayed in the Figure View?", 
           "Plan C" = "What figure would you like displayed in the Figure View?", 
           "Plan D" = "What figure would you like displayed in the Figure View?", 
           "Plan E" = "What figure would you like displayed in the Figure View?", 
           "Plan F" = "What figure would you like displayed in the Figure View?", 
           "Plan G" = "What figure would you like displayed in the Figure View?"
    )
    
  })
  
  question11 <- reactive({
    
    switch(input$goal, 
           "Plan A" = "What table would you like displayed in the Table View?",
           "Plan B" = "What table would you like displayed in the Table View?", 
           "Plan C" = "What table would you like displayed in the Table View?", 
           "Plan D" = "What table would you like displayed in the Table View?", 
           "Plan E" = "What table would you like displayed in the Table View?", 
           "Plan F" = "What table would you like displayed in the Table View?", 
           "Plan G" = "What table would you like displayed in the Table View?"
    )
    
  })
  
  #### End #### 
  
  #### Define renderUI for dropdown menus #### 
  
  output$selection1 <- renderUI({
    
    if(input$goal %in% visibility1){
      selectInput("select1", 
                  label=question1(), 
                  choices=choices1())
    }
    
  })
  
  output$selection2 <- renderUI({
    
    if(input$goal %in% visibility2){
      selectInput("select2", 
                  label=question2(), 
                  choices=choices2())
    }
    
  })
  
  output$selection3 <- renderUI({
    
    if(input$goal %in% visibility3){
      selectInput("select3", 
                  label=question3(), 
                  choices=choices3())
    }
    
  })
  
  output$selection4 <- renderUI({
    
    if(input$goal %in% visibility4){
      selectInput("select4", 
                  label=question4(), 
                  choices=choices4())
    }
    
  })
  
  output$selection5 <- renderUI({
    
    if(input$goal %in% visibility5){
      selectInput("select5", 
                  label=question5(), 
                  choices=choices5()) 
    }
    
  })
  
  output$selection6 <- renderUI({
    
    if(input$goal %in% visibility6){
      selectInput("select6", 
                  label=question6(), 
                  choices=choices6())
    }
    
  })
  
  output$selection7 <- renderUI({
    
    if(input$goal %in% visibility7){
      selectInput("select7", 
                  label=question7(), 
                  choices=choices7())
    }
    
  })
  
  output$selection8 <- renderUI({
    
    if(input$goal %in% visibility8){
      selectInput("select8", 
                  label=question8(), 
                  choices=choices8())
    }
    
  })
  
  output$selection9 <- renderUI({
    
    if(input$goal %in% visibility9){
      selectInput("select9", 
                  label=question9(), 
                  choices=choices9())
    }
    
  })
  
  output$selection10 <- renderUI({
    
    if(input$goal %in% visibility10){
      selectInput("select10", 
                  label=question10(), 
                  choices=choices10())
    }
    
  })
  
  output$selection11 <- renderUI({
    
    if(input$goal %in% visibility11){
      selectInput("select11", 
                  label=question11(), 
                  choices=choices11())
    }
    
  })
  
  #### End #### 
  
  ###############################################
  #### Table generation                      ####
  ###############################################
  
  output$table1 <- renderTable({
    
    #### Create "print" objects ####
    
    printGoal <- input$goal
    printSelect1 <- input$select1
    printSelect2 <- input$select2
    printSelect3 <- input$select3
    printSelect4 <- input$select4
    printSelect5 <- input$select5
    printSelect6 <- input$select6
    printSelect7 <- input$select7
    printSelect8 <- input$select8
    printSelect9 <- input$select9
    printSelect10 <- input$select10
    printSelect11 <- input$select11
    
    #### End #### 
    
    #### Load data and filter for selected simulation ####
    
    # simulations <- read.csv(
    #   "Simulation results.csv", header=TRUE, check.names=FALSE
    # ) %>% filter(
    #   `program.goal` == printGoal, 
    #   `elig.carnegie` == printSelect1, 
    #   `elig.control` == printSelect2, 
    #   `elig.halftime` == printSelect3, 
    #   `elig.efc` == printSelect4, 
    #   `elig.outofstate` == printSelect5, 
    #   `elig.noncitizen` == printSelect6, 
    #   `elig.fafsa` == printSelect7,
    #   `elig.gpa` == printSelect8
    # )
    
    #### End #### 
    
    #### Print Table 1 ####
    
    table1 <- data.frame(
      `Goal` = character(), 
      `Select1` = character(), 
      check.names=FALSE
    ) %>% add_row(
      `Goal` = printGoal, 
      `Select1` = printSelect1
    )
    
    print(table1)
    
    #### End #### 
    
  })
  
  output$figure1 <- renderPlotly({
    
    #### Create "print" objects ####
    
    printGoal <- input$goal
    printSelect1 <- input$select1
    printSelect2 <- input$select2
    printSelect3 <- input$select3
    printSelect4 <- input$select4
    printSelect5 <- input$select5
    printSelect6 <- input$select6
    printSelect7 <- input$select7
    printSelect8 <- input$select8
    printSelect9 <- input$select9
    printSelect10 <- input$select10
    printSelect11 <- input$select11
    
    #### End #### 
    
    #### Load data and filter for selected simulation ####
    
    # simulations <- read.csv(
    #   "Simulation results.csv", header=TRUE, check.names=FALSE
    # ) %>% filter(
    #   `program.goal` == printGoal, 
    #   `elig.carnegie` == printSelect1, 
    #   `elig.control` == printSelect2, 
    #   `elig.halftime` == printSelect3, 
    #   `elig.efc` == printSelect4, 
    #   `elig.outofstate` == printSelect5, 
    #   `elig.noncitizen` == printSelect6, 
    #   `elig.fafsa` == printSelect7,
    #   `elig.gpa` == printSelect8
    # )
    
    #### End #### 
    
    #### Print Figure 1 ####
    
    tempDF <- data.frame(
      `Goal` = character(), 
      `Select1` = character(), 
      check.names=FALSE
    ) %>% add_row(
      `Goal` = printGoal, 
      `Select1` = printSelect1
    ) %>% mutate(
      `Select1 characters` = nchar(`Select1`)
    )
    
    figure1 <- ggplot(
      data=tempDF, 
      mapping=aes(
        x=`Goal`, 
        y=`Select1 characters`
      )
    ) + geom_bar(stat="identity")
    
    figure1 <- ggplotly(figure1)
    
    print(figure1)
    
    #### End #### 
    
  })
  
  output$description1 <- renderText({
    
    #### Create "print" objects ####
    
    printGoal <- input$goal
    printSelect1 <- input$select1
    printSelect2 <- input$select2
    
    #### End #### 
    
    #### Write Description 1 ####
    
    if(printGoal=="Plan A"){
      policyDescription <- paste("Under the selected plan, the federal government sends states block grants of ", "<blockAmount>", " in exchange for the state reducing tuition among eligible students to ",  printSelect1, ". States can repurpose leftover funds after enacting the policy. Eligible students whose tuition is already below the selected threshold are not affected.", sep="")
    }
    
    if(printGoal=="Plan B"){
      policyDescription <- paste("Under the selected plan, the federal government sends states block grants of ", "<blockAmount>", " in exchange for the state reducing tuition among eligible students at eligible public institutions by ", printSelect1, ". States can repurpose leftover funds after enacting the policy.", sep="")
    }
    
    if(printGoal=="Plan C"){
      policyDescription <- paste("Under the selected plan, the federal government sends states block grants of ", "<blockAmount>", " in exchange for the state increasing grants among eligible students at eligible public institutions such that student debt decreases by ", printSelect1, ". States can repurpose leftover funds after enacting the policy. Parent PLUS is included in the debt.", sep="")
    }
    
    if(printGoal=="Plan D"){
      policyDescription <- paste("Under the selected plan, the federal government sends states block grants of ", "<blockAmount>", " in exchange for the state increasing grants among eligible students at eligible public institutions such that net price (cost of attendance minus grants) does not exceed ", printSelect1, " of family income. States can repurpose leftover funds after enacting the policy. Eligible students whose net price is already below the selected threshold are not affected.", sep="")
    }
    
    if(printGoal=="Plan E"){
      policyDescription <- paste("Under the selected plan, the federal government matches state funding for public institutions at a ratio of ", printSelect2, " per state dollar until the two combine for ", printSelect1, " of revenue (with the rest coming from tuition). States can repurpose leftover funds after enacting the policy.", sep="")
    }
    
    if(printGoal=="Plan F"){
      policyDescription <- paste("Under the selected plan, the federal government sends colleges block grants of ", "<blockAmount>", " in exchange for the college charging tuition among eligible students using the following pricing policy:", printSelect1, ". Colleges can repurpose leftover funds after enacting the policy.", sep="")
    }
    
    if(printGoal=="Plan G"){
      policyDescription <- paste("Under the selected plan, each eligible student receiving federal grants will see their federal grants increase by", printSelect1, ". This includes the Pell Grant, FSEOG, and TEACH Grants. It does not include federal work study.", sep="")
    }
    
    #### End #### 
    
    #### Print Description 1 #### 
    
    print(policyDescription)
    
    #### End #### 
    
  })
  
}))