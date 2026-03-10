
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
    "Economic impact"
    # , 
    # "State funding"
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
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices1a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices1b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = choices1c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices1d, 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = choices1e, 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = choices1f, 
           "[Plan G] Increase federal grants to students by X% " = choices1g
    )
    
  })
  
  choices2 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices2a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices2b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = choices2c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices2d, 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = choices2e, 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = choices2f, 
           "[Plan G] Increase federal grants to students by X% " = choices2g
    )
    
  })
  
  choices3 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices3a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices3b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = choices3c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices3d, 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = choices3e, 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = choices3f, 
           "[Plan G] Increase federal grants to students by X% " = choices3g
    )
    
  })
  
  choices4 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices4a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices4b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = choices4c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices4d, 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = choices4e, 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = choices4f, 
           "[Plan G] Increase federal grants to students by X% " = choices4g
    )
    
  })
  
  choices5 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices5a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices5b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = choices5c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices5d, 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = choices5e, 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = choices5f, 
           "[Plan G] Increase federal grants to students by X% " = choices5g
    )
    
  })
  
  choices6 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices6a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices6b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = choices6c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices6d, 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = choices6e, 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = choices6f, 
           "[Plan G] Increase federal grants to students by X% " = choices6g
    )
    
  })
  
  choices7 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices7a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices7b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = choices7c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices7d, 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = choices7e, 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = choices7f, 
           "[Plan G] Increase federal grants to students by X% " = choices7g
    )
    
  })
  
  choices8 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices8a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices8b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = choices8c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices8d, 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = choices8e, 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = choices8f, 
           "[Plan G] Increase federal grants to students by X% " = choices8g
    )
    
  })
  
  choices9 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices9a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices9b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = choices9c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices9d, 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = choices9e, 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = choices9f, 
           "[Plan G] Increase federal grants to students by X% " = choices9g
    )
    
  })
  
  choices10 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices10a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices10b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = choices10c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices10d, 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = choices10e, 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = choices10f, 
           "[Plan G] Increase federal grants to students by X% " = choices10g
    )
    
  })
  
  choices11 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices11a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices11b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = choices11c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices11d, 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = choices11e, 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = choices11f, 
           "[Plan G] Increase federal grants to students by X% " = choices11g
    )
    
  })
  
  #### End #### 
  
  #### Set question visibilities ####
  
  visibility1 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X",
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
    "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%", 
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income",
    "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue", 
    "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy", 
    "[Plan G] Increase federal grants to students by X% "
  )
  
  visibility2 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X", 
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%",
    "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%", 
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income",
    "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue", 
    "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy", 
    "[Plan G] Increase federal grants to students by X% "
  )
  
  visibility3 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X", 
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
    "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%",
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income",
    "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue", 
    "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy", 
    "[Plan G] Increase federal grants to students by X% "
  )
  
  visibility4 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X", 
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
    "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%", 
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income",
    "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue", 
    "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy", 
    "[Plan G] Increase federal grants to students by X% "
  )
  
  visibility5 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X", 
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
    "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%", 
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income",
    "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue",
    "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy", 
    "[Plan G] Increase federal grants to students by X% "
  )
  
  visibility6 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X", 
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
    "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%", 
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income",
    "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue", 
    "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy",
    "[Plan G] Increase federal grants to students by X% "
  )
  
  visibility7 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X", 
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
    "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%", 
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income",
    "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue", 
    "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy", 
    "[Plan G] Increase federal grants to students by X% "
  )
  
  visibility8 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X", 
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
    "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%", 
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income",
    "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue", 
    "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy", 
    "[Plan G] Increase federal grants to students by X% "
  )
  
  visibility9 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X", 
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
    "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%", 
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income",
    "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue", 
    "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy", 
    "[Plan G] Increase federal grants to students by X% "
  )
  
  visibility10 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X", 
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
    "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%", 
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income",
    "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue", 
    "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy", 
    "[Plan G] Increase federal grants to students by X% "
  )
  
  visibility11 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X", 
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
    "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%", 
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income",
    "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue", 
    "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy", 
    "[Plan G] Increase federal grants to students by X% "
  )
  
  #### End #### 
  
  #### Set questions ####
  
  question1 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "To what amount does tuition and fees among eligible students decrease?",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "By what percentage does tuition and fees among eligible students decrease?", 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = "By what percentage does student debt among eligible students decrease?", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "To what maximum percentage of family income is the net price among eligible students reduced?", 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = "To what minimum percentage of total revenue are federal and state funds increased?", 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = "What pricing policy is required among participating colleges?", 
           "[Plan G] Increase federal grants to students by X% " = "By what percentage do total federal grants among eligible students increase?"
    )
    
  })
  
  question2 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "For each $1 in the federal block grant, how much does a participating state need to match?",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "For each $1 in the federal block grant, how much does a participating state need to match?", 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = "For each $1 in the federal block grant, how much does a participating state need to match?", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "For each $1 in the federal block grant, how much does a participating state need to match?", 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = "For each $1 in state funding for public institutions, how much does the federal government match?", 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = "Skipped", 
           "[Plan G] Increase federal grants to students by X% " = "Skipped"
    )
    
  })
  
  question3 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "Is student eligibility limited on the basis of enrollment intensity?",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "Is student eligibility limited on the basis of enrollment intensity?", 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = "Is student eligibility limited on the basis of enrollment intensity?", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "Is student eligibility limited on the basis of enrollment intensity?", 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = "Skipped", 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = "Is student eligibility limited on the basis of enrollment intensity?", 
           "[Plan G] Increase federal grants to students by X% " = "Skipped"
    )
    
  })
  
  question4 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "Is student eligibility limited on a financial basis?",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "Is student eligibility limited on a financial basis?", 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = "Is student eligibility limited on a financial basis?", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "Is student eligibility limited on a financial basis?", 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = "Skipped", 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = "Is student eligibility limited on a financial basis?", 
           "[Plan G] Increase federal grants to students by X% " = "Skipped"
    )
    
  })
  
  question5 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "Is student eligibility limited to in-state students?",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "Is student eligibility limited to in-state students?", 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = "Is student eligibility limited to in-state students?", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "Is student eligibility limited to in-state students?", 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = "Skipped", 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = "Does the policy only apply to in-state students?", 
           "[Plan G] Increase federal grants to students by X% " = "Skipped"
    )
    
  })
  
  question6 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "Skipped",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "Skipped", 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = "Skipped", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "Skipped", 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = "Skipped", 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = "Is institutional eligibility limited to a certain control?", 
           "[Plan G] Increase federal grants to students by X% " = "Is institutional eligibility limited to a certain control?"
    )
    
  })
  
  question7 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "Is institutional eligibility limited to a certain level?",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "Is institutional eligibility limited to a certain level?", 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = "Is institutional eligibility limited to a certain level?", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "Is institutional eligibility limited to a certain level?", 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = "Is institutional eligibility limited to a certain level?", 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = "Is institutional eligibility limited to a certain level?", 
           "[Plan G] Increase federal grants to students by X% " = "Is institutional eligibility limited to a certain level?"
    )
    
  })
  
  question8 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "Should it be assumed that states that declined to participate in ACA’s Medicaid expansion will decline to participate in this program?",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "Should it be assumed that states that declined to participate in ACA’s Medicaid expansion will decline to participate in this program?", 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = "Should it be assumed that states that declined to participate in ACA’s Medicaid expansion will decline to participate in this program?", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "Should it be assumed that states that declined to participate in ACA’s Medicaid expansion will decline to participate in this program?", 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = "Should it be assumed that states that declined to participate in ACA’s Medicaid expansion will decline to participate in this program?", 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = "Skipped", 
           "[Plan G] Increase federal grants to students by X% " = "Skipped"
    )
    
  })
  
  question9 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "What level of increase in a state’s annual appropriations for higher education would be too large to participate in the program?",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "What level of increase in a state’s annual appropriations for higher education would be too large to participate in the program?", 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = "What level of increase in a state’s annual appropriations for higher education would be too large to participate in the program?", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "What level of increase in a state’s annual appropriations for higher education would be too large to participate in the program?", 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = "What level of increase in a state’s annual appropriations for higher education would be too large to participate in the program?", 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = "What level of decrease in a college’s total revenue would be too large to participate in the program?", 
           "[Plan G] Increase federal grants to students by X% " = "Skipped"
    )
    
  })
  
  question10 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "What figure would you like displayed in the Figure View?",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "What figure would you like displayed in the Figure View?", 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = "What figure would you like displayed in the Figure View?", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "What figure would you like displayed in the Figure View?", 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = "What figure would you like displayed in the Figure View?", 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = "What figure would you like displayed in the Figure View?", 
           "[Plan G] Increase federal grants to students by X% " = "What figure would you like displayed in the Figure View?"
    )
    
  })
  
  question11 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "What table would you like displayed in the Table View?",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "What table would you like displayed in the Table View?", 
           "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%" = "What table would you like displayed in the Table View?", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "What table would you like displayed in the Table View?", 
           "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue" = "What table would you like displayed in the Table View?", 
           "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy" = "What table would you like displayed in the Table View?", 
           "[Plan G] Increase federal grants to students by X% " = "What table would you like displayed in the Table View?"
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
    
    if(printGoal=="[Plan A] Fed-state partnership: Reduce tuition and fees to $X"){
      policyDescription <- paste("Under the selected plan, the federal government sends states block grants of ", "<blockAmount>", " in exchange for the state reducing tuition among eligible students to ",  printSelect1, ". The tuition target reflects full-time enrollment; for part-time students, the target is half. States can repurpose leftover funds after enacting the policy. Eligible students whose tuition is already below the selected threshold are not affected.", sep="")
    }
    
    if(printGoal=="[Plan B] Fed-state partnership: Reduce tuition and fees by X%"){
      policyDescription <- paste("Under the selected plan, the federal government sends states block grants of ", "<blockAmount>", " in exchange for the state reducing tuition among eligible students at eligible public institutions by ", printSelect1, ". States can repurpose leftover funds after enacting the policy.", sep="")
    }
    
    if(printGoal=="[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%"){
      policyDescription <- paste("Under the selected plan, the federal government sends states block grants of ", "<blockAmount>", " in exchange for the state increasing grants among eligible students at eligible public institutions such that student debt decreases by ", printSelect1, ". States can repurpose leftover funds after enacting the policy. Parent PLUS is included in the debt.", sep="")
    }
    
    if(printGoal=="[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income"){
      policyDescription <- paste("Under the selected plan, the federal government sends states block grants of ", "<blockAmount>", " in exchange for the state increasing grants among eligible students at eligible public institutions such that net price (cost of attendance minus grants) does not exceed ", printSelect1, " of family income. The tuition target reflects full-time enrollment; for part-time students, the target is half. States can repurpose leftover funds after enacting the policy. Eligible students whose net price is already below the selected threshold are not affected.", sep="")
    }
    
    if(printGoal=="[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue"){
      policyDescription <- paste("Under the selected plan, the federal government matches state funding for public institutions at a ratio of ", printSelect2, " per state dollar until the two combine for ", printSelect1, " of revenue (with the rest coming from tuition). States can repurpose leftover funds after enacting the policy.", sep="")
    }
    
    if(printGoal=="[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy"){
      policyDescription <- paste("Under the selected plan, the federal government sends colleges block grants of ", "<blockAmount>", " in exchange for the college charging tuition among eligible students using the following pricing policy:", printSelect1, ". Colleges can repurpose leftover funds after enacting the policy.", sep="")
    }
    
    if(printGoal=="[Plan G] Increase federal grants to students by X% "){
      policyDescription <- paste("Under the selected plan, each eligible student receiving federal grants will see their federal grants increase by", printSelect1, ". This includes the Pell Grant, FSEOG, and TEACH Grants. It does not include federal work study.", sep="")
    }
    
    #### End #### 
    
    #### Print Description 1 #### 
    
    print(policyDescription)
    
    #### End #### 
    
  })
  
}))