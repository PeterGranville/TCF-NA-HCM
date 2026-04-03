
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
  
  #### Choice list 1 ####
  
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
    "No tuition charged",
    "No unmet need",
    "Net price capped at 10% family income"
  )
  
  choices1f <- choices1e
  
  choices1g <- choices1b
  
  choices1h <- c(
    "All support", 
    "New support"
  )
  
  #### End #### 
  
  #### Choice list 2 ####
  
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
    "$5,000 per eligible FTE", 
    "$10,000 per eligible FTE", 
    "$15,000 per eligible FTE"
  )
  
  choices2f <- choices2e
  
  choices2g <- c(
    "Skipped"
  )
  
  choices2h <- c(
    "5%",
    "10%", 
    "20%"
  )
  
  #### End #### 
  
  #### Choice list 3 ####
  
  choices3a <- c(
    "No restriction based on enrollment intensity",
    "Restricted to students enrolled full-time"
  )
  
  choices3b <- choices3a
  
  choices3c <- choices3a
  
  choices3d <- choices3a
  
  choices3e <- choices3a
  
  choices3f <- choices3a
  
  choices3g <- c(
    "Skipped"
  )
  
  choices3h <- c(
    "$1 federal for every $0.50 state", 
    "$1 federal for every $1 state", 
    "$1 federal for every $2 state"
  )
  
  #### End #### 
  
  #### Choice list 4 ####
  
  choices4a <- c(
    "No means testing",
    "Pell Grant recipients only"
  )
  
  choices4b <- choices4a
  
  choices4c <- choices4a
  
  choices4d <- choices4a
  
  choices4e <- choices4a
  
  choices4f <- choices4a
  
  choices4g <- c(
    "Skipped"
  )
  
  choices4h <- c(
    "Skipped"
  )
  
  #### End #### 
  
  #### Choice list 5 ####
  
  choices5a <- c(
    "Yes",
    "No"
  )
  
  choices5b <- choices5a
  
  choices5c <- choices5a
  
  choices5d <- choices5a
  
  choices5e <- choices5a
  
  choices5f <- choices5a
  
  choices5g <- c(
    "Skipped"
  )
  
  choices5h <- c(
    "Skipped"
  )
  
  #### End #### 
  
  #### Choice list 6 ####
  
  choices6a <- c(
    "Yes", 
    "No"
  )
  
  choices6b <- choices6a
  
  choices6c <- choices6a
  
  choices6d <- choices6a
  
  choices6e <- c(
    "Skipped"
  )
  
  choices6f <- c(
    "Public only", 
    "Public and nonprofit only", 
    "All controls"
  )
  
  choices6g <- choices6f
  
  choices6h <- c(
    "Skipped"
  )
  
  #### End #### 
  
  #### Choice list 7 ####
  
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
  
  choices7h <- choices7a
  
  #### End #### 
  
  #### Choice list 8 ####
  
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
  
  choices8h <- choices8a
  
  #### End #### 
  
  #### Choice list 9 ####
  
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
  
  choices9h <- c(
    "Skipped"
  )
  
  #### End #### 
  
  #### Choice list 10 ####
  
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
  
  choices10g <- choices10f # Also missing 2 and 6 
  
  choices10h <- choices10a 
  
  #### End #### 
  
  #### Choice list 11 ####
  
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
  
  choices11h <- choices11a
  
  #### End #### 
  
  #### Set choice lists ####
  
  choices1 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices1a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices1b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = choices1c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices1d, 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = choices1e, 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = choices1f, 
           "[Plan G] Increase federal grants to students by X%" = choices1g, 
           "[Plan H] Federal match for state support for public higher education" = choices1h
    )
    
  })
  
  choices2 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices2a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices2b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = choices2c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices2d, 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = choices2e, 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = choices2f, 
           "[Plan G] Increase federal grants to students by X%" = choices2g, 
           "[Plan H] Federal match for state support for public higher education" = choices2h
    )
    
  })
  
  choices3 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices3a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices3b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = choices3c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices3d, 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = choices3e, 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = choices3f, 
           "[Plan G] Increase federal grants to students by X%" = choices3g, 
           "[Plan H] Federal match for state support for public higher education" = choices3h
    )
    
  })
  
  choices4 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices4a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices4b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = choices4c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices4d, 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = choices4e, 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = choices4f, 
           "[Plan G] Increase federal grants to students by X%" = choices4g, 
           "[Plan H] Federal match for state support for public higher education" = choices4h
    )
    
  })
  
  choices5 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices5a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices5b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = choices5c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices5d, 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = choices5e, 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = choices5f, 
           "[Plan G] Increase federal grants to students by X%" = choices5g, 
           "[Plan H] Federal match for state support for public higher education" = choices5h
    )
    
  })
  
  choices6 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices6a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices6b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = choices6c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices6d, 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = choices6e, 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = choices6f, 
           "[Plan G] Increase federal grants to students by X%" = choices6g, 
           "[Plan H] Federal match for state support for public higher education" = choices6h
    )
    
  })
  
  choices7 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices7a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices7b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = choices7c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices7d, 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = choices7e, 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = choices7f, 
           "[Plan G] Increase federal grants to students by X%" = choices7g, 
           "[Plan H] Federal match for state support for public higher education" = choices7h
    )
    
  })
  
  choices8 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices8a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices8b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = choices8c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices8d, 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = choices8e, 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = choices8f, 
           "[Plan G] Increase federal grants to students by X%" = choices8g, 
           "[Plan H] Federal match for state support for public higher education" = choices8h
    )
    
  })
  
  choices9 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices9a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices9b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = choices9c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices9d, 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = choices9e, 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = choices9f, 
           "[Plan G] Increase federal grants to students by X%" = choices9g, 
           "[Plan H] Federal match for state support for public higher education" = choices9h
    )
    
  })
  
  choices10 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices10a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices10b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = choices10c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices10d, 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = choices10e, 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = choices10f, 
           "[Plan G] Increase federal grants to students by X%" = choices10g, 
           "[Plan H] Federal match for state support for public higher education" = choices10h
    )
    
  })
  
  choices11 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices11a,
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices11b, 
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = choices11c, 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices11d, 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = choices11e, 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = choices11f, 
           "[Plan G] Increase federal grants to students by X%" = choices11g, 
           "[Plan H] Federal match for state support for public higher education" = choices11h
    )
    
  })
  
  #### End #### 
  
  # #### Make alternative defaults ####
  # 
  # selected10 <- reactive({
  # 
  #   optionNum <- 1 # I may turn this to 2 later 
  #   optionNumNoState <- 1 # I may turn this to 3 later 
  #   
  #   switch(input$goal,
  #          "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = choices10a[optionNum],
  #          "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = choices10b[optionNum],
  #          "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = choices10c[optionNum],
  #          "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = choices10d[optionNum],
  #          "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = choices10e[optionNum],
  #          "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = choices10f[optionNumNoState], # No state role 
  #          "[Plan G] Increase federal grants to students by X%" = choices10g[optionNumNoState], # No state role 
  #          "[Plan H] Federal match for state support for public higher education" = choices10h[optionNum]
  #   )
  #   
  #   rm(optionNum, optionNumNoState)
  # 
  # })
  # 
  # #### End #### 
  
  #### Set question visibilities ####
  
  visibility1 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X",
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
    "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%", 
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income", 
    "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy", 
    "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy", 
    "[Plan G] Increase federal grants to students by X%", 
    "[Plan H] Federal match for state support for public higher education"
  )
  
  visibility2 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X",
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
    "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%", 
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income", 
    "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy", 
    "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy",
    # "[Plan G] Increase federal grants to students by X%",
    "[Plan H] Federal match for state support for public higher education"
  )
  
  visibility3 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X",
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
    "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%", 
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income", 
    "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy", 
    "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy",
    # "[Plan G] Increase federal grants to students by X%",
    "[Plan H] Federal match for state support for public higher education"
  )
  
  visibility4 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X",
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
    "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%", 
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income", 
    "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy", 
    "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy"
    # ,
    # "[Plan G] Increase federal grants to students by X%",
    # "[Plan H] Federal match for state support for public higher education"
  )
  
  visibility5 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X",
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
    "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%", 
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income", 
    "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy", 
    "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy"
    # ,
    # "[Plan G] Increase federal grants to students by X%",
    # "[Plan H] Federal match for state support for public higher education"
  )
  
  visibility6 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X",
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
    "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%", 
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income", 
    # "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy", 
    "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy",
    "[Plan G] Increase federal grants to students by X%"
    # ,
    # "[Plan H] Federal match for state support for public higher education"
  ) 
  
  visibility7 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X",
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
    "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%", 
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income", 
    "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy", 
    "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy",
    "[Plan G] Increase federal grants to students by X%",
    "[Plan H] Federal match for state support for public higher education"
  )
  
  visibility8 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X",
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
    "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%", 
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income", 
    "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy", 
    # "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy",
    # "[Plan G] Increase federal grants to students by X%",
    "[Plan H] Federal match for state support for public higher education"
  )
  
  visibility9 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X",
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
    "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%", 
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income", 
    "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy", 
    "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy"
    # ,
    # "[Plan G] Increase federal grants to students by X%",
    # "[Plan H] Federal match for state support for public higher education"
  )
  
  visibility10 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X",
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
    "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%", 
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income", 
    "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy", 
    "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy",
    "[Plan G] Increase federal grants to students by X%",
    "[Plan H] Federal match for state support for public higher education"
  )
  
  visibility11 <- c(
    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X",
    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
    "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%", 
    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income", 
    "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy", 
    "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy",
    "[Plan G] Increase federal grants to students by X%",
    "[Plan H] Federal match for state support for public higher education"
  )
  
  #### End #### 
  
  #### Set questions ####
  
  question1 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "To what amount will tuition and fees among eligible students decrease?",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "By what percentage will tuition and fees among eligible students decrease?",
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = "By what percentage will unmet need among eligible students decrease?", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "To what maximum percentage of family income will the net price among eligible students decrease?", 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = "What pricing policy must eligible public colleges in participating states adopt?", 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = "What pricing policy must participating colleges adopt?", 
           "[Plan G] Increase federal grants to students by X%" = "By what percentage will total federal grants among eligible students increase?", 
           "[Plan H] Federal match for state support for public higher education" = "Does the federal government match all state support or only match new state support?"
    )
    
  })
  
  question2 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "For each $1 in the federal block grant, how much will a participating state need to match?",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "For each $1 in the federal block grant, how much will a participating state need to match?", 
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = "For each $1 in the federal block grant, how much will a participating state need to match?", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "For each $1 in the federal block grant, how much will a participating state need to match?", 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = "What is the amount of the flat subsidy?", 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = "What is the amount of the flat subsidy?", 
           "[Plan G] Increase federal grants to students by X%" = "Skipped", 
           "[Plan H] Federal match for state support for public higher education" = "By what percentage will state support be assumed to increase?"
    )
    
  })
  
  question3 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "Is student eligibility limited on the basis of enrollment intensity?",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "Is student eligibility limited on the basis of enrollment intensity?", 
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = "Is student eligibility limited on the basis of enrollment intensity?", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "Is student eligibility limited on the basis of enrollment intensity?", 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = "Does the policy only apply to students enrolled full-time?", 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = "Does the policy only apply to students enrolled full-time?", 
           "[Plan G] Increase federal grants to students by X%" = "Skipped", 
           "[Plan H] Federal match for state support for public higher education" = "By what ratio will the federal government match state support?"
    )
    
  })
  
  question4 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "Is student eligibility limited on a financial basis?",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "Is student eligibility limited on a financial basis?", 
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = "Is student eligibility limited on a financial basis?", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "Is student eligibility limited on a financial basis?", 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = "Is student eligibility limited on a financial basis?", 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = "Is student eligibility limited on a financial basis?", 
           "[Plan G] Increase federal grants to students by X%" = "Skipped", 
           "[Plan H] Federal match for state support for public higher education" = "Skipped"
    )
    
  })
  
  question5 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "Is student eligibility limited to in-state students?",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "Is student eligibility limited to in-state students?", 
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = "Is student eligibility limited to in-state students?", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "Is student eligibility limited to in-state students?", 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = "Does the policy only apply to in-state students?", 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = "Does the policy only apply to in-state students?", 
           "[Plan G] Increase federal grants to students by X%" = "Skipped", 
           "[Plan H] Federal match for state support for public higher education" = "Skipped"
    )
    
  })
  
  question6 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "Should federal funds per FTE be bounded, so that the value rises no higher than the value for 15th-highest state and falls no lower than the value for the 35th-highest state?",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "Should federal funds per FTE be bounded, so that the value rises no higher than the value for 15th-highest state and falls no lower than the value for the 35th-highest state?", 
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = "Should federal funds per FTE be bounded, so that the value rises no higher than the value for 15th-highest state and falls no lower than the value for the 35th-highest state?", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "Should federal funds per FTE be bounded, so that the value rises no higher than the value for 15th-highest state and falls no lower than the value for the 35th-highest state?", 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = "Skipped", 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = "Is institutional eligibility limited to certain controls?", 
           "[Plan G] Increase federal grants to students by X%" = "Is institutional eligibility limited to certain controls?", 
           "[Plan H] Federal match for state support for public higher education" = "Skipped"
    )
    
  })
  
  question7 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "Is institutional eligibility limited to a certain level?",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "Is institutional eligibility limited to a certain level?", 
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = "Is institutional eligibility limited to a certain level?", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "Is institutional eligibility limited to a certain level?", 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = "Is institutional eligibility limited to a certain level?", 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = "Is institutional eligibility limited to a certain level?", 
           "[Plan G] Increase federal grants to students by X%" = "Is institutional eligibility limited to a certain level?", 
           "[Plan H] Federal match for state support for public higher education" = "Is institutional eligibility limited to a certain level?"
    )
    
  })
  
  question8 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "Should it be assumed that states that declined to participate in ACA’s Medicaid expansion will decline to participate in this program?",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "Should it be assumed that states that declined to participate in ACA’s Medicaid expansion will decline to participate in this program?", 
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = "Should it be assumed that states that declined to participate in ACA’s Medicaid expansion will decline to participate in this program?", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "Should it be assumed that states that declined to participate in ACA’s Medicaid expansion will decline to participate in this program?", 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = "Should it be assumed that states that declined to participate in ACA’s Medicaid expansion will decline to participate in this program?", 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = "Skipped", 
           "[Plan G] Increase federal grants to students by X%" = "Skipped", 
           "[Plan H] Federal match for state support for public higher education" = "Should it be assumed that states that declined to participate in ACA’s Medicaid expansion will decline to participate in this program?"
    )
    
  })
  
  question9 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "What level of increase in a state’s annual appropriations for higher education would be too large to participate in the program?",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "What level of increase in a state’s annual appropriations for higher education would be too large to participate in the program?", 
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = "What level of increase in a state’s annual appropriations for higher education would be too large to participate in the program?", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "What level of increase in a state’s annual appropriations for higher education would be too large to participate in the program?", 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = "What level of increase in a state’s annual appropriations for higher education would be too large to participate in the program?", 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = "What level of decrease in a college’s total revenue would be too large to participate in the program?", 
           "[Plan G] Increase federal grants to students by X%" = "Skipped", 
           "[Plan H] Federal match for state support for public higher education" = "Skipped"
    )
    
  })
  
  question10 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "What figure would you like displayed in the Figure View?",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "What figure would you like displayed in the Figure View?", 
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = "What figure would you like displayed in the Figure View?", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "What figure would you like displayed in the Figure View?", 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = "What figure would you like displayed in the Figure View?", 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = "What figure would you like displayed in the Figure View?", 
           "[Plan G] Increase federal grants to students by X%" = "What figure would you like displayed in the Figure View?", 
           "[Plan H] Federal match for state support for public higher education" = "What figure would you like displayed in the Figure View?"
    )
    
  })
  
  question11 <- reactive({
    
    switch(input$goal, 
           "[Plan A] Fed-state partnership: Reduce tuition and fees to $X" = "What table would you like displayed in the Table View?",
           "[Plan B] Fed-state partnership: Reduce tuition and fees by X%" = "What table would you like displayed in the Table View?", 
           "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%" = "What table would you like displayed in the Table View?", 
           "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income" = "What table would you like displayed in the Table View?", 
           "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy" = "What table would you like displayed in the Table View?", 
           "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy" = "What table would you like displayed in the Table View?", 
           "[Plan G] Increase federal grants to students by X%" = "What table would you like displayed in the Table View?", 
           "[Plan H] Federal match for state support for public higher education" = "What table would you like displayed in the Table View?"
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
                  choices=choices10()
                  # ,
                  # selected=selected10()
                  )
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
  #### Return policy index                   ####
  ###############################################
  
  # #### Return policy index ####
  # 
  # output$policyIndex <- renderText({
  # 
  #   printGoal <- input$goal
  #   printSelect1 <- input$select1
  #   printSelect2 <- input$select2
  #   printSelect3 <- input$select3
  #   printSelect4 <- input$select4
  #   printSelect5 <- input$select5
  #   printSelect6 <- input$select6
  #   printSelect7 <- input$select7
  #   printSelect8 <- input$select8
  #   printSelect9 <- input$select9
  #   printSelect10 <- input$select10
  #   printSelect11 <- input$select11
  #   
  #   if(substr(printGoal, 7, 7) %in% c("G")){printSelect2 <- "Skipped"}
  #   if(substr(printGoal, 7, 7) %in% c("G")){printSelect3 <- "Skipped"}
  #   if(substr(printGoal, 7, 7) %in% c("G", "H")){printSelect4 <- "Skipped"}
  #   if(substr(printGoal, 7, 7) %in% c("G", "H")){printSelect5 <- "Skipped"}
  #   if(substr(printGoal, 7, 7) %in% c("E", "H")){printSelect6 <- "Skipped"}
  #   if(substr(printGoal, 7, 7) %in% c("F", "G")){printSelect8 <- "Skipped"}
  #   if(substr(printGoal, 7, 7) %in% c("G", "H")){printSelect9 <- "Skipped"}
  # 
  #   inputCombos <- read.csv(
  #     "Simulation results/Input combos.csv",
  #     header=TRUE,
  #     check.names=FALSE
  #   ) %>% filter(
  #     `Plan` == substr(printGoal, 7, 7),
  #     `Choice1` == printSelect1,
  #     `Choice2` == printSelect2,
  #     `Choice3` == printSelect3,
  #     `Choice4` == printSelect4,
  #     `Choice5` == printSelect5,
  #     `Choice6` == printSelect6,
  #     `Choice7` == printSelect7,
  #     `Choice8` == printSelect8,
  #     `Choice9` == printSelect9
  #   )
  # 
  #   print(paste("The selected policy index is ", inputCombos$`Policy index`[1], ". The selected policy is ", printGoal, ". The selected plan is ", substr(printGoal, 7, 7), ".", sep=""))
  # 
  # })
  # 
  # #### End #### 
  
  ###############################################
  #### Visuals generation                    ####
  ###############################################
  
  #### Write function to load data ####
  
  loadData <- function(printGoal1, figureOrTable, selection10, selection11, forDescription, finalDigit){
    
    if(printGoal1=="[Plan A] Fed-state partnership: Reduce tuition and fees to $X"){
      modelChar <- "A"
    } 
    if(printGoal1=="[Plan B] Fed-state partnership: Reduce tuition and fees by X%"){
      modelChar <- "B"
    } 
    if(printGoal1=="[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%"){
      modelChar <- "C"
    } 
    if(printGoal1=="[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income"){
      modelChar <- "D"
    } 
    if(printGoal1=="[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy"){
      modelChar <- "E"
    } 
    if(printGoal1=="[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy"){
      modelChar <- "F"
    } 
    if(printGoal1=="[Plan G] Increase federal grants to students by X%"){
      modelChar <- "G"
    }  
    if(printGoal1=="[Plan H] Federal match for state support for public higher education"){
      modelChar <- "H"
    } 
    
    if(figureOrTable=="Figure"){
      if(selection10=="Institutional participation map"){
        dataNum <- paste("1-", finalDigit, sep="") # Special rule to break up large datasets 
      } 
      if(selection10=="State participation map"){
        dataNum <- "2"
      } 
      if(selection10=="Net price percentiles"){
        dataNum <- "3"
      } 
      if(selection10=="Educational attainment"){
        dataNum <- "4"
      } 
      if(selection10=="Economic impact"){
        dataNum <- "5"
      } 
      if(selection10=="State funding"){
        dataNum <- "6"
      }
    }else{
      if(selection11=="Student participation"){
        dataNum <- "7"
      }
      if(selection11=="Student debt"){
        dataNum <- "8"
      } 
      if(selection11=="Degrees and certificates"){
        dataNum <- "9"
      } 
      if(selection11=="Government cost"){
        dataNum <- "10"
      } 
    }
    
    if(forDescription==TRUE){
      if(modelChar %in% c("A", "B", "C", "D", "E", "H")){
        dataNum <- "6"
      }else{
        dataNum <- "10"
      }
    }
    
    tempDF <- read.csv(
      paste("Simulation results/", modelChar, "-", dataNum, ".csv", sep=""), 
      header=TRUE, 
      check.names=FALSE
    )
    
    return(tempDF)
    
  }
  
  #### End #### 
  
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
    
    if(substr(printGoal, 7, 7) %in% c("G")){printSelect2 <- "Skipped"}
    if(substr(printGoal, 7, 7) %in% c("G")){printSelect3 <- "Skipped"}
    if(substr(printGoal, 7, 7) %in% c("G", "H")){printSelect4 <- "Skipped"}
    if(substr(printGoal, 7, 7) %in% c("G", "H")){printSelect5 <- "Skipped"}
    if(substr(printGoal, 7, 7) %in% c("E", "H")){printSelect6 <- "Skipped"}
    if(substr(printGoal, 7, 7) %in% c("F", "G")){printSelect8 <- "Skipped"}
    if(substr(printGoal, 7, 7) %in% c("G", "H")){printSelect9 <- "Skipped"}
    
    #### End #### 
    
    #### Load data and filter for selected simulation ####
    
    inputCombos <- read.csv(
      "Simulation results/Input combos.csv", 
      header=TRUE, 
      check.names=FALSE
    ) %>% filter(
      `Plan` == substr(printGoal, 7, 7), 
      `Choice1` == printSelect1, 
      `Choice2` == printSelect2, 
      `Choice3` == printSelect3, 
      `Choice4` == printSelect4, 
      `Choice5` == printSelect5,
      `Choice6` == printSelect6,
      `Choice7` == printSelect7, 
      `Choice8` == printSelect8, 
      `Choice9` == printSelect9
    )
    
    lastDigit <- substr(
      inputCombos$`Policy index`[1], 
      nchar(inputCombos$`Policy index`[1]), # Last digit 
      nchar(inputCombos$`Policy index`[1])  # Last digit 
    )
      
    figureData <- loadData(
      printGoal1 = printGoal, 
      figureOrTable = "Figure", 
      selection10 = printSelect10, 
      selection11 = printSelect11, 
      forDescription = FALSE, 
      finalDigit = lastDigit
    )
    rm(lastDigit)
    
    figureData <- figureData %>% filter(
      `Policy index` == inputCombos$`Policy index`[1]
    )
    rm(inputCombos)

    #### End #### 
    
    #### Set color palette ####
    
    colorValues1 <- c(
      "#2F2F2F", # (charcoal)
      "#4C78A8", # (steel blue)
      "#F58518", # (warm orange)
      "#54A24B", # (leaf green)
      "#E45756"   # (muted red)
    ) 
    
    colorValues2 <- c(
      "#7F8C8D", # (gray)
      "#A1C9F4", # (pastel blue)
      "#FFB482", # (peach)
      "#8DE5A1", # (mint green)
      "#FF9F9B"  # (soft coral)
    )
    
    colorValues3 <- c(  
      "#7F8C8D", # (gray)
      "#1F77B4", # (blue)
      "#FF7F0E", # (orange)
      "#2CA02C", # (green)
      "#D62728"  # (red)
    )
    
    colorValues <- colorValues1
    
    #### End #### 
    
    #### Institutional participation map [1] #### 
    
    if(printSelect10=="Institutional participation map"){
      
      figureData <- figureData %>% filter(
        duplicated(`UNITID`)==FALSE
      ) %>% mutate(
        `Participant` = ifelse(
          is.na(`Participant`), 
          "No", 
          `Participant`
        )
      )
      
      collegeData <- read.csv(
        "College info.csv",
        header=TRUE
      )
      figureData <- left_join(x=figureData, y=collegeData, by="UNITID")
      
      figureData <- figureData %>% mutate(
        `Control` = ifelse(
          `CONTROL`==1, 
          "Public", 
          ifelse(
            `CONTROL`==2, 
            "Nonprofit",
            ifelse(
              `CONTROL`==3, 
              "For-profit", 
              "NA"
            )
          )
        )
      )
      
      figureData$hover <- paste(
        "Name: ", figureData$INSTNM, '\n',
        "Location: ", paste(figureData$CITY, ", ", figureData$STABBR, sep=""), '\n',
        "Control: ", figureData$`Control`, '\n',
        "Participant status: ", figureData$`Participant`, '\n',
        "Total participating students: ", comma(figureData$`Total participating students`), '\n',
        "Share participating: ", percent(figureData$`Share participating`, accuracy=0.1), '\n',
        "Total funds received by students: ", dollar(round(figureData$`Total funds received by students`, -3)), '\n',
        "Increased expected degrees and certificates: ", comma(round(figureData$`Increased expected degrees and certificates`)), 
        sep=""
      )
      
      figure1 <- plot_geo(
        figureData, 
        locationmode='USA-states', 
        width=900, 
        height=500
      ) %>% add_trace(type="scatter", mode="markers", lat=~LATITUDE, lon=~LONGITUD, text=~hover, color=~`Participant`, colors = colorValues[c(1,3)]) %>% layout(geo = list(scope = 'usa'))
    }
    
    #### End #### 
    
    #### State participation map [2] #### 
    
    # Won't come from F or G 
    
    if(printSelect10=="State participation map"){
      
      figureData <- figureData %>% mutate(
        `Participant` = ifelse(
          is.na(`Participant`), 
          "No", 
          `Participant`
        )
      ) %>% mutate(
        participant_num = ifelse(`Participation status` == "Yes", 1, 0)
      )
      
      figureData$hover <- paste(
        "Name: ", figureData$`State`, '\n',
        "Participation status: ", figureData$`Participation status`, '\n',
        "Federal block grant: ", dollar(round(figureData$`Federal block grant`, -6)), '\n',
        "Total state contributions: ", dollar(round(figureData$`Total state contributions`, -6)), '\n',
        "Total state contributions as a share of education appropriations: ", percent(figureData$`Total state contributions as a share of education appropriations`, accuracy=0.1), '\n',
        "Number of participating students: ", comma(round(figureData$`Number of participating students`)), '\n',
        "Share of students at public institutions participating: ", percent(figureData$`Share of students at public institutions participating`, accuracy=0.1),
        sep=""
      )
      
      figure1 <- plot_ly(
        width=800, 
        height=500
      ) %>%
        add_trace(
          data = filter(figureData, `Participation status` == "Yes"),
          type = "choropleth",
          locations = ~STABBR,
          locationmode = "USA-states",
          z = 1,
          text = ~hover,
          name = "Yes",
          colorscale = list(c(0, colorValues[3]), c(1, colorValues[3])),
          showscale = FALSE, 
          marker = list(
            line = list(
              color = "black",  
              width = 1  
            )
          )
        ) %>%
        add_trace(
          data = filter(figureData, `Participation status` == "No"),
          type = "choropleth",
          locations = ~STABBR,
          locationmode = "USA-states",
          z = 1,
          text = ~hover,
          name = "No",
          colorscale = list(c(0, colorValues2[1]), c(1, colorValues2[1])),
          showscale = FALSE, 
          marker = list(
            line = list(
              color = "black",
              width = 1   
            )
          )
        ) %>%
        layout(
          geo = list(scope = "usa")
        )
      
    }
    
    #### End #### 
    
    #### Net price percentiles [3] #### 
    
    if(printSelect10=="Net price percentiles"){
      
      figureData <- figureData %>% pivot_longer(
        cols=c(`Pre-policy net price`, `Post-policy net price`), 
        names_to="Policy status", 
        values_to="Net price"
      ) %>% mutate(
        `Participant2` = ifelse(
          `Participant`=="Yes", 
          "Participant", 
          "Not a participant"
        )
      )
      
      figureData <- figureData %>% mutate(
        `Policy status` = factor(
          `Policy status`, 
          levels=c(
            "Pre-policy net price", 
            "Post-policy net price"
          )
        )
      ) %>% mutate(
        `Percentile` = paste(`Percentile`, "th", sep="")
      )
      
      figureData$hover <- paste(
        "Participant status: ", figureData$`Participant2`, '\n',
        "Policy status: ", figureData$`Policy status`, '\n',
        "Net price percentile: ", figureData$`Percentile`, '\n',
        "Net price: ", dollar(figureData$`Net price`, accuracy=1),
        sep=""
      )
      
      figure1 <- ggplot(
        data=figureData,
        mapping=aes(
          x=`Percentile`, 
          y=`Net price`, 
          fill=`Policy status`, 
          text=`hover`
        )
      ) + geom_bar(
        stat="identity", 
        position=position_dodge()
      ) + facet_grid(
        . ~ `Participant2`
      ) + scale_y_continuous(
        labels=dollar_format()
      ) + scale_x_discrete(
        breaks=c("10th", "20th", "30th", "40th", "50th", "60th", "70th", "80th", "90th"), 
        name="Net price percentile"
      ) + scale_fill_manual(
        values=colorValues[c(2, 3)]
      )
      
      figure1 <- ggplotly(figure1, tooltip="text", width=900, height=500)
      
    }
    
    #### End #### 
    
    #### Educational attainment [4] #### 
    
    if(printSelect10=="Educational attainment"){
      
      figureData <- figureData %>% filter(`STABBR` != "DC")
      
      figureData <- figureData %>% rename(
        `Associate's or higher` = `Percentage point change, associate's or higher`, 
        `Bachelor's or higher` = `Percentage point change, bachelor's or higher`
      ) %>% pivot_longer(
        cols=c(`Associate's or higher`, `Bachelor's or higher`), 
        names_to="Level", 
        values_to="Percentage point change in population attainment"
      )
      
      figureData <- figureData %>% mutate(
        `Percentage point change in population attainment` = `Percentage point change in population attainment` * 100
      ) %>% select(
        -(`State`)
      ) %>% rename(
        `State` = `STABBR`
      )
      
      figureData$hover <- paste(
        "State: ", figureData$`State`, '\n',
        "Attainment level: ", figureData$`Level`, '\n',
        "Percentage point change in population attainment: ", round(figureData$`Percentage point change in population attainment`, 3), "pp",
        sep=""
      )
      
      figure1 <- ggplot(
        data=figureData, 
        mapping=aes(
          x=`Percentage point change in population attainment`,
          y=`State`, 
          fill=`Level`, 
          text=`hover`
        )
      ) + geom_bar(
        stat="identity", 
        position=position_dodge()
      ) + scale_y_discrete(
        limits=rev
      ) + scale_fill_manual(
        values=colorValues[c(2, 5)]
      ) + scale_x_continuous(
        label = scales::label_number(suffix = "pp")
      )
    
      figure1 <- ggplotly(figure1, tooltip="text", width=800, height=1500)
      
    }
    
    #### End #### 
    
    #### Economic impact [5] #### 
    
    if(printSelect10=="Economic impact"){
      
      annualCost <- figureData$`Annual cost`[1]
      annualTaxes <- figureData$`Increase in annual taxes`[1]

      figureData <- data.frame(
        `Year` = (1:99)
      ) 
      figureData <- figureData %>% mutate(
        `Cost minus cumulative tax revenue from increased earnings` = annualCost - (annualTaxes * `Year`), 
        `Indicator` = rep(0)
      )
      
      counter <- 0 
      for(i in (1:99)){
        if(figureData$`Cost minus cumulative tax revenue from increased earnings`[i] < 0){
          counter <- counter + 1
          figureData$`Indicator`[i] <- counter
        }
      }
      rm(counter, i)
      
      figureData <- figureData %>% filter(
        `Indicator` <= 5
      ) %>% rename(
        `Years since participation` = `Year`
      )
      
      figureData$hover <- paste(
        "Years since participation: ", figureData$`Years since participation`, '\n',
        "Cost minus cumulative tax revenue from increased earnings: ", dollar(round(figureData$`Cost minus cumulative tax revenue from increased earnings`, -6), accuracy=1),
        sep=""
      )
      
      figure1 <- ggplot(
        data=figureData, 
        mapping=aes(
          x=`Years since participation`, 
          y=`Cost minus cumulative tax revenue from increased earnings`, 
          text=`hover`
        )
      ) + geom_point() + geom_line() + geom_hline(
        yintercept=0
      ) + scale_y_continuous(
        labels=dollar_format(accuracy=1)
      ) + scale_fill_manual(
        values=colorValues[c(1)]
      )
      
      figure1 <- ggplotly(figure1, tooltip="text", width=800, height=500)
      
    }
    
    #### End #### 
    
    #### State funding [6] #### 
    
    # Won't come from F or G 
    
    if(printSelect10=="State funding"){
      
      figureData <- figureData %>% mutate(
        `Overflow amount` = ifelse(
          is.na(`Overflow amount`), 
          0, 
          `Overflow amount`
        )
      )
      
      figureData <- figureData %>% pivot_longer(
        cols=c(`Federal block grant`, `Total state contributions`, `Overflow amount`), 
        names_to="Category", 
        values_to="Amount"
      )
      
      figureData <- figureData %>% filter(
        `STABBR` != "DC"
      ) %>% select(
        -(`State`)
      ) %>% rename(
        `State` = `STABBR`
      )
      
      figureData$hover <- paste(
        "State: ", figureData$`State`, '\n',
        "Category: ", figureData$`Category`, '\n',
        "Amount: ", dollar(round(figureData$`Amount`, -6), accuracy=1),
        sep=""
      )
      
      figure1 <- ggplot(
        data=figureData, 
        mapping=aes(
          x=`Amount`,
          y=`State`, 
          fill=`Category`, 
          text=`hover`
        )
      ) + geom_bar(
        stat="identity", 
        position=position_dodge()
      ) + scale_y_discrete(
        limits=rev
      ) + scale_x_continuous(
        labels=dollar_format(accuracy=1)
      ) + scale_fill_manual(
        values=colorValues[c(2, 3, 4)]
      )
      
      figure1 <- ggplotly(figure1, tooltip="text", width=800, height=1500)
      
    }
    
    #### End #### 
    
    #### Print Figure 1 ####
    
    print(figure1)
    
    #### End #### 
    
  })
  
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
    
    if(substr(printGoal, 7, 7) %in% c("G")){printSelect2 <- "Skipped"}
    if(substr(printGoal, 7, 7) %in% c("G")){printSelect3 <- "Skipped"}
    if(substr(printGoal, 7, 7) %in% c("G", "H")){printSelect4 <- "Skipped"}
    if(substr(printGoal, 7, 7) %in% c("G", "H")){printSelect5 <- "Skipped"}
    if(substr(printGoal, 7, 7) %in% c("E", "H")){printSelect6 <- "Skipped"}
    if(substr(printGoal, 7, 7) %in% c("F", "G")){printSelect8 <- "Skipped"}
    if(substr(printGoal, 7, 7) %in% c("G", "H")){printSelect9 <- "Skipped"}
    
    #### End #### 
    
    #### Load data and filter for selected simulation ####
    
    inputCombos <- read.csv(
      "Simulation results/Input combos.csv", 
      header=TRUE, 
      check.names=FALSE
    ) %>% filter(
      `Plan` == substr(printGoal, 7, 7), 
      `Choice1` == printSelect1, 
      `Choice2` == printSelect2, 
      `Choice3` == printSelect3, 
      `Choice4` == printSelect4, 
      `Choice5` == printSelect5,
      `Choice6` == printSelect6,
      `Choice7` == printSelect7, 
      `Choice8` == printSelect8, 
      `Choice9` == printSelect9
    )
    
    lastDigit <- substr(
      inputCombos$`Policy index`[1], 
      nchar(inputCombos$`Policy index`[1]), # Last digit 
      nchar(inputCombos$`Policy index`[1])  # Last digit 
    )
    
    tableData <- loadData(
      printGoal1 = printGoal, 
      figureOrTable = "Table", 
      selection10 = printSelect10, 
      selection11 = printSelect11, 
      forDescription = FALSE, 
      finalDigit = lastDigit
    )
    rm(lastDigit)
    
    tableData <- tableData %>% filter(
      `Policy index` == inputCombos$`Policy index`[1]
    )
    
    #### End #### 
    
    #### Student participation [7] ####
    #### End #### 
    
    #### Student debt [8] ####
    #### End #### 
    
    #### Degrees and certificates [9] ####
    #### End #### 
    
    #### Government cost [10] ####
    #### End #### 
    
    #### Print Table 1 ####
    
    table1 <- tableData %>% select(
      `Measure`, 
      `Value`
    )
    
    print(table1)
    
    #### End #### 

  })
  
  output$description1 <- renderText({
    
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
    
    if(substr(printGoal, 7, 7) %in% c("G")){printSelect2 <- "Skipped"}
    if(substr(printGoal, 7, 7) %in% c("G")){printSelect3 <- "Skipped"}
    if(substr(printGoal, 7, 7) %in% c("G", "H")){printSelect4 <- "Skipped"}
    if(substr(printGoal, 7, 7) %in% c("G", "H")){printSelect5 <- "Skipped"}
    if(substr(printGoal, 7, 7) %in% c("E", "H")){printSelect6 <- "Skipped"}
    if(substr(printGoal, 7, 7) %in% c("F", "G")){printSelect8 <- "Skipped"}
    if(substr(printGoal, 7, 7) %in% c("G", "H")){printSelect9 <- "Skipped"}
    
    #### End #### 
    
    #### Load file ####
    
    # descriptionData <- loadData(
    #   printGoal1 = printGoal, 
    #   figureOrTable = "Figure", 
    #   selection10 = printSelect10, 
    #   selection11 = printSelect11, 
    #   forDescription = TRUE
    # )
    # # Returns 6 for all letters except F and G, for which it returns 10
    # 
    # inputCombos <- read.csv(
    #   "Simulation results/Input combos.csv", 
    #   header=TRUE, 
    #   check.names=FALSE
    # ) %>% filter(
    #   `Plan` == substr(printGoal, 7, 7), 
    #   `Choice1` == printSelect1, 
    #   `Choice2` == printSelect2, 
    #   `Choice3` == printSelect3, 
    #   `Choice4` == printSelect4, 
    #   `Choice5` == printSelect5,
    #   `Choice6` == printSelect6,
    #   `Choice7` == printSelect7, 
    #   `Choice8` == printSelect8, 
    #   `Choice9` == printSelect9
    # )
    # 
    # descriptionData <- descriptionData %>% filter(
    #   `Policy index` == inputCombos$`Policy index`[1]
    # )
    # rm(inputCombos)
    
    #### End #### 
    
    #### Write Description 1 ####
    
    if(printGoal=="[Plan A] Fed-state partnership: Reduce tuition and fees to $X"){
      policyDescription <- paste("Under the selected plan, the federal government sends states block grants in exchange for the state reducing tuition among eligible students to ",  printSelect1, ". The tuition target reflects full-time enrollment; for part-time students, the target is half. States can repurpose leftover funds after enacting the policy. Eligible students whose tuition is already below the selected threshold are not affected.", sep="")
    }
    
    if(printGoal=="[Plan B] Fed-state partnership: Reduce tuition and fees by X%"){
      policyDescription <- paste("Under the selected plan, the federal government sends states block grants in exchange for the state reducing tuition among eligible students at eligible public institutions by ", printSelect1, ". States can repurpose leftover funds after enacting the policy.", sep="")
    }
    
    if(printGoal=="[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%"){
      policyDescription <- paste("Under the selected plan, the federal government sends states block grants in exchange for the state increasing grants among eligible students at eligible public institutions such that unmet need (cost of attendance minus grants and SAI) decreases by ", printSelect1, ". States can repurpose leftover funds after enacting the policy.", sep="")
    }
    
    if(printGoal=="[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income"){
      policyDescription <- paste("Under the selected plan, the federal government sends states block grants in exchange for the state increasing grants among eligible students at eligible public institutions such that net price (cost of attendance minus grants) does not exceed ", printSelect1, " of family income. The tuition target reflects full-time enrollment; for part-time students, the target is half. States can repurpose leftover funds after enacting the policy. Eligible students whose net price is already below the selected threshold are not affected.", sep="")
    }
    
    if(printGoal=="[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy"){
      policyDescription <- paste("Under the selected plan, the federal government sends states block grants in exchange for the state enacting the following pricing or aid policy for eligible students at eligible public institutions: ", printSelect1, ". States can repurpose leftover funds after enacting the policy.", sep="")
    }
    
    if(printGoal=="[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy"){
      policyDescription <- paste("Under the selected plan, the federal government sends colleges block grants in exchange for the college enacting the following pricing or aid policy for eligible students: ", printSelect1, ". Colleges can repurpose leftover funds after enacting the policy.", sep="")
    }
    
    if(printGoal=="[Plan G] Increase federal grants to students by X%"){
      policyDescription <- paste("Under the selected plan, each eligible student receiving federal grants will see their federal grants increase by ", printSelect1, ". This includes the Pell Grant, FSEOG, and TEACH Grants. It does not include federal work study.", sep="")
    }
    
    if(printGoal=="[Plan H] Federal match for state support for public higher education"){
      policyDescription <- paste("Under the selected plan, the federal government matches ", tolower(printSelect1), " from the state at the specified ratio. It is assumed that half the new funds will be allocated towards reducing net price, while the other half will be allocated towards operations.", sep="")
    }
    
    #### End #### 
    
    #### Print Description 1 #### 
    
    print(policyDescription)
    
    #### End #### 
    
  })
  
}))