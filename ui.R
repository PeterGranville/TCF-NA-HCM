
#### Setup #### 

library(shiny)
library(plotly)
library(shinythemes)

#### End #### 

shinyUI(fluidPage(theme = shinytheme("sandstone"),
                  
                  #### Error message #### 
                  
                  tags$head(tags$style(".shiny-output-error{color: grey;}")),
                  tags$style(type="text/css",
                             ".shiny-output-error { visibility: hidden; }",
                             ".shiny-output-error:before { visibility: visible; content: 'Please wait for the app to load or make another selection.'; }"
                  ),
                  
                  #### End #### 
                  
                  #### Title panel ####
                  
                  titlePanel("Model V2"), 
                  
                  helpText("This is the first iteration of the interactive cost model for a federal-state partnership, provided as a foundation for future iterations."), 
                  
                  helpText("In this first iteration, basic policy designs are provided that would have the federal government create funds to reach a certain policy goal. Future iterations will have more sophisticated designs, such as a federal-state partnership involving shared costs."), 
                  
                  helpText("To toggle across different program designs, use the dropdown menus on the left side of the screen. Some dropdowns have limited options while the results for more program designs are calculated. Future iterations of this model will have greater optionality for the user."),
                  
                  helpText("To view different outcomes from the selected program design, toggle between the tabs on the right side of the screen. Future iterations of this model will provide more statistics."),
                  
                  helpText("The model is built using a synthetic student-level dataset that combines data from IPEDS and information predicted using regressions via NPSAS and NCES Datalab. For details about these source, please refer to the Model V1 'readme' file."),
                  
                  helpText("The tool may take a second to load after entering a new selection."),
                  
                  #### End #### 
                  
                  sidebarLayout(
                    
                    sidebarPanel(
                      
                      #### Input panel #### 
                      
                      selectInput(inputId="goal", 
                                  label="Select a model goal:", 
                                  choices=c(
                                    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X", 
                                    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
                                    "[Plan C] Fed-state partnership: Increase grants to reduce student debt by X%", 
                                    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income", 
                                    "[Plan E] Fed-state partnership: Increase federal and state investment to equal X% of revenue", 
                                    "[Plan F] Fed-college partnership: Government sends colleges subsidy in exchange for X pricing policy", 
                                    "[Plan G] Increase federal grants to students by X%"
                                  )
                      ),
                      uiOutput("selection1"),
                      uiOutput("selection2"), 
                      uiOutput("selection3"), 
                      uiOutput("selection4"), 
                      uiOutput("selection5"), 
                      uiOutput("selection6"), 
                      uiOutput("selection7"), 
                      uiOutput("selection8"), 
                      uiOutput("selection9"), 
                      uiOutput("selection10"), 
                      uiOutput("selection11")
                      
                      #### End #### 
                      
                    ), 
                    
                    mainPanel(
                      
                      textOutput("description1"), 
                      
                      tabsetPanel(
                        tabPanel("Figure View", fluid=TRUE,
                                 
                                 #### Figure 1 #### 
                                 
                                 br(),
                                 plotlyOutput("figure1")
                                 
                                 #### End #### 
                        ),  
                        tabPanel("Table View", fluid=TRUE,
                                 
                                 #### Table 1 #### 
                                 
                                 br(),
                                 tableOutput("table1")
                                 
                                 #### End #### 
                                 
                        )
                      )
                    )
                  )
))

