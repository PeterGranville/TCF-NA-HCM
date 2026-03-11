
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
                  
                  helpText("Introductory text will go here."), 
                  
                  helpText("Introductory text will go here."), 
                  
                  helpText("Introductory text will go here."), 
                  
                  helpText("Introductory text will go here."), 
                  
                  helpText("Introductory text will go here."), 
                  
                  helpText("Introductory text will go here."), 
                  
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
                      br(), 
                      
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

