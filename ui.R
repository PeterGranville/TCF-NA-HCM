
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
                  
                  titlePanel("Model V1"), 
                  
                  helpText("This is the first paragraph of description [WHY IT EXISTS]."), 
                  
                  helpText("This is the second paragraph of description [HOW TO USE]."),
                  
                  helpText("This is the third paragraph of description [METHODS]."),
                  
                  helpText("The tool may take a second to load after entering a new selection."),
                  
                  #### End #### 
                  
                  sidebarLayout(
                    
                    sidebarPanel(
                      
                      #### Input panel #### 
                      
                      selectInput(inputId="view", 
                                  label="Select a model type:", 
                                  choices=c(
                                    "Fed-State Model", 
                                    "Fed-College Model", 
                                    "Fed-Student Model"
                                  )
                      ),
                      uiOutput("selection1"), 
                      uiOutput("selection2"), 
                      uiOutput("selection3"), 
                      uiOutput("selection4")  
                      
                      #### End #### 
                      
                    ), 
                    
                    mainPanel(
                      
                      #### Output figure #### 
                      
                      h3(textOutput("titleFig")),
                      plotlyOutput("displayFig", width = "100%", height = "100%"),
                      br(),
                      
                      #### End #### 
                      
                      #### Output text ####
                      
                      tags$div(
                        style = "border: 2px dashed #acacab; background-color: #F7EFF5; padding: 10px; display: inline-block; width: 750px;",
                        textOutput("summary")
                      ),
                      br(),
                      br()
                      
                      #### End #### 
                      
                    )
                  )
))