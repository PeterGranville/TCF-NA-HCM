
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
                             ".shiny-output-error:before { visibility: visible; content: 'Please wait for the app to load.'; }"
                  ),
                  
                  #### End #### 
                  
                  #### Title panel ####
                  
                  titlePanel(
                    title=div(
                      h1("CAPSTONE Model"),
                      h3("Cost And Performance Simulation for Tertiary Outcomes & National Effects"), 
                      hr(),
                    ), 
                    windowTitle="CAPSTONE Model"
                  ),
                  
                  helpText("How would large-scale investments in higher education reduce financial barriers for students, improve degree attainment, and strengthen the economy? Explore these outcomes, and the policy design choices that affect those outcomes, using the CAPSTONE Model."), 
                  
                  helpText("To begin, select one of eight plans, each offering a different approach to federal investment. Next, choose the policy design choices in the dropdown menus that follow."), 
                  
                  helpText("The last two dropdown menus offer ten different ways to view outcomes of your selected plan and policy design choices. Use those menus and the 'Figure View' and 'Table View' tabs to see projected impacts."), 
                  
                  helpText("The CAPSTONE Model is built by synthesizing data from the Integrated Postsecondary Education Data System and the National Postsecondary Student Aid Study. It is open-source, meaning all our data and methods are transparent. Documentation of methods can be found", tags$a(href = "https://docs.google.com/document/d/18ebSr14Vw9eKj78IWp3HlYaHoz-RkBUIHW1CNxeJcV8/edit?usp=sharing", "here.")), 
                  hr(),
                  
                  #### End #### 
                  
                  sidebarLayout(
                    
                    sidebarPanel(
                      
                      #### Input panel #### 
                      
                      selectInput(inputId="goal", 
                                  label="Select a model goal:", 
                                  choices=c(
                                    "[Plan A] Fed-state partnership: Reduce tuition and fees to $X",
                                    "[Plan B] Fed-state partnership: Reduce tuition and fees by X%", 
                                    "[Plan C] Fed-state partnership: Increase grants to reduce unmet need by X%", 
                                    "[Plan D] Fed-state partnership: Increase grants to reduce net price to X% family income", 
                                    "[Plan E] Fed-state partnership: Government sends states flat per-FTE subsidy in exchange for X pricing policy", 
                                    "[Plan F] Fed-college partnership: Government sends colleges flat per-FTE subsidy in exchange for X pricing policy", 
                                    "[Plan G] Increase federal grants to students by X%", 
                                    "[Plan H] Federal match for state support for public higher education"
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
                      
                      #### Description #### 
                      
                      textOutput("description1"), 
                      br(), 
                      # textOutput("policyIndex"),
                      # br(),
                      
                      #### End #### 
                      
                      tabsetPanel(
                        tabPanel("Figure View", fluid=TRUE,
                                 
                                 #### Figure 1 #### 
                                 
                                 br(),
                                 plotlyOutput("figure1"), 
                                 br()
                                 
                                 #### End #### 
                        ),  
                        tabPanel("Table View", fluid=TRUE,
                                 
                                 #### Table 1 #### 
                                 
                                 br(),
                                 tableOutput("table1"), 
                                 br()
                                 
                                 #### End #### 
                                 
                        )
                      )
                    )
                  )
))

