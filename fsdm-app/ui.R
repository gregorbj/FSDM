#ui.R
#Author: Brian Gregor, Oregon Systems Analytics LLC
#Copyright: 2016, Oregon Department of Transportation 2016
#License: Apache 2


#LOAD RESOURCES
#--------------
#Packages
library(shiny)
library(shinyBS)
library(DT)
library(plotly)
library(tidyverse)
library(DiagrammeR)
#Function to support text area inputs
textareaInput <- function(id, label, value="", rows=5, cols=40, class="form-control"){
  tags$div(
    class="form-group shiny-input-container",
    tags$label('for'=id,label),
    tags$textarea(id=id,class=class,rows=rows,cols=cols,value))
}


#SHINY UI FUNCTION
#-----------------
shinyUI(
  navbarPage(
    "Logic Laboratory",
    
    # 0) Introduction Screen ======================
    tabPanel("Introduction", source("./intropage.R")), # intro page document
        ),
      )
    ),
    
    #Build a Model Screen
    #--------------------
    navbarMenu( "1) Build a Model",
                tabPanel( "User Information",
                          sidebarLayout(
                            sidebarPanel(
                              h4("User Information"),
                              hr(),
                              p("The user information entered below is used to attribute model creation and editing."),
                              textInput("firstName", "First Name"),
                              textInput("lastName", "Last Name"),
                              textInput("organization", "Organization")
                            ),
                            mainPanel(
                              
                            )
                          )
                ),
                tabPanel( "Select Model",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Select Model"),
                              hr(),
                              radioButtons(
                                inputId = "modelAction", 
                                label = "Model Action",
                                choices = list("Create New Model From Scratch" = "newModel",
                                               "Create New Model From Copy" = "copyModel",
                                               "Edit Existing Model" = "editModel",
                                               "Run Existing Model Without Editing" = "runModel")
                              ),
                              conditionalPanel(
                                condition = "input.modelAction == 'newModel' || input.modelAction == 'copyModel'",
                                textInput("modelName", "Model Name", "")
                              ),
                              bsAlert(
                                "nonameAlert"
                              ),
                              bsAlert(
                                "duplicateModel"
                              ),
                              bsAlert(
                                "noAuthorInfo"
                              ),
                              conditionalPanel(
                                condition = "input.modelAction == 'copyModel' || input.modelAction == 'editModel' || input.modelAction == 'runModel'",
                                uiOutput("selectModelFile")
                              ),
                              conditionalPanel(
                                condition = "input.modelAction == 'copyModel'",
                                checkboxInput("copyScenarios", "Copy scenarios too?")
                              ),
                              actionButton("startModeling", "Start Working on Model")
                            ),
                            mainPanel(
                              h4("Model Name: ", textOutput("modelName", inline = TRUE)),
                              h4("Parent Model: ", textOutput("modelParent", inline = TRUE)),
                              h4("Created: ", textOutput("modelCreated", inline = TRUE)),
                              h4("Last Edited: ", textOutput("modelEdited", inline = TRUE)),
                              h4("Attribution History", verbatimTextOutput("modelAttribution"))
                            )
                          )
                ),
                tabPanel( "Edit Concepts",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Edit Concepts"),
                              hr(),
                              textInput("conceptName", "Concept Name"),
                              textInput("varName", "Concept Label"),
                              textareaInput("conceptDesc", "Concept Description"),
                              textInput("minValue", "Minimum Value"),
                              textInput("maxValue", "Maximum Value"),
                              textareaInput("valuesDesc", "Values Description"),
                              textInput("conceptGroup", "Concept Group"),
                              conditionalPanel(
                                condition = "input.modelAction != 'runModel'",
                                wellPanel(
                                  actionButton("addConcept", "New"),
                                  actionButton("updateConcept", "Update"),
                                  actionButton("deleteConcept", "Delete"),
                                  actionButton("undoConceptAction", "Undo"),
                                  bsAlert(
                                    "duplicateConceptName"
                                  ),
                                  bsAlert(
                                    "duplicateConceptVariable"
                                  )
                                )
                              )
                            ),
                            mainPanel(
                              tabPanel("Concepts", DT::dataTableOutput("conceptsTable"), value = "table")
                            )
                          )
                          
                ),
                tabPanel( "Edit Relations",
                          sidebarLayout(
                            sidebarPanel(
                              tabsetPanel(
                                tabPanel(
                                  title = "Edit Relations",
                                  br(),
                                  uiOutput("selectCausalGroup"),
                                  uiOutput("selectAffectedGroup"),
                                  uiOutput("selectCausalConcept"),
                                  uiOutput("selectAffectedConcept"),
                                  selectInput(inputId = "causalDirection", 
                                              label = "Causal Direction", 
                                              choices = c("" ,"Positive", "Negative")),
                                  selectInput(inputId = "causalStrength", 
                                              label = "Causal Strength", 
                                              choices = c("", "VL", "L", "ML", "M", "MH", "H", "VH")),
                                  textareaInput("causalDesc", "Causal Description"),
                                  conditionalPanel(
                                    condition = "input.modelAction != 'runModel'",
                                    wellPanel(
                                      actionButton("updateRelation", "Update"),
                                      actionButton("deleteRelation", "Delete"),
                                      actionButton("undoRelationAction", "Undo")
                                    )
                                  )
                                ),
                                tabPanel(
                                  title = "Relations Graph Format",
                                  br(),
                                  selectInput(inputId = "graphOrientation",
                                              label =  "Graph Orientation",
                                              choices = c("Landscape", "Portrait"),
                                              selected = "Portrait"),
                                  selectInput(inputId = "graphLayout",
                                              label = "Graph Layout",
                                              choices = c("Left-to-Right", "Top-to-Bottom"),
                                              selected = "Top-to-Bottom"),
                                  selectInput(inputId = "nodeShape",
                                              label = "Node Shape",
                                              choices = c("box", "oval", "circle"),
                                              selected = "box"),
                                  selectInput(inputId = "edgeLabel",
                                              label = "Edge Label",
                                              choices = c("label", "value"),
                                              selected = "Level"),
                                  actionButton(inputId = "saveRelationsGraph",
                                               label = "Save Graph")
                                )
                              )
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel(
                                  title = "Relations Graph",
                                  grVizOutput('relations_graph', width = "100%", height = "800px")
                                ),
                                tabPanel(
                                  title = "Relations Map",
                                  plotOutput("relations_map")
                                )
                              )
                            )
                          )
                ),
                tabPanel( "Save Model",
                          conditionalPanel(
                            condition = "input.modelAction == 'runModel'",
                            wellPanel(
                              h4("Run Only Mode"),
                              p("Saving is disabled because program is in run only mode.")
                            )
                          ),
                          conditionalPanel(
                            condition = "input.modelAction != 'runModel'",
                            wellPanel(
                              h4("Save Model Edits"),
                              p("Pressing the ", strong("Save Model"), " button will save the save the edited model, overwriting the Concepts and Relations files and updating the status file. The notes entered into the notes text area will overwrite any notes since the last save in this session."),
                              textareaInput("modelNotes", "Model Notes"),
                              actionButton("saveModel", "Save Model")
                            )
                          )
                )
    ),
    
    #Create Scenarios Screen
    #-----------------------
    navbarMenu("2) Create Scenarios",
               tabPanel("Select Scenario",
                        sidebarLayout(
                          sidebarPanel(
                            br(),
                            radioButtons(
                              inputId = "scenarioAction",
                              label = "Scenario Action",
                              choices = list("Create New Scenario From Scratch" = "newScenario",
                                             "Create New Scenario From Copy" = "copyScenario",
                                             "Edit Existing Scenario" = "editScenario")
                              ),
                            conditionalPanel(
                              condition = "input.scenarioAction == 'newScenario' || input.scenarioAction == 'copyScenario'",
                              textInput("scenarioName", "Scenario Name", "")
                              ),
                            bsAlert(
                              "noscenarioAlert"
                              ),
                            conditionalPanel(
                              condition = "input.scenarioAction == 'copyScenario' || input.scenarioAction == 'editScenario'",
                              uiOutput("selectScenarioFile")
                              ),
                            actionButton("startScenario", "Start Working on Scenario")
                            ),
                          mainPanel(
                            h4("Scenario Name: ", textOutput("scenarioName", inline = TRUE)),
                            h4("Parent Scenario: ", textOutput("scenarioParent", inline = TRUE)),
                            h4("Model Name: ", textOutput("scenarioModelName", inline = TRUE)),
                            h4("Created: ", textOutput("scenarioCreated", inline = TRUE)),
                            h4("Last Edited: ", textOutput("scenarioEdited", inline = TRUE)),
                            h4("Validated: ", textOutput("scenarioValidated", inline = TRUE))
                          )
                        )
               ),
               tabPanel("Edit Scenario Values",
                        sidebarLayout(
                          sidebarPanel(
                            br(),
                            h4("Concept: ", textOutput("scenarioConcept", inline = TRUE)),
                            hr(),
                            textInput("conceptStartValue", "Concept Starting Value"),
                            textInput("conceptStartChange", "Concept Starting Change"),
                            textareaInput("conceptValuesDescription", "Concept Values Description"),
                            actionButton("updateScenario", "Update"),
                            actionButton("undoScenarioAction", "Undo"),
                            actionButton("validateScenario", "Validate and Save")
                           ),
                          mainPanel(
                            DT::dataTableOutput("scenarioTable"),
                            value = "table"
                            )
                          )
                        )
    ),

    #Run the Model Screen
    #--------------------
    tabPanel(
      "3) Run the Model",
      titlePanel("Run the Model"),
      sidebarLayout(
        sidebarPanel(
          h4("List Model Scenarios"),
          actionButton("listScenarios", "List Scenarios"),
          hr(),
          h4("Scenarios that have been validated and may be run"),
          uiOutput("selectScenariosToRun"),
          hr(),
          h4("Scenarios that must be validated before they can be run"),
          verbatimTextOutput("invalidScenarios"),
          hr(),
          p("Pressing the ", strong("Run Model"), " button will run the model for all the scenarios that are checked in the list above. Outputs will be saved in the respective scenario directories."),
          actionButton("runModel", "Run Model"),
          # actionButton("resetRun", "Reset"),
          actionButton("revalidate", "Revalidate Scenarios")
        ),
        mainPanel(
        )
      )
    ),
    
    #Analyze Results Screen
    #----------------------
    tabPanel(
      "4) Analyze Results",
      titlePanel("Analyze Results"),
      sidebarLayout(
        sidebarPanel(
          actionButton("listRunScenarios", "Update Scenario Selection Set"),
          hr(),
          uiOutput("selectScenarioPlot1"),          
          uiOutput("selectScenarioPlot2"),
          uiOutput("selectVarsToPlot"),
          hr(),
          downloadButton("download_plot", label = "Download Image"),
          downloadButton("download_data", label = "Download Scenario Data"),
          textInput("analysisSaveName", "Analysis Save Name", ""),
          bsAlert(
            "noAnalysisNameAlert"
          )          
        ),
        mainPanel(
          plotlyOutput("resultsPlot")
        )
      )
    )
    
  ))